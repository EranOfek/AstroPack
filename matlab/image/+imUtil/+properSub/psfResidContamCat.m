function ContamCat = psfResidContamCat(Obj, Args)
    % Build the contaminator catalogue from the PSF-residual score map.
    %   Peaks in S_PSFresid are places where the difference image carries
    %   the shape the PSF reconstruction fails to represent. Forced
    %   photometry at each peak, with the R PSF on the R image and the
    %   template on D, gives the source flux and the residual flux in the
    %   same units, so their ratio is the residual as a fraction of the
    %   source. A peak is kept as a contaminator when it sits on a real R
    %   source and its fraction is no larger than the bulk of the R
    %   catalogue produces.
    % Input  : - A single element AstroZOGY with S_PSFresid and
    %            PSFresidTemplate populated by subtractionS.
    %          * ...,key,val,...
    %            'ResidThresh' - Sigma threshold for local maxima of
    %                   S_PSFresid. Default is 5.
    %            'BadPixDist' - Reject peaks this close to a NaN or NearEdge
    %                   pixel. The filter smears their influence outward, so
    %                   a peak beside one measures the defect. Default is 3.
    %            'MinSN_R' - Minimum R forced-photometry S/N. Below it the
    %                   ratio is not a fraction of anything. Default is 5.
    %            'RatioPrc' - Percentile of the residual fraction measured
    %                   at unselected Ref positions, used as the cut.
    %                   Selecting on S_PSFresid peaks biases the fraction up
    %                   by about 0.4 dex, so the threshold has to come from
    %                   the unselected sample. Default is 95.
    %            'MaxRatioCut' - Ceiling on that cut. At the default of 1 a
    %                   peak brighter than the source under it is never
    %                   treated as that source's residual. Default is 1.
    % Output : - An AstroCatalog with XPEAK, YPEAK, RA, Dec, SN_RESID,
    %            FLUX_R, FLUXERR_R, SN_R, FLUX_TEMPLATE, FLUXERR_TEMPLATE,
    %            SN_TEMPLATE and FLUX_RATIO. Empty catalogue if no peak
    %            survives.
    % Author : Ruslan Konno + Claude (Aug 2026)

    arguments
        Obj(1,1)
        Args.ResidThresh = 4;
        Args.BadPixDist  = 3;
        Args.MinSN_R     = 5;
        Args.RatioPrc    = 95;
        Args.MaxRatioCut = 1.0;
    end

    ContamCat = AstroCatalog();

    if isempty(Obj.S_PSFresid) || isempty(Obj.PSFresidTemplate)
        return
    end

    T        = Obj.PSFresidTemplate;
    HalfSize = (size(T,1)-1)./2;
    SizeIm   = size(Obj.Image);

    % Same outer-frame background the template derivation uses, so the fits
    % sit on the same zero point as the template.
    Border = true(2*HalfSize+1);
    Border(3:end-2, 3:end-2) = false;
    BorderSub = @(C) C - reshape(median(reshape(C(repmat(Border,1,1,size(C,3))), ...
                                 sum(Border(:)), []), 1, 'omitnan'), 1, 1, []);

    % Both kernels carry unit sum, so both fitted amplitudes are fluxes in
    % counts and their ratio is directly the residual fraction.
    Kt = double(reshape(T, [], 1));
    Kr = Obj.Ref.PSFData.getPSF('StampSize', [2*HalfSize+1, 2*HalfSize+1]);
    Kv = double(reshape(Kr./sum(Kr(:)), [], 1));

    Fit = @(Cube, K) (K.' * double(reshape(Cube, (2*HalfSize+1).^2, []))).' ./ sum(K.^2);

    %--- the threshold, from unselected Ref positions ---
    [R_RA, R_Dec] = Obj.Ref.CatData.getLonLat('rad');
    [SrcX, SrcY]  = Obj.WCS.sky2xy(R_RA./pi.*180, R_Dec./pi.*180);

    RefSel = isfinite(SrcX) & isfinite(SrcY) ...
           & SrcX > HalfSize+1 & SrcX < SizeIm(2)-HalfSize-1 ...
           & SrcY > HalfSize+1 & SrcY < SizeIm(1)-HalfSize-1;

    Xr = round(SrcX(RefSel));  Yr = round(SrcY(RefSel));

    CubeRefD = BorderSub(imUtil.cut.image2cutouts(Obj.Image,     Xr, Yr, HalfSize));
    CubeRefR = BorderSub(imUtil.cut.image2cutouts(Obj.Ref.Image, Xr, Yr, HalfSize));

    FitD_Ref = Fit(CubeRefD, Kt);
    FitR_Ref = Fit(CubeRefR, Kv);
    SNR_Ref  = FitR_Ref ./ (borderSigma(CubeRefR, Border, HalfSize) ./ sqrt(sum(Kv.^2)));

    RefRatio = FitD_Ref ./ FitR_Ref;
    RefOk    = isfinite(RefRatio) & RefRatio > 0 & SNR_Ref >= Args.MinSN_R;

    if ~any(RefOk)
        return
    end
    RatioCut = min(prctile(RefRatio(RefOk), Args.RatioPrc), Args.MaxRatioCut);

    %--- peaks ---
    LocMax = imUtil.sources.findLocalMax(Obj.S_PSFresid, 'Variance',1, ...
                                         'Threshold',Args.ResidThresh);
    if isempty(LocMax)
        return
    end

    Xm = LocMax(:,1);  Ym = LocMax(:,2);  SNm = LocMax(:,3);
    InFr = Xm > HalfSize+1 & Xm < SizeIm(2)-HalfSize-1 ...
         & Ym > HalfSize+1 & Ym < SizeIm(1)-HalfSize-1;
    Xm = round(Xm(InFr));  Ym = round(Ym(InFr));  SNm = SNm(InFr);

    % NaN and NearEdge pixels: the filter smears them outward, so a peak
    % beside one is measuring the defect rather than a residual.
    if ~isempty(Obj.MaskData) && ~Obj.MaskData.isemptyImage
        BadNear = imdilate(Obj.MaskData.findBit({'NaN','NearEdge'}, ...
                           'Method','any', 'OutType','mat'), ...
                           ones(2*Args.BadPixDist+1));
        IsBad = BadNear(sub2ind(SizeIm, Ym, Xm));
        Xm = Xm(~IsBad);  Ym = Ym(~IsBad);  SNm = SNm(~IsBad);
    end

    if isempty(Xm)
        return
    end

    CubeD = BorderSub(imUtil.cut.image2cutouts(Obj.Image,     Xm, Ym, HalfSize));
    CubeR = BorderSub(imUtil.cut.image2cutouts(Obj.Ref.Image, Xm, Ym, HalfSize));

    FluxD    = Fit(CubeD, Kt);
    FluxErrD = borderSigma(CubeD, Border, HalfSize) ./ sqrt(sum(Kt.^2));
    SN_D     = FluxD ./ FluxErrD;

    FluxR    = Fit(CubeR, Kv);
    FluxErrR = borderSigma(CubeR, Border, HalfSize) ./ sqrt(sum(Kv.^2));
    SN_R     = FluxR ./ FluxErrR;

    Keep = isfinite(FluxD) & isfinite(FluxR) & FluxR > 0 ...
         & (SN_R >= Args.MinSN_R) & (FluxD./FluxR <= RatioCut);

    if ~any(Keep)
        return
    end

    [PeakRA, PeakDec] = Obj.WCS.xy2sky(Xm(Keep), Ym(Keep));

    ContamCat = AstroCatalog( ...
        {cast([Xm(Keep), Ym(Keep), PeakRA(:), PeakDec(:), SNm(Keep), ...
               FluxR(Keep), FluxErrR(Keep), SN_R(Keep), ...
               FluxD(Keep), FluxErrD(Keep), SN_D(Keep), ...
               FluxD(Keep)./FluxR(Keep)], 'double')}, ...
        'ColNames', {'XPEAK','YPEAK','RA','Dec','SN_RESID', ...
                     'FLUX_R','FLUXERR_R','SN_R', ...
                     'FLUX_TEMPLATE','FLUXERR_TEMPLATE','SN_TEMPLATE', ...
                     'FLUX_RATIO'}, ...
        'ColUnits', {'pix','pix','deg','deg','','','','','','','',''});
end

function S = borderSigma(Cube, Border, HalfSize)
    % MAD of the cutout border, which BorderSub has already centred on zero.
    M = double(reshape(Cube, (2*HalfSize+1).^2, []));
    B = M(reshape(Border,[],1), :);
    S = 1.4826 .* median(abs(B - median(B,1)), 1).';
end