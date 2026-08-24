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
    %            'BadPixDist' - Reject peaks this close to a NaN pixel. The
    %                   filter smears their influence outward, so a peak
    %                   beside one measures the defect. Default is 3.
    %            'NearEdgeDist' - Same, for NearEdge. Kept tight because the
    %                   bit is inherited through the reference coadd and lands
    %                   far from any real border, so a wide dilation blanks
    %                   good sky. Default is 1.
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
    %            'MinKernelFrac' - Minimum fraction of the kernel's power that
    %                   must fall on the image for a fit to be kept. Peaks near
    %                   the border are fitted on the pixels that exist rather
    %                   than discarded, and this is what rules out the ones
    %                   with too little left to measure. Default is 0.5.
    % Output : - An AstroCatalog with XPEAK, YPEAK, RA, Dec, SN_RESID,
    %            FLUX_R, FLUXERR_R, SN_R, FLUX_TEMPLATE, FLUXERR_TEMPLATE,
    %            SN_TEMPLATE and FLUX_RATIO. Empty catalogue if no peak
    %            survives.
    % Author : Ruslan Konno + Claude (Aug 2026)

    arguments
        Obj(1,1)
        Args.ResidThresh = 4;
        Args.BadPixDist  = 3;
        Args.NearEdgeDist = 1;
        Args.MinSN_R     = 5;
        Args.RatioPrc    = 95;
        Args.MaxRatioCut = 1.5;
        Args.MinKernelFrac = 0.5;
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

    %--- the threshold, from unselected Ref positions ---
    [R_RA, R_Dec] = Obj.Ref.CatData.getLonLat('rad');
    [SrcX, SrcY]  = Obj.WCS.sky2xy(R_RA./pi.*180, R_Dec./pi.*180);

    RefSel = isfinite(SrcX) & isfinite(SrcY) ...
           & SrcX > HalfSize+1 & SrcX < SizeIm(2)-HalfSize-1 ...
           & SrcY > HalfSize+1 & SrcY < SizeIm(1)-HalfSize-1;

    Xr = round(SrcX(RefSel));  Yr = round(SrcY(RefSel));

    CubeRefD = BorderSub(imUtil.cut.image2cutouts(Obj.Image,     Xr, Yr, HalfSize));
    CubeRefR = BorderSub(imUtil.cut.image2cutouts(Obj.Ref.Image, Xr, Yr, HalfSize));

    FitD_Ref = fitPartial(CubeRefD, Kt, HalfSize);
    [FitR_Ref, ~, KpowR_Ref] = fitPartial(CubeRefR, Kv, HalfSize);
    SNR_Ref  = FitR_Ref ./ (borderSigma(CubeRefR, Border, HalfSize) ./ sqrt(KpowR_Ref));

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
    % Peaks only need to be on the image. Requiring the whole stamp to fit
    % blanks a band as wide as the stamp along every border, and a candidate
    % just inside it has its entire outward half-annulus un-inspectable -
    % which is how an 8 sigma residual 4 pix from a candidate went unseen.
    % image2cutouts pads with NaN and fitPartial normalises by the kernel
    % power covered, so MinKernelFrac decides when too little is left.
    Xm = round(Xm);  Ym = round(Ym);
    InFr = Xm >= 1 & Xm <= SizeIm(2) & Ym >= 1 & Ym <= SizeIm(1);
    Xm = Xm(InFr);  Ym = Ym(InFr);  SNm = SNm(InFr);

    % NaN and NearEdge both mark pixels a peak should not be measured on, but
    % they deserve different margins. A NaN is a real hole and the matched
    % filter smears its influence outward, so it keeps the wide dilation.
    % NearEdge is only a geometric band stamped on each sub-image at crop time
    % (imUtil.ccdsec.selectNearEdges, EdgeDist 13). A deep reference is a coadd
    % of coadds combined with OR, so the bit accumulates the edge bands of every
    % contributing crop and ends up far from any real border - on a test crop
    % 59.7% of the flagged pixels were more than 13 pixels from one.
    % imProc.stack.stitchCrops clears it for the same reason. It marks nothing
    % that smears, so it gets the minimum margin.
    if ~isempty(Obj.MaskData) && ~Obj.MaskData.isemptyImage
        BadNaN  = imdilate(Obj.MaskData.findBit({'NaN'}, ...
                           'Method','any', 'OutType','mat'), ...
                           ones(2*Args.BadPixDist+1));
        BadEdge = imdilate(Obj.MaskData.findBit({'NearEdge'}, ...
                           'Method','any', 'OutType','mat'), ...
                           ones(2*Args.NearEdgeDist+1));
        Lin   = sub2ind(SizeIm, Ym, Xm);
        IsBad = BadNaN(Lin) | BadEdge(Lin);
        Xm = Xm(~IsBad);  Ym = Ym(~IsBad);  SNm = SNm(~IsBad);
    end

    if isempty(Xm)
        return
    end

    CubeD = BorderSub(imUtil.cut.image2cutouts(Obj.Image,     Xm, Ym, HalfSize));
    CubeR = BorderSub(imUtil.cut.image2cutouts(Obj.Ref.Image, Xm, Ym, HalfSize));

    [FluxD, FracD, KpowD] = fitPartial(CubeD, Kt, HalfSize);
    FluxErrD = borderSigma(CubeD, Border, HalfSize) ./ sqrt(KpowD);
    SN_D     = FluxD ./ FluxErrD;

    [FluxR, FracR, KpowR] = fitPartial(CubeR, Kv, HalfSize);
    FluxErrR = borderSigma(CubeR, Border, HalfSize) ./ sqrt(KpowR);
    SN_R     = FluxR ./ FluxErrR;

    Keep = isfinite(FluxD) & isfinite(FluxR) & FluxR > 0 ...
         & (FracD >= Args.MinKernelFrac) & (FracR >= Args.MinKernelFrac) ...
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
    % Part of the ring is NaN for a stamp that overruns the image, so both
    % medians omit them rather than returning NaN for the whole column.
    M = double(reshape(Cube, (2*HalfSize+1).^2, []));
    B = M(reshape(Border,[],1), :);
    S = 1.4826 .* median(abs(B - median(B,1,'omitnan')), 1, 'omitnan').';
end

function [A, Frac, Kpow] = fitPartial(Cube, K, HalfSize)
    % Least squares amplitude over the pixels that exist. image2cutouts pads
    % beyond the image border with NaN, so a stamp that overruns still carries
    % every on-image pixel; normalising by the kernel power actually covered
    % keeps the amplitude a flux in counts rather than a fraction of one.
    % Frac is how much of the kernel the fit had to work with - for a template
    % whose flux sits well inside the stamp it stays at 1 until the peak is
    % nearly on the border.
    M = double(reshape(Cube, (2*HalfSize+1).^2, []));
    V = isfinite(M);
    M(~V) = 0;
    Kpow = sum((K.^2) .* V, 1).';
    A    = (K.' * M).' ./ Kpow;
    Frac = Kpow ./ sum(K.^2);
end