function [Template, Info] = psfResidTemplate(Obj, Args)
    % Measure the PSF-reconstruction residual kernel from a difference image.
    %   The reconstructed PSF represents the core but not the profile outside
    %   it. The missing flux does not vanish: it lands in the difference image
    %   as a residual at every persistent source, at a fixed offset and a
    %   fixed fraction of the source flux. No single instance is measurable
    %   and there is no way to pre-select the sources that show one, so the
    %   shape is measured by stacking cutouts on ALL isolated Ref sources and
    %   letting the ones without a residual dilute rather than bias the stack.
    % Input  : - A single element AstroZOGY, or AstroDiff, in which the
    %            difference Image, PSFData, WCS, New and Ref are populated,
    %            along with Ref.CatData and the ZpN/ZpR zeropoints.
    %          * ...,key,val,...
    %            'HalfSizePad' - Cutout half size, in pixels beyond the D PSF
    %                   half size, so the stamp is only just wider than the
    %                   PSF. Default is 3.
    %            'MinMag' - Reject Ref sources brighter than this. Bright
    %                   sources also ring, ringing is a different artifact
    %                   with its own filter, and a stack that mixes the two
    %                   measures neither. Default is 13.
    %            'MaxMag' - Reject Ref sources fainter than this, where the
    %                   residual is not measurable. Default is 16.
    %                   Both limits are field dependent and worth checking
    %                   against Info.NumUsed before trusting a template.
    %            'IsoRadius' - Reject a source with a comparable neighbour
    %                   this close, in pixels. Default is 20.
    %            'NbrMagMax' - What counts as a comparable neighbour when
    %                   IsoRelDeltaMag is empty: any Ref source brighter than
    %                   this. Requiring no neighbour of ANY magnitude is hopeless
    %                   at LAST source densities, since the Ref catalogue reaches
    %                   the limiting magnitude and those sources contribute
    %                   nothing. Default is 19.
    %            'IsoRelDeltaMag' - Isolation relative to the stacking star: a
    %                   neighbour counts only if it is brighter than the star's
    %                   own MAG_PSF plus this, i.e. if it adds more than
    %                   10^(-0.4*IsoRelDeltaMag) of the star's flux to the
    %                   flux-normalised cutout. A fixed NbrMagMax rejects bright
    %                   stars for neighbours too faint to matter, and tightens
    %                   whenever the Ref catalogue gets deeper: on the v5
    %                   references it cut the stack by ~35% and pushed the
    %                   template below significance on 12 of 39 crops, with the
    %                   residual itself unchanged (issue #1267). [] restores the
    %                   absolute NbrMagMax rule. Default is 2.5.
    %            'AlignOnMin' - Shift each cutout so its central minimum sits
    %                   at the centre. Cut positions are catalogue positions
    %                   rounded to whole pixels, so each cutout carries up to
    %                   half a pixel of misregistration, and the structure is
    %                   only a couple of pixels across. The over-subtracted
    %                   pixel is the highest contrast feature locatable in a
    %                   single cutout. Default is true.
    %            'AlignSearchHalf' - Search a (2n+1) box for that minimum, so
    %                   the shift is bounded and cannot drag a cutout onto an
    %                   unrelated feature further out. Default is 1.
    %            'ZeroCoreHalf' - Zero a (2n+1) square at the centre before
    %                   normalising. The core is where a real transient's own
    %                   PSF peaks, so removing it is what buys separation
    %                   between a residual and a point source. 0 disables.
    %                   Default is 1, which matches AlignSearchHalf so the
    %                   mask covers exactly the region the alignment forces
    %                   negative by construction.
    %            'MaxNumStars' - Cap on the number of sources stacked.
    %                   Default is 4000.
    %            'MinNumStars' - Below this, return an empty template.
    %                   Default is 50.
    %            'NormMethod' - What the template is normalised by.
    %                   'abs' - sum|T|, the total absolute residual. It cannot
    %                   cancel between the lobes of a two-lobed residual or
    %                   change sign, so the scale stays put when the balance
    %                   between the lobes shifts.
    %                   'net' - |sum T|, the net residual, as before issue
    %                   #1267. The net sum is the small difference of the two
    %                   lobes and becomes unstable when they nearly cancel.
    %                   Default is 'abs'.
    %            'RadiusSigma' - With NormMethod 'abs', MatchRadius is taken
    %                   from |T| over the pixels at least this many sigma from
    %                   zero (MAD error of the stack). |noise| never cancels, so
    %                   over the whole stamp it accumulates with area and drags
    %                   the radius out to the stamp edge. 0 uses every pixel.
    %                   Ignored for 'net'. Default is 2.
    % Output : - The template, or [] if it could not be measured. It has unit
    %            sum|T| (NormMethod 'abs') or unit |sum T| ('net'). A fitted
    %            amplitude is then directly a flux in counts: the total
    %            absolute residual flux, or the net residual flux.
    %          - A struct with NumSrc, NumInMag, NumIsolated, NumUsed,
    %            FluxFraction, Norm, CoreFraction, Scatter, CentroidOffset, X,
    %            Y and Reason. NumInMag and NumIsolated count the sources in
    %            the magnitude window and those also passing the isolation
    %            cut, so a template that fails can be traced to where its
    %            stars were lost.
    %            Template.*Norm is the physical template, in residual flux per
    %            unit source flux. FluxFraction is the total residual fraction
    %            including the masked core, a diagnostic rather than a scale
    %            factor. Reason is '' on success and
    %            says why otherwise.
    % Author : Ruslan Konno + Claude (Aug 2026)
    % Example: [T,I] = imProc.sub.psfResidTemplate(AD);
    %          S = AD.Fd .* imUtil.filter.filter2_fast(AD.Image, T);

    arguments
        Obj(1,1)
        Args.HalfSizePad          = 3;
        Args.MinMag               = 13;
        Args.MaxMag               = 17;
        Args.IsoRadius            = 20;
        Args.NbrMagMax            = 19;
        Args.IsoRelDeltaMag       = 2.5;
        Args.AlignOnMin logical   = true;
        Args.AlignSearchHalf      = 1;
        Args.ZeroCoreHalf         = 1;
        Args.MaxNumStars          = 4000;
        Args.MinNumStars          = 30;
        Args.MinPeakSN            = 5;
        
        Args.BlobThresh           = 2;
        Args.MinBlobArea          = 25;

        Args.RingWeight           = 1;
        Args.RingWidth            = 1;

        Args.RadiusPrc            = 90;
        Args.RadiusPad            = 1;
        Args.RadiusSigma          = 2;

        Args.NormMethod           = 'abs';
    end

    if ~any(strcmpi(Args.NormMethod, {'abs','net'}))
        error('psfResidTemplate:NormMethod', ...
              'NormMethod must be ''abs'' or ''net'', got ''%s''', Args.NormMethod);
    end
    NormAbs = strcmpi(Args.NormMethod, 'abs');

    Template = [];
    Info     = struct('NumSrc',0, 'NumInMag',0, 'NumIsolated',0, ...
                      'NumUsed',0, 'FluxFraction',NaN, 'Norm',NaN, ...
                      'CoreFraction',NaN, 'Scatter',NaN, 'BlobArea',NaN, ...
                      'RadiusPrc',NaN, 'MatchRadius',NaN, 'PeakSN',NaN, ...
                      'CentroidOffset',[NaN NaN], 'X',[], 'Y',[], 'Reason','');

    Image = Obj.Image;
    if isempty(Image) || isempty(Obj.Ref) || isempty(Obj.Ref.CatData) || ...
            Obj.Ref.CatData.sizeCatalog < 1
        Info.Reason = 'no image or no Ref catalogue';
        return
    end

    SizeIm   = size(Image);
    HalfSize = floor(size(Obj.PSFData.getPSF,1)./2) + Args.HalfSizePad;
    Cen      = HalfSize + 1;

    %--- Ref sources, in the difference-image pixel frame ---
    %  Positions go through RA/Dec and the D-image WCS rather than
    %  Ref.CatData.getXY. The Ref catalogue carries its own pixel frame, and
    %  on a LAST crop the two differ by hundreds of pixels, so the sky
    %  coordinates are the only reliable route into the frame the residuals
    %  live in.
    [R_RA, R_Dec] = Obj.Ref.CatData.getLonLat('rad');
    [SrcX, SrcY]  = Obj.WCS.sky2xy(R_RA./pi.*180, R_Dec./pi.*180);

    R_Flux = Obj.Ref.CatData.getCol('FLUX_PSF');
    R_Mag  = Obj.Ref.CatData.getCol('MAG_PSF');

    % Ref fluxes on the New zeropoint, so the fraction below is the residual
    % relative to the source as it appears in the New image.
    N_Flux = R_Flux .* 10.^(0.4.*(Obj.ZpN - Obj.ZpR));

    Info.NumSrc = numel(SrcX);

    Finite  = isfinite(SrcX) & isfinite(SrcY) & isfinite(N_Flux) & ...
              isfinite(R_Mag) & N_Flux > 0;
    InFrame = SrcX > HalfSize+1 & SrcX < SizeIm(2)-HalfSize-1 ...
            & SrcY > HalfSize+1 & SrcY < SizeIm(1)-HalfSize-1;
    InMag   = (R_Mag > Args.MinMag) & (R_Mag < Args.MaxMag);

    %--- isolation, against comparable neighbours only ---
    if isempty(Args.IsoRelDeltaMag)
        NbrSel = Finite & (R_Mag < Args.NbrMagMax);
    else
        % Only sources that can count for some star in the window: to count,
        % a neighbour must be brighter than MaxMag + IsoRelDeltaMag.
        NbrSel = Finite & (R_Mag < Args.MaxMag + Args.IsoRelDeltaMag);
    end
    NbrXY  = [SrcX(NbrSel), SrcY(NbrSel)];
    NbrMag = R_Mag(NbrSel);
    
    % Only evaluated for sources that already pass the cuts above, so this is
    % not a standalone "is this source isolated" flag.

    Isolated = false(size(SrcX));
    IsoRadSq = Args.IsoRadius.^2;
    Cand     = find(Finite & InFrame & InMag);
    for Ii = 1:numel(Cand)
        Isrc = Cand(Ii);
        % <=1 rather than ==0: the source counts itself whenever it is
        % bright enough to be in the neighbour list.
        Near = (NbrXY(:,1)-SrcX(Isrc)).^2 + (NbrXY(:,2)-SrcY(Isrc)).^2 < IsoRadSq;
        if ~isempty(Args.IsoRelDeltaMag)
            Near = Near & (NbrMag < R_Mag(Isrc) + Args.IsoRelDeltaMag);
        end
        NumNear = sum(Near);
        Isolated(Isrc) = (NumNear <= 1);
    end

    Keep = Finite & InFrame & InMag & Isolated;
    Info.NumInMag    = sum(Finite & InFrame & InMag);
    Info.NumIsolated = sum(Keep);

    if sum(Keep) < Args.MinNumStars
        Info.Reason = sprintf('only %d isolated sources in mag %.1f-%.1f', ...
                              sum(Keep), Args.MinMag, Args.MaxMag);
        return
    end

    X = round(SrcX(Keep));
    Y = round(SrcY(Keep));
    F = N_Flux(Keep);

    if numel(X) > Args.MaxNumStars
        % Deterministic, so a rerun on the same subtraction gives the same
        % template.
        Sub = round(linspace(1, numel(X), Args.MaxNumStars));
        X = X(Sub);  Y = Y(Sub);  F = F(Sub);
    end

    %--- saturated stars have their own residual physics, keep them out ---
    BD_IM  = BitDictionary('BitMask.Image.Default');
    SatAny = false(SizeIm);
    if ~isempty(Obj.New) && ~isempty(Obj.New.MaskData) && ~Obj.New.MaskData.isemptyImage
        SatAny = BD_IM.findBit(Obj.New.MaskData.Image, 'Saturated');
    end
    if ~isempty(Obj.Ref.MaskData) && ~Obj.Ref.MaskData.isemptyImage
        SatAny = SatAny | BD_IM.findBit(Obj.Ref.MaskData.Image, 'Saturated');
    end

    if any(SatAny(:))
        CubeSat = imUtil.cut.image2cutouts(double(SatAny), X, Y, HalfSize);
        HasSat  = squeeze(any(any(CubeSat > 0, 1), 2));
        X = X(~HasSat);  Y = Y(~HasSat);  F = F(~HasSat);
    end

    if numel(X) < Args.MinNumStars
        Info.Reason = sprintf('only %d unsaturated sources', numel(X));
        return
    end

    %--- the cube ---
    %  Background from the outer frame of the cutout rather than the whole
    %  cutout: at this stamp size the centre is dominated by whatever the
    %  subtraction left behind, which is the signal.
    Border = true(2*HalfSize+1);
    Border(3:end-2, 3:end-2) = false;
    BorderSub = @(C) C - reshape(median(reshape(C(repmat(Border,1,1,size(C,3))), ...
                                 sum(Border(:)), []), 1, 'omitnan'), 1, 1, []);

    CubeD = BorderSub(imUtil.cut.image2cutouts(Image, X, Y, HalfSize));

    if Args.AlignOnMin
        Isrch = (Cen-Args.AlignSearchHalf):(Cen+Args.AlignSearchHalf);
        Nsr   = numel(Isrch);
        Sub3  = CubeD(Isrch, Isrch, :);

        [~, Imin] = min(reshape(Sub3, Nsr*Nsr, []), [], 1);
        [Py, Px]  = ind2sub([Nsr Nsr], Imin(:));

        X = X + (Px - (Args.AlignSearchHalf+1));
        Y = Y + (Py - (Args.AlignSearchHalf+1));

        InFrame2 = X > HalfSize+1 & X < SizeIm(2)-HalfSize-1 ...
                 & Y > HalfSize+1 & Y < SizeIm(1)-HalfSize-1;
        X = X(InFrame2);  Y = Y(InFrame2);  F = F(InFrame2);

        CubeD = BorderSub(imUtil.cut.image2cutouts(Image, X, Y, HalfSize));
    end

    FiniteC = squeeze(all(all(isfinite(CubeD), 1), 2));
    CubeD   = CubeD(:,:,FiniteC);
    X = X(FiniteC);  Y = Y(FiniteC);  F = F(FiniteC);
    NumUsed = numel(X);

    if NumUsed < Args.MinNumStars
        Info.Reason = sprintf('only %d finite cutouts', NumUsed);
        return
    end

    %--- stack ---
    %  Each cutout is divided by its own source flux, so the median is the
    %  residual per unit source flux. That is the quantity that is constant
    %  across magnitude when the effect is a linear shape error.
    CubeF = CubeD ./ reshape(F, 1, 1, []);
    T     = median(CubeF, 3, 'omitnan');

    Info.FluxFraction = sum(T(:));
    Info.CoreFraction = sum(sum(T(Cen-1:Cen+1, Cen-1:Cen+1)));

    if Args.ZeroCoreHalf > 0
        Icore = (Cen-Args.ZeroCoreHalf):(Cen+Args.ZeroCoreHalf);
        T(Icore, Icore) = 0;
    else
        Icore = [];
    end

    % Down-weight the ring immediately outside the mask. A PSF that is only
    % slightly larger than the model puts its residual right there, and that
    % is the mildest, most common failure - the one where a real transient
    % sitting near a star is most likely to be mistaken for a residual.
    % Weighting it down rather than masking it keeps some sensitivity while
    % taking the peak off the pixels a point source would also occupy.
    if Args.RingWeight < 1 && Args.RingWidth > 0
        Iring = (Cen-Args.ZeroCoreHalf-Args.RingWidth): ...
                (Cen+Args.ZeroCoreHalf+Args.RingWidth);
        Iring = Iring(Iring >= 1 & Iring <= size(T,1));

        Ring = false(size(T));
        Ring(Iring, Iring) = true;
        if ~isempty(Icore)
            Ring(Icore, Icore) = false;   % already zero, leave it
        end

        T(Ring) = T(Ring) .* Args.RingWeight;
    end

    % Difference images carry negative flux, and the residual is typically
    % two-lobed, over-subtracted on one side and under-subtracted on the
    % other. Dividing by the signed sum would invert a net-negative template
    % and every statistic built from it. Dividing by |sum| keeps the shape,
    % but the net sum is the small difference of the two lobes: it swings
    % with small changes in their balance and can approach zero, scaling the
    % template up by ~20x between reference sets on crops where the residual
    % itself was unchanged (issue #1267). sum|T| cannot cancel, so 'abs'
    % keeps the scale stable, and a fitted amplitude is then the total
    % absolute residual flux. Only a zero norm is unusable, since it carries
    % no scale at all.
    NormSigned = sum(T(:));
    if NormAbs
        Norm = sum(abs(T(:)));
    else
        Norm = abs(NormSigned);
    end
    if ~isfinite(Norm) || Norm == 0
        Info.Reason = sprintf('template norm is %.4g, no scale to normalise by', ...
                              Norm);
        return
    end

    Template        = T ./ Norm;
    Info.Norm       = Norm;
    Info.NormSigned = NormSigned;   % negative means the net residual is negative
    Info.NumUsed    = NumUsed;

    Info.X       = X;
    Info.Y       = Y;

    % Coherence of the stack, measured only where the template is defined.
    % The cube's cores were never zeroed, so including them would score
    % scatter against a template value of zero.
    Valid = true(2*HalfSize+1);
    Valid(Icore, Icore) = false;

    ScatterMap   = std(CubeF./Norm, 0, 3, 'omitnan') ./ max(abs(Template(:)));
    Info.Scatter = median(ScatterMap(Valid), 'omitnan');

    % Is the stack a shape at all, or just noise? A PSF reconstruction that
    % is good enough leaves no coherent residual, and the stack is then
    % consistent with zero everywhere. The max over ~1000 pixels of pure
    % noise sits near 3.5 sigma, so a peak below MinPeakSN means there is
    % nothing to correct for.
    %
    % Returning empty rather than a weak template is deliberate: an empty
    % template leaves S_PSFresid empty, flagNonTransients then skips
    % PSFShape, and every candidate passes. That is the correct answer when
    % the reconstruction is good, and it is safer than matched-filtering the
    % image with noise, which would manufacture contaminators at random
    % positions and flag real transients beside them.
    % std rather than MAD, deliberately. Each cutout is divided by its source
    % flux, so the faintest stars carry hugely amplified noise and std is set
    % by those outliers while the median that forms the template is not - it
    % overstates the error by roughly a factor 2, and by more where the faint
    % end dominates. That is kept on purpose: the inflated error makes
    % MinPeakSN a conservative gate that passes only templates with a strong,
    % well measured residual. Measured on five crops, the robust estimator
    % raised every PeakSN but stopped separating good reconstructions from
    % poor ones, so the bias is doing useful work here.
    SigMed = 1.253 .* std(CubeF./Norm, 0, 3, 'omitnan') ./ sqrt(NumUsed);
    SNmap       = Template ./ SigMed;
    Info.PeakSN = max(abs(SNmap(Valid)));

    % Second, independent test: is there a contiguous patch of significant
    % pixels? The residual we are after is correlated over a few pixels, so
    % it shows up as an extended blob even when no single pixel is
    % exceptional - PeakSN alone rewards a spike and misses exactly that.
    %
    % The blob is measured on a MAD significance map, not on SNmap. Counting
    % pixels above a threshold is only meaningful with unbiased per-pixel
    % errors, whereas PeakSN keeps the std estimator deliberately, for the
    % reason above. The two tests therefore use different error models on
    % purpose.
    MADn    = median(abs(CubeF./Norm - median(CubeF./Norm, 3, 'omitnan')), ...
                     3, 'omitnan');
    SigMad  = 1.253 .* 1.4826 .* MADn ./ sqrt(NumUsed);
    SNmad   = Template ./ SigMad;
    SNmad(~Valid) = 0;

    CCblob  = bwconncomp(SNmad >= Args.BlobThresh, 8);
    Areas   = cellfun(@numel, CCblob.PixelIdxList);
    if isempty(Areas)
        Info.BlobArea = 0;
    else
        Info.BlobArea = max(Areas);
    end

    % Radius the template's residual occupies, used downstream as the
    % candidate match radius. Clamped down to HalfSize: past the stamp edge
    % the template is undefined, and on a template whose last few percent are
    % spread thinly the percentile lands beyond it.
    %
    % 'abs' measures it on |T| over the significant pixels. Every pixel's
    % |noise| counts as positive, so over the whole stamp the percentile is
    % set by the noise floor and the stamp size rather than by the residual,
    % and lands at the edge (issue #1267). The MAD map is the unbiased one,
    % for the same reason as in the blob test. If no pixel is significant,
    % fall back to all of them.
    %
    % 'net' measures it on the template taken in the direction of its net
    % sum. A net-negative template sums to -1, and against a negative total
    % the test "enclosed >= 90% of total" is already met at r = 1.
    [Gxr, Gyr] = meshgrid((1:2*HalfSize+1)-Cen, (1:2*HalfSize+1)-Cen);
    Rgrid = hypot(Gxr, Gyr);
    if NormAbs
        Wrad = abs(Template);
        if Args.RadiusSigma > 0
            Wsig = Wrad .* (abs(SNmad) >= Args.RadiusSigma);
            if sum(Wsig(:)) > 0
                Wrad = Wsig;
            end
        end
    else
        Wrad = Template .* sign(NormSigned);
    end
    Total = sum(Wrad(:));

    Info.RadiusPrc = HalfSize;
    for Rr = 1:HalfSize
        if sum(Wrad(Rgrid <= Rr)) >= (Args.RadiusPrc./100)*Total
            Info.RadiusPrc = Rr;
            break
        end
    end
    Info.MatchRadius = min(Info.RadiusPrc + Args.RadiusPad, HalfSize);

    % Rejected only when BOTH tests find nothing. Either a strong peak or a
    % coherent blob is enough to say there is something to correct for.
    PeakOK = isfinite(Info.PeakSN) && Info.PeakSN >= Args.MinPeakSN;
    BlobOK = Info.BlobArea >= Args.MinBlobArea;

    if ~PeakOK && ~BlobOK
        Template    = [];
        Info.Reason = sprintf(['template peak %.1f sigma and largest blob %d pix, ' ...
                               'both below threshold: PSF reconstruction is good ' ...
                               'enough'], Info.PeakSN, Info.BlobArea);
        return
    end
    
    %--- where the source sits relative to the template origin ---
    %  Not corrected, only reported. The template is deliberately anchored on
    %  the aligned minimum, and the offset between that and the stellar
    %  centroid sets the scale of any apparent asymmetry.
    if ~isempty(Obj.New) && ~isempty(Obj.New.Image)
        CubeN  = BorderSub(imUtil.cut.image2cutouts(Obj.New.Image, X, Y, HalfSize));
        StackN = median(CubeN ./ reshape(F, 1, 1, []), 3, 'omitnan');

        [Gx, Gy] = meshgrid((1:2*HalfSize+1)-Cen, (1:2*HalfSize+1)-Cen);
        W = max(StackN, 0);
        % A NaN fwhm would leave the mask all-true and silently widen the
        % aperture to the whole stamp.
        Fwhm = Obj.PSFData.fwhm;
        if isfinite(Fwhm)
            W(hypot(Gx,Gy) > 2.*Fwhm) = 0;
        end
        if sum(W(:)) > 0
            Info.CentroidOffset = [sum(Gx(:).*W(:))./sum(W(:)), ...
                                   sum(Gy(:).*W(:))./sum(W(:))];
        end
    end
end