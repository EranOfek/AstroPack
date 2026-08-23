function Prof = visitWingProfile(Obj, Args)
    % Build one visit-level PSF wing SHAPE profile per crop from bright stars.
    % Description: Phase 1 of the single-PSF scheme. The per-epoch empirical
    %              wing calibration (imUtil.psf.buildEmpiricalWing inside
    %              buildPSF) re-estimates the wing profile every epoch from a
    %              handful of bright stars; the epoch-to-epoch stochasticity
    %              of that estimate propagates into the PSF normalization and
    %              was measured to dominate the bright-star flux repeatability
    %              (~9 mmag added in quadrature on a dense field). The wing
    %              SHAPE, however, is a stable property of the optics over a
    %              visit. This function measures it once per crop, pooling
    %              bright stars from several epochs, and returns a
    %              shape-normalized radial profile that imUtil.psf.buildPSF
    %              (via its 'WingProfile' argument) re-anchors onto each
    %              epoch's own core at splice time - so cores stay per-epoch
    %              (seeing varies) while the wing shape is shared.
    %              The shape is built by imUtil.psf.buildEmpiricalWing with a
    %              constant unit "core", which makes every star self-normalize
    %              to 1 at the seam ring; the returned Value is therefore a
    %              pure shape (Value(SeamRadius) ~= 1) - its absolute scale is
    %              irrelevant, buildPSF rescales it per epoch.
    % Input  : - An AstroImage array of size [Nepoch x Ncrop]: rows are
    %            epochs, each column is one crop (sub-image). Background /
    %            catalogs are NOT required (stamps are background-subtracted
    %            by a far-out annulus); the Mask, if populated, is used to
    %            exclude saturated pixels.
    %          * ...,key,val,...
    %            'MaxEpochs' - Max number of epochs (evenly spaced through
    %                   the visit) to pool stars from. Default is 5.
    %            'StampRadius' - Bright-star cutout half-size [pix]; the
    %                   profile is measured out to this radius. Must exceed
    %                   the PSF stamp half-size so the spliced range covers
    %                   the full PSF. Default is 20.
    %            'WingAnnulus' - [Rin, Rout] background annulus for the
    %                   bright-star stamps [pix]. Placed far out so it does
    %                   not sit on the wing being measured. Default is [16 20].
    %            'SeamRadius' - Ring radius [pix] at which stars are
    %                   scale-matched to each other (buildEmpiricalWing R1);
    %                   the returned profile starts here. Default is 4.
    %            'PseudoSNThresh' - Bright-star selection threshold in units
    %                   of (Image - median)/robust-std at the local maximum.
    %                   Default is 100.
    %            'NeighRadius' - Reject stars with a neighbor local-max
    %                   within this radius [pix]. Default is 15.
    %            'MaxStarsPerEpoch' - Cap on stars pooled per epoch (the
    %                   brightest are kept). Default is 40.
    %            'MinWingStars' - Minimum pooled stars to trust the profile
    %                   (forwarded to buildEmpiricalWing). Default is 8.
    %            'SatBitName' - Mask bit marking saturated pixels. Default
    %                   is 'Saturated'.
    %            'DiluteFactor' - Pixel dilution for the median/std of the
    %                   image (background level estimate). Default is 101.
    %            'Verbose' - Default is false.
    % Output : - Prof - [1 x Ncrop] struct array with fields:
    %            .Radius     - profile radii [pix] (SeamRadius..StampRadius)
    %            .Value      - shape value at each radius (~1 at SeamRadius)
    %            .Success    - true if enough stars produced a valid profile
    %            .Nstars     - number of stars that contributed
    %            .NepochUsed - number of epochs that contributed stars
    %            On failure for a crop, Success=false and buildPSF falls back
    %            to its per-epoch internal calibration (legacy behavior).
    % Author : D. Kovaleva (Aug 2026)
    % Example: Prof = imProc.psf.visitWingProfile(AllSI);   % [Nep x 24] in
    %          AI   = imProc.sources.multiIterExtractor(AllSI(:,7), ...
    %                     'WingProfile', Prof(7));
    arguments
        Obj AstroImage
        Args.MaxEpochs        (1,1) double  = 5
        Args.StampRadius      (1,1) double  = 20
        Args.WingAnnulus      (1,2) double  = [16 20]
        Args.SeamRadius       (1,1) double  = 4
        Args.PseudoSNThresh   (1,1) double  = 100
        Args.NeighRadius      (1,1) double  = 15
        Args.MaxStarsPerEpoch (1,1) double  = 40
        Args.MinWingStars     (1,1) double  = 20   % pooled across epochs, so higher than
                                                   % buildEmpiricalWing's per-epoch 8; crops
                                                   % that cannot supply this many clean stars
                                                   % return Success=false -> per-crop legacy
                                                   % fallback (graceful degradation)
        Args.SeamMinSN        (1,1) double  = 10   % min S/N of the star's seam-ring median
                                                   % (vs its annulus noise). Stars whose seam
                                                   % scale is at noise level corrupt the pooled
                                                   % shape (scaling by a noisy near-zero value
                                                   % inflates the wing) - on sparse fields the
                                                   % brightest-N selection reaches such stars.
        Args.SatBitName       (1,:) char    = 'Saturated'
        Args.DiluteFactor     (1,1) double  = 101
        Args.Verbose          (1,1) logical = false
    end

    [Nep, Ncrop] = size(Obj);
    Prof = repmat(struct('Radius',[], 'Value',[], 'Success',false, ...
                         'Nstars',0, 'NepochUsed',0), 1, Ncrop);
    StampSize = 2.*Args.StampRadius + 1;

    IepList = unique(round(linspace(1, Nep, min(Nep, Args.MaxEpochs))));

    for Icrop = 1:Ncrop
        CubeAll = [];
        MaskAll = [];
        NepUsed = 0;

        for Ie = IepList
            AI = Obj(Ie, Icrop);
            if isempty(AI.ImageData) || isempty(AI.ImageData.Image)
                continue;
            end
            Im = single(AI.ImageData.Image);

            % global background level + robust noise from a diluted sample
            Samp = double(Im(1:Args.DiluteFactor:end));
            Med  = median(Samp(:), 'omitnan');
            Rstd = tools.math.stat.std_mad(Samp(:), 1);
            if ~(isfinite(Rstd) && Rstd > 0)
                continue;
            end

            % bright local maxima in pseudo-S/N units
            Pos = imUtil.sources.findLocalMax((Im - Med)./Rstd, ...
                        'Variance',1, 'Threshold',Args.PseudoSNThresh);
            if isempty(Pos)
                continue;
            end
            X = Pos(:,1);  Y = Pos(:,2);  S = Pos(:,3);

            % drop stars whose stamp would leave the image
            [NyI, NxI] = size(Im);
            In = X > Args.StampRadius+1 & X < NxI-Args.StampRadius & ...
                 Y > Args.StampRadius+1 & Y < NyI-Args.StampRadius;
            X = X(In); Y = Y(In); S = S(In);
            if isempty(X)
                continue;
            end

            % isolate: reject stars with a bright neighbor (mex needs Y-sorted)
            [Y, Iy] = sort(Y);  X = X(Iy);  S = S(Iy);
            [~, NearestRadius] = imUtil.match.mex.matchSelfCatXY(X, Y, ...
                                     Args.NeighRadius, true, false, false, false);
            Keep = isnan(NearestRadius);
            X = X(Keep); Y = Y(Keep); S = S(Keep);
            if isempty(X)
                continue;
            end

            % keep the brightest
            [~, Is] = sort(S, 'descend');
            Is = Is(1:min(numel(Is), Args.MaxStarsPerEpoch));
            X = X(Is); Y = Y(Is);

            % stamps, background-subtracted by the far-out annulus
            Cube = imUtil.cut.image2cutouts(Im, X, Y, Args.StampRadius);
            [Cube, ~, BgStd, ~] = imUtil.sources.mex.annulus_median(Cube, Args.WingAnnulus, 0);

            % Seam-S/N cut: the per-star scale in buildEmpiricalWing is the
            % seam-ring median; if that is at noise level, 1/V scaling
            % corrupts (inflates) the pooled shape. Keep only stars whose
            % seam-ring median clears SeamMinSN x its ring-median noise
            % (1.2533*std/sqrt(Nring)).
            [NyS, NxS, ~] = size(Cube);
            [XgS, YgS] = meshgrid(1:NxS, 1:NyS);
            SeamSelS = round(hypot(XgS-(NxS+1)/2, YgS-(NyS+1)/2)) == round(Args.SeamRadius);
            NringS   = nnz(SeamSelS);
            KeepSeam = false(size(Cube,3),1);
            for Ist = 1:size(Cube,3)
                Stamp = Cube(:,:,Ist);
                Vseam = median(Stamp(SeamSelS), 'omitnan');
                SigSeam = 1.2533 .* BgStd(Ist) ./ sqrt(NringS);
                KeepSeam(Ist) = isfinite(Vseam) && Vseam > Args.SeamMinSN .* SigSeam;
            end
            Cube = Cube(:,:,KeepSeam);
            X = X(KeepSeam);  Y = Y(KeepSeam);
            if isempty(X)
                continue;
            end

            % saturated-pixel stamps (0 = usable) - keep cube alignment even
            % when an epoch has no mask
            MaskCube = zeros(size(Cube), 'single');
            try
                if ~isempty(AI.MaskData) && ~isempty(AI.MaskData.Image)
                    SatMap = AI.MaskData.findBit(Args.SatBitName);
                    if ~isempty(SatMap)
                        MaskCube = imUtil.cut.image2cutouts(single(SatMap), ...
                                       X, Y, Args.StampRadius);
                    end
                end
            catch
                % mask unavailable/unreadable - proceed unmasked for this epoch
            end

            CubeAll = cat(3, CubeAll, Cube);
            MaskAll = cat(3, MaskAll, MaskCube);
            NepUsed = NepUsed + 1;
        end

        Prof(Icrop).NepochUsed = NepUsed;
        if size(CubeAll,3) >= Args.MinWingStars
            % unit "core" => each star self-normalizes to 1 at the seam ring;
            % result is the pooled, pure-shape wing profile
            [R, V, Nst, Ok] = imUtil.psf.buildEmpiricalWing(CubeAll, MaskAll, ...
                                  ones(StampSize, StampSize), Args.SeamRadius, ...
                                  'MinWingStars',Args.MinWingStars);
            Prof(Icrop).Radius  = R;
            Prof(Icrop).Value   = V;
            Prof(Icrop).Success = Ok;
            Prof(Icrop).Nstars  = Nst;
        end

        if Args.Verbose
            fprintf('visitWingProfile: crop %d: %d stars from %d epochs, success=%d\n', ...
                    Icrop, Prof(Icrop).Nstars, NepUsed, Prof(Icrop).Success);
        end
    end
end
