function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transient candidates that are likely not real transients.

    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'ConfigFile' - Path to JSON configuration file. Fields in
                       the file override corresponding Args fields.
                       Default is ''.

                'PixelScale' - Pixel scale in arcsec per pixel.
                       Default is 1.25.

                'SaturatedNeighborDistanceThreshold' - Maximum distance (pix)
                       to search for nearby saturated pixels.
                       Default is 250.

                'flagNegatives' - Flag negative candidates.
                       Default is true.

                'flagChi2' - Flag candidates based on PSF-fit chi2/dof.
                       Default is true.

                'Chi2dofLimitsLocal' - Lower and upper limits on local
                       chi2/dof (N, R, D). Used primarily for isolated
                       candidates.
                       Default is [0.1 2.2].

                'Chi2dofLimitsGlobal' - Lower and upper limits on global
                       (magnitude-binned median) chi2/dof. Used primarily
                       for blended candidates.
                       Default is [0.1 2.5].

                'flagSaturated' - Flag candidates saturated in both N and R.
                       Default is true.

                'flagBadPix_Hard' - Flag candidates based on hard image
                       bitmask criteria.
                       Default is true.

                'BadPix_Hard' - Cell array of bit names for hard masking.
                       Default is {'Interpolated','NaN','NearEdge',...
                                    'Hole','Negative'}.

                'flagBadPix_Soft' - Flag candidates based on soft bitmask
                       criteria with adaptive thresholds.
                       Default is true.

                'BadPix_Soft' - Cell array of image mask bit names marking
                       suspect pixels in New. Candidates on them below
                       BadPix_SoftMinScore are flagged outright, and above it
                       get the last smear contour when smearThreshold returns
                       more than one.
                       Default is {'DarkHighVal','CR_DeltaHT'}.

                'BadPix_SoftMinScore' - |SCORE| below which a candidate on a
                       BadPix_Soft pixel is flagged on the mask alone, since
                       a faint defect cannot be told from a point source by
                       shape. Not applied when smearTemplate reports
                       Info.NoSmear, i.e. the coadd holds no smear at all.
                       Default is 14, set by what survives the whole chain:
                       at 12, three defects came through across 11 crops, all
                       at SCORE 12.6 to 12.7. Costs 0.15 per cent of real
                       transients, being those that land on a marked pixel by
                       chance alignment within the widened band.

                'flagBadPix_Dense' - Flag candidates sitting on mask
                       structure wider than the defects the smear template
                       was built from, where the template does not apply.
                       Default is true.

                'BadPix_DenseSigma' - Gaussian sigma, in pixels, of the
                       kernel whose overlap with the BadPix_Soft mask
                       measures the local density.
                       Default is 1.

                'BadPix_DenseOffset' - How far above the calibrators' own
                       density a candidate must sit to be flagged. In units
                       of kernel fraction, where a one pixel track gives
                       0.399 and a two pixel one 0.641. Skipped when
                       smearTemplate returned no calibrators, i.e. the
                       derived method, no template, or Info.NoSmear, which
                       passes everything.
                       Default is 0.20.

                'flagSubVisit' - Flag inconsistent saturation between N and R.
                       Default is true.

                'BadPixSatRad' - Radius (pix) for local saturation check.
                       Default is 10.

                'BadPixSatFlux' - Flux threshold for true saturation.
                       Default is 20000.

                'flagStarMatches' - Flag candidates matched to stars.
                       Default is true.

                'StarGalProbEps' - Small value to stabilize log ratios.
                       Default is 1e-6.

                'DefStarProb' - Default stellar probability when no classifier
                       probability is available.
                       Default is 0.97.

                'MinStarProb' - Minimum STAR_PROB when both star and galaxy
                       probabilities exist.
                       Default is 0.6.

                'MinStarProbNoGal' - Minimum STAR_PROB when no GAL_PROB exists.
                       Default is 0.10.

                'StarGalLogRatioThresh' - Threshold on log(STAR/GAL).
                       Default is 0.85.

                'flagMP' - Flag candidates matched to minor planets.
                       Default is true.

                'MPDistThresh' - Matching radius (arcsec).
                       Default is 10.

                'flagRinging' - Flag ringing artifacts (Gabor-based).
                       Default is true.

                'flagPeakValley' - Flag peak-valley pairs in difference image.
                       Default is true.

                'PVDistThresh' - Distance threshold (pix) for peak-valley flag.
                       Default is 10.

                'flagStreak' - Flag streak-like artifacts using RANSAC.
                       Default is true.

                'ignoreStreakPoints' - Filters ignored when fitting streaks.
                       Default is {'BadPixelHard','StarMatch','Ringing',...
                                    'Translient','Streak'}.

                'StreakDistanceThreshold' - Max distance (pix) from streak line.
                       Default is 20.

                'NumStreaks' - Number of streaks to fit.
                       Default is 1.

                'StreakRansacMinNumPts' - Candidate minimum sample sizes.
                       Default is [7 10 13 17 20 23 27 30].

                'StreakRansacNtrial' - Number of RANSAC trials.
                       Default is 1000.

                'StreakRansacMinRMS' - Minimum RMS threshold.
                       Default is 1.0.

                'StreakThresholdDistFWHMFactor' - Distance threshold for streak
                       association in units of FWHM.
                       Default is 2.0.

                'StreakThresholdDistMin' - Minimum distance threshold (pix)
                       for streak association.
                       Default is 5.0.

                'StreakSeed' - Seed for the RANSAC trial draw, so the streak
                       flagging is reproducible run to run (issue #1321).
                       'header' - derive it from the observational header
                           keys of New and Ref via tools.rand.seedFromHeader.
                       [] - unseeded, the previous behaviour.
                       A numeric scalar - use it as the seed.
                       One stream is built per crop and both stability-check
                       fits draw from it, so they stay independent of each
                       other while the pair stays reproducible.
                       Default is 'header'.

                'flagPSFShape' - Flag candidates likely caused by poor PSF
                       reconstruction and contamination from nearby
                       persistent sources.
                       Default is true.

                'SecondMomHardLim' - Three-element second moment threshold:
                       (1) limit on max(X2,Y2), (2) limit on X2+Y2 for the
                       very-poor PSF regime, (3) limit on X2+Y2 beyond which
                       the PSF is not salvageable.
                       Default is [3.0 4.5 5.0].

                'flagExtended' - Flag extended (non-PSF-like) sources.
                       Default is true.

                'ExtendedThreshold' - Threshold on SCORE vs SN_ext.
                       Default is -0.41.

                'ExtendedSatDelta' - Stricter near saturation.
                       Default is 0.5.

                'flagLimitingMag' - Flag candidates fainter than limiting mag
                       in both N and R.
                       Default is true.

                'flagDiffSpike' - Flag diffraction spike artifacts.
                       Default is true.

                'SatCentroidDistThreshold' - Max distance to saturated centroid.
                       Default is 250.

                'DiffSpikeSNRThreshold' - Pixel S/N threshold along spike.
                       Default is 2.0.

                'DiffSpikeFracThreshold' - Required fraction of significant pixels.
                       Default is 0.5.

                'flagDensity' - Flag candidates in crowded regions.
                       Default is true.

                'NeighborDistanceThreshold' - Neighbor radius (pix).
                       Default is 100.

                'NeighborDenThreshold' - Density threshold.
                       Default is 1.0.

                'NeighborExclude' - Filters ignored when counting neighbors.
                       Default is {'BadPixelHard','BadPixelSoft',...
                                    'StarMatch','Ringing',...
                                    'Translient','Streak'}.

                'flagVariable' - Flag candidates matched to variable sources.
                       Default is true.

                'flagNuclearNoise' - Flag nuclear subtraction noise.
                       Default is true.

                'BrightGalMagThresh' - Magnitude threshold for bright galaxies.
                       Default is 17.0.

                'BrightGalPrcThresh' - Percentile threshold for bright galaxies.
                       Default is 80.

                'NuclearDefaultPrcThresh' - Default percentile threshold.
                       Default is 50.

                'NuclearMagBinWidth' - Magnitude bin width for nuclear noise.
                       Default is 0.5.

                --- AstroZOGY ---
                'flagScorr' - Flag candidates based on Scorr statistic.
                       Default is true.

                'ScorrThreshold' - Threshold on Scorr.
                       Default is 5.0.

                'ScorrCorrectionParam' - Correction for faint sources.
                       Default is 0.7.

                'flagTranslients' - Flag candidates based on Translient model.
                       Default is true.

                'TranslientThresh' - Fixed threshold for poor PSF cases.
                       Default is 0.48.

                --- Injections ---
                'injectedSrcs' - [RA,Dec] injected sources to ignore in some
                       tests.
                       Default is [].

    Output  : - An AstroCatalog equal to the input catalog with additional
                columns, including FLAGS_TRANSIENT and optional diagnostics.

    Author  : Ruslan Konno (Jan 2024)
    Example : AD = AstroZOGY('LAST*.fits','LAST*1*.fits');
              AD.subtractionD;
              AD.subtractionS;
              AD.findTransients;
              imProc.sub.flagNonTransients(AD);
    %}

    arguments
        Obj AstroDiff

        % General
        Args.ConfigFile char = ''
        Args.PixelScale double = 1.25
        Args.injectedSrcs double = []


        % Negative candidates
        Args.flagNegatives logical = true

        % Chi2 filters
        Args.flagChi2 logical = true
        Args.Chi2dofLimitsLocal (1,2) double = [0.1 2.2]
        Args.Chi2dofLimitsLocality (1,2) double = [0.1 1.5]
        Args.Chi2dofLimitsGlobal (1,2) double = [0.1 2.5]

        % Saturation / mask neighborhood
        Args.flagSaturated logical = true
        Args.SaturatedNeighborDistanceThreshold double = 250

        % Hard bad-pixel filters
        Args.flagBadPix_Hard logical = true
        Args.BadPix_Hard cell = {'Interpolated','NaN','NearEdge','Hole','Negative'}

        % Soft bad-pixel filters
        Args.flagBadPix_Soft logical = true
        Args.BadPix_Soft cell = {'DarkHighVal', 'CR_DeltaHT'}
        Args.SmearThreshold double = []        % [BinCen BinThr...], empty to calibrate
        Args.smearThresholdArgs cell = {}      % passed to imProc.sub.smearThreshold
        Args.BadPix_SoftMinScore double = 14   % below this, marked candidates are flagged on the mask
        Args.flagBadPix_Dense logical = true
        Args.BadPix_DenseSigma double = 1      % Gaussian sigma for the local mask density
        Args.BadPix_DenseOffset double = 0.2  % above the calibrators' density

        % Holes in the reference filters
        Args.flagRefHole logical = true;
        Args.RefHoleFluxFrac double = 0.5;

        % Sub-visit / asymmetric saturation handling
        Args.flagSubVisit logical = true
        Args.BadPixSatRad double = 10
        Args.BadPixSatFlux double = 20000

        % External matches
        Args.flagStarMatches logical = true
        Args.flagMP logical = true
        Args.MPDistThresh double = 10
        Args.flagVariable logical = true

        % Star/galaxy classification
        Args.StarGalProbEps double = 1e-6
        Args.DefStarProb double = 0.95
        Args.MinStarProb double = 0.6
        Args.MinStarProbNoGal double = 0.10
        Args.StarGalLogRatioThresh double = 0.6

        % D-image artifact filters
        Args.flagRinging logical = true
        Args.flagPeakValley logical = true
        Args.PVDistThresh double = 11.3

        % Streak filter
        Args.flagStreak logical = true
        Args.ignoreStreakPoints cell = {'BadPixelHard','StarMatch','Ringing','Translient','Streak'}
        Args.StreakDistanceThreshold double = 20
        Args.NumStreaks double = 1
        Args.StreakRansacMinNumPts double = [7 10 13 17 20 23 27 30]
        Args.StreakRansacNtrial double = 1000
        Args.StreakRansacMinRMS double = 1.0
        Args.StreakThresholdDistFWHMFactor double = 2.0
        Args.StreakThresholdDistMin double = 5.0
        Args.StreakMinStableOverlap double = 0.7
        Args.StreakSeed = 'header';  % 'header' | [] (unseeded) | numeric scalar

        % N-image PSF shape
        Args.flagPSFShape logical = true
        Args.SecondMomHardLim double = [3.0 4.5 5.0]
        Args.ContamMagThresh double = 0.5
        Args.psfResidContamArgs cell = {}


        % Local background
        Args.flagLocalBack logical = true
        Args.LocalBackStdMax double = 12
        Args.LocalBackMax double = 3.0
        Args.LocalBackSNEscape double = 100

        % Extendedness
        Args.flagExtended logical = true
        Args.ExtendedThreshold double = -0.19
        Args.ExtendedSatDelta double = 0.5

        % Limiting magnitude
        Args.flagLimitingMag logical = true

        % Diffraction spikes
        Args.flagDiffSpike logical = true
        Args.SatCentroidDistThreshold double = 250
        Args.DiffSpikeSNRThreshold double = 2.0
        Args.DiffSpikeFracThreshold double = 0.5

        % Density filter
        Args.flagDensity logical = true
        Args.NeighborDistanceThreshold double = 100
        Args.NeighborDenThreshold double = 1.0
        Args.NeighborExclude cell = {'BadPixelHard','BadPixelSoft','StarMatch','Ringing','Translient','Streak'}

        % Nuclear noise
        Args.flagNuclearNoise logical = true
        Args.BrightGalMagThresh double = 17.0
        Args.BrightGalPrcThresh double = 95
        Args.NuclearDefaultPrcThresh double = 68
        Args.NuclearMagBinWidth double = 0.5

        % AstroZOGY
        Args.flagScorr logical = true
        Args.ScorrThreshold double = 5.0
        Args.ScorrCorrectionParam double = 0.7
        Args.ScorrDiffLowerThreshold double = -0.3

        Args.flagTranslients logical = true
        Args.TranslientThresh double = 0.95

        Args.CandPropsDict char = 'BitMask.TransientsCandidateProps.Default'
    end

    % Don't question all this madness.

    Args = applyConfigFile(Args);

    Nobj = numel(Obj);

    % Get transients filter bit dictionary
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');
    % Get image mask bit dictionary
    BD_IM = BitDictionary('BitMask.Image.Default');

    % Some unit conversion parameters
    Rad2Arcsec = 3600.*180./pi; %206265;
    Arcsec2Rad = 1./Rad2Arcsec; %4.84814e-6;

    for Iobj=Nobj:-1:1
        CandCat = Obj(Iobj).CatData;
        Score = CandCat.getCol('SCORE');
    
        % Get size of catalog and initialize an array holding the filtering
        % summary. Array is initialized as zero and will be updates with 
        % each failed filter.

        NumCand = size(CandCat.Catalog,1);

        % Skip empty catalogs
        if NumCand < 1
            TranCat = CandCat;
            continue
        end

        N_MAG_PSF = [];
        R_MAG_PSF = [];
        D_MAG_PSF = [];

        N_FLUX_PSF = [];
        R_FLUX_PSF = [];
        D_FLUX_PSF = [];

        N_X2 = [];
        N_Y2 = [];
        R_X2 = [];
        R_Y2 = [];

        N_CHI2DOF_Local = [];
        R_CHI2DOF_Local = [];
        D_CHI2DOF_Local = [];

        % Initialize transients bool
        FilterFlags = zeros(NumCand,1);

        % Get positive and negative candidates
        %PosTran = (Score > 0.0);
        NegCand = (Score < 0.0);

        % Get limiting magnitudes of N and R
        N_LIMMAG = Obj(Iobj).New.HeaderData.getVal('LIMMAG');
        R_LIMMAG = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');

        % Get local Chi2
        if CandCat.isColumn('N_PSF_CHI2DOF')
            N_CHI2DOF_Local = CandCat.getCol('N_PSF_CHI2DOF');
        end
        if CandCat.isColumn('R_PSF_CHI2DOF')
            R_CHI2DOF_Local = CandCat.getCol('R_PSF_CHI2DOF');
        end
        if CandCat.isColumn('PSF_CHI2DOF')
            D_CHI2DOF_Local = CandCat.getCol('PSF_CHI2DOF');
        end

        MedDiffVar = median(Obj(Iobj).Var(:));

        DgreaterR = []; % object is brighter in D than R
        DgreaterNearbyR = []; % D magnitude brighter than any nearby R catalog source

        % PSF magnitudes
        if CandCat.isColumn('N_MAG_PSF')
            N_MAG_PSF = CandCat.getCol('N_MAG_PSF');
        end
        if CandCat.isColumn('R_MAG_PSF')
            R_MAG_PSF = CandCat.getCol('R_MAG_PSF');
        end
        if CandCat.isColumn('MAG_PSF')
            D_MAG_PSF = CandCat.getCol('MAG_PSF');
        end

        if ~isempty(R_MAG_PSF) && ~isempty(D_MAG_PSF)
            DgreaterR = R_MAG_PSF > D_MAG_PSF;
        end
        
        % PSF fluxes
        if CandCat.isColumn('N_FLUX_PSF')
            N_FLUX_PSF = CandCat.getCol('N_FLUX_PSF');
        end
        if CandCat.isColumn('R_FLUX_PSF')
            R_FLUX_PSF = CandCat.getCol('R_FLUX_PSF');
        end
        if CandCat.isColumn('FLUX_PSF')
            D_FLUX_PSF = CandCat.getCol('FLUX_PSF');
        end

        % Check if PSF photometry solutions exist
        D_PSFPhot_isSolved = false;
        N_PSFPhot_isSolved = false;
        R_PSFPhot_isSolved = false;

        if ~isempty(D_CHI2DOF_Local) && ~isempty(D_FLUX_PSF) && ~isempty(D_MAG_PSF)
            D_PSFPhot_isSolved = true;
        end
        if ~isempty(N_CHI2DOF_Local) && ~isempty(N_MAG_PSF) && ~isempty(N_FLUX_PSF)
            N_PSFPhot_isSolved = true;
        end
        if ~isempty(R_CHI2DOF_Local) && ~isempty(R_MAG_PSF) && ~isempty(R_FLUX_PSF)
            R_PSFPhot_isSolved = true;
        end

        STD_ANNULUS = [];
        BACK_ANNULUS = [];

        if CandCat.isColumn('STD_ANNULUS')
            STD_ANNULUS = CandCat.getCol('STD_ANNULUS');
        end

        if CandCat.isColumn('BACK_ANNULUS')
            BACK_ANNULUS = CandCat.getCol('BACK_ANNULUS');
        end

        % Check if annulus solutions exist

        Annulus_isSolved = false;
        if ~isempty(STD_ANNULUS) && ~isempty(BACK_ANNULUS)
            Annulus_isSolved = true;
        end
        
        IsolatedCand = [];
        R_SN = [];

        % Get isolated and blended candidates
        if CandCat.isColumn('R_SN')
            R_SN = CandCat.getCol('R_SN');
            IsolatedCand = (R_SN < 3);
            BlendedCand = ~IsolatedCand;

            % These are not clear.
            AmbBlendedCand = BlendedCand & (R_MAG_PSF > R_LIMMAG);
            AmbIsolated = IsolatedCand & (R_MAG_PSF < R_LIMMAG);
        end

        % Get candidate New and Ref bits masks values
        N_BM = CandCat.getCol('N_FLAGS');
        R_BM = CandCat.getCol('R_FLAGS');

        % Get XY coordinates
        [X,Y] = CandCat.getXY();


        N_PSFHalfSize = floor(size(Obj(Iobj).New.PSFData.getPSF,2)/2);
        R_PSFHalfSize = floor(size(Obj(Iobj).Ref.PSFData.getPSF,2)/2);
        NearbyRRadius = 2 * max(20, 2*max(N_PSFHalfSize, R_PSFHalfSize));

        % A point source can only spill onto a candidate that sits inside its
        % PSF footprint, so this test uses the stamp radius rather than
        % NearbyRRadius. The reference catalogue is in its own pixel frame, so
        % separations are measured on the sky and both radii are converted.
        NearbyRRadiusRad = NearbyRRadius * Args.PixelScale * Arcsec2Rad;
        RSrcRadiusRadSq  = (max(N_PSFHalfSize, R_PSFHalfSize) ...
                            .* Args.PixelScale .* Arcsec2Rad).^2;

        if D_PSFPhot_isSolved
            [R_NativeRA, R_NativeDec] = Obj(Iobj).Ref.CatData.getLonLat('rad');
            [CandRA, CandDec]         = CandCat.getLonLat('rad');
            R_NativeMag = Obj(Iobj).Ref.CatData.getCol('MAG_PSF');

            % Only point sources matter. Diffuse flux is not PSF-matched, so a
            % poorly reconstructed PSF cannot turn it into a point-like
            % residual. If the moments are unavailable, treat everything as
            % point-like so that the exemption stays conservative.
            if Obj(Iobj).Ref.CatData.isColumn('X2') && Obj(Iobj).Ref.CatData.isColumn('Y2')
                R_NativeX2 = Obj(Iobj).Ref.CatData.getCol('X2');
                R_NativeY2 = Obj(Iobj).Ref.CatData.getCol('Y2');
                R_IsPoint  = ((R_NativeX2 + R_NativeY2) < Args.SecondMomHardLim(2)) ...
                           | (max(R_NativeX2, R_NativeY2) < Args.SecondMomHardLim(1));
            else
                R_IsPoint = true(size(R_NativeRA));
            end

            DgreaterNearbyR = false(NumCand,1);

            % No point source within the PSF stamp, so nothing can have left a
            % residual here. Default false: where we cannot tell, withhold the
            % exemption.
            NoNearbyRSrc = false(NumCand,1);

            if any(~isnan(R_NativeDec))

                % Sort the reference catalog by Dec and bin it into bands of
                % height NearbyRRadiusRad. Bands are contiguous in the sorted
                % arrays, so each band maps to a single index range. A candidate
                % can only match sources in its own band and the two adjacent
                % ones, which restricts the distance calculation to a thin slab.
                [R_SortedDec, SI] = sort(R_NativeDec);
                R_SortedRA = R_NativeRA(SI);
                R_SortedMag = R_NativeMag(SI);
                R_SortedIsPoint = R_IsPoint(SI);

                RowEdges = min(R_SortedDec):NearbyRRadiusRad:(max(R_SortedDec)+NearbyRRadiusRad);
                NRow = numel(RowEdges)-1;

                RowStart = ones(NRow+1,1);
                RowStart(2:end) = cumsum(histcounts(R_SortedDec, RowEdges)).' + 1;

                CandRow = discretize(CandDec, RowEdges);
                NearbyRRadiusRadSq = NearbyRRadiusRad.^2;

                for Icand = 1:NumCand
                    % Candidates outside the reference catalog Dec range have no
                    % band and therefore no nearby sources.
                    if isnan(CandRow(Icand))
                        continue
                    end

                    Ilow  = RowStart(max(CandRow(Icand)-1, 1));
                    Ihigh = RowStart(min(CandRow(Icand)+2, NRow+1))-1;

                    if Ihigh < Ilow
                        % No Ref sources anywhere in this band, so none
                        % within the PSF stamp either.
                        NoNearbyRSrc(Icand) = true;
                        continue
                    end

                    % Small-angle separation, with RA wrapped to [-pi,pi] and
                    % scaled by cos(Dec).
                    DeltaRA  = R_SortedRA(Ilow:Ihigh) - CandRA(Icand);
                    DeltaRA  = mod(DeltaRA + pi, 2.*pi) - pi;
                    DeltaRA  = DeltaRA .* cos(CandDec(Icand));
                    DeltaDec = R_SortedDec(Ilow:Ihigh) - CandDec(Icand);

                    SlabDistSq = DeltaRA.^2 + DeltaDec.^2;
                    Nearby = SlabDistSq < NearbyRRadiusRadSq;

                    NearPoint = (SlabDistSq < RSrcRadiusRadSq) ...
                        & R_SortedIsPoint(Ilow:Ihigh);
                    NoNearbyRSrc(Icand) = ~any(NearPoint);

                    if any(Nearby)
                        SlabMag = R_SortedMag(Ilow:Ihigh);
                        DgreaterNearbyR(Icand) = ...
                            D_MAG_PSF(Icand) < min(SlabMag(Nearby));
                    end
                end
            end
        end

        RADec = CandCat.getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);    

        % Get candidates near saturated sources
        BitsSatCut = Obj(Iobj).MaskData.bitwise_cutouts([X,Y], ...
                'or', 'HalfSize', Args.SaturatedNeighborDistanceThreshold);
        NearSaturated = BD_IM.findBit(BitsSatCut,'Saturated');

        SaturatedPixels = BD_IM.findBit(Obj.Mask,'Saturated');
        SaturatedIslands = bwconncomp(SaturatedPixels, 8);
        SaturatedIslands_Props = regionprops(SaturatedIslands, ...
            'Centroid', 'Area', 'PixelIdxList');
        SaturationCentroids = vertcat(SaturatedIslands_Props.Centroid);

        % Check N and R PSFs
        if CandCat.isColumn('N_X2')
            N_X2 = CandCat.getCol('N_X2');
        end
        if CandCat.isColumn('N_Y2')
            N_Y2 = CandCat.getCol('N_Y2');
        end

        if CandCat.isColumn('R_X2')
            R_X2 = CandCat.getCol('R_X2');
        end

        if CandCat.isColumn('R_Y2')
            R_Y2 = CandCat.getCol('R_Y2');
        end

        HasRX2Y2 = ~isempty(R_X2) && ~isempty(R_Y2);

        if HasRX2Y2
            R_GoodPSF = ((R_X2 + R_Y2) < Args.SecondMomHardLim(2)) ...
                | (max(R_X2, R_Y2) < Args.SecondMomHardLim(1));
        end

        % Based on sigma in arcsec.
        % 3sigma, narrow
        PointLimit3 = Obj(Iobj).PSFData.fwhm ...
            .* Args.PixelScale .* 1.2739;
        % 5sigma, wide
        PointLimit5 = PointLimit3*5/3;

        % Get star matched candidates
        if CandCat.isColumn('STAR_N')
            StarCand = (CandCat.getCol('STAR_N') > 0.0);
        else
            StarCand = false(NumCand,1);
        end

        % Get galaxy matched candidates
        if CandCat.isColumn('GAL_N')
            GalCand = (CandCat.getCol('GAL_N') > 0.0);
        else
            GalCand = false(NumCand,1);
        end

        % Get Nuclear candidates
        if CandCat.isColumn('GAL_DIST')
            GalDist = CandCat.getCol('GAL_DIST');
            % TODO: rather than doing this here, match2Galaxies should be
            % extended to determined if a source is nuclear or not,
            % probably best to write a dedicated matchTransients2Galaxies
            % function which uses the N, R, and D catalogs
            NuclearCand = GalDist < PointLimit5;
        else
            NuclearCand = false(NumCand,1);
        end

        if CandCat.isColumn('STAR_DIST')
            StarDist = CandCat.getCol('STAR_DIST');
            NearStar = StarDist < PointLimit5;
        else
            NearStar = false(NumCand,1);
        end

        Star_Prob = [];
        Gal_Prob = [];

        if CandCat.isColumn('STAR_PROB')
            Star_Prob = CandCat.getCol('STAR_PROB');
        end

        if CandCat.isColumn('GAL_PROB')
            Gal_Prob = CandCat.getCol('GAL_PROB');
        end

        HasStarGalProb = ~isempty(Star_Prob) && ~isempty(Gal_Prob);

        IsStar = [];

        if HasStarGalProb
            Star_Prob_safe = Star_Prob;
            Gal_Prob_safe  = Gal_Prob;

            Star_Prob_safe(isnan(Star_Prob_safe)) = 0;
            Gal_Prob_safe(isnan(Gal_Prob_safe))   = 0;

            ScoreSG = log((Star_Prob_safe + Args.StarGalProbEps) ./ ...
                          (Gal_Prob_safe  + Args.StarGalProbEps));

            HasStar = Star_Prob_safe > 0;
            HasGal  = Gal_Prob_safe > 0;

            IsStarBoth = HasStar & HasGal & ...
                         ((Star_Prob_safe > Args.MinStarProb) & ...
                         (ScoreSG > Args.StarGalLogRatioThresh)) |...
                         ((Star_Prob_safe > Args.DefStarProb) & ...
                         (Star_Prob_safe > Gal_Prob));

            IsStarNoGal = HasStar & ~HasGal & ...
                          (Star_Prob_safe > Args.MinStarProbNoGal);

            IsStar = IsStarBoth | IsStarNoGal;
        end

        % Find injected sources if given

        Injections = [];

        if ~isempty(Args.injectedSrcs)
            NumInj = size(Args.injectedSrcs,1);
            Injections = false(NumCand,1);
            for IInj = NumInj:-1:1
                InjMatch = CandCat.coneSearch(...
                    Args.injectedSrcs(IInj,1), Args.injectedSrcs(IInj,2), 3.0);
                Injections(InjMatch.Ind) = (InjMatch.Nsrc > 0);
            end
        end

        % ====== Apply flags =====

        % Flag negative candidates
        if Args.flagNegatives
            FilterFlags = setFilterBit(FilterFlags, NegCand, BD_TF, 'Negative');
        end
        
        % ----- Bad Pixels -----

        BadPixHard = false(NumCand, 1);

        % Apply hard bit mask criteria.
        if Args.flagBadPix_Hard

            NumBadHard = numel(Args.BadPix_Hard);

            % New bit mask values.
            N_BadPixHard = false(NumCand,1);
            % Reference bit mask value.
            R_BadPixHard = false(NumCand,1);
    
            for IBad=1:1:NumBadHard
                N_BadPixHard = N_BadPixHard | ...
                    BD_IM.findBit(N_BM, Args.BadPix_Hard(IBad));
                R_BadPixHard = R_BadPixHard | ...
                    BD_IM.findBit(R_BM, Args.BadPix_Hard(IBad));
            end

            BadPixHard = N_BadPixHard | R_BadPixHard;

            FilterFlags = setFilterBit(FilterFlags, BadPixHard, BD_TF, 'BadPixelHard');
        end

        if Args.flagSubVisit && N_PSFPhot_isSolved && R_PSFPhot_isSolved

            N_FLAGS = Obj(Iobj).New.MaskData.bitwise_cutouts([X, Y], ...
                'or', 'HalfSize', Args.BadPixSatRad);
            R_FLAGS = Obj(Iobj).Ref.MaskData.bitwise_cutouts([X, Y], ...
                'or', 'HalfSize', Args.BadPixSatRad);

            N_BadPixSat = BD_IM.findBit(N_FLAGS,'Saturated');
            R_BadPixSat = BD_IM.findBit(R_FLAGS,'Saturated');

            N_hasHighFlux = N_FLUX_PSF > Args.BadPixSatFlux;
            R_hasHighFlux = R_FLUX_PSF > Args.BadPixSatFlux;

            N_FalseSaturation = (N_BadPixSat & ~R_BadPixSat & ~N_hasHighFlux);
            R_FalseSaturation = (~N_BadPixSat & R_BadPixSat & ~R_hasHighFlux);
            FalseSaturation = N_FalseSaturation | R_FalseSaturation;

            FilterFlags = setFilterBit(FilterFlags, FalseSaturation, BD_TF, 'SubVisit');
        end

        % Apply soft bit mask criteria, by the smear statistic.
        %   A defect sits at a fixed detector position, so the registration
        %   applied before coaddition moves it and it becomes a small blob in
        %   the coadd. SN_smear is the response of a matched filter built for
        %   that shape, so a smeared defect scores higher on it than on the
        %   PSF while a real source does the reverse.
        %
        %   The old SCORE - SN_delta test asked whether a candidate looked
        %   like a real source or a single bad pixel. A smeared bad pixel is
        %   neither, so it fell between the two templates and passed.
        if Args.flagBadPix_Soft

            % Candidates on a pixel the New mask already calls suspect.
            Noisy = false(NumCand,1);
            for Ib=1:1:numel(Args.BadPix_Soft)
                Noisy = Noisy | BD_IM.findBit(N_BM, Args.BadPix_Soft{Ib});
            end

            % Below BadPix_SoftMinScore a marked candidate is flagged on the
            % mask alone. A faint defect does not smear enough for the shape
            % statistic to tell it from a point source: PSF sources injected
            % onto real marked sites land among the defects themselves below
            % SCORE ~10, so the statistic can only decide at random there and
            % the mask is the real information.
            %   The value is set by what actually survives. At a floor of 12,
            % three defects came through the whole filter chain across 11
            % crops, 0.27 per subtraction, and all three sat at SCORE 12.62,
            % 12.65 and 12.73, in a band barely above the floor itself. An
            % earlier trials argument had predicted 0.02 per subtraction at
            % that floor; it assumed defects follow their own injected branch
            % with Gaussian scatter, and the ones that get through are
            % precisely the ones that do not.
            %   Raising it to 14 clears all three by 1.3 in SCORE. The cost is
            % real transients that land on a marked pixel by chance alignment
            % and fall in the widened band: the mask covers a mean 6.4 per
            % cent of the detector once dilated by the 7x7 N_FLAGS footprint,
            % and SCORE 12 to 14 holds 2.3 per cent of injected sources, so
            % 0.15 per cent of real transients. Going to 15 would cost 0.23
            % per cent and buy no further margin on the observed cases.
            %   Independent of the smear calibration, so it applies even when
            % no template or threshold could be built.
            %   Except when there is no smear in this coadd at all.
            % imProc.sub.smearTemplate sets Info.NoSmear when the drift is
            % about a pixel per epoch or more: each defect deposit then lands
            % on its own pixel, the clip removes it, and no smear survives to
            % be filtered. Faint marked candidates there are not smear
            % artifacts, so the floor would cost completeness with nothing to
            % catch.
            NoSmear = ~isempty(Obj(Iobj).SmearTemplateInfo) && ...
                      isfield(Obj(Iobj).SmearTemplateInfo, 'NoSmear') && ...
                      Obj(Iobj).SmearTemplateInfo.NoSmear;

            % Beyond the morphology the template was trained on it no longer
            % describes the object. Every defect in a coadd shares one shift
            % history, so the calibrators smearTemplate stacked all have the
            % same mask footprint, and the local mask density measured at
            % them is a single value to within a few per cent. A candidate
            % well above that sits on something wider, a solid clump or two
            % tracks side by side, whose blob is broader than the template
            % and so reads as PSF-like. Flag those on the mask rather than
            % judging them with a template that does not fit them.
            %   The density is the fraction of a sigma 1 Gaussian falling on
            % marked pixels, which is absolute: 0.399 is a one pixel track,
            % 0.641 two pixels wide, 0.883 three. The offset is in those
            % units, so 0.20 is well over half a pixel of extra width.
            Dense = false(NumCand,1);
            STI   = Obj(Iobj).SmearTemplateInfo;
            if Args.flagBadPix_Dense && ~NoSmear && ~isempty(STI) && ...
                    isfield(STI, 'CalX') && ~isempty(STI.CalX) && ...
                    ~Obj(Iobj).New.MaskData.isemptyImage

                MaskSoft = Obj(Iobj).New.MaskData.findBit(Args.BadPix_Soft, ...
                                            'Method','any', 'OutType','mat');

                Kdense    = imUtil.kernel2.gauss(Args.BadPix_DenseSigma, [7 7]);
                Kdense    = Kdense./sum(Kdense, 'all');
                DenseIm   = imUtil.filter.filter2_fast(double(MaskSoft), Kdense);
                SizeDense = size(DenseIm);

                IndCal  = imUtil.image.sub2ind_fast(SizeDense, ...
                              round(STI.CalY), round(STI.CalX));
                VThresh = median(DenseIm(IndCal), 'omitnan') + Args.BadPix_DenseOffset;

                [Xp,Yp] = CandCat.getXY('ColX','XPEAK','ColY','YPEAK');
                IndCand = imUtil.image.sub2ind_fast(SizeDense, ...
                              min(max(round(Yp),1), SizeDense(1)), ...
                              min(max(round(Xp),1), SizeDense(2)));

                Dense = DenseIm(IndCand) > VThresh;
            end

            % No smear in this coadd means nothing here is a smear artifact,
            % so neither the floor nor the density gate applies.
            if NoSmear
                BadPixSoft = false(NumCand,1);
            else
                BadPixSoft = (Noisy & abs(Score) < Args.BadPix_SoftMinScore) | Dense;
            end

            if CandCat.isColumn('SN_smear')
                SN_smear = CandCat.getCol('SN_smear');

                if isempty(Args.SmearThreshold)
                    [BinCen, ~, SmearInfo] = imProc.sub.smearThreshold(...
                        Obj(Iobj), Args.smearThresholdArgs{:});
                else
                    BinCen        = Args.SmearThreshold(:,1);
                    BinThr        = Args.SmearThreshold(:,2:end);
                    SmearInfo     = struct();
                    SmearInfo.Fun = arrayfun(@(Ik) @(A) interp1(BinCen, BinThr(:,Ik), ...
                                        min(max(A,BinCen(1)),BinCen(end)), 'linear'), ...
                                        1:1:size(BinThr,2), 'UniformOutput',false);
                end

                if ~isempty(BinCen)
                    % With more than one keep fraction, marked candidates
                    % above the floor get the last contour. The default is a
                    % single contour, so the floor is then the only thing
                    % that treats marked candidates differently: above it a
                    % tighter contour rejected no additional defects and lost
                    % more real sources.
                    Thresh = SmearInfo.Fun{1}(abs(Score));
                    if numel(SmearInfo.Fun) > 1 && any(Noisy)
                        Thresh(Noisy) = SmearInfo.Fun{end}(abs(Score(Noisy)));
                    end
                    BadPixSoft = BadPixSoft | ((Score - SN_smear) < Thresh);
                end
            end

            FilterFlags = setFilterBit(FilterFlags, BadPixSoft, BD_TF, 'BadPixelSoft');
        end

        % Flag saturated candidates
        if Args.flagSaturated
            N_Saturated = BD_IM.findBit(N_BM,'Saturated');
            R_Saturated = BD_IM.findBit(R_BM,'Saturated');
            
            % Check if candidates are saturated in New and Ref, flag these.
            Saturated = N_Saturated & R_Saturated;

            FilterFlags = setFilterBit(FilterFlags, Saturated, BD_TF, 'Saturated');
            
        end

        if Args.flagRefHole && R_PSFPhot_isSolved && D_PSFPhot_isSolved
            % A negative reference flux inflates the difference. Ask whether
            % that deficit alone accounts for the candidate's difference flux,
            % after converting reference counts into D flux units.
            %   Deliberately not a significance test. The reference PSF-fit
            % error comes from backgroundCube, which uses a non-robust @std
            % over the stamp's outer annulus, so a bright neighbour landing
            % there inflates it and collapses |R_SN|: a -164 e- hole was
            % reported at R_SN = -1.78 while the fit's own chi2/dof sat at
            % 0.04 against a local median of 0.78. R_FLUX_PSF is unaffected
            % by that, so the test uses flux alone.
            RefDeficit = -R_FLUX_PSF .* 10.^(0.4.*(Obj(Iobj).ZpD - Obj(Iobj).ZpR));

            HoleInRef = (R_FLUX_PSF < 0) & (D_FLUX_PSF > 0) & ...
                        (RefDeficit > Args.RefHoleFluxFrac .* D_FLUX_PSF);

            FilterFlags = setFilterBit(FilterFlags, HoleInRef, BD_TF, 'RefHole');
        end

        % ----- D artifacts -----

        % Apply ringing criterium
        if Args.flagRinging && CandCat.isColumn('SN_GABOR')
            GaborSN = CandCat.getCol('SN_GABOR');

            Ringing =  (abs(GaborSN) > abs(Score));
            FilterFlags = setFilterBit(FilterFlags, Ringing, BD_TF, 'Ringing');
        end

        % Apply Peak-Valley criterium
        if Args.flagPeakValley && CandCat.isColumn('PV_DIST')
            PVDist = CandCat.getCol('PV_DIST');

            PVFlagged = (PVDist <= Args.PVDistThresh);
            
            FilterFlags = setFilterBit(FilterFlags, PVFlagged, BD_TF, 'PVDist');
        end
        
        if Args.flagStreak

            SubSel = true(NumCand,1);
            NumExclude = numel(Args.ignoreStreakPoints);

            for IExclude = 1:NumExclude
                BitFound = BD_TF.findBit(FilterFlags, Args.ignoreStreakPoints{IExclude});
                SubSel = SubSel & ~BitFound;
            end

            %   One stream for the whole streak search of this crop, so the
            %   flagging is reproducible run to run (issue #1321) without
            %   reseeding the global generator. Deliberately one stream and
            %   not one seed per call: the two fits below are a stability
            %   check on the same points, so they have to stay independent
            %   draws. Successive calls continue this stream; a shared seed
            %   would make them identical and the check vacuous.
            if isempty(Args.StreakSeed)
                StreakStream = RandStream.getGlobalStream;      % unseeded, as before
            elseif isnumeric(Args.StreakSeed)
                StreakStream = RandStream('threefry', 'Seed',uint32(Args.StreakSeed));
            else
                Heads = [Obj(Iobj).New, Obj(Iobj).Ref];
                if isempty(Heads)
                    Heads = Obj(Iobj);
                end
                StreakStream = RandStream('threefry', 'Seed', ...
                                   tools.rand.seedFromHeader(Heads, ...
                                       'Salt','imProc.sub.flagNonTransients:flagStreak', ...
                                       'FallbackVals',@() [NumCand, size(Obj(Iobj).Image)]));
            end

            for IStreak = 1:Args.NumStreaks

                Xt = X(SubSel);
                Yt = Y(SubSel);

                TDist = max( ...
                    Obj(Iobj).PSFData.fwhm .* Args.StreakThresholdDistFWHMFactor, ...
                    Args.StreakThresholdDistMin);

                %---------------------------
                % First streak solution
                %---------------------------
                Res1.Found = false;
                for IMinNumPts = numel(Args.StreakRansacMinNumPts):-1:1
                    Res1 = tools.math.fit.ransacLinear([Xt,Yt], 'Stream', StreakStream, ...
                        'Ntrial', Args.StreakRansacNtrial, ...
                        'MinRMS', Args.StreakRansacMinRMS, ...
                        'MinNpt', Args.StreakRansacMinNumPts(IMinNumPts), ...
                        'ThresholdDist', TDist);
                    if Res1.Found
                        break
                    end
                end

                if ~Res1.Found
                    break
                end

                ModY1 = Res1.Par(1) + Xt .* Res1.Par(2);
                Streak1 = abs(ModY1 - Yt) < Args.StreakDistanceThreshold;

                %---------------------------
                % Second streak solution
                %---------------------------
                Res2.Found = false;
                for IMinNumPts = numel(Args.StreakRansacMinNumPts):-1:1
                    Res2 = tools.math.fit.ransacLinear([Xt,Yt], 'Stream', StreakStream, ...
                        'Ntrial', Args.StreakRansacNtrial, ...
                        'MinRMS', Args.StreakRansacMinRMS, ...
                        'MinNpt', Args.StreakRansacMinNumPts(IMinNumPts), ...
                        'ThresholdDist', TDist);
                    if Res2.Found
                        break
                    end
                end

                if ~Res2.Found
                    break
                end

                ModY2 = Res2.Par(1) + Xt .* Res2.Par(2);
                Streak2 = abs(ModY2 - Yt) < Args.StreakDistanceThreshold;

                %---------------------------
                % Stability check
                %---------------------------
                NUnion = nnz(Streak1 | Streak2);
                if NUnion == 0
                    break
                end

                OverlapFrac = nnz(Streak1 & Streak2) / NUnion;

                if OverlapFrac < Args.StreakMinStableOverlap
                    break
                end

                % Keep only stable points
                Streak = Streak1 & Streak2;

                if ~any(Streak)
                    break
                end

                FilterFlags(SubSel) = setFilterBit( ...
                    FilterFlags(SubSel), Streak, BD_TF, 'Streak');

                SubSel(SubSel) = ~Streak;
            end
        end

        % ----- PSF Shape -----

        if Args.flagExtended && CandCat.isColumn('SN_ext1')

            SN_ext = CandCat.getCol('SN_ext1');

            ExtendedThreshold = ones(NumCand,1)*Args.ExtendedThreshold;

            if exist('NearSaturated', 'var')
                ExtendedThreshold = ExtendedThreshold + Args.ExtendedSatDelta*NearSaturated;
            end

            ExtendedSource = abs(Score) - abs(SN_ext) < ExtendedThreshold;

            FilterFlags = setFilterBit(FilterFlags, ExtendedSource, BD_TF, 'Extended');
        end

        % ----- PSF reconstruction residuals -----
        %  The reconstructed PSF represents the core but not the profile
        %  outside it, and the missing flux lands in D as a residual at
        %  every persistent source. imProc.sub.psfResidContamCat
        %  measures those residuals directly rather than extrapolating a
        %  tail from the PSF model, and a candidate fails when it is not
        %  bright enough to be anything other than the residual it sits on.
        if Args.flagPSFShape && D_PSFPhot_isSolved

            % No usable template means the test cannot be evaluated at all.
            % The column is still written, as a NaN array, so every crop the
            % filter runs on carries the same columns and merge across crops
            % does not fail on a mismatched column set. NaN is therefore
            % distinct from 0, which means the test ran and found no residual
            % within reach.
            HasResidTemplate = isprop(Obj(Iobj),'S_PSFresid') && ...
                               ~isempty(Obj(Iobj).S_PSFresid);

            if HasResidTemplate

                ContamCat = imProc.sub.psfResidContamCat(Obj(Iobj), ...
                                Args.psfResidContamArgs{:});

                % Match out to where the template's flux actually is, not to
                % the stamp edge. psfResidTemplate measures it and clamps it
                % down to the half size, so a diffuse template falls back to
                % the old behaviour rather than reaching past its own
                % definition.
                MatchRadT = Obj(Iobj).PSFresidTemplateInfo.MatchRadius;

                % Strongest residual within reach of each candidate. Not the
                % nearest one: the nearest is not necessarily the one that
                % could have produced it, and candidates routinely have
                % several.
                MaxContamFlux = nan(NumCand,1);
                if ContamCat.sizeCatalog > 0
                    CxT = ContamCat.getCol('XPEAK');
                    CyT = ContamCat.getCol('YPEAK');
                    CfT = ContamCat.getCol('FLUX_TEMPLATE');
                    for ICand = 1:NumCand
                        Inc = (CxT - X(ICand)).^2 + (CyT - Y(ICand)).^2 <= MatchRadT.^2;
                        if any(Inc)
                            MaxContamFlux(ICand) = max(CfT(Inc));
                        end
                    end
                end

                % Same convention as ContaminationMag: a log10 flux ratio. A
                % candidate with no residual in reach passes, since there is
                % nothing that could have produced it.
                ContamMag  = log10(D_FLUX_PSF ./ MaxContamFlux);
                HasContam  = isfinite(ContamMag);
                Passes_PSFShape = ~(HasContam & (ContamMag <= Args.ContamMagThresh));

                % 0 rather than NaN where the test ran and found nothing. The
                % decision above has already been taken from MaxContamFlux,
                % where NaN is what makes such a candidate pass.
                ContamFluxCol = MaxContamFlux;
                ContamFluxCol(~isfinite(ContamFluxCol)) = 0;

                % Either moment alone is enough. A PSF stretched along one
                % axis has a sum the joint limit lets through whenever the
                % other moment is small, and that is exactly the shape whose
                % reconstruction cannot be trusted.
                N_NotSalvagablePSF = ((N_X2 + N_Y2) >= Args.SecondMomHardLim(3)) ...
                                   | (max(N_X2, N_Y2) >= Args.SecondMomHardLim(1));

                PSF_Flagged = ~Passes_PSFShape | N_NotSalvagablePSF;
                FilterFlags = setFilterBit(FilterFlags, PSF_Flagged, BD_TF, 'PSFShape');
            else
                ContamFluxCol = nan(NumCand,1);
            end

            TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
                   ContamFluxCol, 'SCORE', {'FLUX_CONTAM'}, {''});
        end

        % ----- Disturbed local background -----
        %  Annulus statistics measured in the difference image, on the
        %  candidate itself. This is independent of the residual template:
        %  it asks whether the neighbourhood is locally disturbed at all,
        %  which catches diffuse structure, galaxy light and unmodelled
        %  background with no persistent point source to blame. The residual
        %  filter can only speak about candidates that have a contaminator
        %  in reach, so the two do not overlap.
        %
        %  The escape on D_FLUX_PSF/STD_ANNULUS keeps a candidate that is
        %  simply very bright: a high annulus STD next to a strong source is
        %  the source's own wings, not a disturbed field.
        if Args.flagLocalBack && D_PSFPhot_isSolved && Annulus_isSolved

            Passes_LocalBack = ...
                  (STD_ANNULUS < Args.LocalBackStdMax ...
                   & abs(BACK_ANNULUS) < Args.LocalBackMax) ...
                | (abs(D_FLUX_PSF./STD_ANNULUS) > Args.LocalBackSNEscape);

            FilterFlags = setFilterBit(FilterFlags, ~Passes_LocalBack, ...
                                       BD_TF, 'LocalBack');
        end

        if Args.flagDiffSpike

            NearSatNotStar = NearSaturated & ~StarCand;

            if ~isempty(IsStar)
                NearSatNotStar = NearSaturated & ~IsStar;
            end

            X_NearSaturated = X(NearSatNotStar);
            Y_NearSaturated = Y(NearSatNotStar);

            NNearSat = sum(NearSatNotStar);

            IsDiffSpike = false(NumCand,1);
            IsDiffSpikeSubSel = false(NNearSat, 1);

            for INearSat = 1:NNearSat
                X_INearSat = X_NearSaturated(INearSat);
                Y_INearSat = Y_NearSaturated(INearSat);

                SatCentDist = sqrt( ...
                    (SaturationCentroids(:,1)-X_INearSat).^2 + ...
                    (SaturationCentroids(:,2)-Y_INearSat).^2);

                SatIdx = find(SatCentDist < Args.SatCentroidDistThreshold);

                X_SatCent = SaturationCentroids(SatIdx,1);
                Y_SatCent = SaturationCentroids(SatIdx,2);
                Dist_SatCent = SatCentDist(SatIdx);

                NumSatIdx = numel(SatIdx);

                HereIsDiffSpikeSubSel = false;

                for ISatIdx = 1:NumSatIdx

                    NumLinePixels = ceil(Dist_SatCent(ISatIdx));

                    X_Line = linspace(X_INearSat, X_SatCent(ISatIdx), NumLinePixels);
                    Y_Line = linspace(Y_INearSat, Y_SatCent(ISatIdx), NumLinePixels);
                    
                    % sample matrix values (interp2 uses x=col, y=row)
                    Vals_Line = interp2(double(Obj(Iobj).Image), X_Line, Y_Line, 'linear', NaN);
                                    
                    % remove NaNs (edges etc.)
                    Good = ~isnan(Vals_Line);
                    Vals_Line = Vals_Line(Good);
    
                    SN_Line = Vals_Line/sqrt(MedDiffVar);
                    Significant_Line = abs(SN_Line) > Args.DiffSpikeSNRThreshold;
                    NumSpikePixels = sum(Significant_Line);
                    HereIsDiffSpikeSubSel = HereIsDiffSpikeSubSel | ...
                        (NumSpikePixels/NumLinePixels > Args.DiffSpikeFracThreshold);
                end
                IsDiffSpikeSubSel(INearSat) = HereIsDiffSpikeSubSel;
            end

            IsDiffSpike(NearSatNotStar) = IsDiffSpikeSubSel;
            FilterFlags = setFilterBit(FilterFlags, IsDiffSpike, BD_TF, 'DiffSpike');
            
        end

        % ----- Photometry Flux -----

        if Args.flagLimitingMag && CandCat.isColumn('N_MAG_PSF') && CandCat.isColumn('R_MAG_PSF')
            MagBelowLimit = (N_MAG_PSF > N_LIMMAG) & (R_MAG_PSF > R_LIMMAG);
            FilterFlags = FilterFlags + MagBelowLimit.*2.^BD_TF.name2bit('LIMMAG');
        end        

        % Apply Chi2 per degrees of freedom criterium.
        if Args.flagChi2 && N_PSFPhot_isSolved && R_PSFPhot_isSolved && D_PSFPhot_isSolved

            % Test local Chi2
            N_Passes_CHI2DOF_Local = ...
                (N_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (N_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));

            % Nothing detectable at the candidate position in R, so the narrow N
            % fit is uncontaminated and can be required as well.
            % TODO: temporary, to be removed once the new hot pixel filter
            % is implemented
            CleanIsolated = IsolatedCand & ~AmbIsolated;

            % The residual itself has to look like the PSF in the difference
            % image. This is the direct test of whether the subtraction produced
            % a believable point source.
            Passes_CHI2DOF_D = ...
                (D_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (D_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));

            % The PSF reconstruction has to work in the regime of the
            % subtraction. Globally in N and R ...
            N_CHI2DOF_Global = CandCat.getCol('N_PSF_CHI2DOF_MED');
            R_CHI2DOF_Global = CandCat.getCol('R_PSF_CHI2DOF_MED');

            N_Passes_CHI2DOF_Global = ...
                (N_CHI2DOF_Global > Args.Chi2dofLimitsGlobal(1)) & ...
                (N_CHI2DOF_Global < Args.Chi2dofLimitsGlobal(2));
            R_Passes_CHI2DOF_Global = ...
                ((R_CHI2DOF_Global > Args.Chi2dofLimitsGlobal(1)) & ...
                (R_CHI2DOF_Global < Args.Chi2dofLimitsGlobal(2))) | ...
                isnan(R_CHI2DOF_Global);

            Passes_CHI2DOF_Global = N_Passes_CHI2DOF_Global & R_Passes_CHI2DOF_Global;

            % ... and locally around the candidate.
            N_CHI2DOF_Locality = CandCat.getCol('N_PSF_CHI2DOF_LOCAL_MED');
            R_CHI2DOF_Locality = CandCat.getCol('R_PSF_CHI2DOF_LOCAL_MED');

            N_Passes_CHI2DOF_Locality = ...
                ((N_CHI2DOF_Locality > Args.Chi2dofLimitsLocality(1)) & ...
                (N_CHI2DOF_Locality < Args.Chi2dofLimitsLocality(2))) | ...
                isnan(N_CHI2DOF_Locality);

            R_Passes_CHI2DOF_Locality = ...
                ((R_CHI2DOF_Locality > Args.Chi2dofLimitsLocality(1)) & ...
                (R_CHI2DOF_Locality < Args.Chi2dofLimitsLocality(2))) | ...
                isnan(R_CHI2DOF_Locality);

            Passes_CHI2DOF_Locality = ...
                N_Passes_CHI2DOF_Locality & R_Passes_CHI2DOF_Locality;

            % A candidate brighter than any nearby persistent source cannot be
            % the residual of one.
            if ~isempty(DgreaterNearbyR)
                Passes_CHI2DOF_Locality = Passes_CHI2DOF_Locality | DgreaterNearbyR;
            end

            Passes_CHI2DOF = Passes_CHI2DOF_D ...
                & Passes_CHI2DOF_Global & Passes_CHI2DOF_Locality ...
                & (~CleanIsolated | N_Passes_CHI2DOF_Local);

            CHI2DOF_Flagged = ~Passes_CHI2DOF;
            FilterFlags = setFilterBit(FilterFlags, CHI2DOF_Flagged, BD_TF, 'PSFChi2');
        end

        % ----- Physical contaminants -----

        % Flag stars as non-transients
        if Args.flagStarMatches
            FilterFlags = setFilterBit(FilterFlags, IsStar, BD_TF, 'StarMatch');
        end

        % Flag minor planets as non-transients
        if Args.flagMP && CandCat.isColumn('N_DistMP') && CandCat.isColumn('R_DistMP')
            MinorPlanet = (CandCat.getCol('N_DistMP') < Args.MPDistThresh) | ...
                          (CandCat.getCol('R_DistMP') < Args.MPDistThresh);

            FilterFlags = setFilterBit(FilterFlags, MinorPlanet, BD_TF, 'MPMatch');
            
        end
        
        if Args.flagVariable && CandCat.isColumn('GAL_DIST') && CandCat.isColumn('STAR_DIST')
            % TODO: Maybe move the catalog matching elsewhere
      
            % Get coordinates center of candidates catalog and radius to
            % furtherst candidate from the center.
            MidRA = median(RA);
            MidDec = median(Dec);

            MaxDist = max(celestial.coo.sphere_dist(RA, Dec,...
                MidRA*ones(NumCand,1), MidDec*ones(NumCand,1)));
            MaxDistAngle = AstroAngle(MaxDist, 'rad');
    
            % QSO for galaxies
            % Use the maxium candidate distance + maximum galaxy distance
            % among candidates as search radius for QSOs.
            GalSearchRadius = MaxDistAngle.convert('arcsec').Angle + max(GalDist);

            % Get local QSO catalog
            QSOCat = catsHTM.cone_search('QSO1M', ...
                    MidRA, MidDec, GalSearchRadius, 'OutType','AstroCatalog');

            % If local QSO catalog not empty, match QSOs to candidates.
            if QSOCat.sizeCatalog < 1
                VariableGal = zeros(NumCand,1);
            else
                QSOCat.sortrows('Dec');
                [QSOLon, QSOLat] = QSOCat.getLonLat('rad');
    
                % We're matching galaxy nuclei, so the matching radius is
                % on candidate postions.
                MatchResQSO = VO.search.search_sortedlat_multi( ...
                    [QSOLon, QSOLat], RA, Dec, -PointLimit5*Arcsec2Rad);
    
                % Flag candidates as variable if matched to a QSO.
                VariableGal = vertcat(MatchResQSO.Nmatch) > 0;
            end

            % VarStars for stars
            % Note that we're using GAIA which is not only stars but
            % variable galaxies also. I'll keep refereing to them as stars
            % but matching variable galaxies this way is also a good thing.

            % Use the maxium candidate distance + maximum star distance
            % among candidates as search radius for variable stars.
            StarSearchRadius = MaxDistAngle.convert('arcsec').Angle + max(StarDist);

            % Get local variable star catalog.
            VarStarCat = catsHTM.cone_search('GAIADR3var', MidRA, MidDec, ...
                StarSearchRadius, 'OutType','AstroCatalog');

            % If local variable star catalog not empty, match variable stars
            % to candidates.
            if VarStarCat.sizeCatalog < 1
                VariableStar = zeros(NumCand,1);
            else
                VarStarCat.sortrows('Dec');
                [VarStarLon, VarStarLat] = VarStarCat.getLonLat('rad');
    
                % Use maximum star distance as matching radius to variable
                % stars.
                MatchResVarStar = VO.search.search_sortedlat_multi( ...
                    [VarStarLon, VarStarLat], RA, Dec, ...
                    -max(StarDist)*Arcsec2Rad);
    
                VarStarmatch = vertcat(MatchResVarStar.Nmatch) > 0;

                % Flag candidates as variable if matched to a variable star
                % and if the candidate is on star position.
                VariableStar = NearStar & VarStarmatch;
            end
            
            % Flag variable sources, AGNs as well as stars.
            VariableSource = VariableGal | VariableStar;
            
            FilterFlags = setFilterBit(FilterFlags, VariableSource, BD_TF, 'Variable');

        end

        % Always last
        if Args.flagDensity

            % Only count neighbors that have passed filters mentioned in
            % Args.NeighborExlude
            ExcludeNeighbor = false(NumCand,1);
            NumExclude = numel(Args.NeighborExclude);
            
            for IExclude = 1:NumExclude
                ExcludeNeighbor = ExcludeNeighbor | ...
                    BD_TF.findBit(FilterFlags, Args.NeighborExclude{IExclude});
            end

            % Initialize arrays, number of neighbors and the local density.
            NumNeighbors = zeros(NumCand,1);
            LocalDensity = zeros(NumCand,1);

            % Iterate through each candidate
            for Itran = NumCand:-1:1
                % Get distance to all other candidates
                NeighborDist = sqrt((X(Itran)-X(:)).^2+(Y(Itran)-Y(:)).^2);
                % Test distance against threshold
                IsNeighbor = NeighborDist < Args.NeighborDistanceThreshold;
                % Exclude itself
                IsNeighbor = IsNeighbor & (NeighborDist > 0);
                % Remove excluded neighbors
                IsNeighbor = IsNeighbor & ~ExcludeNeighbor;
                % Count remaining neighbors and remember.
                NumNeighbors(Itran) = sum(IsNeighbor);
                % Sum the reciprocal distance to each neighbor and save as
                % the local density.
                LocalDensity(Itran) = sum(1./NeighborDist(IsNeighbor));
            end

            % Add number of neighbors and the local density to catalog
            NumNeighbors = cast(NumNeighbors,'double');
            LocalDensity = cast(LocalDensity, 'double');
            TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
                cell2mat({NumNeighbors,LocalDensity}), ...
                'SCORE', {'N_NEIGH','DENSITY'}, {'',''});

            % Test number of neighbors against threshold
            Overdensity = (LocalDensity > 1.0) | ...
                (NumNeighbors.*LocalDensity >= Args.NeighborDenThreshold);

            % Update flags
            FilterFlags = setFilterBit(FilterFlags, Overdensity, BD_TF, 'Overdensity');
            
        end

        % Check for nuclear noise
        if Args.flagNuclearNoise && any(NuclearCand)

            GalPSFNoiseCand = NuclearCand | (NearStar & GalCand);

            % Get R magnitude and score of nuclear candidates
            NuclearRMag = R_MAG_PSF(GalPSFNoiseCand);
            NuclearScore = Score(GalPSFNoiseCand);

            % Initialize result array
            NumNuclear = sum(GalPSFNoiseCand);
            NuclearNoise = false(NumNuclear,1);

            % Only test nuclear candidates if it's detectable in R image
            RDetNuclear = (NuclearRMag < R_LIMMAG);
            BrightNuclear = (NuclearRMag < Args.BrightGalMagThresh);
            TopPercentile = Args.NuclearDefaultPrcThresh .* ones(NumNuclear,1);
            TopPercentile(BrightNuclear) = Args.BrightGalPrcThresh;

            R_MAG_PSF_4Nuc = R_MAG_PSF;
            Score_4Nuc = Score;

            % Some comparison sources are at the edge of the nan-border
            % and have a low R_MAG_PSF but high SCORE value, which
            % leads to filtering of real nuclear transients.
            % TODO: this maybe should be done cleaner
            ExcludeComparison = false(NumCand,1);

            if any(BadPixHard)
                ExcludeComparison = ExcludeComparison | BadPixHard;
            end
            
            if ~isempty(Injections)
                ExcludeComparison = ExcludeComparison | Injections;
            end

            if any(NearStar)
                ExcludeComparison = ExcludeComparison | ~NearStar;
            end

            if CandCat.isColumn('S_CORR')
                Scorr = CandCat.getCol('S_CORR');

                Score_4Nuc = Scorr;
                NuclearScore = Scorr(GalPSFNoiseCand);
                
                ExcludeComparison = ExcludeComparison | (sign(Scorr) ~= sign(Score));
            end
            
            % Loop through each and assign corresponding median
            for INuclear = 1:NumNuclear
                if ~RDetNuclear(INuclear)
                    continue
                end
                % Construct R mag bin
                % Use the nuclear candidate R mag as the upper edge 
                % (faint end) and -0.5 as the lower edge (bright end).
                % This way the nuclear candidate has the highest R
                % magnitude in the sample and if the true image flux is the
                % same at N epoch, it will have the lowest Score.
                TargetRMag = NuclearRMag(INuclear);
                DynamicBinMin = TargetRMag - Args.NuclearMagBinWidth;
                DynamicBinMax = TargetRMag;
                BinnedMags = (R_MAG_PSF_4Nuc > DynamicBinMin) ...
                    & (R_MAG_PSF_4Nuc < DynamicBinMax);

                BinnedMags = BinnedMags & ~ExcludeComparison;
                
                % If bin is empty, assume that this magnitude range is well
                % subtracted and don't flag the candidate.
                % TODO: this could be done more elegantly by verifying
                % againt the R catalog
                if sum(BinnedMags) == 0
                    continue
                end

                % Test if candidate score is above median score for its
                % R mag bin. This should be true if the candidate is the
                % only transient source in its bin.
                BinThresholdS = prctile(Score_4Nuc(BinnedMags), TopPercentile(INuclear));
                NuclearNoise(INuclear) = (NuclearScore(INuclear) < BinThresholdS);
            end

            if ~isempty(DgreaterR)
                NuclearNoise = NuclearNoise & ~DgreaterR(GalPSFNoiseCand);
            end

            FilterFlags(GalPSFNoiseCand) = setFilterBit(...
                FilterFlags(GalPSFNoiseCand), NuclearNoise, BD_TF, 'NuclearNoise');
        end

        % ----- AstroZOGY -----

        if Args.flagScorr
            % Get Scorr and difference between Score and Scorr
            Scorr = CandCat.getCol('S_CORR');
            SDiff = abs(Score) - abs(Scorr);

            % Exclude isolated candidates.
            % Exclude also galaxy matched candidates that are not nuclear.
            ExcludeCand = (GalCand & ~NuclearCand & ~NearStar & abs(Scorr) > 3.0);
            
            if ~isempty(IsolatedCand)
                ExcludeCand = ExcludeCand | IsolatedCand | AmbBlendedCand;
            end

            if ~isempty(DgreaterR)
                ExcludeCand = ExcludeCand | DgreaterR;
            end

            % Test if Score is higher than Scorr (has to be), Scorr is
            % above threshold and the difference between Score and Scorr is
            % below threshold.

            ScorrSane = (sign(Scorr) == sign(Score) ) ...
                & (SDiff > Args.ScorrDiffLowerThreshold );

            ScorrHigh = ((abs(Scorr) > Args.ScorrThreshold) | ...
                (SDiff < Args.ScorrCorrectionParam));

            ScorrGood = ScorrSane & (ScorrHigh | ExcludeCand);

            ScorrFlagged = ~ScorrGood;
            FilterFlags = setFilterBit(FilterFlags, ScorrFlagged, BD_TF, 'Scorr');
        end

        if Args.flagTranslients
            % Get S2 and Z2 AICs and their difference.
            S2_AIC = CandCat.getCol('S2_AIC');
            Z2_AIC = CandCat.getCol('Z2_AIC');
            AIC_Diff = S2_AIC - Z2_AIC;

            % Exclude isolated candidates unless PSF shape is poor.
            % Exclude also galaxy matched candidates that are not nuclear
            % and do not match to stars.
            ExcludeCand = (GalCand & ~NuclearCand & ~NearStar);

            if ~isempty(IsolatedCand)
                ExcludeCand = ExcludeCand | (IsolatedCand & ~AmbIsolated);
            end

            if ~isempty(DgreaterNearbyR)
                ExcludeCand = ExcludeCand | DgreaterNearbyR;
            end

            IsNotTranslient = (AIC_Diff < Args.TranslientThresh) ...
                | ExcludeCand;

            TranslientFlagged = ~IsNotTranslient;
            FilterFlags = setFilterBit(FilterFlags, TranslientFlagged, BD_TF, 'Translient');

        end

        % ----- Candidate property bits -----
        % Properties that depend on the Ref source catalogue and so cannot be
        % recovered from the output columns. Bit values come from the
        % candidate-property dictionary; imProc.sub.calibrateTransients sets
        % further bits in the same column later in the pipeline.

        BD_CP = BitDictionary(Args.CandPropsDict);

        CandProps = zeros(NumCand,1);

        CandProps = setCandPropBit(CandProps, NoNearbyRSrc, BD_CP, ...
            'NoNearbyRSrc');
        CandProps = setCandPropBit(CandProps, DgreaterNearbyR, BD_CP, ...
            'DgreaterNearbyR');

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(FilterFlags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});

        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(CandProps, 'double'), 'FLAGS_TRANSIENT', ...
            {'CAND_PROPS'}, {''});

    end
  
end

function Args = applyConfigFile(Args)
    %{
    Read a JSON configuration file and override matching Args fields.

    Input   : - Args structure or arguments block struct.
                The structure must contain the field:
                'ConfigFile' - Path to JSON configuration file. If empty or
                       if the file does not exist, Args is returned
                       unchanged.

    Output  : - Args structure with fields updated from the JSON file.
                Only fields already present in Args are updated.
                Unknown configuration fields are ignored with a warning.

    Description : The configuration file is parsed using jsondecode. For
                  each field in the JSON object, the function checks whether
                  the same field exists in Args. If it does, the value from
                  the configuration file is copied into Args. If it does
                  not, a warning is issued and the field is ignored.

                  Numeric 2-element vectors are reshaped into row vectors
                  before assignment. This is useful for thresholds that are
                  expected to remain in 1x2 form.

    Author  : Ruslan Konno (Mar 2026)
    Example : Args.ConfigFile = 'flagNonTransients.json';
              Args = applyConfigFile(Args);
    %}    

    if isempty(Args.ConfigFile) || ~exist(Args.ConfigFile, 'file')
        return
    end

    fid = fopen(Args.ConfigFile, 'r');
    if fid < 0
        error('flagNonTransients:ConfigOpenFailed', ...
            'Could not open config file: %s', Args.ConfigFile);
    end

    cleaner = onCleanup(@() fclose(fid));
    raw = fread(fid, inf, '*char')';
    config = jsondecode(raw);

    configFields = fieldnames(config);
    for iField = 1:numel(configFields)
        key = configFields{iField};

        if ~isfield(Args, key)
            warning('flagNonTransients:UnknownConfigField', ...
                'Unknown config field ignored: %s', key);
            continue
        end

        val = config.(key);

        % Keep 2-element numeric vectors row-shaped for arguments that
        % expect 1x2 arrays.
        if isnumeric(val) && isvector(val) && numel(val) == 2
            val = reshape(val, 1, []);
        end

        Args.(key) = val;
    end
end

function FilterFlags = setFilterBit(FilterFlags, Mask, BD_TF, BitName)
    %{
    Set a transient-filter bit for all candidates selected by a mask.

    Input   : - Column vector of filter bit values for all candidates.
              - Logical mask selecting candidates for which to set the bit.
              - BitDictionary object for transient-filter bits.
              - Bit name to set.

    Output  : - Updated column vector of filter bit values.

    Description : This is a small helper function that updates the
                  FLAGS_TRANSIENT bitmask. For all entries where Mask is
                  true, the bit corresponding to BitName is added to
                  FilterFlags using the transient-filter bit dictionary.

                  If Mask is empty, the function returns immediately
                  without modifying FilterFlags.

    Author  : Ruslan Konno (Mar 2026)
    Example : FilterFlags = setFilterBit(FilterFlags, NegCand, BD_TF, ...
                  'Negative');
    %}    

    if isempty(Mask)
        return
    end
    FilterFlags = FilterFlags + Mask .* 2.^BD_TF.name2bit(BitName);
end

function CandProps = setCandPropBit(CandProps, Mask, BD_CP, BitName)
    %{
    Set a candidate-property bit for all candidates selected by a mask.

    Input   : - Column vector of candidate-property bit values.
              - Logical mask selecting candidates for which to set the bit.
              - BitDictionary object for candidate-property bits.
              - Bit name to set.

    Output  : - Updated column vector of candidate-property bit values.

    Description : Counterpart of setFilterBit for the CAND_PROPS column.
                  Uses bitor rather than addition so that setting a bit that
                  is already present is a no-op.

                  If Mask is empty, the function returns immediately without
                  modifying CandProps.

    Author  : Ruslan Konno
    Example : CandProps = setCandPropBit(CandProps, NoNearbyRSrc, BD_CP, ...
                  'NoNearbyRSrc');
    %}

    if isempty(Mask)
        return
    end

    [~, BitDec] = BD_CP.name2bit(BitName);
    Sel = logical(Mask);
    CandProps(Sel) = bitor(CandProps(Sel), BitDec);
end
