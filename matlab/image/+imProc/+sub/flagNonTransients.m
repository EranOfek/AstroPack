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

                'PointLimitSigmaFactor' - Conversion factor from FWHM to
                       an approximate 3-sigma radius used for nuclear
                       matching.
                       Default is 1.2739.

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

                'BadPix_Soft' - Cell array of {bitName, thresholdIncrement}.
                       Default is {{'DarkHighVal',1.2},{'CR_DeltaHT',2.9}}.

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

                'MinStarProb' - Minimum STAR_PROB when both star and galaxy
                       probabilities exist.
                       Default is 0.15.

                'MinStarProbNoGal' - Minimum STAR_PROB when no GAL_PROB exists.
                       Default is 0.30.

                'StarGalLogRatioThresh' - Threshold on log(STAR/GAL).
                       Default is 1.0.

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

                'flagNPsfShape' - Flag candidates with poor N-image PSF shape.
                       Default is true.

                'SecondMomSoftLim' - Soft limit on second moments.
                       Default is 1.3.

                'SecondMomHardLim' - Hard limit on second moments.
                       Default is 5.5.

                'SecondMomAsymLim' - Asymmetry threshold |X2-Y2|.
                       Default is 5.

                'OmniDirectionThreshold' - [circVar, angleErr] thresholds.
                       Default is [0.7 57.0].

                'PeakDistThreshold' - Peak alignment threshold (pix).
                       Default is 3.0.

                'ContaminationBackRatio' - Tail flux relative to background.
                       Default is 0.1.

                'ContaminationMag' - Required log flux ratio for passing.
                       Default is 0.48.

                'ContaminationRadius' - Matching radius in PSF units.
                       Default is 1.5.

                'ContaminatorBlendChi2Thresh' - Chi2 threshold for blended
                       contaminators.
                       Default is 6.5.

                'flagDPSFShape' - Flag candidates based on D-image PSF shape.
                       Default is false.

                'PSFShapeXYMeanD' - Mean of Gaussian model in (X2,Y2).
                       Default is [1.06919192,1.24191919].

                'PSFShapeCovD' - Covariance matrix of Gaussian model.
                       Default is [0.06467546,0.02720397;
                                   0.02720397,0.06933742].

                'PSFShapeConfThreshD' - Confidence threshold.
                       Default is 0.95.

                'flagExtended' - Flag extended (non-PSF-like) sources.
                       Default is true.

                'ExtendedThreshold' - Threshold on SCORE vs SN_ext.
                       Default is -0.59.

                'ExtendedSatDelta' - Relaxation near saturation.
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

                'TranslientExpThresh' - Exponential threshold parameters.
                       Default is [9.76703546,-0.09972362,-0.08558244].

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
        Args.PointLimitSigmaFactor double = 1.2739   % converts FWHM to ~3 sigma radius
        Args.injectedSrcs double = []

        % Negative candidates
        Args.flagNegatives logical = true

        % Chi2 filters
        Args.flagChi2 logical = true
        Args.Chi2dofLimitsLocal (1,2) double = [0.1 2.2]
        Args.Chi2dofLimitsGlobal (1,2) double = [0.1 2.5]

        % Saturation / mask neighborhood
        Args.flagSaturated logical = true
        Args.SaturatedNeighborDistanceThreshold double = 250

        % Hard bad-pixel filters
        Args.flagBadPix_Hard logical = true
        Args.BadPix_Hard cell = {'Interpolated','NaN','NearEdge','Hole','Negative'}

        % Soft bad-pixel filters
        Args.flagBadPix_Soft logical = true
        Args.BadPix_Soft cell = {{'DarkHighVal',1.2},{'CR_DeltaHT',2.9}}

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
        Args.MinStarProb double = 0.15
        Args.MinStarProbNoGal double = 0.01
        Args.StarGalLogRatioThresh double = 1.0

        % D-image artifact filters
        Args.flagRinging logical = true
        Args.flagPeakValley logical = true
        Args.PVDistThresh double = 10

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

        % N-image PSF shape
        Args.flagNPsfShape logical = true
        Args.SecondMomSoftLim double = 1.3
        Args.SecondMomHardLim double = 5.5
        Args.SecondMomAsymLim double = 5
        Args.OmniDirectionThreshold (1,2) double = [0.7 57.0]
        Args.PeakDistThreshold double = 3.0

        % Contamination logic
        Args.ContaminationBackRatio double = 0.1
        Args.ContaminationMag double = 0.48
        Args.ContaminationRadius double = 1.5
        Args.ContaminatorBlendChi2Thresh double = 6.5
        Args.ContaminationSelfRadiusFactor double = 1.5
        Args.ContaminationStdAnnulusMax double = 7.0
        Args.ContaminationBackAnnulusMax double = 3.0
        Args.ContaminationBackAnnulusFallbackMax double = 5.0

        % D-image PSF shape
        Args.flagDPSFShape logical = false
        Args.PSFShapeXYMeanD (1,2) double = [1.06919192, 1.24191919]
        Args.PSFShapeCovD (2,2) double = [0.06467546, 0.02720397; 0.02720397, 0.06933742]
        Args.PSFShapeConfThreshD double = 0.95

        % Extendedness
        Args.flagExtended logical = true
        Args.ExtendedThreshold double = -0.59
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
        Args.BrightGalPrcThresh double = 80
        Args.NuclearDefaultPrcThresh double = 50
        Args.NuclearMagBinWidth double = 0.5

        % AstroZOGY
        Args.flagScorr logical = true
        Args.ScorrThreshold double = 5.0
        Args.ScorrCorrectionParam double = 0.7

        Args.flagTranslients logical = true
        Args.TranslientThresh double = 0.48
        Args.TranslientExpThresh (1,3) double = [9.76703546, -0.09972362, -0.08558244]
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
    
        % Based on sigma in arcsec.
        PointLimit = Obj(Iobj).PSFData.fwhm ...
            .* Args.PixelScale .* Args.PointLimitSigmaFactor;

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
        N_X2 = [];
        N_Y2 = [];
        R_X2 = [];
        R_Y2 = [];

        % Initialize transients bool
        FilterFlags = zeros(NumCand,1);

        % Get positive and negative candidates
        %PosTran = (Score > 0.0);
        NegCand = (Score < 0.0);

        % Get limiting magnitudes of N and R
        N_LIMMAG = Obj(Iobj).New.HeaderData.getVal('LIMMAG');
        R_LIMMAG = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');

        MedDiffVar = median(Obj(Iobj).Var(:));

        % N and R PSF magnitudes
        if CandCat.isColumn('N_MAG_PSF')
            N_MAG_PSF = CandCat.getCol('N_MAG_PSF');
        end
        if CandCat.isColumn('R_MAG_PSF')
            R_MAG_PSF = CandCat.getCol('R_MAG_PSF');
        end
        
        % Get isolated and blended candidates
        if CandCat.isColumn('R_SN')
            R_SN = CandCat.getCol('R_SN');
            IsolatedCand = (R_SN < 3);
            BlendedCand = ~IsolatedCand;

            % These might actually be isolated.
            AmbBlendedCand = BlendedCand & (R_MAG_PSF > R_LIMMAG);
        end

        % Get candidate New and Ref bits masks values
        N_BM = CandCat.getCol('N_FLAGS');
        R_BM = CandCat.getCol('R_FLAGS');

        % Get XY coordinates
        [X,Y] = CandCat.getXY();

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

        HasNX2Y2 = ~isempty(N_X2) && ~isempty(N_Y2);
        if HasNX2Y2
            N_GoodPSF = (N_X2 < Args.SecondMomSoftLim) & ...
                        (N_Y2 < Args.SecondMomSoftLim);
        else
            N_GoodPSF = true(NumCand,1);
        end

        if CandCat.isColumn('R_X2')
            R_X2 = CandCat.getCol('R_X2');
        end

        if CandCat.isColumn('R_Y2')
            R_Y2 = CandCat.getCol('R_Y2');
        end
        HasRX2Y2 = ~isempty(R_X2) && ~isempty(R_Y2);

        if HasRX2Y2
            R_GoodPSF = (R_X2 < Args.SecondMomHardLim) ...
                & (R_Y2 < Args.SecondMomHardLim);
        end

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
            % 5sig for nuclear check, dirty, I know
            % TODO: rather than doing this here, match2Galaxies should be
            % extended to determined if a source is nuclear or not,
            % probably best to write a dedicated matchTransients2Galaxies
            % function which uses the N, R, and D catalogs
            NuclearCand = GalDist < PointLimit*5/3; 
        else
            NuclearCand = false(NumCand,1);
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

            HasStar = ~isnan(Star_Prob);
            HasGal  = ~isnan(Gal_Prob);

            IsStarBoth = StarCand & HasStar & HasGal & ...
                         (Star_Prob > Args.MinStarProb) & ...
                         (ScoreSG > Args.StarGalLogRatioThresh);

            IsStarNoGal = StarCand & HasStar & ~HasGal & ...
                          (Star_Prob > Args.MinStarProbNoGal);

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

        if Args.flagSubVisit

            N_FLAGS = Obj(Iobj).New.MaskData.bitwise_cutouts([X, Y], ...
                'or', 'HalfSize', Args.BadPixSatRad);
            R_FLAGS = Obj(Iobj).Ref.MaskData.bitwise_cutouts([X, Y], ...
                'or', 'HalfSize', Args.BadPixSatRad);

            N_BadPixSat = BD_IM.findBit(N_FLAGS,'Saturated');
            R_BadPixSat = BD_IM.findBit(R_FLAGS,'Saturated');

            N_hasHighFlux = CandCat.getCol('N_FLUX_PSF') > Args.BadPixSatFlux;
            R_hasHighFlux = CandCat.getCol('R_FLUX_PSF') > Args.BadPixSatFlux;

            N_FalseSaturation = (N_BadPixSat & ~R_BadPixSat & ~N_hasHighFlux);
            R_FalseSaturation = (~N_BadPixSat & R_BadPixSat & ~R_hasHighFlux);
            FalseSaturation = N_FalseSaturation | R_FalseSaturation;

            FilterFlags = setFilterBit(FilterFlags, FalseSaturation, BD_TF, 'SubVisit');
        end

        % Apply soft bit mask criteria.
        if Args.flagBadPix_Soft && CandCat.isColumn('SN_delta')

            SN_delta = CandCat.getCol('SN_delta');
            SdiffSd = Score - SN_delta;

            BPSThresh = zeros(NumCand,1);

            NumBadSoft = numel(Args.BadPix_Soft);

            for IBad=1:1:NumBadSoft
                IBadPix_Soft = Args.BadPix_Soft{IBad};

                BPinNew = BD_IM.findBit(N_BM, IBadPix_Soft{1});
                %BPinRef = BD_IM.findBit(R_BM, IBadPix_Soft{1});

                BPSThresh(BPinNew) = BPSThresh(BPinNew) ...
                    + IBadPix_Soft{2};
            end

            BadPixSoft = (SdiffSd < BPSThresh);

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

            for IStreak = 1:Args.NumStreaks

                Xt = X(SubSel);
                Yt = Y(SubSel);

                TDist = max( ...
                    Obj(Iobj).PSFData.fwhm .* Args.StreakThresholdDistFWHMFactor, ...
                    Args.StreakThresholdDistMin);

                Res.Found = false;
                for IMinNumPts = numel(Args.StreakRansacMinNumPts):-1:1
                    Res = tools.math.fit.ransacLinear([Xt,Yt], ...
                        'Ntrial', Args.StreakRansacNtrial, ...
                        'MinRMS', Args.StreakRansacMinRMS, ...
                        'MinNpt', Args.StreakRansacMinNumPts(IMinNumPts), ...
                        'ThresholdDist', TDist);
                    if Res.Found
                        break
                    end
                end

                if ~Res.Found
                    break
                end

                ModY = Res.Par(1) + Xt .* Res.Par(2);
                Streak = abs(ModY - Yt) < Args.StreakDistanceThreshold;

                FilterFlags(SubSel) = setFilterBit( ...
                    FilterFlags(SubSel), Streak, BD_TF, 'Streak');

                SubSel(SubSel) = ~Streak;
            end
        end

        % ----- PSF Shape -----

        if Args.flagExtended && CandCat.isColumn('SN_ext')

            SN_ext = CandCat.getCol('SN_ext');

            ExtendedThreshold = ones(NumCand,1)*Args.ExtendedThreshold;

            if exist('NearSaturated', 'var')
                ExtendedThreshold = ExtendedThreshold + Args.ExtendedSatDelta*NearSaturated;
            end

            ExtendedSource = abs(Score) - abs(SN_ext) < ExtendedThreshold;

            FilterFlags = setFilterBit(FilterFlags, ExtendedSource, BD_TF, 'Extended');
        end

        if Args.flagDPSFShape
            X2 = CandCat.getCol('X2');
            Y2 = CandCat.getCol('Y2');

            X2Y2 = [X2(:),Y2(:)];

            ProbD = mvnpdf(X2Y2, Args.PSFShapeXYMeanD, Args.PSFShapeCovD);

            PassesD = ProbD > (1-Args.PSFShapeConfThreshD);
            PSFShapeFlagged = ~PassesD;
            FilterFlags = setFilterBit(FilterFlags, PSFShapeFlagged, BD_TF, 'DPSFShape');
        end        

        if Args.flagNPsfShape

            % Test global shape. For isolated candidates only N shape.
            N_Passes_PSF_Global = N_GoodPSF;
            R_Passes_PSF_Global = (R_GoodPSF | IsolatedCand);
    
            N_Passes_PSFShape = N_Passes_PSF_Global;
            R_Passes_PSFShape = R_Passes_PSF_Global;

            % Use hard limits on global shape no matter local results.
            N_Passes_HardLim = (N_X2 < Args.SecondMomHardLim) ...
                & (N_Y2 < Args.SecondMomHardLim);

            ContaminationFlux = zeros(NumCand,1);

            % If the global PSF is wide, check for local contaminating
            % sources
            if any(~N_GoodPSF) && any(N_Passes_HardLim)

                % N flux may be centered on a center-of-mass between e.g.
                % galaxy nucleus and a SN
                % So we will use the N image PSF stamp and moments, but
                % with photometry from the R image, that way a transient
                % will not affect the astrometry.
                R_IntFlux = Obj(Iobj).Ref.CatData.getCol('FLUX_APER_3');

                N_IntFlux = R_IntFlux*10^(0.4*(Obj.ZpN-Obj.ZpR));

                % Get sources that contaminate beyond the PSF stamp
                % User the smaller PSF between N and R
                N_PSFSize = floor(size(Obj(Iobj).New.PSFData.getPSF,2)/2);
                R_PSFSize = floor(size(Obj(Iobj).Ref.PSFData.getPSF,2)/2);
                PSFSize_Min = min(N_PSFSize,R_PSFSize)-2.0;
                PSFSize_Max = max(N_PSFSize,R_PSFSize);

                % Recalculating the moments due to issue #701, this should change once the
                % issue is properly fixed. TODO
                NewPSF = Obj(Iobj).New.PSF;
                PSFbw = imbinarize(NewPSF);
                stats = regionprops(PSFbw, 'Orientation');
                if numel(stats) > 1
                    stats = stats([stats.Orientation] ~= 0);
                end
                PSFnew = imrotate(NewPSF, -stats.Orientation, 'bilinear', 'crop');
                [~, M2, ~] = imUtil.image.moment2(PSFnew, ...
                    N_PSFSize, N_PSFSize, 'MaxIter',-1,...
                    'MomRadius', 1.7*Obj(Iobj).New.PSFData.fwhm);

                Med_NX2 = M2.X2;
                Med_NY2 = M2.Y2;

                % Get the flux fraction that is expected in the tails
                % beyond the PSF stamp.
                FractionTailFlux = 1 - ...
                    erf((PSFSize_Min)./sqrt(2*Med_NX2))*erf((PSFSize_Min)./sqrt(2*Med_NY2));
                N_TailFlux = N_IntFlux*FractionTailFlux;

                % Count all sources with a tail flux of more than 10% of 
                % the background as contaminating sources.
                Contaminators = (N_TailFlux > Args.ContaminationBackRatio*Obj.BackN);

                % Match candidates to contaminating sources within
                % contamination radius.
                [R_NativeRA, R_NativeDec] = Obj(Iobj).Ref.CatData.getLonLat('rad');
                WideRadius = ceil(Args.ContaminationRadius*PSFSize_Max*Args.PixelScale);

                % Select positions and tail fluxes of contaminating sources.
                R_NativeContRa = R_NativeRA(Contaminators);
                R_NativeContDec = R_NativeDec(Contaminators);
                N_ContTailFlux = N_TailFlux(Contaminators);

                % Blended sources in the R image will be counted as one
                % source in the R-image catalog. A contamination can occur
                % at the edge of an unregistered source, so we'll identify
                % poorly fitted R-image sources and use a bigger radius for
                % them. 

                R_NativeCHI2 = Obj(Iobj).Ref.CatData.getCol('PSF_CHI2DOF');
                R_NativeContCHI2 = R_NativeCHI2(Contaminators);
                BlendedContaminators = ...
                    (R_NativeContCHI2 > Args.ContaminatorBlendChi2Thresh);

                % Same for blended contaminators
                R_NativeBlendedContRa = R_NativeContRa(BlendedContaminators);
                R_NativeBlendedContDec = R_NativeContDec(BlendedContaminators);

                % Match candidates to contaminating sources in wide range.
                if sum(Contaminators) > 0
                    N_ContCatMatchWide = VO.search.search_sortedlat_multi( ...
                        [R_NativeContRa, R_NativeContDec], RA, Dec, ...
                        -WideRadius*Arcsec2Rad);

                    if sum(BlendedContaminators) > 0
                        N_BlendedContCatMatchWide = VO.search.search_sortedlat_multi( ...
                            [R_NativeBlendedContRa, R_NativeBlendedContDec], RA, Dec, ...
                            -2.*WideRadius*Arcsec2Rad);

                        % Merge N_BlendedContCatMatchWide into N_ContCatMatchWide row by row
                        % map blended-subset indices back to full contaminator indices
                        BlendedToContIdx = find(BlendedContaminators);
                        
                        for i = 1:numel(N_ContCatMatchWide)
                        
                            % --- general matches ---
                            indA  = N_ContCatMatchWide(i).Ind(:);
                            distA = N_ContCatMatchWide(i).Dist(:);
                        
                            % --- blended matches, remapped to full contaminator indexing ---
                            indB_local = N_BlendedContCatMatchWide(i).Ind(:);
                            distB      = N_BlendedContCatMatchWide(i).Dist(:);
                        
                            if ~isempty(indB_local)
                                indB = BlendedToContIdx(indB_local);
                            else
                                indB = [];
                            end
                        
                            % append only indices not already present
                            isNew = ~ismember(indB, indA);
                        
                            indMerged  = [indA;  indB(isNew)];
                            distMerged = [distA; distB(isNew)];
                        
                            % update struct
                            N_ContCatMatchWide(i).Ind    = indMerged;
                            N_ContCatMatchWide(i).Dist   = distMerged;
                            N_ContCatMatchWide(i).Nmatch = numel(indMerged);
                        
                            if ~isempty(indMerged)
                                [~, kmin] = min(distMerged);
                                N_ContCatMatchWide(i).Ind1 = indMerged(kmin);
                            else
                                N_ContCatMatchWide(i).Ind1 = [];
                            end
                        end
    
                    end

                    NumMatchesWideCont = vertcat(N_ContCatMatchWide.Nmatch);
                else
                    NumMatchesWideCont = zeros(NumCand,1);
                end

                N_Passes_Local = (NumMatchesWideCont < 1);
                CandFluxes = CandCat.getCol('FLUX_PSF');

                SelfSrcRad = Args.ContaminationSelfRadiusFactor .* ...
                    Args.PixelScale .* Arcsec2Rad;

                STD_ANNULUS = CandCat.getCol('STD_ANNULUS');
                BACK_ANNULUS = CandCat.getCol('BACK_ANNULUS');

                N_Passes_Local_Aper = ...
                    (STD_ANNULUS < Args.ContaminationStdAnnulusMax) & ...
                    (abs(BACK_ANNULUS) < Args.ContaminationBackAnnulusMax);

                for ICand = 1:NumCand

                    if N_Passes_Local(ICand)
                        N_Passes_Local(ICand) = ...
                            abs(BACK_ANNULUS(ICand)) < Args.ContaminationBackAnnulusFallbackMax;
                        continue
                    end
                    
                    IdxRef = N_ContCatMatchWide(ICand).Ind(:);
                    DistRad   = N_ContCatMatchWide(ICand).Dist(:);

                    % Ignore self-contamination.
                    IdxRef = IdxRef(DistRad > SelfSrcRad);

                    if isempty(IdxRef)
                        N_Passes_Local(ICand) = true;
                        continue
                    end

                    ContaminationFlux(ICand) = sum(N_ContTailFlux(IdxRef));
                    CandFlux = CandFluxes(ICand);

                    MagContamination = log10(CandFlux/ContaminationFlux(ICand));
                
                    N_Passes_Local(ICand) = (MagContamination > Args.ContaminationMag) ...
                        & N_Passes_Local_Aper(ICand);
                end

                % Update candidates as passing if they are not near any
                % contaminating sources.
                N_Passes_PSFShape = N_Passes_PSFShape | N_Passes_Local;
                N_Passes_PSF_Global = N_Passes_PSF_Global | N_Passes_Local;
            end
            
            TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
                   cell2mat({ContaminationFlux}), ...
                   'SCORE', {'FLUX_CONTAM'}, {''});

            % Test local shape. Only use local shape if global fails or
            % candidate is near saturated pixels.
            %if  any(N_Passes_HardLim)
              
            % Test if candidate is on emission peak in PSF stamp and
            % gradient consistent with circular direction.
            GDIRCVAR = CandCat.getCol('GDIRCVAR');
            GDIRERROR = CandCat.getCol('GDIRERROR');
            PassesGDir = (GDIRCVAR > Args.OmniDirectionThreshold(1)) & ...
                         (GDIRERROR < Args.OmniDirectionThreshold(2));

            PeakDist = CandCat.getCol('PEAK_DIST');
            PassesPeak = PeakDist < Args.PeakDistThreshold;

            N_Passes_Local_Circ = (PassesPeak & PassesGDir & ...
                                 (R_GoodPSF | IsolatedCand));

            N_Passes_PSFShape = N_Passes_PSFShape | N_Passes_Local_Circ;
            %end

            Passes_PSFShape = N_Passes_PSFShape & R_Passes_PSFShape;

            PSF_Flagged = ~Passes_PSFShape;
            FilterFlags = setFilterBit(FilterFlags, PSF_Flagged, BD_TF, 'NPSFShape');
            
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
        if Args.flagChi2

            % Get global Chi2
            N_CHI2DOF_Global = CandCat.getCol('N_PSF_CHI2DOF_MED');
            R_CHI2DOF_Global = CandCat.getCol('R_PSF_CHI2DOF_MED');

            % Get local Chi2
            N_CHI2DOF_Local = CandCat.getCol('N_PSF_CHI2DOF');
            R_CHI2DOF_Local = CandCat.getCol('R_PSF_CHI2DOF');
            D_CHI2DOF_Local = CandCat.getCol('PSF_CHI2DOF');

            % Test global Chi2
            N_Passes_CHI2DOF_Global = ...
                (N_CHI2DOF_Global > Args.Chi2dofLimitsGlobal(1)) & ...
                (N_CHI2DOF_Global < Args.Chi2dofLimitsGlobal(2));
            R_Passes_CHI2DOF_Global = ... 
                ((R_CHI2DOF_Global > Args.Chi2dofLimitsGlobal(1)) & ...
                (R_CHI2DOF_Global < Args.Chi2dofLimitsGlobal(2))) |...
                isnan(R_CHI2DOF_Global);

            Passes_CHI2DOF_D = ...
                (D_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (D_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));
            
            Passes_CHI2DOF_Global = N_Passes_CHI2DOF_Global ...
                & R_Passes_CHI2DOF_Global & Passes_CHI2DOF_D;

            % Test local Chi2
            N_Passes_CHI2DOF_Local = ...
                (N_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (N_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));

            R_Passes_CHI2DOF_Local = ...
                (R_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (R_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));
            
            % For isolated candidates, apply local test.
            Passes_CHI2DOF_Isolated = N_Passes_CHI2DOF_Local & IsolatedCand;

            % For blended candidates, apply global test.
            Passes_CHI2DOF_Blended = Passes_CHI2DOF_Global ...
                & BlendedCand;

            % Test ambigiously blended candidate. If a non-ambigious
            % blended candidate passes the blended condition, it is fine.
            % If an ambigious blended candidate passes the blended
            % condition, it has an extra condition to pass.
            Passes_CHI2DOF_Blended = Passes_CHI2DOF_Blended & ...
                (~AmbBlendedCand ...
                | (AmbBlendedCand ...
                & (N_Passes_CHI2DOF_Local | ~R_Passes_CHI2DOF_Local)));

            Passes_CHI2DOF = Passes_CHI2DOF_Isolated | ...
                Passes_CHI2DOF_Blended;

            CHI2DOF_Flagged = ~Passes_CHI2DOF;
            FilterFlags = setFilterBit(FilterFlags, CHI2DOF_Flagged, BD_TF, 'PSFChi2');

        end

        % ----- Physical contaminants -----

        % Flag stars as non-transients
        if Args.flagStarMatches
            FilterFlags = setFilterBit(FilterFlags, IsStar, BD_TF, 'StarMatch');
        end

        % Flag minor planets as non-transients
        if Args.flagMP
            MinorPlanet = (CandCat.getCol('N_DistMP') < Args.MPDistThresh) | ...
                          (CandCat.getCol('R_DistMP') < Args.MPDistThresh);

            FilterFlags = setFilterBit(FilterFlags, MinorPlanet, BD_TF, 'MPMatch');
            
        end
        
        if Args.flagVariable
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
                    [QSOLon, QSOLat], RA, Dec, -PointLimit*Arcsec2Rad);
    
                % Flag candidates as variable if matched to a QSO.
                VariableGal = vertcat(MatchResQSO.Nmatch) > 0;
            end

            % VarStars for stars
            % Note that we're using GAIA which is not only stars but
            % variable galaxies also. I'll keep refereing to them as stars
            % but matching variable galaxies this way is also a good thing.

            % Get star distances and find stars matched on candidate
            % position.
            StarDist = CandCat.getCol('STAR_DIST');
            NearStar = StarDist <= PointLimit;

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

            %NuclearCat = CandCat.selectRows(NuclearCand);
            % Get R magnitude and score of nuclear candidates
            NuclearRMag = R_MAG_PSF(NuclearCand);
            NuclearScore = Score(NuclearCand);

            % Initialize result array
            NumNuclear = sum(NuclearCand);
            NuclearNoise = false(NumNuclear,1);

            % Only test nuclear candidates if it's detectable in R image
            RDetNuclear = (NuclearRMag < R_LIMMAG);
            BrightNuclear = (NuclearRMag < Args.BrightGalMagThresh);
            TopPercentile = Args.NuclearDefaultPrcThresh .* ones(NumNuclear,1);
            TopPercentile(BrightNuclear) = Args.BrightGalPrcThresh;

            R_MAG_PSF_4Nuc = R_MAG_PSF;
            Score_4Nuc = Score;

            if ~isempty(Injections)
                R_MAG_PSF_4Nuc = R_MAG_PSF_4Nuc(~Injections);
                Score_4Nuc = Score_4Nuc(~Injections);
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

                % Some comparison sources are at the edge of the nan-border
                % and have a low R_MAG_PSF but high SCORE value, which
                % leads to filtering of real nuclear transients.
                % TODO: this maybe should be done cleaner
                if exist('BadPixHard','var')
                    BadPixHard_4Nuc = BadPixHard;

                    if ~isempty(Injections)
                        BadPixHard_4Nuc = BadPixHard_4Nuc(~Injections);
                    end

                    BinnedMags = BinnedMags & ~BadPixHard_4Nuc;

                end
                
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

            FilterFlags(NuclearCand) = setFilterBit(...
                FilterFlags(NuclearCand), NuclearNoise, BD_TF, 'NuclearNoise');

        end

        % ----- AstroZOGY -----

        if Args.flagScorr
            % Get Scorr and difference between Score and Scorr
            Scorr = CandCat.getCol('S_CORR');
            SDiff = abs(Score) - abs(Scorr);

            % Exclude isolated candidates unless PSF shape is poor.
            % Exclude also galaxy matched candidates that are not nuclear
            % and do not match to stars.
            ExcludeCand = (GalCand & ~NuclearCand & ~StarCand);

            if ~isempty(IsStar)
                ExcludeCand = (GalCand & ~NuclearCand & ~IsStar);
            end

            if exist('IsolatedCand', 'var')
                ExcludeCand = ExcludeCand | IsolatedCand;
            end

            if exist('N_Passes_PSF_Global','var')
                ExcludeCand = ExcludeCand & N_Passes_PSF_Global;
            end

            % Test if Score is higher than Scorr (has to be), Scorr is
            % above threshold and the difference between Score and Scorr is
            % below threshold.
            ScorrGood = (abs(Score) >= abs(Scorr)) ...
                & ((abs(Scorr) > Args.ScorrThreshold) | ...
                (SDiff < Args.ScorrCorrectionParam)) | ExcludeCand;

            ScorrFlagged = ~ScorrGood;
            FilterFlags = setFilterBit(FilterFlags, ScorrFlagged, BD_TF, 'Scorr');

        end

        if Args.flagTranslients
            % Get S2 and Z2 AICs and their difference.
            S2_AIC = CandCat.getCol('S2_AIC');
            Z2_AIC = CandCat.getCol('Z2_AIC');
            AIC_Diff = S2_AIC - Z2_AIC;
            AIC_Diff_Thresh = ...
                Args.TranslientExpThresh(1)...
                .*exp(Args.TranslientExpThresh(1)*Score)...
                +Args.TranslientExpThresh(3);

            AIC_Diff_Thresh(~N_GoodPSF) = Args.TranslientThresh;

            % Exclude isolated candidates unless PSF shape is poor.
            % Exclude also galaxy matched candidates that are not nuclear
            % and do not match to stars.
            ExcludeCand = (GalCand & ~NuclearCand & ~StarCand);

            if ~isempty(IsStar)
                ExcludeCand = (GalCand & ~NuclearCand & ~IsStar);
            end

            if exist('IsolatedCand', 'var')
                ExcludeCand = ExcludeCand | IsolatedCand;
            end

            %if exist('N_Passes_PSF_Global','var')
            %    ExcludeCand = ExcludeCand & N_Passes_PSF_Global;
            %end

            IsNotTranslient = (AIC_Diff < AIC_Diff_Thresh) ...
                | ExcludeCand;

            TranslientFlagged = ~IsNotTranslient;
            FilterFlags = setFilterBit(FilterFlags, TranslientFlagged, BD_TF, 'Translient');

        end

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(FilterFlags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});
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
