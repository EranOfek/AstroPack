function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transients candidates that are likely not real transients.
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'PixelScale' - Pizel scale in arcsec per pixel. Default is
                        1.25.
                'SaturatedNeighborDistanceThreshold' - Maximum distance in 
                        which to look for neighbor candidates on saturated
                        pixels. Default is 100.
                'flagNegatives' - Bool on whether to flag negative
                       candidates. Default is true.
                'flagChi2' - Bool on whether to flag transients candidates
                       based on how well the PSF fits to a stamp on the transient.
                       The goodness value is a Chi2 per degrees of freedom.
                       Default is true.
                'Chi2dofLimitsLocal' - Local limits on Chi2 per degrees of 
                       freedom. If 'flagChi2' is true, candidates outside 
                       these limits are flagged. The tested Chi2 is of the 
                       PSF fit on the candidate position in the N image. 
                       The first two values are the lower and upper bound 
                       applied to isolated candidates, the value is an 
                       upper bound applied to blended canditaes.  
                       Default is [0.1 2.0 100.0].
                'Chi2dofLimitsGlobal' - Global limits on Chi2 per degrees
                       of freedom. If 'flagChi2' is true, candidates outside
                       of these limits are flagged. The tested Chi2 is the
                       median Chi2 of all N- and R-image catalog sources 
                       binned in magnitude. The N and R sources are binned 
                       seprately. The N/R-image magnitude of a candidate 
                       is used to find the corresponding median Chi2, 
                       which is compared against the limits. These limits 
                       are applied to blended candidates only. The first 
                       value is the lower bound, and the second value is 
                       the upper bound. Default is [0.0 1.2].
                'flagSaturated' - Bool on whether to flag transients 
                       candidates that are saturated in both reference and 
                       new images. Default is true.
                'flagBadPix_Hard' - Bool on whether to flag transients
                       candidates based on hard bit mask criteria. 
                       Default is true.
                'BadPix_Hard' - Hard bit mask criteria for bad pixels.  
                       Default is {'Interpolated', 'NaN', 'NearEdge', 
                       'Hole', 'Negative'}.
                'flagBadPix_Soft' - Bool on whether to flag transients
                       candidates based on soft bit mask criteria. 
                       Default is true.
                'BadPix_Soft' - Soft bit mask criteria for bad pixels and 
                       their threshold increment. Transients candidates are
                       tested on whether they are more PSF- or Delta-like.
                       Bad pixels identified in the bitmask must pass a
                       higher threshold, incremented for each identified
                       bad pixel type. The increment is additive.
                       Default is {{'DarkHighVal', 1.2}, {'CR_DeltaHT', 2.9}}.
                'flagStarMatches' - Bool on whether to flag transients
                       candidates that have matching star positions.
                       Default is true.
                'flagMP' - Bool on whether to flag transients candidates
                       that have matching minor planet postions. Default is
                       true.
                'flagRinging' - Bool on whether to flag transients
                       candidates that may be caused by ringing artifacts.
                       Default is true.
                'flagNPsfShape' - Bool on whether to flag transients for
                       which the N-image PSF is misshapen. Default is
                       true.
                'SecondMomSoftLim' - Threshold on second moments of the
                       New image PSF. If the x^2 or y^2 moments are higher
                       than the threshold, the PSF is deemed to be too wide
                       or too elongated. If a candidate fails this criterium, 
                       it is subjected to further tests, otherwise is passes. 
                       Default is 1.2.
                'SecondMomAsymLim' - Threshold on asymetry of the second
                       moments of the New image PSF. If abs(x^2-y^2) is
                       higher than the threshold, the PSF is deemed to be
                       too elongated. If a candidate fails this criterium, 
                       it is subjected to further tests, otherwise it passes.
                       Default is 1.00.
                'SecondMomHardLim' - Threshold on second moments of the New
                       Image PSF. This threshold is applied if New image
                       PSF fails SeconMomSoftLim or SecondMomAsymLim. If
                       x^2 OR y^2 are higher than this limit, all
                       candidates are flagged. Default is 2.7.
                'OmniDirectionThreshold' - Thresholds for local directional
                       gradient. These are applied localy in New image if
                       the image fails the SecondMomSoftLim or SecondMomAsymLim
                       thresholds. The first value is the minimum circular 
                       variance of the direction gradient, and the second 
                       value is the maximum deviation from an assumed 
                       circular gradiant in degrees. If a candidate fails 
                       these additional criteria, it is flagged as a false 
                       positive. Default is [0.7 57.0].
                'PeakDistThreshold' - Threshold for the distance between 
                       D-image and S-image peaks. This threshold is applied 
                       if New image PSF fails SeconMomSoftLim or SecondMomAsymLim.
                       Default is 3.00.
                'ContaminationFlux' - Contamination flux in units of the
                       background. If the N image PSF fails the 
                       SecondMomSoftLim or SecondMomAsymLim thresholds, 
                       all N-image sources are retrieved which produce flux 
                       above the ContaminationFlux beyond the PSF stamp.
                       Default is 0.01;
                'ContaminationRadius' - Contamination radius in units of
                       the PSF half size in pixels. If the N image PSF fails 
                       the SecondMomSoftLim or SecondMomAsymLim thresholds, 
                       all candidates are tested wether they are within the
                       ContaminationRadius of a contaminating N-image
                       source. All candidates a that are, are flagged.
                       Default is 1.5.
                'flagDPSFShape' - Bool on whether to falg candidates that 
                       are not PSF-like within the D image. This filter
                       compares the second moments of the candidate's 
                       D-image PSF against a 2D Gaussian fit derived from
                       historical PSF shapes. Default is true.
                'PSFShapeXYMeanD' - The mean values of the 2D Gaussian fit
                       used when flagDPSFShape is true. 
                       Default is [1.06919192, 1.24191919].
                'PSFShapeCovD' - The covariance matrics of the 2D Gaussian 
                       fit used when flagDPSFShape is true. 
                       Default is [0.06467546, 0.02720397; 0.02720397, 0.06933742].
                'PSFShapeConfThreshD' - The confidence level beyond which
                       candidates are flagged after the 2D Gaussian fit 
                       comparison when flagDPSFShape is true. 
                       Default is 0.95.
                'flagLimitingMag' - Bool on whether to flag candidates that
                       are above the limiting magnitude. Candidate is
                       filteres if it is above limiting magnitude in New
                       and Ref. Default is true.
                'flagPeakValley' - Bool on whether to flag candidates that
                       are peaks (valleys) and are too close to valleys 
                       (peaks). A peak is a candidate with a positive
                       signal and a valley is a candidate with a negative
                       signal. Default is true.
                'PVDistThresh' - Distance threshold in pixels between 
                       peaks and valleys below which to flag candidates. 
                       Default is 10.
                'flagStreak' - Bool on whether to flag candidates induced
                       by streaks (e.g. satellites). Default is true.
                'ignoreSreakPoints' - Filters for which to ignore candidates
                       that fail them when fitting a streak line. 
                       Default {'BadPixelHard', 'StarMatch', 'Ringing', 
                       'Translient', 'Streak'}.
                'StreakDistanceThreshold' - Maximum distance from a fitted
                       streak for which to flag candidates. Default is 20.
                'NumStreaks' - Number of streaks to fit for. Default is 1.
                'flagDiffSpike' - Bool on whether to flag candidates
                       induced by diffraction spikes. Default is true.
                'SatCentroidDistThreshold' - Maximum distance from
                       candidate to centroid of saturated pixels. Saturation
                       cenroid within this distance will be considered for 
                       further testing. Default is 200.
                'DiffSpikeSNRThreshold' - Minimum SNR of pixels along 
                       the line between candidate and saturation centroid. 
                       Pixels above the threshold will be counted for final
                       decision. Default is 2.0.
                'DiffSpikeFracThreshold' - Minimum fraction of pixels along 
                       the line between candidate and saturation centroid 
                       to fulfill the SNR threshold. If the fraction of 
                       pixels alon the line fulfills this threshold, 
                       the candidate is counted as caused by a 
                       diffraction spike. Default is 0.5.
                'flagDensity' - Bool on whether to flag candidates that are
                       too close to each other, i.e., that have too many
                       neighbors. Default is true.
                'NeighborDistanceThreshold' - Distance threshold below
                       which a candidates count as neighbors.
                       Default is 100.
                'NeighborExclude' - Filters for which to ignore candidates 
                       as neighbors if they fail them. Default is 
                       {'BadPixelHard', 'StarMatch', 'Ringing', 
                       'Translient', 'Streak'}.
                'NeighborDenThreshold' - Density threshold above which 
                       candidates are flagged for density. The density 
                       is calculated as the number of neighbors times 
                       the sum of reciprocal distances to all neighbors.
                       Default is 1.0.
                'NeighborNumThresholdSaturated' - Threshold for the number 
                       of neighbors at which to filter candidates if they
                       have saturated neighbors. Default is 2.
                'flagVariable' - Bool on whether to flag candidate that 
                       coincide with known variable sources. Default is true.
                'flagNuclearNoise' - Flag for nuclear noise. Nuclear 
                       candidates are flagged if their S score is not within
                       the top 50 percentile of all R-image coincident
                       candidates a that are brighter by 0.5 mag than the 
                       nuclear candidate. Default is true.
                'BrightGalMagThresh' - Threshold on magnitude for nuclear
                       candidates above which to increase the threshold. 
                       Default is 17.0.
                'BrightGalPrcThresh' - Increased top percentile threshold 
                       for nuclear candidates above magnitude threshold. 
                       Default is 80.
                --- AstroZOGY ---
                'flagScorr' - Bool on whether to flag candidates based on 
                       source noise corrected S statistic. Default is true.
                'ScorrThreshold' - Threshold value for Scorr. Default is 5.0.
                'ScorrCorrectionParam' - A parameter added to Scorr. This 
                       helps faint candidates for which the source noise is
                       overestimated. Default is 0.7.
                'flagTranslients' - Bool on whether to flag transients 
                       candidates which score higher in Z2 than S2.
                       Default is true.
    Output  : - An AstroCatalog which is equal to the input catalog of AD 
                but with additional columns.
    Author  : Ruslan Konno (Jan 2024)
    Example : AD = AstroZOGY('LAST*.fits','LAST*1*.fits');
              AD.subtractionD;
              AD.subtractionS;
              AD.findTransients;
              imProc.sub.flagNonTransients(AD);
    %}

    arguments
        Obj AstroDiff

        Args.ConfigFile = '';

        Args.PixelScale = 1.25;
        Args.SaturatedNeighborDistanceThreshold = 250;
    
        Args.flagNegatives logical = true;

        Args.flagChi2 logical = true;
        Args.Chi2dofLimitsLocal = [0.1 2.2];
        Args.Chi2dofLimitsGlobal = [0.1 2.5];
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'NearEdge',...
            'Hole', 'Negative'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'DarkHighVal', 1.2}, ...
            {'CR_DeltaHT',2.9}};

        Args.flagSubVisit = true;
        Args.BadPixSatRad = 10;
        Args.BadPixSatFlux = 20000;

        Args.flagStarMatches logical = true;

        Args.flagMP logical = true;
        Args.MPDistThresh = 10;

        Args.flagRinging logical = true;

        Args.flagNPsfShape logical = true;
        Args.SecondMomSoftLim = 1.2;
        Args.SecondMomHardLim = 2.7;
        Args.SecondMomAsymLim = 1.0;
        Args.OmniDirectionThreshold = [0.7 57.0];
        Args.PeakDistThreshold = 3.0;
        Args.ContaminationBackRatio = 0.1;
        Args.ContaminationMag = 0.48;
        Args.ContaminationRadius = 1.5;
        Args.OverWritePSFLimit = true;
        Args.OverwritePSFLimitVal = 5;

        Args.flagDPSFShape logical = false;
        Args.PSFShapeXYMeanD = [1.06919192, 1.24191919]
        Args.PSFShapeCovD = [0.06467546, 0.02720397;...
            0.02720397, 0.06933742];
        Args.PSFShapeConfThreshD = 0.95;

        Args.flagExtended logical = true;
        Args.ExtendedThreshold = -0.59;
        Args.ExtendedSatDelta = 0.5;
        
        Args.flagLimitingMag logical = true;

        Args.flagPeakValley logical = true;
        Args.PVDistThresh = 10;
       
        Args.flagStreak logical = true;
        Args.ignoreStreakPoints = {'BadPixelHard',  ...
            'StarMatch', 'Ringing', 'Translient', 'Streak'};
        Args.StreakDistanceThreshold = 20;
        Args.NumStreaks = 1;

        Args.flagDiffSpike logical = true;
        Args.SatCentroidDistThreshold = 250;
        Args.DiffSpikeSNRThreshold = 2.0;
        Args.DiffSpikeFracThreshold = 0.5;
        
        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.NeighborDenThreshold = 1.0;
        Args.NeighborExclude = {'BadPixelHard', 'BadPixelSoft', ...
            'StarMatch', 'Ringing', 'Translient', 'Streak'};
    
        Args.flagVariable logical = true;

        Args.flagNuclearNoise logical = true;
        Args.BrightGalMagThresh = 17.0;
        Args.BrightGalPrcThresh = 80;

        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;
        Args.ScorrCorrectionParam = 0.7;

        Args.flagTranslients logical = true;
        Args.TranslientThresh = 0.48;

        % --- Injections ---
        Args.injectedSrcs = [];

    end

    % Don't question this madness.

    if ~isempty(Args.ConfigFile) && exist(Args.ConfigFile,'file')
        fid = fopen(Args.ConfigFile);
        raw = fread(fid, inf);
        str = char(raw');
        fclose(fid);
    
        config = jsondecode(str);
    
        configFields = fieldnames(config);
        for i = 1:numel(configFields)
            key = configFields{i};
            if isfield(Args, key)
                val = config.(key);
    
                % Convert 2-element column vector to row vector
                if isnumeric(val) && isvector(val) && numel(val) == 2
                    val = reshape(val, 1, []);  % Ensure 1x2 row vector
                end
    
                Args.(key) = val;
            else
                warning("Unknown config field: %s", key);
            end
        end
    end

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

        PointLimit = Obj(Iobj).PSFData.fwhm*Args.PixelScale*1.2739; % 3sig in arcsec

        % Get size of catalog and initialize an array holding the filtering
        % summary. Array is initialized as zero and will be updates with 
        % each failed filter.

        NumCand = size(CandCat.Catalog,1);

        % Skip empty catalogs
        if NumCand < 1
            TranCat = CandCat;
            continue
        end

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

        if exist('N_X2', 'var') && exist('N_Y2', 'var')
            N_GoodPSF = ...
                      (N_X2 < Args.SecondMomSoftLim) & ...
                      (N_Y2 < Args.SecondMomSoftLim) & ...
                      (abs(N_X2-N_Y2) < Args.SecondMomAsymLim);
        end

        if CandCat.isColumn('R_X2')
            R_X2 = CandCat.getCol('R_X2');
        end
        if CandCat.isColumn('R_Y2')
            R_Y2 = CandCat.getCol('R_Y2');
        end
        
        if exist('R_X2', 'var') && exist('R_Y2', 'var')
            R_GoodPSF = ...
                      (R_X2 < Args.SecondMomHardLim) & ...
                      (R_Y2 < Args.SecondMomHardLim) & ...
                      (abs(R_X2-R_Y2) < Args.SecondMomAsymLim);
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
            % 4sig for nuclear check, dirty, I know
            % TODO: rather than doing this here, match2Galaxies should be
            % extended to determined if a source is nuclear or not,
            % probably best to write a dedicated matchTransients2Galaxies
            % function which uses the N, R, and D catalogs
            NuclearCand = GalDist < PointLimit*4/3; 
        else
            NuclearCand = false(NumCand,1);
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
            FilterFlags = FilterFlags + NegCand.*2.^BD_TF.name2bit('Negative');
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

            FilterFlags = FilterFlags + BadPixHard.*2.^BD_TF.name2bit('BadPixelHard');
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

            FilterFlags = FilterFlags + FalseSaturation.*2.^BD_TF.name2bit('SubVisit');
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
            FilterFlags = FilterFlags + BadPixSoft.*2.^BD_TF.name2bit('BadPixelSoft');
        end        

        % Flag saturated candidates
        if Args.flagSaturated
            N_Saturated = BD_IM.findBit(N_BM,'Saturated');
            R_Saturated = BD_IM.findBit(R_BM,'Saturated');
            
            % Check if candidates are saturated in New and Ref, flag these.
            Saturated = N_Saturated & R_Saturated;

            FilterFlags = FilterFlags + Saturated.*2.^BD_TF.name2bit('Saturated');
        end

        % ----- D artifacts -----

        % Apply ringing criterium
        if Args.flagRinging && CandCat.isColumn('SN_GABOR')
            GaborSN = CandCat.getCol('SN_GABOR');

            Ringing =  (abs(GaborSN) > abs(Score));
            FilterFlags = FilterFlags + Ringing.*2.^BD_TF.name2bit('Ringing');
        end

        % Apply Peak-Valley criterium
        if Args.flagPeakValley && CandCat.isColumn('PV_DIST')
            PVDist = CandCat.getCol('PV_DIST');

            PVFlagged = (PVDist <= Args.PVDistThresh);
            FilterFlags = FilterFlags + PVFlagged.*2.^BD_TF.name2bit('PVDist');
        end
        
        if Args.flagStreak

            SubSel = true(NumCand,1);
            NumExclude = numel(Args.ignoreStreakPoints);

            for IExclude = 1:NumExclude
                BitFound = BD_TF.findBit(FilterFlags, ...
                    Args.ignoreStreakPoints{IExclude});
                SubSel = SubSel & ~BitFound;
            end

            for IStreak=1:Args.NumStreaks

                Xt = X(SubSel);
                Yt = Y(SubSel);
                TDist = max(Obj(Iobj).PSFData.fwhm*2,5);
    
                MinNumPts = [7 10 13 17 20 23 27 30];
                NumMinNumPts = numel(MinNumPts);
                for IMinNumPts = NumMinNumPts:-1:1
                    Res = tools.math.fit.ransacLinear([Xt,Yt], 'Ntrial', 1000, ...
                        'MinRMS', 1.0,'MinNpt',MinNumPts(IMinNumPts), ...
                        'ThresholdDist',TDist);
                    if Res.Found
                        break
                    end
                end
    
                if Res.Found
                    ModY = Res.Par(1)+Xt.*Res.Par(2);
                    Streak = abs(ModY - Yt) < Args.StreakDistanceThreshold;
                    FilterFlags(SubSel) = FilterFlags(SubSel) + Streak.*2.^BD_TF.name2bit('Streak');
                    SubSel(SubSel) = ~Streak;
                else
                    break
                end
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

            FilterFlags = FilterFlags + ExtendedSource.*2.^BD_TF.name2bit('Extended');

        end

        if Args.flagDPSFShape
            X2 = CandCat.getCol('X2');
            Y2 = CandCat.getCol('Y2');
            CHI2DOF = CandCat.getCol('PSF_CHI2DOF');

            X2Y2 = [X2(:),Y2(:)];

            ProbD = mvnpdf(X2Y2, Args.PSFShapeXYMeanD, Args.PSFShapeCovD);

            PassesD = ProbD > (1-Args.PSFShapeConfThreshD);
            PassesD = PassesD | (~PassesD & ...
                (X2 < 1.85) & (Y2 < 1.85) & (CHI2DOF < 1.0));
            PSFShapeFlagged = ~PassesD;
            FilterFlags = FilterFlags + PSFShapeFlagged.*2.^BD_TF.name2bit('DPSFShape');
        end        

        if Args.flagNPsfShape

            % Test global shape. For isolated candidates only N shape.
            N_Passes_PSF_Global = N_GoodPSF;
            R_Passes_PSF_Global = (R_GoodPSF | IsolatedCand);
    
            N_Passes_PSFShape = N_Passes_PSF_Global;
            R_Passes_PSFShape = R_Passes_PSF_Global;

            % Use hard limits on global shape no matter local results.
            N_Passes_HardLim =  (N_X2 < Args.SecondMomHardLim) & ...
                                (N_Y2 < Args.SecondMomHardLim);

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
                PSFSize_Min = min(N_PSFSize,R_PSFSize);
                PSFSize_Max = max(N_PSFSize,R_PSFSize);

                if Args.OverWritePSFLimit
                    PSFSize_Min = Args.OverwritePSFLimitVal;
                end

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
                BlendedContaminators = (R_NativeContCHI2 > 10.0);

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

                SelfSrcRad = 1.5*Args.PixelScale*Arcsec2Rad;
               
                for ICand = 1:NumCand
                    if N_Passes_Local(ICand)
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
                
                    N_Passes_Local(ICand) = (MagContamination > Args.ContaminationMag);
                end

                STD_ANNULUS = CandCat.getCol('STD_ANNULUS');
                BACK_ANNULUS = CandCat.getCol('BACK_ANNULUS');

                N_Passes_Local_Aper = (STD_ANNULUS < 7.0) & (abs(BACK_ANNULUS) < 3.0);

                N_Passes_Local = N_Passes_Local & N_Passes_Local_Aper;

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
            if  any(N_Passes_HardLim)
              
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
            end

            Passes_PSFShape = N_Passes_PSFShape & R_Passes_PSFShape;

            PSF_Flagged = ~Passes_PSFShape;
            FilterFlags = FilterFlags + PSF_Flagged.*2.^BD_TF.name2bit('NPSFShape');
        end

        if Args.flagDiffSpike

            NearSatNotStar = NearSaturated & ~StarCand;

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
            FilterFlags = FilterFlags + IsDiffSpike.*2.^BD_TF.name2bit('DiffSpike');
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
            Passes_CHI2DOF_Local = ...
                (N_CHI2DOF_Local > Args.Chi2dofLimitsLocal(1)) & ...
                (N_CHI2DOF_Local < Args.Chi2dofLimitsLocal(2));

            % For isolated candidates, apply local test.
            % For blended candidates, apply global test.
            Passes_CHI2DOF = (Passes_CHI2DOF_Local & IsolatedCand) | ...
                (Passes_CHI2DOF_Global & BlendedCand);

            CHI2DOF_Flagged = ~Passes_CHI2DOF;
            FilterFlags = FilterFlags + CHI2DOF_Flagged.*2.^BD_TF.name2bit('PSFChi2');

        end

        % ----- Physical contaminants -----

        % Flag stars as non-transients
        if Args.flagStarMatches

            % Relax flagging for galaxy-star confusion if candidate is
            % nuclear and the number of matched galaxies is equal or higher
            % than the number of matched stars.
            NStars = CandCat.getCol('STAR_N');
            NGal = CandCat.getCol('GAL_N');
            ExcludeGalaxy = NuclearCand & (NGal >= NStars);

            IsStar = StarCand & ~ExcludeGalaxy;

            FilterFlags = FilterFlags + IsStar.*2.^BD_TF.name2bit('StarMatch');
        end

        % Flag minor planets as non-transients
        if Args.flagMP
            MinorPlanet = (CandCat.getCol('N_DistMP') < Args.MPDistThresh) | ...
                          (CandCat.getCol('R_DistMP') < Args.MPDistThresh);

            FilterFlags = FilterFlags + MinorPlanet.*2.^BD_TF.name2bit('MPMatch');
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
            
            FilterFlags = FilterFlags + VariableSource.*2.^BD_TF.name2bit('Variable');

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
            FilterFlags = FilterFlags + Overdensity.*2.^BD_TF.name2bit('Overdensity');
            
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
            TopPercentile = 50*ones(NumNuclear,1);
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
                DynamicBinMin = TargetRMag - 0.5;
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

            FilterFlags(NuclearCand) = FilterFlags(NuclearCand) + ...
                NuclearNoise.*2.^BD_TF.name2bit('NuclearNoise');

        end

        % ----- AstroZOGY -----

        if Args.flagScorr
            % Get Scorr and difference between Score and Scorr
            Scorr = CandCat.getCol('S_CORR');
            SDiff = abs(Score) - abs(Scorr);

            % Test if Score is higher than Scorr (has to be), Scorr is
            % above threshold and the difference between Score and Scorr is
            % below threshold.
            ScorrGood = (abs(Score) >= abs(Scorr)) ...
                & ((abs(Scorr) > Args.ScorrThreshold) | ...
                (SDiff < Args.ScorrCorrectionParam));

            ScorrFlagged = ~ScorrGood;
            FilterFlags = FilterFlags + ScorrFlagged.*2.^BD_TF.name2bit('Scorr');

        end

        if Args.flagTranslients
            % Get S2 and Z2 AICs and their difference.
            S2_AIC = CandCat.getCol('S2_AIC');
            Z2_AIC = CandCat.getCol('Z2_AIC');
            AIC_Diff = S2_AIC - Z2_AIC;

            % Exclude isolated candidates unless PSF shape is poor.
            % Exclude also galaxy matched candidates that are not nuclear
            % and do not match to stars.
            ExcludeCand = (GalCand & ~NuclearCand & ~StarCand);

            if exist('IsolatedCand', 'var')
                ExcludeCand = ExcludeCand | IsolatedCand;
            end

            if exist('N_Passes_PSF_Global','var')
                ExcludeCand = ExcludeCand & N_Passes_PSF_Global;
            end

            IsNotTranslient = (AIC_Diff < Args.TranslientThresh) ...
                | ExcludeCand;

            TranslientFlagged = ~IsNotTranslient;
            FilterFlags = FilterFlags + TranslientFlagged.*2.^BD_TF.name2bit('Translient');

        end

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(FilterFlags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});
    end
  
end
