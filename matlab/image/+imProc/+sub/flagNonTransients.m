function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transients candidates that are likely not real transients.
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'flagNegatives' - Bool on whether to flag negative
                       candidates. Default is true.
                'flagChi2' - Bool on whether to flag transients candidates
                       based on how well the PSF fits to a stamp on the transient.
                       The goodness value is a Chi2 per degrees of freedom.
                       Default is true.
                'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
                       'flagChi2' is true, candidates outside these
                       limits are flagged. The first two values are the 
                       lower and upper bound applied to isolated candidates,
                       the value is an upper bound applied to blended 
                       canditaes. Default is [0.1 2.0 100.0].
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
                'flagSaturated' - Bool on whether to flag transients 
                       candidates that are saturated in both reference and 
                       new images. Default is true.
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
                       it has additionally fail SecondMomHardLim, 
                       SecondMomFinalLim, OmniDirectionThreshold, or 
                       PeakDistThreshold to be flagged as false positive.
                       Default is 1.2.
                'SecondMomAsymLim' - Threshold on asymetry of the second
                       moments of the New image PSF. If abs(x^2-y^2) is
                       higher than the threshold, the PSF is deemed to be
                       too elongated. If a candidate fails this criterium, 
                       it has additionally fail SecondMomHardLim, 
                       SecondMomFinalLim, OmniDirectionThreshold, or 
                       PeakDistThreshold to be flagged as false positive.
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
                       Default is 1.33.
                % TODO: docs
                'flagDPSFShape' - Default is true.
                'PSFShapeXYMeanD' - Default is [1.06919192, 1.24191919].
                'PSFShapeCovD' - Default is [0.06467546, 0.02720397;...
                        0.02720397, 0.06933742].
                'PSFShapeProbThresholdD' - Default is 0.05.

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
                'ignoreSreakPoints' - Default {'BadPixelHard', 'StarMatch', ...
                       'Ringing', 'Translient', 'Streak'}.
                'StreakDistanceThreshold' - Default is 20.
                'NumStreaks' - Default is 1.

                'flagDensity' - Bool on whether to flag transients that are
                       too close to each other, i.e., that have too many
                       neighbors. Default is true.
                'NeighborDistanceThreshold' - Distance threshold below
                       which a close transient counts as a neighbor.
                       Default is 100.
                'NeighborExclude' - Default is {'BadPixelHard', 'StarMatch', ...
                       'Ringing', 'Translient', 'Streak'}.
                'NeighborDenThreshold' - Default is 1.0.
                'SaturatedNeighborDistanceThreshold' - Default is 100.
                'NeighborNumThresholdSaturated' - Threshold for the number 
                       of neighbors at which to filter the transients
                       candidate. Default is 2.
                'flagVariable' - Default is true.
                'VarStarDist' - Default is 3.
                'flagNuclear' - Default is true.
                --- AstroZOGY ---
                'flagScorr' - Bool on whether to flag candidates based on 
                       source noise corrected S statistic. Default is true.
                'ScorrThreshold' - Threshold value for Scorr. Default is 5.0.
                'ScorrCorrectionParam' - Default is 0.7.
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

        %TODO: put all all of this in a config file

        Args.PixelScale = 1.25;
        Args.SaturatedNeighborDistanceThreshold = 100;
    
        Args.flagNegatives logical = true;

        Args.flagChi2 logical = true;
        Args.Chi2dofLimitsLocal = [0.1 2.0 100.0];
        Args.Chi2dofLimitsGlobal = 1.2;
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'NearEdge',...
            'Hole', 'Negative'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'DarkHighVal', 1.2}, ...
            {'CR_DeltaHT',2.9}};

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;

        Args.flagNPsfShape logical = true;
        Args.SecondMomSoftLim = 1.2;
        Args.SecondMomHardLim = 2.7;
        Args.SecondMomAsymLim = 1.0;
        Args.OmniDirectionThreshold = [0.7 57.0];
        Args.PeakDistThreshold = 3.0;

        Args.flagDPSFShape logical = true;
        Args.PSFShapeXYMeanD = [1.06919192, 1.24191919]
        Args.PSFShapeCovD = [0.06467546, 0.02720397;...
            0.02720397, 0.06933742];
        Args.PSFShapeProbThresholdD = 0.05;
        
        Args.flagLimitingMag logical = true;

        Args.flagPeakValley logical = true;
        Args.PVDistThresh = 10;
       
        Args.flagStreak logical = true;
        Args.ignoreStreakPoints = {'BadPixelHard',  ...
            'StarMatch', 'Ringing', 'Translient', 'Streak'};
        Args.StreakDistanceThreshold = 20;
        Args.NumStreaks = 1;
        
        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.NeighborDenThreshold = 1.0;
        Args.NeighborExclude = {'BadPixelHard', 'BadPixelSoft', ...
            'StarMatch', 'Ringing', 'Translient', 'Streak'};
        Args.NeighborNumThresholdSaturated = 2;
    
        Args.flagVariable logical = true;
        Args.VarStarDist = 3;

        Args.flagNuclear logical = true;

        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;
        Args.ScorrCorrectionParam = 0.7;

        Args.flagTranslients logical = true;
    end

    % Don't question this madness.

    Nobj = numel(Obj);

    % Get transients filter bit dictionary
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');
    % Get image mask bit dictionary
    BD_IM = BitDictionary('BitMask.Image.Default');

    Arcsec2Rad = 4.84814e-6;

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

        % Initialize transients bool
        FilterFlags = zeros(NumCand,1);

        % Get positive and negative candidates
        %PosTran = (Score > 0.0);
        NegCand = (Score < 0.0);

        % Get limiting magnitudes of N and R
        N_LIMMAG = Obj(Iobj).New.HeaderData.getVal('LIMMAG');
        R_LIMMAG = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');

        % N and R PSF magnitudes
        N_MAG_PSF = CandCat.getCol('N_MAG_PSF');
        R_MAG_PSF = CandCat.getCol('R_MAG_PSF');
        
        % Get isolated and blended candidates
        R_SN = CandCat.getCol('R_SN');
        IsolatedCand = ((abs(R_SN) < 3) | (R_MAG_PSF > R_LIMMAG));
        BlendedCand = ~IsolatedCand;

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
                'or', 'HalfSize',Args.SaturatedNeighborDistanceThreshold);
        NearSaturated = BD_IM.findBit(BitsSatCut,'Saturated');

        % Check N and R PSFs
        N_X2 = CandCat.getCol('N_X2');
        N_Y2 = CandCat.getCol('N_Y2');

        N_GoodPSF = ...
                  (N_X2 < Args.SecondMomSoftLim) & ...
                  (N_Y2 < Args.SecondMomSoftLim) & ...
                  (abs(N_X2-N_Y2) < Args.SecondMomAsymLim);

        R_X2 = CandCat.getCol('R_X2');
        R_Y2 = CandCat.getCol('R_Y2');

        R_GoodPSF = ...
                  (R_X2 < Args.SecondMomHardLim) & ...
                  (R_Y2 < Args.SecondMomHardLim) & ...
                  (abs(R_X2-R_Y2) < Args.SecondMomAsymLim);

        % Get star matched candidates
        StarCand = (CandCat.getCol('STAR_N') > 0.0);

        % Get Galaxy matched candidates
        %GalCand = (CandCat.getCol('GAL_N') > 0.0);

        % Get Nuclear candidates
        GalDist = CandCat.getCol('GAL_DIST');
        NuclearCand = GalDist < 3.0;


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

            PVFlagged = (PVDist < Args.PVDistThresh);
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
    
                MinNumPts = [5 7 10 13 17 20 23 27 30];
                NumMinNumPts = numel(MinNumPts);
                for IMinNumPts = NumMinNumPts:-1:1
                    Res = tools.math.fit.ransacLinear([Xt,Yt], 'Ntrial', 1000, ...
                        'MinRMS', 0.5,'MinNpt',MinNumPts(IMinNumPts), ...
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

        if Args.flagDPSFShape
            X2 = CandCat.getCol('X2');
            Y2 = CandCat.getCol('Y2');
            CHI2DOF = CandCat.getCol('PSF_CHI2DOF');

            X2Y2 = [X2(:),Y2(:)];

            ProbD = mvnpdf(X2Y2, Args.PSFShapeXYMeanD, Args.PSFShapeCovD);

            PassesD = ProbD > Args.PSFShapeProbThresholdD;
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

            % If the global PSF is wide, check for local contaminating
            % sources
            if any(~N_GoodPSF) && any(N_Passes_HardLim)
                N_Aper3Flux = Obj(Iobj).New.CatData.getCol('FLUX_APER_3');
                N_NativeX2 = Obj(Iobj).New.CatData.getCol('X2');
                N_NativeY2 = Obj(Iobj).New.CatData.getCol('Y2');
                N_NativeXY2_Max = max(N_NativeX2,N_NativeY2);
        
                % Define the contamination radius as the distance at which
                % the source is at least as bright as 1% of the background.
                DistThresh = sqrt(N_NativeXY2_Max).*sqrt(...
                    -2.*log(0.01.*Obj(Iobj).BackN./(N_Aper3Flux)));

                % Get sources that contaminate beyond the PSF stamp
                % User the smaller PSF between N and R
                N_PSFSize = floor(size(Obj(Iobj).New.PSFData.getPSF,2)/2);
                R_PSFSize = floor(size(Obj(Iobj).Ref.PSFData.getPSF,2)/2);
                PSFSize_Min = min(N_PSFSize,R_PSFSize);

                N_ContSrcs = (DistThresh > PSFSize_Min-1);
          
                % Match candidates to New image sources within wide range 
                % equal to 1.5 times the PSF size. The candidate should 
                % match at least itself. If there is no match, then likely 
                % the candidate is contaminated by a source beyond this
                % range.
                [N_NativeRA, N_NativeDec] = Obj(Iobj).New.CatData.getLonLat('rad');
                WideRadius = PSFSize_Min*1.5*Args.PixelScale;
                N_CatMatchWide = VO.search.search_sortedlat_multi( ...
                    [N_NativeRA, N_NativeDec], RA, Dec, ...
                    WideRadius*Arcsec2Rad);
                NumMatchesWideAll = vertcat(N_CatMatchWide.Nmatch);

                % Select coordinates of contaminating sources.
                N_NativeContRa = N_NativeRA(N_ContSrcs);
                N_NativeContDec = N_NativeDec(N_ContSrcs);

                % Match candidates to contaminating sources in wide range.
                N_ContCatMatchWide = VO.search.search_sortedlat_multi( ...
                    [N_NativeContRa, N_NativeContDec], RA, Dec, ...
                    -WideRadius*Arcsec2Rad);
                NumMatchesWideCont = vertcat(N_ContCatMatchWide.Nmatch);

                % Match candidates to contaminating sources on the
                % candidate position. 
                NumMatchesSame = arrayfun(...
                    @(x) sum(x.Dist < 3.0*Arcsec2Rad), N_ContCatMatchWide);

                % If the number of contaminating sources in wide range 
                % and on candidate position is the same (1), then the 
                % candidate is not contaminated. If the wide range number 
                % is higher, then the candidate may be contaminated.

                N_Passes_Local = (NumMatchesWideAll > 0) & ...
                    (NumMatchesWideCont - NumMatchesSame < 1);

                % Update candidates as passing if they are not near any
                % contaminating sources.
                N_Passes_PSFShape = N_Passes_PSFShape | N_Passes_Local;
                N_Passes_PSF_Global = N_Passes_PSF_Global | N_Passes_Local;
            end

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

                % Use only local test for candidates near saturated pixels.
                N_Passes_PSFShape(NearSaturated) = N_Passes_Local_Circ(NearSaturated);
            end

            Passes_PSFShape = N_Passes_PSFShape & R_Passes_PSFShape;

            PSF_Flagged = ~Passes_PSFShape;
            FilterFlags = FilterFlags + PSF_Flagged.*2.^BD_TF.name2bit('NPSFShape');
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

            % Test global Chi2
            N_Passes_CHI2DOF_Global = (N_CHI2DOF_Global < Args.Chi2dofLimitsGlobal) & ...
                (N_CHI2DOF_Local < Args.Chi2dofLimitsLocal(3));
            R_Passes_CHI2DOF_Global = (R_CHI2DOF_Global < Args.Chi2dofLimitsGlobal)...
                | isnan(R_CHI2DOF_Global);
            Passes_CHI2DOF_Global = N_Passes_CHI2DOF_Global ...
                & R_Passes_CHI2DOF_Global;

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
            MinorPlanet = ~isnan(CandCat.getCol('N_DistMP')) | ...
                          ~isnan(CandCat.getCol('R_DistMP'));

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
                    [QSOLon, QSOLat], RA, Dec, -3*Arcsec2Rad);
    
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
            NearStar = StarDist <= Args.VarStarDist;

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

            % Special treatment if the candidate is near a saturated
            % source.
            Overdensity = Overdensity | ...
                (NearSaturated & (NumNeighbors >= Args.NeighborNumThresholdSaturated));

            % Update flags
            FilterFlags = FilterFlags + Overdensity.*2.^BD_TF.name2bit('Overdensity');
            
        end

        % Only check for nuclear noise if the PSF is not good
        if Args.flagNuclear && any(NuclearCand)

            %NuclearCat = CandCat.selectRows(NuclearCand);
            % Get R magnitude and score of nuclear candidates
            NuclearRMag = R_MAG_PSF(NuclearCand);
            NuclearScore = Score(NuclearCand);

            % Initialize result array
            NumNuclear = sum(NuclearCand);
            NuclearNoise = false(NumNuclear,1);

            % Only test nuclear candidates if it's detectable in R image
            BrightNuclear = (NuclearRMag < R_LIMMAG);

            % Loop through each and assign corresponding median
            for INuclear = 1:NumNuclear
                if ~BrightNuclear(INuclear)
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
                BinnedMags = (R_MAG_PSF > DynamicBinMin) & (R_MAG_PSF < DynamicBinMax);
                
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
                BinMedianS = median(Score(BinnedMags));
                NuclearNoise(INuclear) = (NuclearScore(INuclear) < BinMedianS);
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

            % TODO: Bright galaxy centers have overestimated significance either
            % due to source noise, wrong estimation of the zero point, 
            % PSF misrconstruction or lack of color correction. 
            % Before that's figured out, I'm just
            % increasing the Scorr requirement for galaxy centers.
            % TODO: Consider the new NuclearNoise filter 
            
            if CandCat.isColumn('GAL_DIST')
                NuclearBrightCandidate = NuclearCand & (N_MAG_PSF < 17.0);  
                
                ScorrGood(NuclearBrightCandidate) = ...
                        (abs(Score(NuclearBrightCandidate)) >= abs(Scorr(NuclearBrightCandidate))) ...
                  & (abs(Scorr(NuclearBrightCandidate)) > Args.ScorrThreshold+3) ...
                  & (SDiff(NuclearBrightCandidate) < abs(Scorr(NuclearBrightCandidate)));
            end
    
            ScorrFlagged = ~ScorrGood;
            FilterFlags = FilterFlags + ScorrFlagged.*2.^BD_TF.name2bit('Scorr');

        end

        if Args.flagTranslients
            % Get S2 and Z2 AICs and their difference.
            S2_AIC = CandCat.getCol('S2_AIC');
            Z2_AIC = CandCat.getCol('Z2_AIC');
            AIC_Diff = S2_AIC - Z2_AIC;

            % Exclude isolated candidates unless PSF shape is bad.
            ExcludeCand = (IsolatedCand | (Score > 8.0)) & N_Passes_PSF_Global;
            IsNotTranslient = (AIC_Diff < 0) | ExcludeCand;

            % Relax if candidate is near galaxy but is not nuclear
            if CandCat.isColumn('GAL_DIST')
                IsNotTranslient = IsNotTranslient | ...
                    (~NuclearCand & (AIC_Diff < 1.0));
            end

            TranslientFlagged = ~IsNotTranslient;
            FilterFlags = FilterFlags + TranslientFlagged.*2.^BD_TF.name2bit('Translient');

        end

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(FilterFlags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});
    end
  
end