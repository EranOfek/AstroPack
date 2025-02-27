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
                       'filterChi2' is true, all transients candidates outside these
                       limits are flagged. Default is [0.23 1.41].
                'MinNRChi2dof' - Lower limit on Chi2 per degrees of freedom
                       for New and Ref images. Condition requires that in
                       at least one of the images, the source is not
                       overfitted. Only one image, New or Ref, has to pass.
                       Default is 0.1.
                'flagSaturated' - Bool on whether to flag transients 
                       candidates that are saturated in both reference and 
                       new images. Default is true.
                'flagBadPix_Hard' - Bool on whether to flag transients
                       candidates based on hard bit mask criteria. 
                       Default is true.
                'BadPix_Hard' - Hard bit mask criteria for bad pixels.  
                       Default is {'Interpolated', 'NaN', 'NearEdge',
                       'CoaddLessImages', 'Hole'}.
                'flagBadPix_Soft' - Bool on whether to flag transients
                       candidates based on soft bit mask criteria. 
                       Default is true.
                'BadPix_Soft' - Soft bit mask criteria for bad pixels and 
                       their score threshold values. Transients candidates
                       that contain soft bad pixels are only flagged as 
                       non-transients if their score values are below the 
                       respective thresholds. Default is Default is {{'HighRN', 6.0},
                       {'SrcNoiseDominated', 7.0}, {'FlatHighStd',7.0}, 
                       {'DarkHighVal', 13.0}}.
                'flagSNR' - Bool on whether to flag transients candidates
                       based the signal-to-noise ratio in the subtraction
                       image. Default is true.
                'SNRThreshold' - Threshold for the signal-to-noise ratio
                       filter. Default is 5.0.
                'flagStarMatches' - Bool on whether to flag transients
                       candidates that have matching star positions.
                       Default is true.
                'flagMP' - Bool on whether to flag transients candidates
                       that have matching minor planet postions. Default is
                       true.
                'flagRinging' - Bool on whether to flag transients
                       candidates that may be caused by ringing artifacts.
                       Default is true.
                'flagDensity' - Bool on whether to flag transients that are
                       too close to each other, i.e., that have too many
                       neighbors. Default is true.
                'NeighborDistanceThreshold' - Distance threshold below
                       which a close transient counts as a neighbor.
                       Default is 100.
                'NeighborNumThreshold' - Threshold for the number of
                       neighbors at which to filter the transients
                       candidate. Default is 2.
                'flagPeakDist' - Bool on whether to flag transients for
                       which the peak pixel coordinates deviates too far
                       from the peak sub-pixel coordinates. Default is
                       true.
                'PeakDistThreshold' - Threshold distance for the pixel to
                       sub-pixel peak distance filter. Default is 1.33.
                'flagLimitingMag' - Bool on whether to flag candidates that
                       are above the limiting magnitude. Candidate is
                       filteres if it is above limiting magnitude in New
                       and Ref. Default is true.
                'LimitingMagOverwriteVal' - Static magnitude value to use 
                       as the limiting magnitude. If NaN, magnitude instead
                       is read from the image header. Default is NaN.
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
                --- AstroZOGY ---
                'flagTranslients' - Bool on whether to flag transients 
                       candidates which score higher in Z2 than S2.
                       Default is true.
                'ignoreTranslient_NothingInRef' - Do not flag candidates
                       for translient if source is not detected in the
                       reference image. Default is true.
                'flagScorr' - Bool on whether to flag candidates based on 
                       source noise corrected S statistic. Default is true.
                'ScorrThreshold' - Threshold value for Scorr. Default is 5.0.
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
    
        Args.flagNegatives logical = true;

        Args.flagChi2 logical = true;
        Args.DChi2dofLimits = [0.2 1.5];
        Args.NRChi2dofLimits = [0.1 2.0];
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'NearEdge',...
            'Hole', 'Negative'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'HighRN', 5.0, 7.0}, {'SrcNoiseDominated', 5.0, 7.0}, ...
            {'FlatHighStd', 5.0, 7.0}, {'DarkHighVal', 5.0, 7.0},...
            {'CoaddLessImages',5.0, 7.0}};

        Args.flagSNR logical = true;
        Args.SNRThreshold = 5.0;

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;

        Args.flagPeakDist logical = true;
        Args.PeakDistThreshold = 2.1;

        Args.flagLimitingMag logical = true;    
        Args.LimitingMagOverwriteVal = NaN;

        Args.flagPeakValley logical = true;
        Args.PVDistThresh = 10;

        Args.flagPSFShape logical = true;
        Args.PSFShapeXYMeanN = [0.75694019, 0.82121291]
        Args.PSFShapeCovN = [0.01267776, 0.005022;...
            0.005022,  0.01129344];
        Args.PSFShapeProbThresholdN = 0.05;
        Args.PSFShapeXYMeanD = [1.06919192, 1.24191919]
        Args.PSFShapeCovD = [0.06467546, 0.02720397;...
            0.02720397, 0.06933742];
        Args.PSFShapeProbThresholdD = 0.05;
        
        Args.flagStreak logical = true;
        Args.ignoreStreakPoints = {'BadPixelHard', 'StarMatch', ...
            'Ringing', 'Translient'};
        
        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.NeighborNumThreshold = 30;
        Args.NeighborExclude = {'BadPixelHard', 'StarMatch', ...
            'Ringing', 'Translient', 'Streak'};

        Args.flagCR logical = true;
        Args.CRDeltaSN = 0.5;
        Args.CRDeltaSN_BP = 5.0;

        Args.flagVariable logical = true;
        Args.VarStarDist = 3;

        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;
        Args.ScorrCorrectionParam = 0.7;

        Args.flagTranslients logical = true;
        Args.TranslientCorrectionParam = 30;
        Args.ignoreTranslient_NothingInRef = true;
    end

    Nobj = numel(Obj);

    % Get transients filter bit dictionary
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');

    Arcsec2Rad = 4.84814e-6;

    for Iobj=Nobj:-1:1
        Cat = Obj(Iobj).CatData;
        Score = Cat.getCol('SCORE');

        % Get size of catalog and initialize a bool array corresponding to
        % the catalog rows. Array is initialized as all true and will be
        % negated for rows with rejected candidates.
        CatSize = size(Cat.Catalog,1);

        % Skip empty catalogs
        if CatSize < 1
            TranCat = Cat;
            continue
        end

        % Initialize transients bool
        TF_Flags = zeros(CatSize,1);

        % Flag negative candidates
        if Args.flagNegatives
            NegativeFlagged = (Score < 0.0);
            TF_Flags = TF_Flags + NegativeFlagged.*2.^BD_TF.name2bit('Negative');
        end

        % Apply Chi2 per degrees of freedom criterium.
        if Args.flagChi2 && Cat.isColumn('PSF_CHI2DOF')

            %D_CHI2DOF = Cat.getCol('PSF_CHI2DOF');
            N_CHI2DOF = Cat.getCol('N_PSF_CHI2DOF');
            R_CHI2DOF = Cat.getCol('R_PSF_CHI2DOF');
            Negatives = Score < 0;

            NR_CHI2DOF = N_CHI2DOF;
            NR_CHI2DOF(Negatives) = R_CHI2DOF(Negatives);

            GoodChi2dofNR = ...
                (NR_CHI2DOF > Args.NRChi2dofLimits(1)) & ...
                (NR_CHI2DOF < Args.NRChi2dofLimits(2));

            Chi2dofFlagged = ~GoodChi2dofNR;
            TF_Flags = TF_Flags + Chi2dofFlagged.*2.^BD_TF.name2bit('PSFChi2');
 
        end
    
        % Apply bit mask critera.
        if (Args.flagBadPix_Hard || Args.flagBadPix_Soft || Args.flagSaturated) && ...
                (Cat.isColumn('N_FLAGS') && Cat.isColumn('R_FLAGS'))
            BD = BitDictionary('BitMask.Image.Default');
            BM_new = Cat.getCol('N_FLAGS');
            BM_ref = Cat.getCol('R_FLAGS');
        end

        % Apply criterium for saturated candidates.
        if Args.flagSaturated && exist('BD','var')

            FlagSrcNoiseDom_New = BD.findBit(BM_new,'Saturated');
            FlagSrcNoiseDom_Ref = BD.findBit(BM_ref,'Saturated');
            
            % Check if candidates are saturated in New and Ref, flag these.
            SaturatedInBoth = FlagSrcNoiseDom_New & FlagSrcNoiseDom_Ref;

            SaturationFlagged = SaturatedInBoth;
            TF_Flags = TF_Flags + SaturationFlagged.*2.^BD_TF.name2bit('Saturated');
        end

        % Apply hard bit mask criteria.
        if Args.flagBadPix_Hard && exist('BD','var')

            NBadHard = numel(Args.BadPix_Hard);

            % New bit mask values.
            FlagBadHard_New = false(CatSize,1);
            % Reference bit mask value.
            FlagBadHard_Ref = false(CatSize,1);
    
            for IBad=1:1:NBadHard
                FlagBadHard_New = FlagBadHard_New | ...
                    BD.findBit(BM_new, Args.BadPix_Hard(IBad));
                FlagBadHard_Ref = FlagBadHard_Ref | ...
                    BD.findBit(BM_ref, Args.BadPix_Hard(IBad));
            end

            BadHardIdx = FlagBadHard_New | FlagBadHard_Ref;

            BadHardFlagged = BadHardIdx;
            TF_Flags = TF_Flags + BadHardFlagged.*2.^BD_TF.name2bit('BadPixelHard');
        end

        % Apply soft bit mask criteria.
        if Args.flagBadPix_Soft && exist('BD','var') && Cat.isColumn('SN')

            NBadSoft = numel(Args.BadPix_Soft);

            % New bit mask values.
            FlagBadSoft_New = false(CatSize,1);
            % Reference bit mask values.
            FlagBadSoft_Ref = false(CatSize,1);
    
            for IBad=1:1:NBadSoft
                IBadPix_Soft = Args.BadPix_Soft{IBad};

                AboveThreshold = (abs(Cat.getCol('SCORE')) >= IBadPix_Soft{2})...
                    & (abs(Cat.getCol('SN')) >= IBadPix_Soft{3});

                FlagBadSoft_New = FlagBadSoft_New | (...
                    BD.findBit(BM_new, IBadPix_Soft{1}) & ~AboveThreshold);

                FlagBadSoft_Ref = FlagBadSoft_Ref | (...
                    BD.findBit(BM_ref, IBadPix_Soft{1}) & ~AboveThreshold);
            end

            BadSoftIdx = (FlagBadSoft_New | FlagBadSoft_Ref);

            BadSoftFlagged = BadSoftIdx;
            TF_Flags = TF_Flags + BadSoftFlagged.*2.^BD_TF.name2bit('BadPixelSoft');
        end

        % Flag stars as non-transients
        if Args.flagStarMatches && Cat.isColumn('STAR_N')
            IsStar = (Cat.getCol('STAR_N') > 0.0);

            % Relax flagging for galaxy-star confusion
            if Cat.isColumn('STAR_DIST') && Cat.isColumn('GAL_DIST')
                GalaxyDist = Cat.getCol('GAL_DIST');
                NStars = Cat.getCol('STAR_N');
                NGal = Cat.getCol('GAL_N');
                ExcludeGalaxy = (GalaxyDist <= 3) & (NGal >= NStars);

                IsStar = IsStar & ~ExcludeGalaxy;
            end

            StarFlagged = IsStar;
            TF_Flags = TF_Flags + StarFlagged.*2.^BD_TF.name2bit('StarMatch');
        end

        % Flag minor planets as non-transients
        if Args.flagMP && Cat.isColumn('N_DistMP') && Cat.isColumn('R_DistMP')

            MPFlagged = ~isnan(Cat.getCol('N_DistMP')) | ...
                                        ~isnan(Cat.getCol('R_DistMP'));

            TF_Flags = TF_Flags + MPFlagged.*2.^BD_TF.name2bit('MPMatch');
        end
        
        % Apply ringing criterium
        if Args.flagRinging && Cat.isColumn('SN_GABOR')
            GaborSN = Cat.getCol('SN_GABOR');

            IsRinging =  abs(GaborSN) > abs(Score);

            RingingFlagged = IsRinging;
            TF_Flags = TF_Flags + RingingFlagged.*2.^BD_TF.name2bit('Ringing');
        end

        % Apply signal to noise criterium
        if Args.flagSNR && Cat.isColumn('SN')

            SNR = Cat.getCol('SN');
            SNRBelowThresh = (SNR < Args.SNRThreshold);

            SNRFlagged = SNRBelowThresh;
            TF_Flags = TF_Flags + SNRFlagged.*2.^BD_TF.name2bit('SNR');
        end

        if Args.flagPeakDist && Cat.isColumn('PEAK_DIST')

            PeakDist = Cat.getCol('PEAK_DIST');
            PeakTooFar = PeakDist > Args.PeakDistThreshold;

            PeakFlagged = PeakTooFar;
            TF_Flags = TF_Flags + PeakFlagged.*2.^BD_TF.name2bit('PeakDist');

        end

        if Args.flagLimitingMag
            N_Mag = Cat.getCol('N_MAG_PSF');
            R_Mag = Cat.getCol('R_MAG_PSF');
            
            LimitingMagVal_N = Args.LimitingMagOverwriteVal;
            LimitingMagVal_R = Args.LimitingMagOverwriteVal;

            if isnan(Args.LimitingMagOverwriteVal)
                LimitingMagVal_N = Obj(Iobj).New.HeaderData.getVal('LIMMAG');
                LimitingMagVal_R = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');
            end

            MagBelowLimit = (N_Mag > LimitingMagVal_N) & (R_Mag > LimitingMagVal_R);

            LimMagFlagged = MagBelowLimit;
            TF_Flags = TF_Flags + LimMagFlagged.*2.^BD_TF.name2bit('LIMMAG');
            
        end

        if Args.flagPeakValley && Cat.isColumn('PV_DIST')
            PVDist = Cat.getCol('PV_DIST');
            PeakValley = PVDist < Args.PVDistThresh;

            PVFlagged = PeakValley;
            TF_Flags = TF_Flags + PVFlagged.*2.^BD_TF.name2bit('PVDist');

        end
        
        if Args.flagPSFShape
            X2N = Cat.getCol('N_X2');
            Y2N = Cat.getCol('N_Y2');

            X2D = Cat.getCol('X2');
            Y2D = Cat.getCol('Y2');
            
            X2Y2N = [X2N(:),Y2N(:)];
            X2Y2D = [X2D(:),Y2D(:)];

            ProbN = mvnpdf(X2Y2N, Args.PSFShapeXYMeanN, Args.PSFShapeCovN);
            ProbD = mvnpdf(X2Y2D, Args.PSFShapeXYMeanD, Args.PSFShapeCovD);

            PassesN = ProbN > Args.PSFShapeProbThresholdN;
            PassesD = ProbD > Args.PSFShapeProbThresholdD;
            
            SecondMomentFlagged = PassesN & PassesD;
            
            PSFShapeFlagged = ~SecondMomentFlagged;
            TF_Flags = TF_Flags + PSFShapeFlagged.*2.^BD_TF.name2bit('PSFShape');
        end

        if Args.flagStreak
            [X,Y] = Cat.getXY();

            Ntran = numel(X(:));
            SubSel = true(Ntran,1);
            NExclude = numel(Args.ignoreStreakPoints);

            for IExclude = 1:NExclude
                BitFound = BD_TF.findBit(TF_Flags, Args.ignoreStreakPoints{IExclude});
                SubSel = SubSel & ~BitFound;
            end

            Xt = X(SubSel);
            Yt = Y(SubSel);
            TDist = max(Obj(Iobj).PSFData.fwhm*2,5);

            MinNpts = [5 7 10 13 17 20 23 27 30];
            NMinNpts = numel(MinNpts);
            for IMinNpts = NMinNpts:-1:1
                Res = tools.math.fit.ransacLinear([Xt,Yt], 'Ntrial', 1000, ...
                    'MinRMS', 0.5,'MinNpt',MinNpts(IMinNpts), 'ThresholdDist',TDist);
                if Res.Found
                    break
                end
            end

            if Res.Found
                ModY = Res.Par(1)+Xt.*Res.Par(2);
                Streaked = abs(ModY - Yt) < 20;
                TF_Flags(SubSel) = TF_Flags(SubSel) + Streaked.*2.^BD_TF.name2bit('PSFShape');
            end
        end

        if Args.flagDensity
            XY = Cat.getXY;
            Ntran = numel(XY(:,2));

            % Only count neighbors that have passed filters mentioned in
            % Args.NeighborExlude

            ExcludeNeighbor = false(Ntran,1);
            NExclude = numel(Args.NeighborExclude);
            for IExclude = 1:NExclude
                ExcludeNeighbor = ExcludeNeighbor | ...
                    BD_TF.findBit(TF_Flags, Args.NeighborExclude{IExclude});
            end

            % Iterate through each candidate
            for Itran = Ntran:-1:1
                % Get distance to all other candidates
                NeighborDist = sqrt((XY(Itran,2)-XY(:,2)).^2+(XY(Itran,1)-XY(:,1)).^2);
                % Test distance against threshold
                IsNeighbor = NeighborDist < Args.NeighborDistanceThreshold;
                % Remove excluded neighbors
                IsNeighbor = IsNeighbor & ~ExcludeNeighbor;
                % Count remaining neighbors
                % and remove itself if it was not excluded
                Nneighbors(Itran) = sum(IsNeighbor) - ~ExcludeNeighbor(Itran);
            end

            % Add number of neighbors to catalog
            Nneighbors = transpose(Nneighbors);
            TranCat(Iobj) = Obj(Iobj).CatData.insertCol(cast(Nneighbors,'double'), ...
                'SCORE', {'N_NEIGH'}, {''});
            % Test number of neighbors against threshold
            Overdensity = (Nneighbors >= Args.NeighborNumThreshold);
            % Update flags
            OverdensityFlagged = Overdensity;
            TF_Flags = TF_Flags + OverdensityFlagged.*2.^BD_TF.name2bit('Overdensity');
            
        end

        if Args.flagCR
            SN_delta = Cat.getCol('SN_delta');
            CR_BP_New = BD.findBit(BM_new,'CR_DeltaHT');

            NoNCRs = (abs(Score) - abs(SN_delta) >  Args.CRDeltaSN);

            NoNCRs(CR_BP_New) = (...
                abs(Score(CR_BP_New)) - abs(SN_delta(CR_BP_New)) >  Args.CRDeltaSN_BP);

            TF_Flags = TF_Flags + ~NoNCRs.*2.^BD_TF.name2bit('CRDelta');
        end

        if Args.flagVariable
            % TODO: Move the catalog matching elsewhere
            GalaxyDist = Cat.getCol('GAL_DIST');
            Nuclear = GalaxyDist <= 3;
   
            RADec = Cat.getLonLat('rad');
    
            RA = RADec(:,1);
            Dec = RADec(:,2);
    
            MidRA = median(RA);
            MidDec = median(Dec);
    
            MaxDist = max(celestial.coo.sphere_dist(RA, Dec,...
                MidRA*ones(CatSize,1), MidDec*ones(CatSize,1)));
        
            MaxDistAngle = AstroAngle(MaxDist, 'rad');
    
            % QSO for galaxies
            GalSearchRadius = MaxDistAngle.convert('arcsec').Angle + max(GalaxyDist);
            QSOCat = catsHTM.cone_search('QSO1M', ...
                    MidRA, MidDec, GalSearchRadius, 'OutType','AstroCatalog');

            if QSOCat.sizeCatalog < 1
                VariableGal = zeros(CatSize,1);
            else

                QSOCat.sortrows('Dec');
        
                [QSOLon, QSOLat] = QSOCat.getLonLat('rad');
    
                MatchResQSO = VO.search.search_sortedlat_multi( ...
                    [QSOLon, QSOLat], RA, Dec, -3*Arcsec2Rad);
    
                QSOmatch = vertcat(MatchResQSO.Nmatch) > 0;
    
                VariableGal = Nuclear & QSOmatch;
            end

            % VarStars for stars
            StarDist = Cat.getCol('STAR_DIST');
            NearStar = StarDist <= Args.VarStarDist;

            StarSearchRadius = MaxDistAngle.convert('arcsec').Angle + max(StarDist);

            VarStarCat = catsHTM.cone_search('GAIADR3var', MidRA, MidDec, ...
                StarSearchRadius, 'OutType','AstroCatalog');

            if VarStarCat.sizeCatalog < 1
                VariableStar = zeros(CatSize,1);
            else

                VarStarCat.sortrows('Dec');
        
                [VarStarLon, VarStarLat] = VarStarCat.getLonLat('rad');
    
                MatchResVarStar = VO.search.search_sortedlat_multi( ...
                    [VarStarLon, VarStarLat], RA, Dec, ...
                    -max(StarDist)*Arcsec2Rad);
    
                VarStarmatch = vertcat(MatchResVarStar.Nmatch) > 0;
                
                VariableStar = NearStar & VarStarmatch;
            end
            
            VariableSource = VariableGal | VariableStar;
            
            TF_Flags = TF_Flags + VariableSource.*2.^BD_TF.name2bit('Variable');

        end

        % ----- AstroZOGY -----

        if Args.flagScorr && Cat.isColumn('S_CORR')
            Scorr = Cat.getCol('S_CORR');

            ScorrGood = (abs(Score) >= abs(Scorr)) ...
                & ((abs(Scorr) > Args.ScorrThreshold) | ...
                (abs(Score) - abs(Scorr) < Args.ScorrCorrectionParam));

            %TODO: Galaxy centers have overestimated significance either
            %due to source noise, wrong estimation of the zero point, or
            %lack of color correction. Before that's figured out, I'm just
            %increasing the Scorr requirement for galaxy centers.
            
            if Cat.isColumn('GAL_DIST')
                GalaxyDist = Cat.getCol('GAL_DIST');
                NuclearCandidate = GalaxyDist <= 3;

                ScorrGood(NuclearCandidate) = ...
                    (abs(Score(NuclearCandidate)) >= abs(Scorr(NuclearCandidate))) ...
                & (abs(Scorr(NuclearCandidate)) > Args.ScorrThreshold+3);
            end

            ScorrFlagged = ~ScorrGood;
            TF_Flags = TF_Flags + ScorrFlagged.*2.^BD_TF.name2bit('Scorr');

        end

        if Args.flagTranslients && Cat.isColumn('S2_AIC') && Cat.isColumn('Z2_AIC')
            S2_AIC = Cat.getCol('S2_AIC');
            Z2_AIC = Cat.getCol('Z2_AIC');

            IgnoreTranslientCol = false(CatSize,1);
            if Args.ignoreTranslient_NothingInRef
                LimitingMagVal_R = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');
                R_Mag = Cat.getCol('R_MAG_PSF');
                IgnoreTranslientCol = IgnoreTranslientCol | ...
                    (R_Mag > LimitingMagVal_R);
            end
        
            Z2_AIC = Z2_AIC - Args.TranslientCorrectionParam;
            IsTranslient = (Z2_AIC > S2_AIC) & ~IgnoreTranslientCol;

            TranslientFlagged = IsTranslient;
            TF_Flags = TF_Flags + TranslientFlagged.*2.^BD_TF.name2bit('Translient');

        end

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(TF_Flags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});
    end
  
end