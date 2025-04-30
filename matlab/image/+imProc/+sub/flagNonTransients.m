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
                       limits are flagged. Default is [0.1 2.0].
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
                       Default is {{'HighRN', 1.2}, {'FlatHighStd',1.2}, 
                       {'DarkHighVal', 1.2}, {'CR_DeltaHT', 0.3}}.
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
                       Default is 1.4.
                'SecondMomAsymLim' - Threshold on asymetry of the second
                       moments of the New image PSF. If abs(x^2-y^2) is
                       higher than the threshold, the PSF is deemed to be
                       too elongated. If a candidate fails this criterium, 
                       it has additionally fail SecondMomHardLim, 
                       SecondMomFinalLim, OmniDirectionThreshold, or 
                       PeakDistThreshold to be flagged as false positive.
                       Default is 0.33.
                'SecondMomHardLim' - Threshold on second moments of the New
                       Image PSF. This threshold is applied if New image
                       PSF fails SeconMomSoftLim or SecondMomAsymLim. If
                       x^2 AND y^2 are higher than this limit, all
                       candidates are flagged. Default is 2.0.
                'SecondMomFinalLim' - Threshold on second moments of the New
                       Image PSF. This threshold is applied if New image
                       PSF fails SeconMomSoftLim or SecondMomAsymLim. If
                       x^2 OR y^2 are higher than this limit, all
                       candidates are flagged. Default is 2.3.
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
                'ignoreSreakPoints' - Default {'BadPixelHard', 'StarMatch', ...
                       'Ringing', 'Translient', 'Streak'}.
                'StreakDistanceThreshold' - Default is 20.
                'NumStreaks' - Default is 2.

                'flagDensity' - Bool on whether to flag transients that are
                       too close to each other, i.e., that have too many
                       neighbors. Default is true.
                'NeighborDistanceThreshold' - Distance threshold below
                       which a close transient counts as a neighbor.
                       Default is 100.
                'NeighborExclude' - Default is {'BadPixelHard', 'StarMatch', ...
                       'Ringing', 'Translient', 'Streak'}.
                'NeighborDenThreshold' - Default is 3.2.
                'SaturatedNeighborDistanceThreshold' - Default is 50.
                'NeighborNumThresholdSaturated' - Threshold for the number 
                       of neighbors at which to filter the transients
                       candidate. Default is 2.
                'flagVariable' - Default is true.
                'VarStarDist' - Default is 3.
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
    
        Args.flagNegatives logical = true;

        Args.flagChi2 logical = true;
        Args.Chi2dofLimits = [0.1 2.0];
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'NearEdge',...
            'Hole', 'Negative'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'HighRN', 1.2},  ...
            {'FlatHighStd', 1.2}, {'DarkHighVal', 1.2}, {'CR_DeltaHT',0.3}};

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;

        Args.flagNPsfShape logical = true;
        Args.SecondMomSoftLim = 1.4;
        Args.SecondMomAsymLim = 0.25;
        Args.SecondMomHardLim = 2.0;
        Args.SecondMomFinalLim = 2.3;
        Args.OmniDirectionThreshold = [0.7 57.0];
        Args.PeakDistThreshold = 3.0;

        Args.flagDPSFShape logical = true;
        Args.PSFShapeXYMeanD = [1.06919192, 1.24191919]
        Args.PSFShapeCovD = [0.06467546, 0.02720397;...
            0.02720397, 0.06933742];
        Args.PSFShapeProbThresholdD = 0.05;
        
        Args.flagLimitingMag logical = true;
        Args.LimitingMagOverwriteVal = NaN;

        Args.flagPeakValley logical = true;
        Args.PVDistThresh = 10;
       
        Args.flagStreak logical = true;
        Args.ignoreStreakPoints = {'BadPixelHard', 'StarMatch', ...
            'Ringing', 'Translient', 'Streak'};
        Args.StreakDistanceThreshold = 20;
        Args.NumStreaks = 2;
        
        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.SaturatedNeighborDistanceThreshold = 50;
        Args.NeighborDenThreshold = 3.2;
        Args.NeighborExclude = {'BadPixelHard', 'StarMatch', ...
            'Ringing', 'Translient', 'Streak'};
        Args.NeighborNumThresholdSaturated = 2;
    
        Args.flagVariable logical = true;
        Args.VarStarDist = 3;

        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;
        Args.ScorrCorrectionParam = 0.7;

        Args.flagTranslients logical = true;
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

        if Args.flagChi2 || Args.flagTranslients
            R_MAG = Cat.getCol('R_MAG_PSF');
            R_LIMMAG = Obj(Iobj).Ref.HeaderData.getVal('LIMMAG');
            R_SN = Cat.getCol('R_SN');
            NothingInRef = ((abs(R_SN) < 3) | (R_MAG > R_LIMMAG));
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
                (NR_CHI2DOF > Args.Chi2dofLimits(1)) & ...
                (NR_CHI2DOF < Args.Chi2dofLimits(2));

            if exist('NothingInRef','var')
                GoodChi2dofNR = GoodChi2dofNR | ~NothingInRef;
            end

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

            FlagSaturated_New = BD.findBit(BM_new,'Saturated');
            FlagSaturated_Ref = BD.findBit(BM_ref,'Saturated');
            
            % Check if candidates are saturated in New and Ref, flag these.
            SaturatedInBoth = FlagSaturated_New & FlagSaturated_Ref;

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
        if Args.flagBadPix_Soft && exist('BD','var') && Cat.isColumn('SN_delta')

            SN_delta = Cat.getCol('SN_delta');
            SdiffSd = Score - SN_delta;

            BPSThresh = zeros(CatSize,1);

            NBadSoft = numel(Args.BadPix_Soft);

            for IBad=1:1:NBadSoft
                IBadPix_Soft = Args.BadPix_Soft{IBad};

                BPinN = BD.findBit(BM_new, IBadPix_Soft{1});
                BPinR = BD.findBit(BM_ref, IBadPix_Soft{1});

                BPSThresh(BPinN | BPinR) = BPSThresh(BPinN | BPinR) ...
                    + IBadPix_Soft{2};
            end

            BadSoftFlagged = (SdiffSd < BPSThresh);
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
        
        if Args.flagDPSFShape
            X2D = Cat.getCol('X2');
            Y2D = Cat.getCol('Y2');

            X2Y2D = [X2D(:),Y2D(:)];

            ProbD = mvnpdf(X2Y2D, Args.PSFShapeXYMeanD, Args.PSFShapeCovD);

            PassesD = ProbD > Args.PSFShapeProbThresholdD;
            
            PSFShapeFlagged = ~PassesD;
            TF_Flags = TF_Flags + PSFShapeFlagged.*2.^BD_TF.name2bit('DPSFShape');
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

            for IStreak=1:Args.NumStreaks

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
                    Streaked = abs(ModY - Yt) < Args.StreakDistanceThreshold;
                    TF_Flags(SubSel) = TF_Flags(SubSel) + Streaked.*2.^BD_TF.name2bit('Streak');
                    SubSel(SubSel) = ~Streaked;
                else
                    break
                end
            end
        end

        if Args.flagDensity
            XY = Cat.getXY;
            Ntran = numel(XY(:,2));

            % Only count neighbors that have passed filters mentioned in
            % Args.NeighborExlude

            ExcludeNeighbor = false(Ntran,1);
            NExclude = numel(Args.NeighborExclude);
            FlagSaturated_Ref = BD.findBit(BM_ref,'Saturated');
            
            for IExclude = 1:NExclude
                ExcludeNeighbor = ExcludeNeighbor | ...
                    BD_TF.findBit(TF_Flags, Args.NeighborExclude{IExclude});
            end

            NearSaturated = false(Ntran,1);
            Nneighbors = zeros(Ntran,1);
            LocalDensity = zeros(Ntran,1);
            % Iterate through each candidate
            for Itran = Ntran:-1:1
                % Get distance to all other candidates
                NeighborDist = sqrt((XY(Itran,2)-XY(:,2)).^2+(XY(Itran,1)-XY(:,1)).^2);
                % Test distance against threshold
                IsNeighbor = NeighborDist < Args.NeighborDistanceThreshold;
                % Exclude itself
                IsNeighbor = IsNeighbor & (NeighborDist > 0);
                IsSaturatedNeighbor = IsNeighbor & FlagSaturated_Ref &...
                    (NeighborDist < Args.SaturatedNeighborDistanceThreshold);
                % Remove excluded neighbors
                NearSaturated0 = any(IsSaturatedNeighbor);
                NearSaturated(Itran) = NearSaturated0;
                IsNeighbor = IsNeighbor & ~ExcludeNeighbor;
                % Count remaining neighbors
                Nneighbors0 = sum(IsNeighbor);
                Nneighbors(Itran) = Nneighbors0;
                LocalDensity(Itran) = sum(1./NeighborDist(IsNeighbor));
            end

            % Add number of neighbors to catalog
            Nneighbors = cast(Nneighbors,'double');
            LocalDensity = cast(LocalDensity, 'double');
            TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
                cell2mat({Nneighbors,LocalDensity}), ...
                'SCORE', {'N_NEIGH','DENSITY'}, {'',''});
            % Test number of neighbors against threshold
            Overdensity = (LocalDensity > 1.0) | ...
                (Nneighbors.*LocalDensity >= Args.NeighborDenThreshold);
            Overdensity = Overdensity | ...
                (NearSaturated & (Nneighbors >= Args.NeighborNumThresholdSaturated));
            % Update flags
            OverdensityFlagged = Overdensity;

            TF_Flags = TF_Flags + OverdensityFlagged.*2.^BD_TF.name2bit('Overdensity');
            
        end

        if Args.flagNPsfShape && Cat.isColumn('N_X2') && Cat.isColumn('N_Y2')

            X2N = Cat.getCol('N_X2');
            Y2N = Cat.getCol('N_Y2');
            %XYN = Cat.getCol('N_XY');

            PassesN = (X2N < Args.SecondMomSoftLim) & ...
                      (Y2N < Args.SecondMomSoftLim) & ...
                      (abs(X2N-Y2N) < Args.SecondMomAsymLim);

            if Args.flagTranslients
                DoNotExclude = ~PassesN;
            end
            
            if Cat.isColumn('GDIRCVAR') && Cat.isColumn('GDIRERROR') && ...
                    Cat.isColumn('PEAK_DIST')
              
                GDIRCVAR = Cat.getCol('GDIRCVAR');
                GDIRERROR = Cat.getCol('GDIRERROR');
                PassesGDir = (GDIRCVAR > Args.OmniDirectionThreshold(1)) & ...
                             (GDIRERROR < Args.OmniDirectionThreshold(2));

                PeakDist = Cat.getCol('PEAK_DIST');
                PassesPeak = PeakDist < Args.PeakDistThreshold;

                PassesHardLim = (X2N < Args.SecondMomHardLim) | ...
                                (Y2N < Args.SecondMomHardLim);

                PassesFinalLim = (X2N < Args.SecondMomFinalLim) & ...
                                (Y2N < Args.SecondMomFinalLim);
                            
                PassesLocal = (PassesPeak & PassesGDir & ...
                    PassesHardLim & PassesFinalLim);

                PassesN = PassesN | PassesLocal;

                if Args.flagDensity && exist('NearSaturated','var')
                    PassesN(NearSaturated) = PassesLocal(NearSaturated);
                end
            end

            NShapeFlagged = ~PassesN;

            TF_Flags = TF_Flags + NShapeFlagged.*2.^BD_TF.name2bit('NPSFShape');
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
            
            SDiff = abs(Score) - abs(Scorr);

            ScorrGood = (abs(Score) >= abs(Scorr)) ...
                & ((abs(Scorr) > Args.ScorrThreshold) | ...
                (SDiff < Args.ScorrCorrectionParam));

            %TODO: Galaxy centers have overestimated significance either
            %due to source noise, wrong estimation of the zero point, or
            %lack of color correction. Before that's figured out, I'm just
            %increasing the Scorr requirement for galaxy centers.
            
            if Cat.isColumn('GAL_DIST')
                GalaxyDist = Cat.getCol('GAL_DIST');
                NuclearCandidate = GalaxyDist <= 3;
                
                ScorrGood(NuclearCandidate) = ...
                    (abs(Score(NuclearCandidate)) >= abs(Scorr(NuclearCandidate))) ...
                  & (abs(Scorr(NuclearCandidate)) > Args.ScorrThreshold+3) ...
                  & (SDiff(NuclearCandidate) < abs(Scorr(NuclearCandidate)));
            end

            ScorrFlagged = ~ScorrGood;
            TF_Flags = TF_Flags + ScorrFlagged.*2.^BD_TF.name2bit('Scorr');

        end

        if Args.flagTranslients && Cat.isColumn('S2_AIC') && Cat.isColumn('Z2_AIC')
            S2_AIC = Cat.getCol('S2_AIC');
            Z2_AIC = Cat.getCol('Z2_AIC');
            
            AIC_Diff = S2_AIC - Z2_AIC;


            IsNotTranslient = (AIC_Diff < 0);

            ExcludeCand = false(Ntran,1);
            if exist('NothingInRef','var')
                ExcludeCand = NothingInRef;
            end

            if exist('DoNotExclude','var')
                ExcludeCand = ~DoNotExclude;
            end
            
            IsNotTranslient = IsNotTranslient | ExcludeCand;

            if Cat.isColumn('GAL_DIST')
                GalaxyDist = Cat.getCol('GAL_DIST');
                NotNuclear = GalaxyDist > 3;
                IsNotTranslient = IsNotTranslient | ...
                    (NotNuclear & (AIC_Diff < 7.0));
            end

            TranslientFlagged = ~IsNotTranslient;
            TF_Flags = TF_Flags + TranslientFlagged.*2.^BD_TF.name2bit('Translient');

        end

        % Safe flags as bit value.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(TF_Flags, 'double'), 'SCORE', ...
            {'FLAGS_TRANSIENT'}, {''});
    end
  
end