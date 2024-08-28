function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transients candidates that are likely not real transients. 
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'flagValleys' - Bool on whether to flag negative
                       candidates. Default is true.
                'flagChi2' - Bool on whether to flag transients candidates
                       based on how well the PSF fits to a stamp on the transient.
                       The goodness value is a Chi2 per degrees of freedom.
                       Default is true.
                'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
                       'filterChi2' is true, all transients candidates outside these
                       limits are flagged. Default is [0.1 1.5].
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
                       'CoaddLessImages', 'Hole', 'CR_DeltaHT'}.
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
                'ExcludedNeighbors' - Flags for which not to count nearby
                       transients candidates as neighbors. Values are
                       names of columns in the candidate catalog, this is
                       meant to see if the candidates fail any other
                       filters. Default is ["BadPixel_Hard","StarMatches"].
                'flagPeakDist' - Bool on whether to flag transients for
                       which the peak pixel coordinates deviates too far
                       from the peak sub-pixel coordinates. Default is
                       true.
                'PeakDistThreshold' - Threshold distance for the pixel to
                       sub-pixel peak distance filter. Default is 1.5.
                'PeakDistThresholdGal' - Threshold distance for the pixel to
                       sub-pixel peak distance filter if cnadidate has a 
                       galaxy match. Default is 2.0.
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
                --- AstroZOGY ---
                'flagTranslients' - Bool on whether to flag transients 
                       candidates which score higher in Z2 than S2.
                       Default is true.
                'ignoreTranslient_NothingInRef' - Do not flag candidates
                       for translient if source is not detected in the
                       reference image. Default is true.
                'ignoreTranslient_GalaxyNuclear' - Do not flag candidates
                       for translient if source is matched to a galaxy and 
                       close to the nucleus. Default is true.
                'TranslientGalaxyDistThresh' - Threshold distance from galaxy
                       below which not to apply transient flagging.
                       Default is 3.0.
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
    
        Args.flagValleys logical = true;

        Args.flagChi2 logical = true;
        Args.Chi2dofLimits = [0.1 1.5];
        Args.MinNRChi2dof = 0.1;
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'NearEdge',...
            'Hole', 'CR_DeltaHT', 'Negative'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'HighRN', 6.0, 14.0}, {'SrcNoiseDominated', 6.0, 14.0}, ...
            {'FlatHighStd',6.0, 14.0}, {'DarkHighVal', 6.0, 14.0},...
            {'CoaddLessImages', 6.0, 14.0}};

        Args.flagSNR logical = true;
        Args.SNRThreshold = 5.0;

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;

        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.NeighborNumThreshold = 2;
        Args.ExcludedNeigbhors = ["BadPixel_Hard","STAR_N"];

        Args.flagPeakDist logical = true;
        Args.PeakDistThreshold = 1.5;
        Args.PeakDistThresholdGal = 2.0;

        Args.flagLimitingMag logical = true;
        Args.LimitingMagOverwriteVal = NaN;

        Args.flagPeakValley logical = true;
        Args.PVDistThresh = 10;

        Args.flagFocusing logical = false;
        Args.FocusFWHMThreshSoft = 3.5;
        Args.FocusFWHMThreshHard = 4.0;
        Args.Focus2ndMomentThresh = 2.0;
        
        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;

        Args.flagTranslients logical = true;
        Args.TranslientCorrectionParam = 20;
        Args.ignoreTranslient_NothingInRef = true;
        Args.ignoreTranslient_GalaxyNuclear = false;
        Args.TranslientGalaxyDistThresh = 1.0;

    end

    Nobj = numel(Obj);

    % Get transients filter bit dictionary
    BD_TF = BitDictionary('BitMask.TransientsFilter.Default');

    for Iobj=Nobj:-1:1
        Cat = Obj(Iobj).CatData;

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
        IsTransient = true(CatSize,1);

        % Flag negative candidates
        if Args.flagValleys
            Score = Cat.getCol('SCORE');

            IsTransient = IsTransient & ~(Score < 0.0);
            ValleyFlagged = (Score < 0.0);
            TF_Flags = TF_Flags + ValleyFlagged.*2.^BD_TF.name2bit('Negative');
        end

        % Apply Chi2 per degrees of freedom criterium.
        if Args.flagChi2 && Cat.isColumn('CHI2DOF')
            DChi2 = Cat.getCol('CHI2DOF');
            GoodChi2dofD = (DChi2 > Args.Chi2dofLimits(1)) &...
                (DChi2 < Args.Chi2dofLimits(2));
            GoodChi2dof = GoodChi2dofD;

            % Demand also that in at least New or Ref, 
            % the candidate is not overfitted
            if Cat.isColumn('N_CHI2DOF') && Cat.isColumn('R_CHI2DOF')
                NChi2 = Cat.getCol('N_CHI2DOF');
                RChi2 = Cat.getCol('R_CHI2DOF');
                GoodChi2dofNR = (NChi2 > Args.MinNRChi2dof) |...
                    (RChi2 > Args.MinNRChi2dof);
                GoodChi2dof = GoodChi2dofD & GoodChi2dofNR;            
            end

            Chi2dofFlagged = ~GoodChi2dof;
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
        if Args.flagBadPix_Soft && exist('BD','var') && Cat.isColumn('PSF_SNm')

            NBadSoft = numel(Args.BadPix_Soft);

            % New bit mask values.
            FlagBadSoft_New = false(CatSize,1);
            % Reference bit mask values.
            FlagBadSoft_Ref = false(CatSize,1);
    
            for IBad=1:1:NBadSoft
                IBadPix_Soft = Args.BadPix_Soft{IBad};

                FlagBadSoft_New = FlagBadSoft_New | ...
                    (BD.findBit(BM_new, IBadPix_Soft{1}) & ...
                abs(Cat.getCol('SCORE')) < IBadPix_Soft{2} & ...
                abs(Cat.getCol('PSF_SNm')) < IBadPix_Soft{3});

                FlagBadSoft_Ref = FlagBadSoft_Ref | ...
                    (BD.findBit(BM_ref, IBadPix_Soft{1})& ...
                abs(Cat.getCol('SCORE')) < IBadPix_Soft{2} & ...
                abs(Cat.getCol('PSF_SNm')) < IBadPix_Soft{3});
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
                StarDist = Cat.getCol('STAR_DIST');
                GalaxyDist = Cat.getCol('GAL_DIST');
                ExcludeGalaxy = GalaxyDist < 1.3*StarDist;

                if Cat.isColumn('R_PSF_SNm')
                    R_SNm = Cat.getCol('R_PSF_SNm');
                    Low_R_SNm = R_SNm < 5.0;
                    ExcludeGalaxy = ExcludeGalaxy & Low_R_SNm;
                end
            IsStar = IsStar & ~ ExcludeGalaxy;
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
            Score = Cat.getCol('SCORE');

            IsRinging =  GaborSN > abs(Score);

            RingingFlagged = IsRinging;
            TF_Flags = TF_Flags + RingingFlagged.*2.^BD_TF.name2bit('Ringing');
        end

        % Apply signal to noise criterium
        if Args.flagSNR && Cat.isColumn('PSF_SNm')

            SNR = Cat.getCol('PSF_SNm');
            SNRBelowThresh = (SNR < Args.SNRThreshold);

            SNRFlagged = SNRBelowThresh;
            TF_Flags = TF_Flags + SNRFlagged.*2.^BD_TF.name2bit('SNR');
        end

        % Apply density criterium
        if Args.flagDensity

            XY = Cat.getXY;
            Ntran = numel(XY(:,2));

            ExcludeNeighbor = false(Ntran,1);
            if ~isempty(Args.ExcludedNeigbhors)
                NumExclusions = numel(Args.ExcludedNeigbhors);
                for IExclusion = 1:1:NumExclusions
                    ExclusionColName = Args.ExcludedNeigbhors(IExclusion);
                    if Cat.isColumn(ExclusionColName)
                        ExclusionCol = Cat.getCol(ExclusionColName);
                        ExcludeNeighbor = ExcludeNeighbor | ...
                        (ExclusionCol > 0.0);
                    end
                end
            end

            for Itran = Ntran:-1:1
                NeighborDist = sqrt((XY(Itran,2)-XY(:,2)).^2+(XY(Itran,1)-XY(:,1)).^2);
                IsNeighbor = NeighborDist < Args.NeighborDistanceThreshold;
                IsNeighbor = IsNeighbor & ~ExcludeNeighbor;
                Nneighbors(Itran) = sum(IsNeighbor) - ~ExcludeNeighbor(Itran);
            end

            Nneighbors = transpose(Nneighbors);
            Cat(Iobj) = Cat(Iobj).insertCol(cast(Nneighbors,'double'), ...
                'SCORE', {'N_NEIGH'}, {''});
            Overdensity = (Nneighbors >= Args.NeighborNumThreshold);

            OverdensityFlagged = Overdensity;
            TF_Flags = TF_Flags + OverdensityFlagged.*2.^BD_TF.name2bit('Overdensity');
            
        end

        if Args.flagPeakDist && Cat.isColumn('PEAK_DIST') 

            PeakDist = Cat.getCol('PEAK_DIST');
            PeakTooFar = PeakDist > Args.PeakDistThreshold;
            if Cat.isColumn('GAL_N')
                IsInGalaxy = Cat.getCol('GAL_N') > 0;
                PeakTooFar(IsInGalaxy) = PeakDist(IsInGalaxy)  > Args.PeakDistThresholdGal;
            end

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


        if Args.flagFocusing
            NFWHM = Obj(Iobj).New.PSFData.fwhm;

            X2 = Cat.getCol('X2');
            Y2 = Cat.getCol('Y2');
            
            FWHMFlaggedHard = ones(CatSize,1)*(NFWHM > Args.FocusFWHMThreshHard);
            FWHMFlaggedSoft = ones(CatSize,1)*(NFWHM > Args.FocusFWHMThreshSoft);
            SecondMomentFlagged = (X2 > Args.Focus2ndMomentThresh) | ...
                (Y2 > Args.Focus2ndMomentThresh);
            FocusFlagged = (FWHMFlaggedHard) | ...
                (FWHMFlaggedSoft & SecondMomentFlagged);
            TF_Flags = TF_Flags + FocusFlagged.*2.^BD_TF.name2bit('Focusing');
        end
              
        % ----- AstroZOGY -----

        if Args.flagScorr && Cat.isColumn('S_CORR')
            Scorr = Cat.getCol('S_CORR');

            ScorrBelowThresh = (abs(Scorr) < Args.ScorrThreshold);

            ScorrFlagged = ScorrBelowThresh;
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
            if Args.ignoreTranslient_GalaxyNuclear && Cat.isColumn('GAL_DIST')
                GalaxyDist = Cat.getCol('GAL_DIST');
                IgnoreTranslientCol = IgnoreTranslientCol | ...
                    GalaxyDist < Args.TranslientGalaxyDistThresh;
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