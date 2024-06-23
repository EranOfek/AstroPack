function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transients candidates that are likely not real transients. 
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'flagChi2' - Bool on whether to flag transients candidates
                       based on how well the PSF fits to a stamp on the transient.
                       The goodness value is a Chi2 per degrees of freedom.
                       Default is true.
                'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
                       'filterChi2' is true, all transients candidates outside these
                       limits are flagged. Default is [0.1 1.5].
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
                'SNRCol' - Name of the column holding the signal-to-noise
                       ratio values. Default is 'PSF_SNm'.
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
                --- AstroZOGY ---
                'flagTranslients' - Bool on whether to flag transients 
                       candidates which score higher in Z2 than S2.
                       Default is true.
                'ignoreTranslient_NothingInRef' - 
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

        Args.flagChi2 logical = true;
        Args.Chi2dofLimits = [0.1 1.5];
        
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
        Args.SNRCol = 'PSF_SNm';

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;

        Args.flagDensity logical = true;
        Args.NeighborDistanceThreshold = 100;
        Args.NeighborNumThreshold = 2;
        Args.ExcludedNeigbhors = ["BadPixel_Hard","StarMatches"];

        Args.flagPeakDist logical = true;
        Args.PeakDistThreshold = 1.5;
        Args.PeakDistThresholdGal = 1.5;

        % --- AstroZOGY ---
        Args.flagScorr logical = true;
        Args.ScorrThreshold = 5.0;

        Args.flagTranslients logical = true;
        Args.ignoreTranslient_NothingInRef = true;
        Args.TranslientRefSNThresh = 5.0;
        Args.ignoreTranslient_GalaxyNuclear = true;
        Args.TranslientGalaxyDistThresh = 3.0;
        
    end

    Nobj = numel(Obj);

    for Iobj=Nobj:-1:1
        Cat = Obj(Iobj).CatData;

        % Get size of catalog and initialize a bool array corresponding to
        % the catalog rows. Array is initialized as all true and will be
        % negated for rows with rejected candidates.
        CatSize = size(Cat.Catalog,1);

        if CatSize < 1
            TranCat = Cat;
            continue
        end

        IsTransient = true(CatSize,1);

        % Apply Chi2 per degrees of freedom criterium.
        if Args.flagChi2 && Cat.isColumn('D_Chi2dof')
            GoodChi2dof = (Cat.getCol('D_Chi2dof') > Args.Chi2dofLimits(1)) &...
                (Cat.getCol('D_Chi2dof') < Args.Chi2dofLimits(2));
            Obj(Iobj).CatData.insertCol(cast(~GoodChi2dof,'double'), ...
                'Score', {'NotPSFLike'}, {''});
            IsTransient = IsTransient & GoodChi2dof;
        end
    
        % Apply bit mask critera.
        if (Args.flagBadPix_Hard || Args.flagBadPix_Soft || ...
                Args.flagSaturated) && Cat.isColumn('NewMaskVal')
            BD = BitDictionary('BitMask.Image.Default');
            BM_new = BD.bitdec2name(Cat.getCol('NewMaskVal'));
            BM_ref = BD.bitdec2name(Cat.getCol('RefMaskVal'));
        end

        % Apply StN threshold criteria for saturated candidates.
        if Args.flagSaturated && Cat.isColumn('NewMaskVal') && ...
                Cat.isColumn('PSF_SNm')

            FlagSrcNoiseDom_New = cell2mat(cellfun(@(c) any(strcmp(c, ...
                'Saturated')), BM_new, 'UniformOutput', false));

            FlagSrcNoiseDom_Ref = cell2mat(cellfun(@(c) any(strcmp(c, ...
                'Saturated')), BM_ref, 'UniformOutput', false));
            
            % Check if candidates are saturated in New and Ref, flag these.
            SaturatedInBoth = FlagSrcNoiseDom_New & FlagSrcNoiseDom_Ref;
            % Check if candidates are saturated in New but not Ref, flag
            % these if StN is below threshold.
            SaturatedOnlyInNew = FlagSrcNoiseDom_New & ~FlagSrcNoiseDom_Ref;

            PassesSaturated = ~SaturatedInBoth | SaturatedOnlyInNew;

            Obj(Iobj).CatData.insertCol(cast(SaturatedInBoth,'double'), ...
                'Score', {'Saturated'}, {''});
            IsTransient = IsTransient & PassesSaturated;

        end

        % Hard bit mask criteria.
        if Args.flagBadPix_Hard && Cat.isColumn('NewMaskVal')

            NBadHard = numel(Args.BadPix_Hard);

            % New bit mask values.
            FlagBadHard_New = false(CatSize,1);
            % Reference bit mask value.
            FlagBadHard_Ref = false(CatSize,1);
    
            for IBad=1:1:NBadHard
                FlagBadHard_New = FlagBadHard_New | ...
                    cell2mat(cellfun(@(c) any(strcmp(c, Args.BadPix_Hard(IBad))), ...
                BM_new, 'UniformOutput', false));
                FlagBadHard_Ref = FlagBadHard_Ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.BadPix_Hard(IBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            BadHardIdx = FlagBadHard_New | FlagBadHard_Ref;

            IsTransient = IsTransient & ~BadHardIdx;
            Obj(Iobj).CatData.insertCol(cast(BadHardIdx,'double'), ...
                'Score', {'BadPixel_Hard'}, {''});
        end

        % Soft bit mask criteria.
        if Args.flagBadPix_Soft && Cat.isColumn('NewMaskVal')

            NBadSoft = numel(Args.BadPix_Soft);
            % New bit mask values.
            FlagBadSoft_New = false(CatSize,1);
            % Reference bit mask values.
            FlagBadSoft_Ref = false(CatSize,1);
    
            for IBad=1:1:NBadSoft
                IBadPix_Soft = Args.BadPix_Soft{IBad};

                FlagBadSoft_New = FlagBadSoft_New | ...
                    (cell2mat(cellfun(@(c)any(strcmp(c, IBadPix_Soft{1})), ...
                BM_new, 'UniformOutput', false)) & ...
                abs(Cat.getCol('Score')) < IBadPix_Soft{2} & ...
                abs(Cat.getCol('PSF_SNm')) < IBadPix_Soft{3});

                FlagBadSoft_Ref = FlagBadSoft_Ref | ...
                    (cell2mat(cellfun(@(c)any(strcmp(c, IBadPix_Soft{1})), ...
                BM_ref, 'UniformOutput', false)) & ...
                abs(Cat.getCol('Score')) < IBadPix_Soft{2} & ...
                abs(Cat.getCol('PSF_SNm')) < IBadPix_Soft{3});
            end

            BadSoftIdx = (FlagBadSoft_New | FlagBadSoft_Ref);

            IsTransient = IsTransient & ~BadSoftIdx;
            Obj(Iobj).CatData.insertCol(cast(BadSoftIdx,'double'), ...
                'Score', {'BadPixel_Soft'}, {''});

        end

        if Args.flagStarMatches && Cat.isColumn('StarMatches')
            StarMatches = (Cat.getCol('StarMatches') > 0.0);
            IsStar = StarMatches;
            if Cat.isColumn('GalaxyMatches')
                StarDist = Cat.getCol('StarDist');
                GalaxyDist = Cat.getCol('GalaxyDist');
                ExcludeGalaxy = GalaxyDist < 1.3*StarDist;

                if Cat.isColumn('R_SNm')
                    R_SNm = Cat.getCol('R_SNm');
                    Low_R_SNm = R_SNm < 5.0;
                    ExcludeGalaxy = ExcludeGalaxy & Low_R_SNm;
                end
            IsStar = IsStar & ~ ExcludeGalaxy;
            end
            IsTransient = IsTransient & ~IsStar;
        end

        if Args.flagMP && Cat.isColumn('DistMP_new')
            IsTransient = IsTransient & isnan(Cat.getCol('DistMP_new'));
            IsTransient = IsTransient & isnan(Cat.getCol('DistMP_ref'));
        end
        
        if Args.flagRinging && Cat.isColumn('GaborSN')
            GaborSN = Cat.getCol('GaborSN');
            Score = Cat.getCol('Score');

            IsRinging =  GaborSN > Score;
            IsTransient = IsTransient & ~IsRinging;

            Obj(Iobj).CatData.insertCol(cast(IsRinging,'double'), ...
                'Score', {'IsRinging'}, {''});
        end

        if Args.flagSNR && Cat.isColumn(Args.SNRCol)
            SNR = Cat.getCol(Args.SNRCol);

            SNRBelowThresh = (SNR < Args.SNRThreshold);
            Obj(Iobj).CatData.insertCol(cast(SNRBelowThresh,'double'), ...
                'Score', {'SNRBelowThresh'}, {''});
            IsTransient = IsTransient & ~SNRBelowThresh;
        end

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
                'Score', {'Neighbors'}, {''});
            Overdensity = (Nneighbors >= Args.NeighborNumThreshold);
            Obj(Iobj).CatData.insertCol(cast(Overdensity,'double'), ...
                'Score', {'Overdensity'}, {''});
            IsTransient = IsTransient & ~Overdensity;
        end

        if Args.flagPeakDist && (Cat.isColumn('X1') && Cat.isColumn('Y1')) 
            XY = Cat.getXY('ColX','XPEAK','ColY','YPEAK');
            XY1 = Cat.getXY('ColX','X1','ColY','Y1');

            PeakDist = sqrt((XY(:,1)-XY1(:,1)).^2+(XY(:,2)-XY1(:,2)).^2);
            PeakTooFar = PeakDist > Args.PeakDistThreshold;
            if Cat.isColumn('GalaxyMatches')
                IsInGalaxy = Cat.getCol('GalaxyMatches') > 0;
                PeakTooFar(IsInGalaxy) = PeakDist(IsInGalaxy)  > Args.PeakDistThresholdGal;
            end
            Obj(Iobj).CatData.insertCol(cast(PeakTooFar,'double'), ...
                'Score', {'PeakTooFar'}, {''});
            IsTransient = IsTransient & ~PeakTooFar;

        end
                
        % ----- AstroZOGY -----

        if Args.flagScorr && Cat.isColumn('Scorr')
            Scorr = Cat.getCol('Scorr');

            ScorrBelowThresh = (abs(Scorr) < Args.ScorrThreshold);
            Obj(Iobj).CatData.insertCol(cast(ScorrBelowThresh,'double'), ...
                'Score', {'ScorrBelowThresh'}, {''});
            IsTransient = IsTransient & ~ScorrBelowThresh;
        end

        if Args.flagTranslients && Cat.isColumn('S2_AIC') && Cat.isColumn('Z2_AIC')
            S2_AIC = Cat.getCol('S2_AIC');
            Z2_AIC = Cat.getCol('Z2_AIC');

            IgnoreTranslientCol = false(CatSize,1);
            if Args.ignoreTranslient_NothingInRef && Cat.isColumn('R_SNm')
                R_SNm = Cat.getCol('R_SNm');
                IgnoreTranslientCol = IgnoreTranslientCol | ...
                    (R_SNm < Args.TranslientRefSNThresh);
            end
            if Args.ignoreTranslient_GalaxyNuclear && Cat.isColumn('GalaxyDist')
                GalaxyDist = Cat.getCol('GalaxyDist');
                IgnoreTranslientCol = IgnoreTranslientCol | ...
                    GalaxyDist < Args.TranslientGalaxyDistThresh;
            end            
        
            IsTranslient = (Z2_AIC > S2_AIC) & ~IgnoreTranslientCol;
            Obj(Iobj).CatData.insertCol(cast(IsTranslient,'double'), ...
                'Score', {'Translient'}, {''});
            IsTransient = IsTransient & ~IsTranslient;
        end

        % Sub-select passing candidates only.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(~IsTransient,'double'), 'Score', ...
            {'LikelyNotTransient'}, {''});
    end
  
end