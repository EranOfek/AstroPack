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
                       limits are flagged. Default is [0.07 5.87].
                'flagSaturated' - Bool on whether to flag transients 
                       candidates that are saturated in both reference and 
                       new images. Default is true.
                'flagBadPix_Hard' - Bool on whether to flag transients
                       candidates based on hard bit mask criteria. 
                       Default is true.
                'BadPix_Hard' - Hard bit mask criteria for bad pixels.  
                       Default is {'Interpolated', 'NaN', 'FlatHighStd',
                       'DarkHighVal'}.
                'flagBadPix_Soft' - Bool on whether to flag transients
                       candidates based on soft bit mask criteria. 
                       Default is true.
                'BadPix_Soft' - Soft bit mask criteria for bad pixels and 
                       their score threshold values. Transients candidates
                       that contain soft bad pixels are only flagged as 
                       non-transients if their score values are below the 
                       respective thresholds. Default is {{'HighRN', 5.6}, 
                       {'Edge', 8}, {'NearEdge', 8}, {'SrcNoiseDominated',
                       12.0}}.
                'flagStarMatches' - Bool on whether to flag transients
                       candidates that have matching star positions.
                       Default is true.
                'flagMP' - Bool on whether to flag transients candidates
                       that have matching minor planet postions. Default is
                       true.
                'flagRinging' - Bool on whether to flag transients
                       candidates that may be caused by ringing artifacts.
                       Default is true.
                'GaborSNRatioThreshold' - Threshold value on GaborSN
                       used to flag transients candidates for ringing.
                       Default is 1.
                --- AstroZOGY ---
                'flagTranslients' - Bool on whether to flag transients 
                       candidates which score higher in Z2 than S2.
                       Default is true.
                'S2toZ2RatioThresh' - Threshold value for the S2 to Z2
                       ratio below which to classify a transients candidate
                       as a transLient. Default value is 0.66.
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
        Args.Chi2dofLimits = [0.07 5.87];
        
        Args.flagSaturated logical = true;

        Args.flagBadPix_Hard logical  = true;
        Args.BadPix_Hard       = {'Interpolated', 'NaN', 'FlatHighStd',...
            'DarkHighVal'};

        Args.flagBadPix_Soft logical  = true;
        Args.BadPix_Soft       = {{'HighRN', 5.6}, {'Edge', 8}, {'NearEdge', 8},...
            {'SrcNoiseDominated', 12.0}};

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagRinging logical = true;
        Args.GaborSNRatioThreshold = 0.66;

        % --- AstroZOGY ---
        Args.flagTranslients logical = true;
        Args.S2toZ2RatioThresh = 0.66;

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
                'Score', {'Chi2dof_Flag'}, {''});
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
                abs(Cat.getCol('Score')) < IBadPix_Soft{2});

                FlagBadSoft_Ref = FlagBadSoft_Ref | ...
                    (cell2mat(cellfun(@(c)any(strcmp(c, IBadPix_Soft{1})), ...
                BM_ref, 'UniformOutput', false)) & ...
                abs(Cat.getCol('Score')) < IBadPix_Soft{2});
            end

            BadSoftIdx = (FlagBadSoft_New | FlagBadSoft_Ref);

            IsTransient = IsTransient & ~BadSoftIdx;
            Obj(Iobj).CatData.insertCol(cast(BadSoftIdx,'double'), ...
                'Score', {'BadPixel_Soft'}, {''});

        end

        if Args.flagStarMatches && Cat.isColumn('StarMatches')
            IsTransient = IsTransient & (Cat.getCol('StarMatches') < 1);
        end

        if Args.flagMP && Cat.isColumn('SolarDist')
            IsTransient = IsTransient & isnan(Cat.getCol('SolarDist'));
        end
        
        if Args.flagTranslients && Cat.isColumn('S2_AIC') && Cat.isColumn('Z2_AIC')
            S2_AIC = Cat.getCol('S2_AIC');
            Z2_AIC = Cat.getCol('Z2_AIC');
            S2toZ2Ratio = S2_AIC./Z2_AIC;
            IsTranslient = ~(S2toZ2Ratio > Args.S2toZ2RatioThresh);
            Obj(Iobj).CatData.insertCol(cast(IsTranslient,'double'), ...
                'Score', {'Translient'}, {''});            
            IsTransient = IsTransient & ~IsTranslient;
        end

        if Args.flagRinging && Cat.isColumn('GaborSN')
            GaborSN = Cat.getCol('GaborSN');
            Score = Cat.getCol('Score');

            ScoreToGaborRatio = abs(Score./GaborSN);

            IsRinging =  Args.GaborSNRatioThreshold > ScoreToGaborRatio;
            IsTransient = IsTransient & ~IsRinging;

            Obj(Iobj).CatData.insertCol(cast(IsRinging,'double'), ...
                'Score', {'IsRinging'}, {''});
        end

        % Sub-select passing candidates only.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(~IsTransient,'double'), 'Score', ...
            {'LikelyNotTransient'}, {''});
    end
  
end