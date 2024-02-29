function TranCat = flagNonTransients(Obj, Args)
    %{
    Flag transients candidates that are likely not real transients. 
    Input   : - An AstroDiff object in which CatData is populated.
              * ...,key,val,...
                'flagChi2' - Bool on whether to flag transients candidates
                       based on Chi2 per degrees of freedom criterium. Default is
                       true.
                'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
                       'filterChi2' is true, all transients candidates outside these
                       limits are flagged. Default is [0.5 2].
                'flagSrcNoiseDominated' - Bool on whether to flag transients 
                       candidates with source dominated noise that do not pass 
                       a StN threshold value. Default is true.
                'SrcNoise_SNRThresh' - StN threshold to apply to source 
                       noise dominated candidates. Default is 8.
                'SrcNoise_ScoreThresh' - Score threshold to apply to source 
                       noise dominated candidates. Default is 8.
                'flagSaturated' - Bool on whether to flag transients 
                       candidates that are saturated. Default is true.
                'Saturated_SNRThresh' - StN threshold to apply to
                       candidates that show saturation in new image but not
                       in reference image. Default is 5.
                'flagBadPix_Hard' - Bool on whether to flag transients
                       candidates based on hard bit mask criteria. 
                       Default is true.
                'NewMask_BadHard' - Hard bit mask criteria for bad pixels in 
                       AD.New image. Default is {'Interpolated','NaN'}.
                'RefMask_BadHard' - Hard bit mask criteria for bad pixels in 
                       AD.Ref image. Default is {'Interpolated','NaN'}.
                'flagBadPix_Medium' - Bool on whether to flag transients
                       candidates based on medium bit mask criteria. 
                       Default is true.
                'BadPixThresh_Medium' - Threshold score value below which
                       a pixel containing a medium criterium is to be
                       flagged. Default is 50.
                'NewMask_BadMedium' - Medium bit mask criteria for bad pixels in 
                       AD.New image. Default is {'Saturated', 'NearEdge', 'FlatHighStd', 
                       'Overlap','Edge','CR_DeltaHT'}.
                'RefMask_BadMedium' - Medium bit mask criteria for bad pixels in 
                       AD.Ref image. Default is {'Saturated', 'NearEdge', 'FlatHighStd', 
                       'Overlap','Edge','CR_DeltaHT'}.
                'flagBadPix_Soft' - Bool on whether to flag transients
                       candidates based on soft bit mask criteria. 
                       Default is true.
                'BadPixThresh_Soft' - Threshold score value below which
                       a pixel containing a soft criterium is to be
                       flagged. Default is 10.
                'NewMask_BadSoft' - Soft bit mask criteria for bad pixels in 
                       AD.New Image. Default is {'HighRN', 'DarkHighVal',
                       'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
                'RefMask_BadSoft' - Soft bit mask criteria for bad pixels in 
                       AD.Ref image. Default is {'HighRN', 'DarkHighVal',
                       'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
                'flagStarMatches' - Bool on whether to flag transients
                       candidates that have matching star positions.
                       Default is true.
                'flagMP' - Bool on whether to flag transients candidates
                       that have matching minor planet postions. Default is
                       ture.
                --- AstroZOGY ---
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

        Args.flagChi2 logical = true;
        Args.Chi2dofLimits = [0.5 2];
        
        Args.flagSrcNoiseDominated logical = true;
        Args.SrcNoise_SNRThresh = 8.0;
        Args.SrcNoise_ScoreThresh = 8.0;

        Args.flagSaturated logical = true;
        Args.Saturated_SNRThresh = 5.0;

        Args.flagBadPix_Hard logical  = true;
        Args.NewMask_BadHard       = {'Interpolated', 'NaN'};
        Args.RefMask_BadHard       = {'Interpolated', 'NaN'};

        Args.flagBadPix_Medium logical = true;
        Args.BadPixThresh_Medium = 50;
        Args.NewMask_BadMedium       = {'NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT', 'DarkHighVal'};
        Args.RefMask_BadMedium       = {'NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT', 'DarkHighVal'};
        
        Args.flagBadPix_Soft logical  = true;
        Args.BadPixThresh_Soft = 10;
        Args.NewMask_BadSoft       = {'HighRN', ...
            'BiasFlaring', 'Hole'};
        Args.RefMask_BadSoft       = {'HighRN', ...
            'BiasFlaring', 'Hole'};

        Args.flagStarMatches logical = true;
        Args.flagMP logical = true;

        Args.flagTranslients logical = true;
    end

    Nobj = numel(Obj);

    for Iobj=Nobj:-1:1
        Cat = Obj(Iobj).CatData;

        % Get size of catalog and initialize a bool array corresponding to
        % the catalog rows. Array is initialized as all true and will be
        % negated for rows with rejected candidates.
        CatSize = size(Cat.Catalog,1);
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
        if (Args.flagBadPix_Hard || Args.flagBadPix_Medium || ...
                Args.flagBadPix_Soft || Args.flagSrcNoiseDominated) &&...
                Cat.isColumn('NewMaskVal')
            BD = BitDictionary('BitMask.Image.Default');
            BM_new = BD.bitdec2name(Cat.getCol('NewMaskVal'));
            BM_ref = BD.bitdec2name(Cat.getCol('RefMaskVal'));
        end

        % Apply StN threshold criteria for source noise dominated
        % candidates.
        if Args.flagSrcNoiseDominated && Cat.isColumn('NewMaskVal') && ...
                Cat.isColumn('PSF_SNm')

            FlagSrcNoiseDom_New = cell2mat(cellfun(@(c) any(strcmp(c, ...
                'SrcNoiseDominated')), BM_new, 'UniformOutput', false));

            FlagSrcNoiseDom_Ref = cell2mat(cellfun(@(c) any(strcmp(c, ...
                'SrcNoiseDominated')), BM_ref, 'UniformOutput', false));
            
            FlagSrcNoiseDom = FlagSrcNoiseDom_New | FlagSrcNoiseDom_Ref;

            PassesSrcNoise = ~FlagSrcNoiseDom | (FlagSrcNoiseDom & ...
                (abs(Cat.getCol('PSF_SNm')) > Args.SrcNoise_SNRThresh) & ...
                abs(Cat.getCol('Score')) > Args.SrcNoise_ScoreThresh);

            IsTransient = IsTransient & PassesSrcNoise;

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
            % these of StN is below threshold.
            SaturatedOnlyInNew = FlagSrcNoiseDom_New & ~FlagSrcNoiseDom_Ref;

            PassesSaturated = ~SaturatedInBoth | (SaturatedOnlyInNew &...
                abs(Cat.getCol('PSF_SNm')) > Args.Saturated_SNRThresh);

            IsTransient = IsTransient & PassesSaturated;

        end       

        % Hard bit mask criteria.
        if Args.flagBadPix_Hard && Cat.isColumn('NewMaskVal')

            % New bit mask criteria.
            NBadHard_New = numel(Args.NewMask_BadHard);
            FlagBadHard_New = false(CatSize,1);
    
            for INewBad=1:1:NBadHard_New
                FlagBadHard_New = FlagBadHard_New | ...
                    cell2mat(cellfun(@(c) any(strcmp(c, Args.NewMask_BadHard(INewBad))), ...
                BM_new, 'UniformOutput', false));
            end

            % Reference bit mask criteria.
            NBadHard_Ref = numel(Args.RefMask_BadHard);
            FlagBadHard_Ref = false(CatSize,1);
    
            for IRefBad=1:1:NBadHard_Ref
                FlagBadHard_Ref = FlagBadHard_Ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.RefMask_BadHard(IRefBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            BadHardIdx = FlagBadHard_New | FlagBadHard_Ref;

            IsTransient = IsTransient & ~BadHardIdx;
            Obj(Iobj).CatData.insertCol(cast(BadHardIdx,'double'), ...
                'Score', {'BadPixel_Hard'}, {''});
        end

        % Hard bit mask criteria.
        if Args.flagBadPix_Medium && Cat.isColumn('NewMaskVal') 

            % New bit mask criteria.
            NBadMedium_New = numel(Args.NewMask_BadMedium);
            FlagBadMedium_New = false(CatSize,1);
    
            for INewBad=1:1:NBadMedium_New
                FlagBadMedium_New = FlagBadMedium_New | ...
                    cell2mat(cellfun(@(c) any(strcmp(c, Args.NewMask_BadMedium(INewBad))), ...
                BM_new, 'UniformOutput', false));
            end

            % Reference bit mask criteria.
            NBadMedium_Ref = numel(Args.RefMask_BadMedium);
            FlagBadMedium_Ref = false(CatSize,1);
    
            for IRefBad=1:1:NBadMedium_Ref
                FlagBadMedium_Ref = FlagBadMedium_Ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.RefMask_BadMedium(IRefBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            BadMediumIdx = (abs(Cat.getCol('Score')) < Args.BadPixThresh_Medium) &...
                          (FlagBadMedium_New | FlagBadMedium_Ref);

            IsTransient = IsTransient & ~BadMediumIdx;
            Obj(Iobj).CatData.insertCol(cast(BadMediumIdx,'double'), ...
                'Score', {'BadPixel_Medium'}, {''});
        end        

        % Soft bit mask criteria.
        if Args.flagBadPix_Soft && Cat.isColumn('NewMaskVal')

            % New bit mask criteria.
            NBadSoft_New = numel(Args.NewMask_BadSoft);
            FlagBadSoft_New = false(CatSize,1);
    
            for INewBad=1:1:NBadSoft_New
                FlagBadSoft_New = FlagBadSoft_New | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.NewMask_BadSoft(INewBad))), ...
                BM_new, 'UniformOutput', false));
            end

            % Reference bit mask criteria.
            NBadSoft_Ref = numel(Args.RefMask_BadSoft);
            FlagBadSoft_Ref = false(CatSize,1);
    
            for IRefBad=1:1:NBadSoft_Ref
                FlagBadSoft_Ref = FlagBadSoft_Ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.RefMask_BadSoft(IRefBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            BadSoftIdx = (abs(Cat.getCol('Score')) < Args.BadPixThresh_Soft) &...
                          (FlagBadSoft_New | FlagBadSoft_Ref);

            IsTransient = IsTransient & ~BadSoftIdx;
            Obj(Iobj).CatData.insertCol(cast(BadSoftIdx,'double'), ...
                'Score', {'BadPixel_Soft'}, {''});

        end

        if Args.flagStarMatches && Cat.isColumn('StarMatches')
            IsTransient = IsTransient & (Cat.getCol('StarMatches') < 1);
        end

        if Args.flagMP && Cat.isColumn('DistMP')
            IsTransient = IsTransient & isnan(Cat.getCol('DistMP'));
        end
        
        if Args.flagTranslients && Cat.isColumn('Translient')
            IsTransient = IsTransient & ~Cat.getCol('Translient');
        end

        % Sub-select passing candidates only.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(~IsTransient,'double'), 'Score', ...
            {'LikelyNotTransient'}, {''});
    end
  
end