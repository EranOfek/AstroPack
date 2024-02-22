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
                'filterMag' - Bool on whether to flag transients candidates
                       based on magnitude. Deault is true.
                'MagLim' - Upper magnitude limit. If 'flagMag' is true,
                       all transients candidates below this limit are flagged. 
                       Default is 21.
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
        
        Args.flagMag logical = true;
        Args.MagLim = 21;
        
        Args.flagBadPix_Hard logical  = true;
        Args.NewMask_BadHard       = {'Interpolated', 'NaN'};
        Args.RefMask_BadHard       = {'Interpolated', 'NaN'};

        Args.flagBadPix_Medium logical = true;
        Args.BadPixThresh_Medium = 50;
        Args.NewMask_BadMedium       = {'Saturated','NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT'};
        Args.RefMask_BadMedium       = {'Saturated','NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT'};
        
        Args.flagBadPix_Soft logical  = true;
        Args.BadPixThresh_Soft = 10;
        Args.NewMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'};
        Args.RefMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'};      

        Args.flagStarMatches logical = true;

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
        if Args.flagChi2
            IsTransient = IsTransient & (Cat.getCol('D_Chi2dof') > Args.Chi2dofLimits(1)) &...
                (Cat.getCol('D_Chi2dof') < Args.Chi2dofLimits(2));
        end

        % Apply magnitude criterium.
        % TODO: Think about this a bit more. Sometimes the magnitude
        % is like ~37 which is clearly nonesense. This can happen
        % due to e.g. the background subtraction which gives a negative
        % flux within a pixel. A brightening or fading transient could 
        % generally have an unrealstic magnitude in the reference or new 
        % image, respectively, since it might just not be present and thus 
        % consistent with background. So there is a gray area between clear 
        % artifacts and a somewhat oversubtracted background at the position 
        % of a real transient. At the moment it's a simple cut on the new 
        % image value. There's a limiting and a background magnitude in an 
        % image's header, but the background one is currently not working
        % properly (2024-02-13). If we take the limiting magnitude then it
        % should be some above it since we do want to spot subthreshold
        % events.

        if Args.flagMag
            IsTransient = IsTransient & (Cat.getCol('N_Mag') < Args.MagLim);
        end
    
        % Apply bit mask critera.
        BD = BitDictionary('BitMask.Image.Default');
        BM_new = BD.bitdec2name(Cat.getCol('NewMaskVal'));
        BM_ref = BD.bitdec2name(Cat.getCol('RefMaskVal'));

        % Hard bit mask criteria.
        if Args.flagBadPix_Hard

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
        if Args.flagBadPix_Medium

            % New bit mask criteria.
            NBadMedium_New = numel(Args.NewMask_BadMedium);
            FlagBadMedium_New = false(CatSize,1);
    
            for INewBad=1:1:NBadMedium_New
                FlagBadMedium_New = FlagBadHard_New | ...
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
        if Args.flagBadPix_Soft

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

        if Args.flagStarMatches
            IsTransient = IsTransient & (Cat.getCol('StarMatches') > 0);
        end


        if (class(Obj(Iobj)) == "AstroZOGY")  && Args.flagTranslients
            IsTransient = IsTransient & ~Cat.getCol('Translient');
        end

        % Sub-select passing candidates only.
        TranCat(Iobj) = Obj(Iobj).CatData.insertCol(...
            cast(~IsTransient,'double'), 'Score', ...
            {'LikelyNotTransient'}, {''});
    end
  
end