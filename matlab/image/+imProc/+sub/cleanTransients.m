function TranCat = cleanTransients(Obj, Args)
    %{
    Clean transients candidates based on image quality and PSF fit
    criteria. All filtered candidates are removed from AD.CatData.
    Input   : - An AstroDiff object in which CatData is populated.
             * ...,key,val,...
        'filterChi2'    - Bool on whether to remove transients candidates
            based on Chi2 per degrees of freedom criterium. Default is
            true.
        'Chi2dofLimits' - Limits on Chi2 per degrees of freedom. If
            'filterChi2' is true, all transients candidates outside these
            limits are removed. Default is [0.5 2].
        'filterMag' - Bool on whether to remove transients candidates
            based on magnitude. Deault is true.
        'MagLim'    - Upper magnitude limit. If 'filterMag' is true,
            all transients candidates below this limit are removed. Default
            is 21.
        'filterBadPix_Hard' - Bool on whether to remove transients
            candidates based on hard bit mask criteria. Default is true.
        'NewMask_BadHard'   - Hard bit mask criteria for bad pixels in 
            AD.New image. Default is {'Saturated', 'NearEdge', 'FlatHighStd', 
            'Overlap','Edge','CR_DeltaHT', 'Interpolated','NaN'}.
        'RefMask_BadHard'   - Hard bit mask criteria for bad pixels in 
            AD.Ref image. Default is {'Saturated', 'NearEdge', 'FlatHighStd', 
            'Overlap','Edge','CR_DeltaHT', 'Interpolated','NaN'}.
        'filterBadPix_Soft' - Bool on whether to remove transients
            candidates based on soft bit mask criteria. Default is true.
        'NewMask_BadSoft'   - Soft bit mask criteria for bad pixels in 
            AD.New Image. Default is {'HighRN', 'DarkHighVal',
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
        'RefMask_BadSoft'   - Soft bit mask criteria for bad pixels in 
            AD.Ref image. Default is {'HighRN', 'DarkHighVal',
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'}.
    Output  : An AstroCatalog with filtered candidates removed from it.
    Author  : Ruslan Konno (Jan 2024)
    Example : imProc.sub.cleanTransients(AD)
    %}

    arguments
        Obj AstroDiff

        Args.filterChi2 logical = true;
        Args.Chi2dofLimits = [0.5 2];
        
        Args.filterMag logical = true;
        Args.MagLim = 21;
        
        Args.filterBadPix_Hard logical  = true;
        Args.NewMask_BadHard       = {'Saturated','NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT', 'Interpolated', 'NaN'};
        Args.RefMask_BadHard       = {'Saturated','NearEdge','FlatHighStd',...
            'Overlap','Edge','CR_DeltaHT', 'Interpolated', 'NaN'};
        
        Args.filterBadPix_Soft logical  = true;
        Args.NewMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'};
        Args.RefMask_BadSoft       = {'HighRN', 'DarkHighVal', ...
            'BiasFlaring', 'Hole', 'SrcNoiseDominated'};        
    end

    numObj = numel(Obj);

    for Iobj=numObj:-1:1
        Cat = Obj(Iobj).CatData.Catalog;

        % Get size of catalog and initialize a bool array corresponding to
        % the catalog rows. Array is initialized as all true and will be
        % negated for rows with rejected candidates.
        sizeCat = size(Cat,1);
        keep = true(sizeCat,1);

        % Apply Chi2 per degrees of freedom criterium.
        if Args.filterChi2
            keep = keep & (Cat.Chi2_D > Args.Chi2dofLimits(1)) &...
                (Cat.Chi2_D < Args.Chi2dofLimits(2));
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

        if Args.filterMag
            keep = keep & (Cat.N_Mag < Args.MagLim);
        end
    
        % Apply bit mask critera.
        BD = BitDictionary('BitMask.Image.Default');
        BM_new = BD.bitdec2name(Cat.NewMaskVal);
        BM_ref = BD.bitdec2name(Cat.RefMaskVal);

        % Hard bit mask criteria.
        if Args.filterBadPix_Hard

            % New bit mask criteria.
            num_BadHard_new = numel(Args.NewMask_BadHard);
            Flag_BadHard_new = false(sizeCat,1);
    
            for INewBad=1:1:num_BadHard_new
                Flag_BadHard_new = Flag_BadHard_new | ...
                    cell2mat(cellfun(@(c) any(strcmp(c, Args.NewMask_BadHard(INewBad))), ...
                BM_new, 'UniformOutput', false));
            end

            % Reference bit mask criteria.
            num_BadHard_ref = numel(Args.RefMask_BadHard);
            Flag_BadHard_ref = false(sizeCat,1);
    
            for IRefBad=1:1:num_BadHard_ref
                Flag_BadHard_ref = Flag_BadHard_ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.RefMask_BadHard(IRefBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            keep = keep & ~Flag_BadHard_new & ~Flag_BadHard_ref;
        end

        % Soft bit mask criteria.
        if Args.filterBadPix_Soft

            % New bit mask criteria.
            num_BadSoft_new = numel(Args.NewMask_BadSoft);
            Flag_BadSoft_new = false(sizeCat,1);
    
            for INewBad=1:1:num_BadSoft_new
                Flag_BadSoft_new = Flag_BadSoft_new | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.NewMask_BadSoft(INewBad))), ...
                BM_new, 'UniformOutput', false));
            end

            % Reference bit mask criteria.
            num_BadSoft_ref = numel(Args.RefMask_BadSoft);
            Flag_BadSoft_ref = false(sizeCat,1);
    
            for IRefBad=1:1:num_BadSoft_ref
                Flag_BadSoft_ref = Flag_BadSoft_ref | ...
                    cell2mat(cellfun(@(c)any(strcmp(c, Args.RefMask_BadSoft(IRefBad))), ...
                BM_ref, 'UniformOutput', false));
            end

            keep = keep & ~Flag_BadSoft_new & ~Flag_BadSoft_ref;
        end  

        % Sub-select passing candidates only.
        TranCat(Iobj) = Obj(Iobj).CatData.selectRows(keep);
    end
   
end