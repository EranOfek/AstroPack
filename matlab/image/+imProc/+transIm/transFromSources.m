function transFromSources(Obj, Ref, Args)
    % Find transformation between images by patern match of sources in the image and ref image.
    %   Currently this function only deals with images that are roughly shifted in
    %   X and Y only.

    arguments
        Obj
        Ref                               = [];
        
        Args.RangeX                       = [-1000 1000];
        Args.RangeY                       = [-1000 1000];
        Args.StepX                        = 3;
        Args.StepY                        = 3;
        Args.MaxMethod                    = 'thresh_fracmax';
        Args.Threshold                    = 5;
        Args.FracOfMax                    = 0.5;
        Args.find_shift_pairsArgs cell    = {};
        
        Args.IndOfRef                     = 1;
        
        
        
        Args.popEmptyCat logical          = true;
        Args.findMeasureSourcesArgs cell  = {};
    end
    
    Nobj = numel(Obj);
    if isempty(Ref)
        Ref = Obj(Args.IndOfRef);
    end
    
    % make sure all the catalogs are populated
    if Args.popEmptyCat
        if isa(Obj, 'AstroImage') 
            % sizeCatalog is a method of both AstroImage and AstroCatalog
            Nsrc = sizeCatalg(Obj);
            Flag0 = Nsrc==0;
            Obj(Flag0) = imProc.sources.findMeasureSources(Obj(Flag0), Args.findMeasureSourcesArgs{:});
        end
        if isa(Ref, 'AstroImage')
            Nsrc = sizeCatalg(Ref);
            Ref = imProc.sources.findMeasureSources(Ref, Args.findMeasureSourcesArgs{:});
        end
    end
    
    % get Ref catalog
    if isa(Ref, 'AstroImage')
        CatRef = Ref.CatData;
    elseif isa(Ref, 'AstroCatalog')
        CatRef = Ref;
    else
        error('Unknown Obj type option');
    end

    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            CatObj = Obj(Iobj).CatData;
        elseif isa(Obj, 'AstroCatalog')
            CatObj = Obj(Iobj);
        else
            error('Unknown Obj type option');
        end
        
        CatXY = getXY(CatObj);
        RefXY = getXY(CatRef);
        
        % pattern match CatObj and CatRef:
        ResPat = imUtil.patternMatch.find_shift_pairs(CatXY, RefXY, 'RangeX',Args.RangeX,...
                                                                 'RangeY',Args.RangeY,...
                                                                 'StepX',Args.StepX,...
                                                                 'StepY',Args.StepY,...
                                                                 'MaxMethod',Args.MaxMethod,...
                                                                 'Threshold',Args.Threshold,...
                                                                 'FracOfMax',Args.FracOfMax,...
                                                                 find_shift_pairsArgs{:});
        
        % Fit transformation to initial shift guess
        ...
        % Store transformation
        ...
    end
end