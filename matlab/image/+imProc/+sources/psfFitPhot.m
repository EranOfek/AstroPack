function Result = psfFitPhot(Obj, Args)
    %
    
    
    arguments
        Obj AstroImage
        Args.XY                      = [];  % empty - find sources, or read from catalog
        
        Args.ColX                    = Obj(1).DefNamesX;        
        Args.ColY                    = Obj(1).DefNamesY;        
        Args.CreateNewObj logical    = false;
    end
    
    Result = Obj;
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if Args.CreateNewObj && isempty(Obj(Iobj).catData)
            Result(Iobj).CatData = Obj(Iobj).CatData.copy;
        end
       
        % get PSF
        PSF = 
        
        
        if isempty(Args.XY)
            % get X/Y ccordinates from catalog
            
            XY = getXY(Obj(Iobj).CatData, 'ColX',Args.ColX, 'ColY', Args.ColY);
            if isempty(XY)
                % find sources
                [Src] = imUtil.sources.findSources(Obj(Iobj).Image,...
                                                        'BackIm',Obj(Iobj).Back,...
                                                        'VarIm',Obj(Iobj).Var,...
                                                        'Psf',PSF,...
                                                        
                XY = [Src.XPEAK, Src.YPEAK];
                                                        
                                                                                      
                error('Find sources in psfFitPhot is not yet implemented');
            end 
        else
            XY = Args.XY;
        end
        
        % subtract Background
        ImageSubBack = Obj(Iobj).Image - Obj(Iobj).Back;
        
        % get Cube of stamps around sources
        
        
        % PSF fitting
            
        
        
    
    end
    
end
