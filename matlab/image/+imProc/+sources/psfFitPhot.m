function Result = psfFitPhot(Obj, Args)
    %
    
    
    arguments
        Obj AstroImage
        Args.XY                      = [];  % empty - find sources, or read from catalog
        Args.PSF                     = [];  % PSF, or function_handle
        Args.PSFArgs cell            = {};
        
        Args.ColX                    = Obj(1).DefNamesX;        
        Args.ColY                    = Obj(1).DefNamesY;       
        Args.ColBack                 = 'BACK_IM';
        Args.ColVar                  = 'STD_IM';
        Args.ColStd                  = [];
        Args.FitRadius               = 3;
        Args.backgroundCubeArgs cell = {};
        
        Args.CreateNewObj logical    = false;
        Args.mexCutout logical       = true;
        Args.Circle logical          = false;
        Args.psfPhotCubeArgs cell    = {};
    end
    
    Result = Obj;
    
    if isa(Args.PSF, 'function_handle')
        Args.PSF = Args.PSF(Args.PSFArgs{:});
    end
        
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if Args.CreateNewObj && isempty(Obj(Iobj).catData)
            Result(Iobj).CatData = Obj(Iobj).CatData.copy;
        end
       
        % get PSF
        if isempty(Args.PSF)
            % try to read PSF from AstroPSF
            PSF = Result(Iobj).PSFData.getPSF;
        else
            PSF = Args.PSF;
        end
        
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
                Back = Src.BACK_IM;
                Std  = Src.STD_IM;
            else
                % get also the Back and STD
                Back = getCol(Obj(Iobj).CatData, Args.ColBack);
                if isempty(Args.ColVar)
                    Std = sqrt(getCol(Obj(Iobj).CatData, Args.ColVar));
                else
                    Std = getCol(Obj(Iobj).CatData, Args.ColStd);
                end
            end 
        else
            % XY provided by user
            XY = Args.XY;
            % get Back/Var at these positions
            
        end
        
        % subtract Background
        ImageSubBack = Obj(Iobj).Image - Obj(Iobj).Back;
        
        % get Cube of stamps around sources
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, X, Y, MaxRadius, 'mexCutout',Args.mexCutout, 'Circle',Args.Circle);
        
        % PSF fitting
        [Result, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
                                                                'Std',Std,...
                                                                'Back',Back,...
                                                                'FitRadius',Args.FitRadius,...
                                                                'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                                Args.psfPhotCubeArgs{:});
                                                                
        % second iteration
        Image = imUtil.cut.cutouts2image(Cube, Image, X, Y)
        
    
    end
    
end
