function Result = psfFitPhot(Obj, Args)
    %
    % Example: AI=AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    %          AI=imProc.background.background(AI);
    %          AI=imProc.sources.findMeasureSources(AI);
    %          AI=imProc.psf.constructPSF(AI);
    %          R = imProc.sources.psfFitPhot(AI);
    
    
    arguments
        Obj AstroImage
        Args.XY                      = [];  % empty - find sources, or read from catalog
        Args.PSF                     = [];  % PSF, or function_handle
        Args.PSFArgs cell            = {};
        Args.UpdateCat logical       = true;
        
        Args.ColX                    = AstroCatalog.DefNamesX;        
        Args.ColY                    = AstroCatalog.DefNamesY;       
        Args.ColBack                 = 'BACK_IM';
        Args.ColVar                  = 'VAR_IM';  % prefered over ColStd
        Args.ColStd                  = [];
        Args.FitRadius               = 3;
        Args.HalfSize                = 8;
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
                                                        'Psf',PSF);
                                                        
                XY = [Src.XPEAK, Src.YPEAK];
                Back = Src.BACK_IM;
                Std  = Src.STD_IM;
            else
                % get also the Back and STD
                Back = getCol(Obj(Iobj).CatData, Args.ColBack);
                if isempty(Args.ColVar)
                    if isempty(Args.ColStd)
                        error('Either ColStd or ColVar must be provided');
                    end
                    Std = getCol(Obj(Iobj).CatData, Args.ColStd);
                else
                    Std = sqrt(getCol(Obj(Iobj).CatData, Args.ColVar));
                end
            end 
        else
            % XY provided by user
            XY = Args.XY;
            % get Back/Var at these positions
            Ind  = imUtil.image.sub2ind_fast(size(Obj(Iobj).Image), XY(:,1), XY(:,2));
            Back = Obj(Iobj).Back(Ind);
            Var  = Obj(Iobj).Var(Ind);
        end
        
        % subtract Background
        ImageSubBack = Obj(Iobj).Image - Obj(Iobj).Back;
        
        % get Cube of stamps around sources
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(ImageSubBack, XY(:,1), XY(:,2), Args.HalfSize, 'mexCutout',Args.mexCutout, 'Circle',Args.Circle);
        
        % PSF fitting
        
        
        warning('results are somewhat differnt than APER + larger scatter...');
        
        % Cube is Background subtracted
        [Result, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
                                                                'Std',Std,...
                                                                'Back',0,...
                                                                'FitRadius',Args.FitRadius,...
                                                                'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                                Args.psfPhotCubeArgs{:});
                                                                
        % source measured position is at:
        % RoundX + Result.DX
        
        % second iteration - need to round X/Y???
        %Image = imUtil.cut.cutouts2image(Cube, Obj(Iobj).Image, X, Y)
        
        
        % add sources to catalog
        % calculate magnitude
        if Args.UpdateCat
            Obj(Iobj).CatData.insertCol(double([Result.DX+RoundX, Result.DY+RoundY, Result.Flux, Result.Mag, Result.Chi2./Result.Dof]),...
                                    Inf,...
                                    {'X',      'Y',      'FLUX_PSF',  'MAG_PSF', 'PSF_CHI2DOF'},...
                                    {'pix',    'pix',    '',          'mag',     ''});
        end
    end
    
end
