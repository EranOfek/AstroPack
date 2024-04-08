function [ResultObj, Result] = psfFitPhot(Obj, Args)
    % Execute PSF photometry on a list of coordinates and add the results
    %   to the input AstroCatalog.
    % Input  : - An AstroImage object with an optional catalog data with
    %            initial source positions.
    %          * ...,key,val,...
    %            'XY' - A two column matrix of [X,Y] coordimates. If empty,
    %                   then will assume that the X,Y coordinates are
    %                   available in the AstroCatalog in the AStroImage
    %                   object. The column names are defined by the 'ColX', and
    %                   'ColY' arguments.
    %            'PSF' - Either a PSF stamp (matrix), a function_handle
    %                   that generates a PSF stamp, or empty.
    %                   If empty, will take the PSF from the PSFData field
    %                   in the AstroImage. Default is [].
    %            'PSFArgs' - If 'PSF' is a function handle this is a cell
    %                   array of additional arguments to pass to this function.
    %                   Default is {}.
    %            'UpdateCat' - A logical indicating if to add PSF measured quantities
    %                   to the AstroCatalog in the input AstroImage.
    %                   Default is true.
    %
    %            'ColX' - If 'XY' is empty, then these are the column names
    %                   from which to obtain the initial X coordinates for the
    %                   PSF fitting. Default is AstroCatalog.DefNamesX.
    %            'ColY' - Like 'ColX', but for the Y axis.
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColBack' - Like 'ColX', but for the background.
    %                   Default is 'BACK_IM'.
    %            'ColVar' - Like 'ColX', but for the variance.
    %                   Default is 'VAR_IM'.
    %            'ColStd' - Like 'ColX', but for the std of the background.
    %                   If given, this will override 'ColVar'.
    %                   Default is [].
    %            'FitRadius' - PSF fitting radius. Points outside this
    %                   radius will not be used. Default is 3.
    %            'HalfSize' - Default half size for the sources stamps.
    %                   Default is 8.
    %                   If PSF is provided, then this will be set to the
    %                   half PSF size.
    %            'backgroundCubeArgs' - A cell array of additional arguments to
    %                   pass to the 'backgroundCubeArgs' arguments in the
    %                   imUtil.sources.psfPhotCube function.
    %                   Default is {}.
    %
    %            'CreateNewObj' - Create new AstroCatalog object in the
    %                   AstroImage object. Default is false.
    %           
    %            'mexCutout' - mexCutout argument for
    %                   imUtil.cut.image2cutouts. Default is true.
    %            'Circle' - argument for
    %                   imUtil.cut.image2cutouts. Default is false.
    %            'psfPhotCubeArgs' - A cell array of additional arguments
    %                   to pass to imUtil.sources.psfPhotCube.
    %                   Default is {}.
    %            'ZP' - ZP for magnitude calculations. Default is 25.
    % Output : - The input AstroImage object, where the following column
    %            names were optionally added to the AStroCatalog:
    %            {'X',      'Y',      'FLUX_PSF',  'MAG_PSF', 'MAGERR_PSF', 'PSF_CHI2DOF','SN'}
    %            SN is the measurment S/N (negative oif flux is negative).
    %            MAGERR_PSF is always positive as it is defined as
    %            1.086/abs(SN).
    %          - A structure array with the PSF fitting data ONLY for the
    %            last image.
    % Author : Eran Ofek (Feb 2022)
    % Example: AI=AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    %          AI=imProc.background.background(AI);
    %          AI=imProc.sources.findMeasureSources(AI);
    %          AI=imProc.psf.populatePSF(AI);
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
        Args.ZP                      = 25;

        Args.ColSN                   = 'SN_3';  % if empty don't use
        
        Args.MaxIter                 = 20;
        
        Args.Method                  = 'old'; 
    end
    
    ResultObj = Obj;
    Result    = [];
    
    if isa(Args.PSF, 'function_handle')
        Args.PSF = Args.PSF(Args.PSFArgs{:});
    end
        
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if Args.CreateNewObj && isempty(Obj(Iobj).CatData)
            ResultObj(Iobj).CatData = Obj(Iobj).CatData.copy;
        end
       
        % get PSF
        if isempty(Args.PSF)
            % try to read PSF from AstroPSF
            PSF = ResultObj(Iobj).PSFData.getPSF;
            if isempty(PSF)
                error('No PSF found in AstroImage');
            end
        else
            PSF = Args.PSF;
        end
        
        % make sure that the PSF size is consistent with HalfSize
        Args.HalfSize = (size(PSF,1)-1).*0.5;
        
        if ~isempty(PSF)

            if isempty(Args.ColSN)
                SN = [];
            else
                SN = getCol(Obj(Iobj).CatData, Args.ColSN);
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
                    Std  = sqrt(Src.VAR_IM);
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
                Std  = sqrt(Var);
            end
            
            % subtract Background
            ImageSubBack = Obj(Iobj).Image - Obj(Iobj).Back;
            
            % get Cube of stamps around sources
            [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(ImageSubBack, XY(:,1), XY(:,2), Args.HalfSize, 'mexCutout',Args.mexCutout, 'Circle',Args.Circle);
            
            % PSF fitting
            
            % Cube is Background subtracted
            switch lower(Args.Method)                
                case 'old'
                    [Result, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
                                                                    'Std',Std,...
                                                                    'Back',0,...
                                                                    'FitRadius',Args.FitRadius,...
                                                                    'ZP',Args.ZP,...
                                                                    'SN',SN,...
                                                                    'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                                    'MaxIter',Args.MaxIter,...
                                                                     Args.psfPhotCubeArgs{:});
                    
                case 'new'
                    
                    % experimental
                    
%                     [Ny, Nx, Nim] = size(Cube);
%                     M = reshape(Cube,Nx*Ny,Nim);
%                     [U,S,V] = svd(M');
                    
                                        
                    Result                = imUtil.psf.psfPhot(Cube, 'PSF',PSF,...
                                                            'Std',Std,...
                                                            'Back',0,...
                                                            'FitRadius',Args.FitRadius,...
                                                            'ZP',Args.ZP,...
                                                            'ConvThresh', 1e-4,... 
                                                            'SN', SN,... % test (if SN is given, ConvThresh doesn't matter)
                                                            'FitRadius', Args.HalfSize,... % 3, Args.HalfSize,... %test
                                                            'RadiusRange', 0.5,... % test % 0.2, 0.5, 1.0
                                                            'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                            'MaxIter',Args.MaxIter,...
                                                            Args.psfPhotCubeArgs{:}); 

                otherwise
                    error('Incorrect method in psfFitPhot');
            end
                                                                
            
            % source measured position is at:
            % RoundX + Result.DX
            Result.RoundX = RoundX;
            Result.RoundY = RoundY;
            Result.X = Result.RoundX + Result.DX;
            Result.Y = Result.RoundY + Result.DY;
            Result.MagErr = 1.086./abs(Result.SNm);     % mag err always positive
            
            
            % second iteration - need to round X/Y???
            %Image = imUtil.cut.cutouts2image(Cube, Obj(Iobj).Image, X, Y)
            
            
            % add sources to catalog
            % calculate magnitude
            if Args.UpdateCat
                ResultObj(Iobj).CatData.insertCol(double([Result.X, Result.Y, Result.Flux, Result.Mag, Result.MagErr, Result.Chi2./Result.Dof,Result.SNm]),...
                                        Inf,...
                                        {'X',      'Y',      'FLUX_PSF',  'MAG_PSF', 'MAGERR_PSF', 'PSF_CHI2DOF','SN'},...
                                        {'pix',    'pix',    '',          'mag',     'mag',        '',''});
            end
        else % empty PSF
            % PSF is empty - skip
        end
    end
    
end
