function Result = forcedPhot(Obj, Args)
    % Perform forced photometry on images in an AstroImage object.
    %       Given a s et of coordinates [X,Y] or [RA,Dec] perform forced
    %       photometry on images.
    %       This can be either aperture photometry, or PSF photometry, with
    %       or without position refinment.
    %       The output is written either to an AstroCatalaog object or
    %       added to the AstroCatalog in the AstroImage.
    % Input  : - An AstroImage object.
    %          * ...,key,val,...
    %
    
    arguments
        Obj AstroImage
        Args.Coo
        Args.CooUnits               = 'deg';   % 'pix'|'deg'|'rad
        Args.CalcPSF logical        = true;
        Args.MinEdgeDist            = 10;      % pix
        
        Args.MomentMaxIter          = 0;       % 0 - no iterations
        Args.constructPSFArgs cell  = {};
    end

    switch lower(Args.CooUnits)
        case 'pix'
            IsSpherical = false;
        case 'deg'
            IsSpherical = true;
        case 'rad'
            IsSpherical = true;
            
        otherwise
            error('Unknown CooUnits option');
    end

    if IsSpherical
        Args.Coo = convert.angular(Args.CooUnits,'deg',Args.Coo);
    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if IsSpherical
            [X,Y] = Obj(Iobj).WCS.sky2xy(Arg.Coo(:,1), Args.Coo(:,2), 'InUnits','deg');
        end

        % check if sources are in footprint
        [Ny, Nx] = Obj(Iobj).sizeImage;
        FlagIn      = X>1 & X<Nx & Y>1 & Y<Ny;
        FlagInGood  = X>Args.MinEdgeDist & X<(Nx-Args.MinEdgeDist) & Y>Args.MinEdgeDist & Y<(Ny-Args.MinEdgeDist);
        
        % force photometry on sources
        [M1,M2,Aper] = imUtil.image.moment2(Image,X,Y, 'MaxIter',Args.MomentMaxIter);
        
        % generate PSF
        if Obj(Iobj).isemptyPSF
            % No PSF in AstroImage
            % generate PSF
            Obj(Iobj) = imProc.psf.constructPSF(Obj(Iobj), Args.constructPSFArgs{:});
        end
        PSF = Obj(Iobj).PSFData.Data;
        
        % prepare a Cube of background subtracted stamps around sources
        % Cube = 
        % imUtil.sources.backgroundCube
        
        % psf photometry        
        [Result, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
                                                                'Std',Std,...
                                                                'Back',0,...
                                                                'FitRadius',Args.FitRadius,...
                                                                'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                                Args.psfPhotCubeArgs{:});
                  
                                                            
       % imProc.sources.psfFitPhot(Obj(Iobj), 'XY',[X,Y], 'UpdateCat',false, 
    end
end
   
