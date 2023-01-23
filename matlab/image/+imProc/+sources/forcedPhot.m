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
        Args.CooUnits                = 'deg';   % 'pix'|'deg'|'rad
        Args.CalcPSF logical         = true;
        Args.MinEdgeDist             = 10;      % pix
        
        Args.MomentMaxIter           = 0;       % 0 - no iterations
        Args.UseMomCoo logical       = false;
        Args.constructPSFArgs cell   = {};
        Args.ImageProp               = 'Image';
        Args.UseBack logical         = false;
        
        Args.AnnulusRad              = 2;
        Args.backgroundCubeArgs cell = {};
        
        Args.FitRadius  = 3;
        Args.SmallStep  = 1e-3;
        Args.MaxStep    = 0.2;
        Args.ConvThresh = 1e-4;
        Args.MaxIter    = 10;      % use 1 for no itrations
        Args.UseSourceNoise = 'off';
        Args.ZP         = 25; 
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

    Result = MatchedSources;
    
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
        
        if Args.UseMomCoo
            
        
        % generate PSF
        if Obj(Iobj).isemptyPSF
            % No PSF in AstroImage
            % generate PSF
            Obj(Iobj) = imProc.psf.constructPSF(Obj(Iobj), Args.constructPSFArgs{:});
        end
        PSF = Obj(Iobj).PSFData.Data;
    
        % stamps around sources
        [Cube] = imUtil.cut.image2cutouts(Obj(Iobj).(Args.ImageProp), X, Y);
    
        % background
        if Args.UseBack
            % use existing background/var from AstroImage
            Back = Obj(Iobj).Back;
            Std  = sqrt(Obj(Iobj).Var);
        else
            % calculate background from annulus in stamps
            [Back, Std] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:});
        end
           
        % psf photometry        
        [Result, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
                                                                'Std',Std,...
                                                                'Back',Back,...
                                                                'FitRadius',Args.FitRadius,...
                                                                'SmallStep',Args.SmallStep,...
                                                                'MaxStep',Args.MaxStep,...
                                                                'ConvThresh',Args.ConvThresh,...
                                                                'MaxIter',Args.MaxIter,...
                                                                'UseSourceNoise',Args.UseSourceNoise,...
                                                                'ZP',Args.ZP);
    
        % Store forced photometry results in MatchedSources object
        Result.Data.X1
        Result.Data.Y1
        Result.Data.X2
        Result.Data.Y2
        Result.Data.XY
        %Result.Data.MAG_APER_1
        
        
                                                            
    end
    
end
   
