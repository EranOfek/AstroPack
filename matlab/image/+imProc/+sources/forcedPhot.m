function [Result] = forcedPhot(Obj, Args)
    % Perform forced photometry on images in an AstroImage object.
    %       Given a s et of coordinates [X,Y] or [RA,Dec] perform forced
    %       photometry on images.
    %       This can be either aperture photometry, or PSF photometry, with
    %       or without position refinment.
    %       The output is written either to an AstroCatalaog object or
    %       added to the AstroCatalog in the AstroImage.
    % Input  : - An AstroImage object.
    %            The AstroImage must iunclude: an header or a populated
    %            AstroWCS; and A populated AstroCatalog.
    %          * ...,key,val,...
    %            'Coo' - A two column matrix of [X, Y] or [RA, Dec]
    %                   coordinates of positions in the image for which to
    %                   calculate forced photometry.
    %                   Note that it is possible to automatically add known
    %                   stars to the list (see: AddRefStarsDist argument).
    %                   The added stars will be appended to the bottom of
    %                   the list.
    %                   Default is [].
    %            'CooUnits' - Units of coordinates provided in the 'Coo'
    %                   argument. This can be:
    %                   'pix' - Pixel coordinates.
    %                   'deg' - J2000.0 RA/Dec in deg.
    %                   'rad' - J2000.0 RA/Dec in rad.
    %                   Default is 'deg'.
    %            'Moving' - A logical indicating if the source is moving.
    %                   If true, then the 'Coo' argument contains entry per
    %                   each AstroImage element (which we refere as epoch),
    %                   and the forced photometry is performed on each
    %                   image in a different location.
    %                   Default is false.
    %            'ColNames' - A cell array of column names to add to the
    %                   output MergedSources Data property.
    %                   Select names from the following list:
    %                   'RA',Dec' - J2000.0 RA/Dec (units specified in the
    %                           'CooOutUnits' argument).
    %                   'X', 'Y' - X/Y pixel coordinates of the final fitted
    %                           source position.
    %                   'Xstart', 'Ystart' - X/Y pixel coordinates of the
    %                           initial source position.
    %                   'FLAG_POS' - A flag indicating if the source is:
    %                           1 - within image and at least 'MinEdgeDist' pixels
    %                               from edge.
    %                           0 - outside the image.
    %                   'FLAGS' - Bit mask flags propagated from the image
    %                           mask.
    %                   'Chi2','Dof','Chi2dof' - chi^2, dof, and chi^2/dof
    %                           of the fitted PSF.
    %                   'FLUX_PSF','MAG_PSF','MAGERR_PSF' - PSF flux, mag
    %                           and mag error. Magnitudes are in luptitude,
    %                           where zero point is set by the 'ZP'
    %                           argument.
    %                   'FLUX_APER_%d' - Aperture photometry flux for all
    %                           requested apertures.
    %                   'BACK_ANNULUS','STD_ANNULUS' - back and std in annulus.
    %                   Default is:
    %                   {'RA','Dec','X','Y','Xstart','Ystart','Chi2dof','FLUX_PSF','MAG_PSF','MAGERR_PSF','BACK_ANNULUS', 'STD_ANNULUS','FLUX_APER','FLAG_POS','FLAGS'};  % 'Chi2','Dof'}
    %            'CooOutUnits' - Output J2000.0 RA/Dec units.
    %                   Default is 'deg'.
    %            'MinEdgeDist' - Number of pixels of source from image edge
    %                   in order to declare the object in/out image.
    %                   Default is 20.
    %            'AddRefStarsDist' - Angular distance in arcsec, around the
    %                   mean position of the sources in the 'Coo' argument.
    %                   If larger then 0 (and not NaN), then will search
    %                   for sources using the catsHTM.cone_search function
    %                   in the catalog specified in the 'AddCatName'
    %                   argument. These sources will be appended the the
    %                   Coo list and forced photometry will be calculated
    %                   for these sources too.
    %                   Default is 500 [arcsec].
    %            'AddCatName' - Catalog name (in catsHTM) in which to
    %                   search for additional sources.
    %                   Default is 'GAIADR3'.
    %            'PopulateWCS' - A logical indicating if to populate the
    %                   WCS from the header.
    %                   (set to false to save time is WCS is already
    %                   populated).
    %                   Default is true.
    %            'RefColNames' - A cell array of columns in the reference
    %                   catalog (specified in AddCatName). These columns,
    %                   for each source will be added to the SrcData
    %                   property in the output MatchedSources object.
    %
    %            'MomentMaxIter' - Number of iterations used in the 1st and
    %                   2nd moment estimation. If 0, then will use the
    %                   provided input X/Y coordinates without iterations.
    %                   Note that chaning this parameter may change the
    %                   position of the source by more than on pixel.
    %                   Default is 0.
    %            UseMomCoo' - A logical. If true, then will use the 1st
    %                   moment coordinates as the initial position for the
    %                   PSF photometry. If false, then will use the input
    %                   coordinates. Default is false.
    %            'AperRadius' - A vector of aperture photometry radii in
    %                   pixels. Default is [2 4 6].
    %            'Annulus' - Annulus in which to calculate sky background
    %                   and std.
    %                   Note that the PSF phot. has a different annulus.
    %                   Default is [10 12] pix.
    %
    %            'constructPSFArgs' - A cell array of additional arguments
    %                   to pass to imProc.psf.populatePSF.
    %                   Default is {}.
    %            'ImageProp' - AstroImage Image property on which to calculate the PSF
    %                   photometry and moments. Default is 'Image'.
    %            'UseBack' - A logical. If true, then will use existing
    %                   back and var in the AstroImage.
    %                   Otherwise, will recalculate them from the annulus
    %                   around each source.
    %                   Default is false.
    %
    %            'AnnulusRad' - Radius of annulus used for background
    %                   estimation in the PSF photometry.
    %                   Outer radius is taken as the stamp radius.
    %                   Default is 2 [pix].
    %            'backgroundCubeArgs' - A cell array of additional
    %                   parameters to pass to imUtil.sources.backgroundCube
    %                   Default is {}.
    %            'FlagsHalfSize' - The FLAGS columns (in ColNames) is propagating
    %                 the FLAGS from the image bit mask. This parameter
    %                 indicate the half size of the cutout in the bit mask
    %                 image around each source from which to propagate the
    %                 bit masks using the 'or' operator.
    %                 Default is 3 [pix].
    %            'ReconstructPSF' - A logical indicatig if to force the
    %                   generation of a PSF stamp even if it is already
    %                   exist in the AstroPSF in the AstroImage.
    %                   Default is false.
    %            'HalfSizePSF' - Half size of the constructed PSF (unless
    %                   PSF is provided). Default is 12 [pix].
    %            'FitRadius' - Radius around source center to fit.
    %                   This can be used in order to exclude regions
    %                   outside the stellar core.
    %                   Default is 3.
    %            'SmallStep' - Gradient step size. Default is 1e-4 (pix).
    %            'MaxStep' - Maximum step size in each iteration.
    %                   Default is 0.2.
    %            'ConvThresh' - Convergence threshold. Default is 1e-4.
    %            'MaxIter' - Max number of iterations. Default is 10.
    %            'UseSourceNoise' - A string indicating if implement
    %                   source noise in the fit. The function use the 
    %                   last estimator of the psf flux by the current best 
    %                   fit fromthe previous step. 
    %                   'all' - use from the second iteration and on.
    %                   'last' - use only in the last (additional) iteration. 
    %                   'off' - only background noise. 
    %                   Default is 'off'.
    %            'ZP' - ZP for magnitude calculations. Default is 25.
    % Output : - A MatchedSources object with the forced photometry data
    %            for each epoch and source.
    %            The 'ColNames' input arguments controls which data will be
    %            included in the MatchedSources object.
    % Author: Eran Ofek (Jan 2023)
    % Example: Rc = AI.cooImage;
    %          R=imProc.sources.forcedPhot(AI,'Coo',Rc.Center);
    
    arguments
        Obj AstroImage
        Args.Coo                     = zeros(0,2);
        Args.CooUnits                = 'deg';   % 'pix'|'deg'|'rad
        Args.Moving logical          = false;
        Args.ColNames                = {'RA','Dec','X','Y','Xstart','Ystart','Chi2dof','FLUX_PSF','MAG_PSF','MAGERR_PSF','BACK_ANNULUS', 'STD_ANNULUS','FLUX_APER','FLAG_POS','FLAGS'};  % 'Chi2','Dof'
        Args.CooOutUnits             = 'deg';
        Args.MinEdgeDist             = 20;      % pix
        Args.AddRefStarsDist         = 500;     % arcsec; 0/NaN for no addition
        Args.AddCatName              = 'GAIADR3';
        Args.PopulateWCS logical     = true;
        Args.RefColNames cell        = {'phot_g_mean_mag','phot_g_mean_flux_over_error','phot_bp_mean_mag','phot_bp_mean_flux_over_error','phot_rp_mean_mag','phot_rp_mean_flux_over_error'};
        
        Args.MomentMaxIter           = 0;       % 0 - no iterations
        Args.UseMomCoo logical       = false;
        Args.AperRadius              = [2 4 6];
        Args.Annulus                 = [10 12];

        Args.constructPSFArgs cell   = {};
        Args.ImageProp               = 'Image';
        Args.UseBack logical         = false;
        
        Args.AnnulusRad              = 2;
        Args.backgroundCubeArgs cell = {};
        
        Args.FlagsHalfSize           = 3;

        Args.ReconstructPSF logical  = false;
        Args.HalfSizePSF             = 12;
        Args.FitRadius               = 3;
        Args.SmallStep               = 1e-3;
        Args.MaxStep                 = 0.2;
        Args.ConvThresh              = 1e-4;
        Args.MaxIter                 = 10;      % use 1 for no itrations
        Args.UseSourceNoise          = 'off';
        Args.ZP                      = 25; 
    end

    RAD  = 180./pi;
    Ncol = numel(Args.ColNames);

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

    
    if ~isnan(Args.AddRefStarsDist) && Args.AddRefStarsDist>0
        if ~IsSpherical
            error('AddRefStarsDist>0 can be use only when using spherical coordinates')
        end

        CatH   = catsHTM.cone_search(Args.AddCatName, mean(Args.Coo(:,1))./RAD, mean(Args.Coo(:,2))./RAD,  Args.AddRefStarsDist, 'OutType','AstroCatalog');
        %Args.Coo = [Args.Coo; CatAdd(:,1:2).*RAD];  % deg 
        %CatAdd  = CatAdd(:,1:2).*RAD;  % deg 
        CatAdd = CatH.getLonLat('deg');
        AddColsFromRef = true;
    else
        CatAdd  = zeros(0,2);
        AddColsFromRef = false;
    end

    Nobj = numel(Obj);

    % Propagate WCS from header to AstroWCS
    if Args.PopulateWCS
        for Iobj=1:1:Nobj
            if ~Obj(1).WCS.Success
                Obj(Iobj).WCS = AstroWCS.header2wcs(Obj(Iobj).HeaderData);
            end
        end
    end

    Result    = MatchedSources;
    
    if Args.Moving
        NsrcUser = 1;
    else
        NsrcUser = size(Args.Coo,1);
    end
    Nsrc = NsrcUser + size(CatAdd,1);
    
    
    if AddColsFromRef
        NrefC = numel(Args.RefColNames);
        CatAddCol = CatH.getCol(Args.RefColNames);
        for IrefC=1:1:NrefC            
            Result.SrcData.(Args.RefColNames{IrefC}) = [nan(1, NsrcUser), CatAddCol(:,IrefC).'];
        end
    end

    
    Result.JD = Obj.julday;
    
    Naper = numel(Args.AperRadius);
    for Icol=1:1:Ncol
        % init
        if strcmp(Args.ColNames{Icol},'FLUX_APER')
            ColStr = tools.cell.cellstr_prefix((1:Naper),'FLUX_APER_');
            for Iaper=1:1:Naper
                Result.Data.(ColStr{Iaper}) = nan(Nobj, Nsrc);
            end
        else
            Result.Data.(Args.ColNames{Icol}) = nan(Nobj, Nsrc);
        end
    end
    
    if Args.Moving
        Nmove = size(Args.Coo,1);
        if Nmove~=Nobj
            error('For Moving=true the number of entries must be equal to the number of AstroImage elements');
        end
    end
    
    for Iobj=1:1:Nobj
        
       
        if IsSpherical
            ProcessImage = Obj(Iobj).WCS.Success;
        else
            ProcessImage = true;
        end
        
        if ProcessImage
        
            if Args.Moving
                if IsSpherical
                    [X,Y] = Obj(Iobj).WCS.sky2xy(Args.Coo(Iobj,1), Args.Coo(Iobj,2), 'InUnits','deg');
                else
                    X = Args.Coo(Iobj,1);
                    Y = Args.Coo(Iobj,2);
                end
            else
                if IsSpherical
                    [X,Y] = Obj(Iobj).WCS.sky2xy(Args.Coo(:,1), Args.Coo(:,2), 'InUnits','deg');
                else
                    X = Args.Coo(:,1);
                    Y = Args.Coo(:,2);
                end
            end
            % add CatAdd [deg] - convert RA/Dec to X/Y
            if AddColsFromRef
                [Xcat,Ycat] = Obj(Iobj).WCS.sky2xy(CatAdd(:,1), CatAdd(:,2), 'InUnits','deg');
                X = [X; Xcat];
                Y = [Y; Ycat];
            end

            % check if sources are in footprint
            [Ny, Nx] = Obj(Iobj).sizeImage;
            %FlagIn      = X>1 & X<Nx & Y>1 & Y<Ny;
            FlagIn  = X>Args.MinEdgeDist & X<(Nx-Args.MinEdgeDist) & Y>Args.MinEdgeDist & Y<(Ny-Args.MinEdgeDist);        

            %ResIn  = Obj(Iobj).isSkyCooInImage(MeanCoo(1), MeanCoo(2),'UNIQSEC',CooUnits);

            % force photometry on sources
            [M1,M2,Aper] = imUtil.image.moment2(Obj(Iobj).(Args.ImageProp), X, Y,...
                                    'MaxIter',Args.MomentMaxIter, 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus);
            %[M1,M2,Aper] = imUtil.image.moment2(Cube, X, Y, 'MaxIter',Args.MomentMaxIter);

            if Args.UseMomCoo
                %X = nan(Nsrc,1);
                %Y = nan(Nsrc,1);
                X = M1.X;
                Y = M1.Y;
            end


            % generate PSF
            if Obj(Iobj).isemptyPSF || Args.ReconstructPSF
                % if there is no PSF in AstroImage or PSF reconstruction is requested, generate a PSF
                Obj(Iobj) = imProc.psf.populatePSF(Obj(Iobj), 'RadiusPSF',Args.HalfSizePSF, Args.constructPSFArgs{:});
            end
            PSF = Obj(Iobj).PSFData.Data;

            HalfSizePSF = (size(Obj(Iobj).PSFData.Data,1)-1).*0.5;
            % stamps around sources
            [Cube] = imUtil.cut.image2cutouts(Obj(Iobj).(Args.ImageProp), X, Y, HalfSizePSF);


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
            [ResultPSF, CubePsfSub] = imUtil.sources.psfPhotCube(Cube, 'PSF',PSF,...
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
            
            Xpos = X(:).' + ResultPSF.DX(:).';
            Ypos = Y(:).' + ResultPSF.DY(:).';
            [RA, Dec] = Obj(Iobj).WCS.xy2sky(Xpos,Ypos,'OutUnits',Args.CooOutUnits);
            for Icol=1:1:Ncol
                switch Args.ColNames{Icol}
                    case 'RA'
                        Result.Data.RA(Iobj,:)  = RA;
                    case 'Dec'
                        Result.Data.Dec(Iobj,:) = Dec;
                    case 'X'
                        % The position is relative to X and Y which are the stamps center:
                        Result.Data.X(Iobj,:)            = Xpos;
                    case 'Y'
                        Result.Data.Y(Iobj,:)            = Ypos;
                    case 'Xstart'
                        Result.Data.Xstart(Iobj,:)       = X(:).';
                    case 'Ystart'
                        Result.Data.Ystart(Iobj,:)       = Y(:).';
                    case 'X2'
                        Result.Data.X2(Iobj,:)           = M2.X2(:).';
                    case 'Y2'
                        Result.Data.Y2(Iobj,:)           = M2.Y2(:).';
                    case 'XY'
                        Result.Data.XY(Iobj,:)           = M2.XY(:).';
                    case 'FLAG_POS'
                        Result.Data.FLAG_POS(Iobj,:)     = FlagIn;
                    case 'FLAGS'
                        FlagsXY                          = bitwise_cutouts(Obj(Iobj).MaskData, [X(FlagIn),Y(FlagIn)], 'or', 'HalfSize',Args.FlagsHalfSize);
                        Result.Data.FLAGS(Iobj,FlagIn)   = FlagsXY(:).';
                    case 'BACK_ANNULUS'
                        Result.Data.BACK_ANNULUS(Iobj,:) = Aper.AnnulusBack(:).';
                    case 'STD_ANNULUS'
                        Result.Data.STD_ANNULUS(Iobj,:)  = Aper.AnnulusStd(:).';
                    case 'FLUX_APER'
                        
                        ColStr = tools.cell.cellstr_prefix((1:Naper),'FLUX_APER_');
                        for Iaper=1:1:Naper
                            Result.Data.(ColStr{Iaper})(Iobj,:)   = Aper.AperPhot(:,Iaper).';
                        end
                    case 'FLUX_PSF'
                        Result.Data.FLUX_PSF(Iobj,:)     = ResultPSF.Flux(:).';
                    case 'FLUXERR_PSF'
                        Result.Data.FLUXERR_PSF(Iobj,:)  = 1./ResultPSF.SNm(:).';
                    case 'MAG_PSF'
                        Result.Data.MAG_PSF(Iobj,:)      = convert.luptitude(ResultPSF.Flux(:).', 10.^(0.4.*Args.ZP));
                    case 'MAGERR_PSF'
                        Result.Data.MAGERR_PSF(Iobj,:)  = 1.086./ResultPSF.SNm(:).';
                    case 'Chi2'
                        Result.Data.Chi2(Iobj,:)        = ResultPSF.Chi2(:).';
                    case 'Dof'
                        Result.Data.Dof(Iobj,:)         = ResultPSF.Dof(:).';
                    case 'Chi2dof'
                        Result.Data.Chi2dof(Iobj,:)     = (ResultPSF.Chi2(:)./ResultPSF.Dof(:)).';
                    otherwise
                        error('Unknown ColNames %s option',Args.ColNames{Icol})
                end
            end
        end
                  
    end
    
end
   
