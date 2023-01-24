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
    %            'Coo' - 
    %            'CooUnits' -
    %            'CalcPSF' - 
    %            'ColNames' -
    %            'MinEdgeDist' -
    %            'AddRefStarsDist' - 
    %            'AddCatName' - 
    %            
    % Example: Rc = AI.cooImage;
    % %        R=imProc.sources.forcedPhot(AI,'Coo',Rc.Center);
    
    arguments
        Obj AstroImage
        Args.Coo
        Args.CooUnits                = 'deg';   % 'pix'|'deg'|'rad
        Args.CalcPSF logical         = true;
        Args.ColNames                = {'X','Y','Xstart','Ystart','Chi2dof','FLUX_PSF','MAG_PSF','MAGERR_PSF','BACK_ANNULUS', 'STD_ANNULUS','FLUX_APER'};  % 'Chi2','Dof'
        Args.MinEdgeDist             = 10;      % pix
        Args.AddRefStarsDist         = 500;     % arcsec; 0/NaN for no addition
        Args.AddCatName              = 'GAIADR3';

        Args.MomentMaxIter           = 0;       % 0 - no iterations
        Args.UseMomCoo logical       = false;
        Args.constructPSFArgs cell   = {};
        Args.ImageProp               = 'Image';
        Args.UseBack logical         = false;
        
        Args.AnnulusRad              = 2;
        Args.backgroundCubeArgs cell = {};
        
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

        CatAdd   = catsHTM.cone_search(Args.AddCatName, mean(Args.Coo(:,1))./RAD, mean(Args.Coo(:,2))./RAD,  Args.AddRefStarsDist);
        Args.Coo = [Args.Coo; CatAdd(:,1:2).*RAD];  % deg 
    end


    Result    = MatchedSources;
    Nsrc      = size(Args.Coo,1);
    Result.JD = Obj.julday;
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if IsSpherical
            [X,Y] = Obj(Iobj).WCS.sky2xy(Args.Coo(:,1), Args.Coo(:,2), 'InUnits','deg');
        end

        % check if sources are in footprint
        [Ny, Nx] = Obj(Iobj).sizeImage;
        FlagIn      = X>1 & X<Nx & Y>1 & Y<Ny;
        FlagInGood  = X>Args.MinEdgeDist & X<(Nx-Args.MinEdgeDist) & Y>Args.MinEdgeDist & Y<(Ny-Args.MinEdgeDist);
        
        

        % force photometry on sources
        [M1,M2,Aper] = imUtil.image.moment2(Obj(Iobj).(Args.ImageProp), X, Y, 'MaxIter',Args.MomentMaxIter);
        %[M1,M2,Aper] = imUtil.image.moment2(Cube, X, Y, 'MaxIter',Args.MomentMaxIter);
        
        if Args.UseMomCoo
            X = M1.X;
            Y = M1.Y;
        end
            
        
        % generate PSF
        if Obj(Iobj).isemptyPSF || Args.ReconstructPSF
            % No PSF in AstroImage
            % generate PSF
            Obj(Iobj) = imProc.psf.constructPSF(Obj(Iobj), 'HalfSize',Args.HalfSizePSF, Args.constructPSFArgs{:});
       
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
        if Iobj==1
            % init
            Result(Iobj).Data.X       = nan(Nobj, Nsrc);
            Result(Iobj).Data.Y       = nan(Nobj, Nsrc);
            Result(Iobj).Data.Xstart  = nan(Nobj, Nsrc);
            Result(Iobj).Data.Ystart  = nan(Nobj, Nsrc);


            Naper = numel(Aper.AperRadius);
        end


        for Icol=1:1:Ncol
            switch Args.ColNames{Icol}
                case 'X'
                    % The position is relative to X and Y which are the stamps center:
                    Result(Iobj).Data.X(Iobj,:)   = X(:).' + ResultPSF.DX(:).';
                case 'Y'
                    Result(Iobj).Data.Y(Iobj,:)   = Y(:).' + ResultPSF.DY(:).';
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
   
