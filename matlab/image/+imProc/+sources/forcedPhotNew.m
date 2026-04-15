function [Result] = forcedPhotNew(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Jan) 
    % Example: 

    

    arguments
        Obj
        Args.Coo                     = zeros(0,2);  % or a single AstroCatalog
        Args.CooUnits                = 'deg';   % 'pix'|'deg'|'rad
        Args.CatIsUniform            = true; % true - one catalog for all images; false - one line in the catalog per image 
        Args.ColRA                   = 'RA';
        Args.ColDec                  = 'Dec';
        Args.ColX                    = 'X';
        Args.ColY                    = 'Y';
        Args.includeDistortion       = true;

        Args.constructPSFArgs cell   = {};
        Args.ImageProp               = 'ImageData';
        Args.UseBack logical         = false;

        Args.ColCell                 = {'JD', 'XPEAK','YPEAK','BACK_IM','VAR_IM',...           
                                              'X1', 'Y1',...
                                              'X2','Y2','XY',...
                                              'FLUX_APER', 'APER_AREA', 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                              'FLUXERR_APER',...
                                              'MAG_APER', 'MAGERR_APER', 'BACKMAG_ANNULUS',...
                                              'FLUX_PSF', 'MAG_PSF', 'MAGERR_PSF', 'SN', 'CHI2DOF',...
                                              'FLUX_XYPEAK'};
        Args.ColUnits                = {};  % copy as is!
        Args.OutputType              = 'concatai'; % 'AstroCatalog';  % 'ContcatAI'|'AstroCatalog'|'AstroImage'|'table'|'MatchedSources'
        Args.CreateNewObj            = true;  % relevant only for 'AstroImage' output

        Args.Moving logical          = false;

        Args.ColNames                = {'RA','Dec','X','Y','Xstart','Ystart','Chi2dof','FLUX_PSF','FLUXERR_PSF','MAG_PSF','MAGERR_PSF','BACK_ANNULUS', 'STD_ANNULUS','FLUX_APER','FLAG_POS','FLAGS','SN'};  % 'Chi2','Dof'
        Args.ReadColFromHeader       = true;   % If column name doesn't exist try to read it from header
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

        
        
        Args.AnnulusRad              = 2;
        Args.backgroundCubeArgs cell = {};
        
        Args.FlagsHalfSize           = 3;

        Args.ReconstructPSF logical  = false;
        Args.HalfSizePSF             = 6;
        Args.FitRadius               = 3;
        Args.SmallStep               = 1e-3;
        Args.MaxStep                 = 0.2;
        Args.ConvThresh              = 1e-4;
        Args.MaxIter                 = 10;      % use 1 for no itrations
        Args.UseSourceNoise          = 'off';
        Args.ZP                      = 25; 
        Args.HeaderZP                = true; %false;   % Use ZP from image header (PH_ZP); (if nan returns to Args.ZP)
        
    end

    % empty object list returns the input untouched 
    if isempty(Args.Coo)
        Result = Obj;
        return;
    end
    
    RAD  = 180./pi;

    Nobj = numel(Obj);

    Naper = numel(Args.AperRadius);

    if strcmpi(Args.OutputType, 'concatai') || strcmpi(Args.OutputType, 'astroimage')
        if Args.CreateNewObj
            Result = Obj.copy;
        else
            Result = Obj;
        end
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

    Ncol = numel(Args.ColCell);

    if isempty(Args.ColUnits)
        [Args.ColUnits{1:Ncol}] = deal(''); 
    end

    % convert coordinates to deg:
    if IsSpherical
        Args.Coo = convert.angular(Args.CooUnits,'deg',Args.Coo);
    end

    % Propagate WCS from header to AstroWCS
    if Args.PopulateWCS
        for Iobj=1:1:Nobj
            if ~Obj(1).WCS.Success
                Obj(Iobj).WCS = AstroWCS.header2wcs(Obj(Iobj).HeaderData);
            end
        end
    end


    % Add reference stars:
    if ~isnan(Args.AddRefStarsDist) && Args.AddRefStarsDist>0
        if ~IsSpherical
            error('AddRefStarsDist>0 can be use only when using spherical coordinates')
        end

        CatH   = catsHTM.cone_search(Args.AddCatName, mean(Args.Coo(:,1))./RAD, mean(Args.Coo(:,2))./RAD,  Args.AddRefStarsDist, 'OutType','AstroCatalog');
        %Args.Coo = [Args.Coo; CatAdd(:,1:2).*RAD];  % deg 
        %CatAdd  = CatAdd(:,1:2).*RAD;  % deg 
        CatAdd = CatH.getLonLat('deg');
        Nadd   = size(CatAdd,1);
        % convert to X/Y
        
    else
        CatAdd  = zeros(0,2);
        Nadd    = 0;
    end

    

   
    if isa(Args.Coo, 'AstroCatalog')
        if IsSpherical
            % read RA/Dec
            [RA, Dec] = Args.Coo.getLonLat('ColLon',Args.ColRA, 'ColLat',Args.ColDec, 'Units','deg');
            Nsrc      = numel(RA);
        else
            % read X/Y
            [X, Y] = Args.Coo.getXY('ColX',Args.ColX, 'ColY',Args.ColY);
            Nsrc   = numel(X);
        end
    else
        % Args.Coo input is a amtrix
        if IsSpherical
            Args.Coo = convert.angular(Args.CooUnits, 'deg', Args.Coo);
            RA   = Args.Coo(:,1);
            Dec  = Args.Coo(:,2);
            Nsrc = numel(RA);
        else
            X    = Args.Coo(:,1);
            Y    = Args.Coo(:,2);
            Nsrc = numel(X);
        end
    end

    Nobj = numel(Obj);

    % Two modes:
    % Args.CatIsUniform=tru means that all sources are measured in all
    % images
    if IsSpherical && Nobj>1 && ~Args.CatIsUniform && Nsrc~=Nobj
        error('For CatIsUniform=false, number of sources should be equal to the number of images');
    end


    % properties that may have multiple columns:
    NcolOut = Ncol;
    if any(strcmp(Args.ColCell,'FLUX_CONV'))
        NcolOut = NcolOut + Ntemplate - 1;
    end
    if any(strcmp(Args.ColCell,'MAG_CONV'))
        NcolOut = NcolOut + Ntemplate - 1;
    end
    if any(strcmp(Args.ColCell,'FLUX_APER'))
        NcolOut = NcolOut + numel(Args.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'FLUXERR_APER'))
        NcolOut = NcolOut + numel(Args.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'MAG_APER'))
        NcolOut = NcolOut + numel(Args.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'MAGERR_APER'))
        NcolOut = NcolOut + numel(Args.AperRadius) - 1;
    end
    if any(strcmp(Args.ColCell,'APER_AREA'))
        NcolOut = NcolOut + numel(Args.AperRadius) - 1;
    end
    ColCellOut = cell(1,NcolOut);
    [ColUnitsOut{1:1:NcolOut}] = deal('');


    for Iobj=1:1:Nobj
        % convert RA/Dec to X/Y
        if IsSpherical
            if Args.CatIsUniform
                [XI, YI] = Obj(Iobj).WCS.sky2xy(RA, Dec, 'InUnits','deg', 'includeDistortion', Args.includeDistortion);
            else
                % single coordinate
                [XI, YI] = Obj(Iobj).WCS.sky2xy(RA(Iobj), Dec(Iobj), 'InUnits','deg', 'includeDistortion', Args.includeDistortion);
            end

        else
            if Args.CatIsUniform
                XI = X;
                YI = Y;
            else
                XI = X(Iobj);
                YI = Y(Iobj);
            end
        end % if IsSpherical

        % add reference stars
        if Nadd>0
            [Xadd, Yadd] = Obj(Iobj).WCS.sky2xy(CatAdd(:,1), CatAdd(:,2), 'InUnits','deg', 'includeDistortion', Args.includeDistortion);
            XI = [XI; Xadd];
            YI = [YI; Yadd];
        end


        % generate PSF
        if Obj(Iobj).isemptyPSF || Args.ReconstructPSF
            % if there is no PSF in AstroImage or PSF reconstruction is requested, generate a PSF
            Obj(Iobj) = imProc.psf.populatePSF(Obj(Iobj), 'RadiusPSF',Args.HalfSizePSF, Args.constructPSFArgs{:});
        end
        PSF = Obj(Iobj).PSFData.Data;

        HalfSizePSF = (size(Obj(Iobj).PSFData.Data,1)-1).*0.5;
        % stamps around sources
        [Cube] = imUtil.cut.image2cutouts(Obj(Iobj).(Args.ImageProp).Data, XI, YI, HalfSizePSF);

        % background
        if Args.UseBack
            % use existing background/var from AstroImage
            Back = Obj(Iobj).Back;
            Std  = sqrt(Obj(Iobj).Var);
        else
            % calculate background from annulus in stamps
            [Back, Std] = imUtil.sources.backgroundCube(Cube, 'AnnulusRad',Args.AnnulusRad, Args.backgroundCubeArgs{:});
        end

        
        % Moments and aperture phot:
        [M1,M2,Aper] = imUtil.image.moment2(Obj(Iobj).(Args.ImageProp).Data, XI, YI,...
                                'MaxIter',Args.MomentMaxIter, 'AperRadius',Args.AperRadius, 'Annulus',Args.Annulus);

        % if Args.UseMomCoo
        %     %X = nan(Nsrc,1);
        %     %Y = nan(Nsrc,1);
        %     X1 = M1.X;
        %     Y1 = M1.Y;
        % else
        %     X1 = X;
        %     Y1 = Y;
        % end

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
                                                        
        
        
        Xpos = XI(:) + ResultPSF.DX(:);
        Ypos = YI(:) + ResultPSF.DY(:);
        % RA, Dec of fitted position
        [RAI, DecI] = Obj(Iobj).WCS.xy2sky(Xpos,Ypos,'OutUnits',Args.CooOutUnits);

        


        [Ny, Nx] = Obj(Iobj).sizeImage;
        %FlagIn      = X>1 & X<Nx & Y>1 & Y<Ny;
        FlagIn  = Xpos>Args.MinEdgeDist & Xpos<(Nx-Args.MinEdgeDist) & Ypos>Args.MinEdgeDist & Ypos<(Ny-Args.MinEdgeDist);        


        if any(FlagIn) && ~isempty(Obj(Iobj).Mask)
            FlagsXY  = nan(Nsrc,1);
            FlagsXY(FlagIn)  = bitwise_cutouts(Obj(Iobj).MaskData, [Xpos(FlagIn),Ypos(FlagIn)], 'or', 'HalfSize',Args.FlagsHalfSize);
        else
            FlagsXY  = nan(Nsrc, 1);
        end

        NsrcAll = Nsrc + Nadd;
        Data    = nan(NsrcAll, NcolOut);
        K = 0;
        for Icol=1:1:Ncol
            K = K + 1;
            ColCellOut{K} = Args.ColCell{Icol};
            switch Args.ColCell{Icol}
                case 'X'
                    Data(:,K) = Xpos;
                case 'Y'
                    Data(:,K) = Ypos;
                case 'X1'
                    Data(:,K) = M1.X;  % NaN position corresponds to out of image...
                case 'Y1'
                    Data(:,K) = M1.Y;
                case 'XPEAK'
                    % This is X initial rather than Xpeak
                    Data(:,K) = XI;
                case 'YPEAK'
                    % This is Y initial rather than Ypeak
                    Data(:,K) = YI;
                case 'RA_IN'
                    Data(:,K) = RA;
                case 'Dec_IN'
                    Data(:,K) = Dec;
                case 'RA'
                    Data(:,K) = RAI;
                case 'Dec'
                    Data(:,K) = DecI;    
                case 'X2'
                    Data(:,K) = M2.X2;
                case 'Y2'
                    Data(:,K) = M2.Y2;
                case 'XY'
                    Data(:,K) = M2.XY;
                case 'FLUX_PSF'
                    Data(:,K) = ResultPSF.Flux;
                case 'MAG_PSF'
                    Data(:,K) = ResultPSF.Mag;
                case 'SN'
                    Data(:,K) = ResultPSF.SNm;
                case 'FLUXERR_PSF'
                    Data(:,K) = ResultPSF.Flux./ResultPSF.SNm;
                case 'MAGERR_PSF'
                    Data(:,K) = 1.086./ResultPSF.SNm;
                case 'CHI2DOF'
                    Data(:,K) = ResultPSF.Chi2./ResultPSF.Dof;

                case 'FLUX_APER_1'
                    Data(:,K) = Aper.AperPhot(:,1);
                    %Data(:,K:K+Naper-1) = Aper.AperPhot;
                    %[ColCellOut(K:K+Naper-1)] = deal(sprintf_cell('FLUX_APER',(1:1:Naper)));
                    %K = K + Naper - 1;
                case 'FLUX_APER_2'
                    Data(:,K) = Aper.AperPhot(:,2);
                case 'FLUX_APER_3'
                    Data(:,K) = Aper.AperPhot(:,3);

                case 'FLUXERR_APER_1'
                    Data(:,K) = Aper.AperPhotErr(:,1);
                    %Data(:,K:K+Naper-1) = sqrt(Aper.AperPhotErr.^2 + (Aper.AnnulusStd./sqrt(Aper.AnnulusBackArea)).^2);
                    %[ColCellOut(K:K+Naper-1)] = deal(sprintf_cell('FLUXERR_APER',(1:1:Naper)));
                    %K = K + Naper - 1;
                case 'FLUXERR_APER_2'
                    Data(:,K) = Aper.AperPhotErr(:,2);
                case 'FLUXERR_APER_3'
                    Data(:,K) = Aper.AperPhotErr(:,3);
                case 'MAG_APER_1'
                    Data(:,K) = convert.luptitude(Aper.AperPhot(:,1), 10.^(0.4.*Args.ZP));
                    %Data(:,K:K+Naper-1) = convert.luptitude(Aper.AperPhot, 10.^(0.4.*Args.ZP));
                    %[ColCellOut(K:K+Naper-1)] = deal(sprintf_cell('MAG_APER',(1:1:Naper)));
                    %K = K + Naper - 1;
                case 'MAG_APER_2'
                    Data(:,K) = convert.luptitude(Aper.AperPhot(:,2), 10.^(0.4.*Args.ZP));
                case 'MAG_APER_3'
                    Data(:,K) = convert.luptitude(Aper.AperPhot(:,3), 10.^(0.4.*Args.ZP));    
                case 'MAGERR_APER_1'
                    Data(:,K) = 1.086.*sqrt(Aper.AperPhotErr(:,1).^2 + (Aper.AnnulusStd./sqrt(Aper.AnnulusBackArea)).^2)./Aper.AperPhot(:,1);
                    %Data(:,K:K+Naper-1) = 1.086.*sqrt(Aper.AperPhotErr.^2 + (Aper.AnnulusStd./sqrt(Aper.AnnulusBackArea)).^2)./Aper.AperPhot;
                    %[ColCellOut(K:K+Naper-1)] = deal(sprintf_cell('MAGERR_APER',(1:1:Naper)));
                    %K = K + Naper - 1;
                case 'MAGERR_APER_2'
                    Data(:,K) = 1.086.*sqrt(Aper.AperPhotErr(:,2).^2 + (Aper.AnnulusStd./sqrt(Aper.AnnulusBackArea)).^2)./Aper.AperPhot(:,2);
                case 'MAGERR_APER_3'
                    Data(:,K) = 1.086.*sqrt(Aper.AperPhotErr(:,3).^2 + (Aper.AnnulusStd./sqrt(Aper.AnnulusBackArea)).^2)./Aper.AperPhot(:,3);    
                case 'APER_AREA_1'
                    Data(:,K) = Aper.AperArea(:,1);
                    %Data(:,K:K+Naper-1) = Aper.AperArea;
                    %[ColCellOut(K:K+Naper-1)] = deal(sprintf_cell('APER_AREA',(1:1:Naper)));
                    %K = K + Naper - 1;
                case 'APER_AREA_2'
                    Data(:,K) = Aper.AperArea(:,2);
                case 'APER_AREA_3'
                    Data(:,K) = Aper.AperArea(:,3);
                    
                case 'BACK_IM'
                    Data(:,K) = Obj(Iobj).getImageVal(round(Xpos), round(Ypos), 'DataProp',{'Back'});
                case 'VAR_IM'  
                    Data(:,K) = Obj(Iobj).getImageVal(round(Xpos), round(Ypos), 'DataProp',{'Var'});
                case 'BACK_ANNULUS'
                    Data(:,K) = Aper.AnnulusBack;
                case 'BACKMAG_ANNULUS'
                    Data(:,K) = convert.luptitude(Aper.AnnulusBack, 10.^(0.4.*Args.ZP));
                case 'STD_ANNULUS'
                    Data(:,K) = Aper.AnnulusStd;
                case 'FLAGS'
                    Data(:,K) = FlagsXY;
                case 'FLAG_IN'
                    Data(:,K) = FlagsIn;
                case 'MITER'
                    % do nothing
                    % for forced photometry MITER is NaN
                case 'FORCED'
                    Data(:,K) = 1;

                case 'FLUX_XYPEAK'
                    % flux at XPEAK, YPEAK
                    Data(:,K) = imUtil.image.getValPos(Obj(Iobj).(Args.ImageProp).Data - Obj(Iobj).BackData.Data, XI, YI);

                otherwise
                    % attempt to read value from header

                    if Args.ReadColFromHeader
                        Val  = Obj(Iobj).HeaderData.getVal(Args.ColCell{Icol}, 'UseDict',false);
                            
                        if isnan(Val)
                            % Any other column will be set to NaN
                            %error('Unknown ColCell name option: %s - not available also [as numeric] in header',Args.ColCell{Icol});
                        else
                            Data(:,K) = repmat(Val, NsrcAll, 1);
                        end
                    end

            end % switch Args.ColCell{Icol}
        end %for Icol=1:1:Ncol

        switch lower(Args.OutputType)
            case 'concatai'
                % concat catalog to AstroCatalog in input AstroImage
                Result(Iobj).CatData.Catalog = [Result(Iobj).CatData.Catalog; Data];

            case {'astrocatalog','astroimage'}
                %Out = AstroCatalog({Data}, 'ColNames',ColCellOut, 'ColUnits',ColUnitsOut);

                if strcmpi(Args.OutputType, 'astroimage')
                    Result(Iobj).CatData.Catalog  = Data;
                    Result(Iobj).CatData.ColNames = ColCellOut;
                    Result(Iobj).CatData.ColUnits = ColUnitsOut;
                else
                    if Iobj==1
                        Result = AstroCatalog([Nobj, 1]);
                    end
                    %Result(Iobj) = Out;
                    Result(Iobj).Catalog = Data;
                    Result(Iobj).ColNames = ColCellOut;
                    Resuly(Iobj).ColUnits = ColUnitsOut;
                end
            case 'table'
                if Iobj==1
                    Result = array2table(Data, 'VariableNames',Args.ColCell, 'VariableUnits',Args.ColUnits);
                else
                    Result = [Result; array2table(Data)];
                end
            case 'matchedsources'
                if Iobj==1
                    Result = MatchedSources;
                    for Icol=1:1:Ncol
                        Result.Data.(Args.ColCell{Icol}) = nan(Nobj, NsrcAll);
                    end
                end
                % table2struct / scalar do not do the work
                for Icol=1:1:Ncol
                    Result.Data.(Args.ColCell{Icol})(Iobj,:) = Data(:,Icol).';
                end
                
                Result.JD(Iobj) = Obj(Iobj).HeaderData.julday();
            otherwise
                error('Unknown OutputType option: %s', Args.OutputType);
        end % switch lower(Args.OutputType)
    
    end %for Iobj=1:1:Nobj
end
                        



%%%
function CellStr = sprintf_cell(Str,Ind)
    N = numel(Ind);
    CellStr = cell(1,N);
    for I=1:1:N
        CellStr{I} = sprintf('%s_%d',Str,Ind(I));
    end
end



        


