function TranCat=findTransients(AD, Args)
    %{
    Search for positive and negative transients by selecting local
    minima and maxima with an absolute value above a set detection 
    threshold. Results are saved as an AstroCatalog under
    AD.CatData.
    Input   : - An AstroDiff object in which the threshold image is
                populated.
              * ...,key,val,...
                'Threshold' - Threshold to be applied to the threshold image. Search
                       for local maxima only above this threshold. Default is 5.
                'findLocalMaxArgs' - Args passed into imUtil.sources.findLocalMax()
                       when looking for local maxima. Default is {}.
                'includePsfFit' - Bool on whether to perform PSF photometry 
                       on images AD, AD.New, and AD.Ref. Include results in catalog.
                       Default is true.
                'HalfSizePSF' - Half size of area on transients positions in 
                       image. Actual size will be 1+2*HalfSizePSF. Used to cut out 
                       an image area to perform PSF photometry on.
                       Default is 7.
                'psfPhotCubeArgs' - Args passed into imUtil.sources.psfPhotCube when
                       performing PSF photometry on AD, AD.New, and AD.Ref cut outs.
                       Default is {}.
                'include2ndMoment' - Bool whether to derive 2nd moments. 
                       Default is true. 
                'includeAperturePhot' - Bool whether to add aperture photometry results. 
                       Default is true.
                'includeBitMaskVal' - Bool on whether to retrieve bit mask
                       values from AD.New and AD.Ref, and add to catalog.
                       Default is true.
                'BitCutHalfSize' - Half size of area on transients positions in 
                       image bit masks. Actual size will be 1+2*BitCutHalfSize. Used
                       to retrieve bit mask values around transient positions.
                       Default is 3.
                'includeSkyCoord' - Bool on whether to retrieve sky
                       coordinates from AD.New and add to catalog. Default
                       is true.
                'includeObsTime' - Bool on whether to retrieve observation
                       times from AD.New and add to catalog. Default is true.
    Output  : - An AstroCatalog containing the found transients candidates
                with the following columns;
                TODO: Update this once sure about the final catalog
                columns.
                .XPEAK - Image x-coordinate of the peak position.
                .YPEAK - Image y-coordinate of the peak position.
                .RA - Sky RA-coordinate of the peak position. In deg.
                .Dec - Sky Dec-coordinate of the peak position. In deg.
                .StarJD - Start of exposure time bin. In JD.
                .MidJD - Center of exposure time bin. In JD.
                .EndJD - End of exposure time bin. In JD.
                .PSF_SNm - S/N for measurment in difference image, assuming 
                       gain=1 (Poisson errors).
                .CHI2DOF - Chi2 per degrees of freedom of PSF fit to difference
                       image.
                .NewMaskVal - Array of bit mask values in new image around peak 
                       position within area defined by 'BitCutHalfSize.'
                .RefMaskVal - Array of bit mask values in reference image around
                       peak position within area defined by 'BitCutHalfSize.'
                .Score - Peak value of the threshold image.
                .N_SNm - S/N for measurment in new image, assuming 
                       gain=1 (Poisson errors).
                .N_Chi2dof - Chi2 per degrees of freedom of PSF fit to new
                       image.
                .N_Flux - Flux on peak position in new image. In electrons.
                .N_Mag - Magnitude on peak position in new image.
                .R_SNm - S/N for measurment in reference image, assuming 
                       gain=1 (Poisson errors).
                .R_Chi2dof - Chi2 per degrees of freedom of PSF fit to
                       reference image.
                .R_Flux - Flux on peak position in reference image. In
                       electrons.
                .R_Mag - Magnitude on peak position in reference image.
    Author  : Ruslan Konno (Jan 2024)
    Example : AD = AstroZOGY('LAST*.fits','LAST*1*.fits');
              AD.subtractionD;
              AD.subtractionS;
              imProc.sub.findTransients(AD);
    %}
    arguments
        AD AstroDiff

        Args.Threshold                  = 5;
        Args.findLocalMaxArgs cell      = {};

        Args.includePsfFit logical      = true;
        Args.HalfSizePSF                = 7;
        Args.psfPhotCubeArgs cell       = {};

        Args.includeAperturePhot logical = true;
        Args.include2ndMoment logical = true;

        Args.includeBitMaskVal logical  = true;
        Args.BitCutHalfSize             = 3;

        Args.includeSkyCoord logical    = true;
        Args.includeObsTime logical     = true;

        Args.includeGaborSN logical = true;
    
        Args.includePVdist logical = true;
    end

    Nobj = numel(AD);

    % reverse order to initiate Result array with proper size on first 
    % iteration
    for Iobj=Nobj:-1:1
        % for each image

        % find positive and negative sources in S
        [PosLocalMax] = imUtil.sources.findLocalMax(AD(Iobj).ThresholdImage, ...
            'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});
        [NegLocalMax] = imUtil.sources.findLocalMax(-AD(Iobj).ThresholdImage, ...
            'Variance',1, 'Threshold',Args.Threshold, Args.findLocalMaxArgs{:});

        % Output *LocalMax contains: [X,Y,SN,ImageIndex,LinaerIndexIn2D]
        % Merge pos/neg lists and add sign to the SN column
        NegLocalMax(:,3) = -NegLocalMax(:,3);
        LocalMax = [PosLocalMax; NegLocalMax];
        Nsrc     = size(LocalMax,1);

        [M1, M2, Aper] = imUtil.image.moment2(AD(Iobj).New.Image, ...
            LocalMax(:,1), LocalMax(:,2));

        % Construct AstroCatalog holding transints candidates

        ColNames = {'XPEAK', 'YPEAK', 'SCORE'};
        ColUnits = {'','',''};
        
        TranCat(Iobj) = AstroCatalog({cast([LocalMax(:,1), LocalMax(:,2), LocalMax(:,3)],'double')},...
            'ColNames', ColNames, 'ColUnits', ColUnits);

        % Skip if no candidates found
        if Nsrc < 1
            continue
        end

        if Args.includePsfFit

            % PSF fit all candidates in the D image
            PSFSize = floor(size(AD(Iobj).PSFData.getPSF,2)/2);
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Image, M1.X, M1.Y, PSFSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            %Psf = imUtil.psf.full2stamp(AD(Iobj).PSFData.getPSF, 'StampHalfSize',Args.HalfSizePSF.*ones(1,2), 'IsCorner',false);
            [ResultD, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', AD(Iobj).PSFData.getPSF, Args.psfPhotCubeArgs{:});
        
            % PSF fit all candidates in the New image
            CutHalfSize = (size(AD(Iobj).New.PSFData.getPSF,1)-1).*0.5;
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).New.Image, M1.X, M1.Y, CutHalfSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            [ResultN, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', AD(Iobj).New.PSFData.getPSF, Args.psfPhotCubeArgs{:});
            
            % PSF fit all candidates in the Ref image
            CutHalfSize = (size(AD(Iobj).Ref.PSFData.getPSF,1)-1).*0.5;
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Ref.Image, M1.X, M1.Y, CutHalfSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            [ResultR, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', AD(Iobj).Ref.PSFData.getPSF, Args.psfPhotCubeArgs{:});
        
            % Get chi2 per degrees of freedom of the PSF fit on the difference
            % image.
            CHI2DOF = ResultD.Chi2./ResultD.Dof;

            % Estimate flux and magnitude erro
            D_FLUXERR_PSF = sqrt(abs(ResultD.Flux));
            D_MAGERR_PSF = 1.086./D_FLUXERR_PSF;

            N_FLUXERR_PSF = sqrt(abs(ResultN.Flux));
            N_MAGERR_PSF = 1.086./N_FLUXERR_PSF;

            R_FLUXERR_PSF = sqrt(abs(ResultR.Flux));
            R_MAGERR_PSF = 1.086./R_FLUXERR_PSF;

            % Insert results into catalog.
            Data = cell2mat({ResultD.SNm, CHI2DOF, ...
                ResultD.Flux, D_FLUXERR_PSF, ResultD.Mag, D_MAGERR_PSF,...
                ResultN.SNm, ResultN.Chi2./ResultN.Dof, ...
                ResultN.Flux, N_FLUXERR_PSF, ResultN.Mag, N_MAGERR_PSF,...
                ResultR.SNm, ResultR.Chi2./ResultR.Dof, ...
                ResultR.Flux, R_FLUXERR_PSF, ResultR.Mag, R_MAGERR_PSF});
            Data = cast(Data, 'double');
            TranCat(Iobj) = TranCat(Iobj).insertCol( Data, 'SCORE',...
                {'PSF_SNm', 'CHI2DOF', ...
                'FLUX_PSF', 'FLUXERR_PSF', 'MAG_PSF', 'MAGERR_PSF',...
                'N_PSF_SNm', 'N_CHI2DOF', ...
                'N_FLUX_PSF', 'N_FLUXERR_PSF', 'N_MAG_PSF', 'N_MAGERR_PSF',...
                'R_PSF_SNm', 'R_CHI2DOF', ...
                'R_FLUX_PSF', 'R_FLUXERR_PSF', 'R_MAG_PSF', 'R_MAGERR_PSF'},...
                {'','', ...
                'e','e','mag','mag', ...
                '','', ...
                'e','e','mag','mag', ...
                '','', ...
                'e','e','mag','mag'}...
                );

        end

        if Args.includeAperturePhot
            % Get aperture photometry
            Data = cell2mat({cast(Aper.AperPhot,'double'), ...
                cast(Aper.AperPhotErr,'double'), cast(Aper.AnnulusBack,'double'), ...
                cast(Aper.AnnulusStd,'double')});
            TranCat(Iobj) = TranCat(Iobj).insertCol( Data, 'SCORE',...
                {'FLUX_APER_1', 'FLUX_APER_2', 'FLUX_APER_3',...
                'FLUXERR_APER_1', 'FLUXERR_APER_2', 'FLUXERR_APER_3', ...
                'BACK_ANNULUS', 'STD_ANNULUS'}, ...
                {'e','e','e','e','e','e','e','e'}...
                );
        end

        if Args.include2ndMoment
            % Get moments

            PeakDist = sqrt((LocalMax(:,1)-M1.X).^2+(LocalMax(:,2)-M1.Y).^2);

            Data = cell2mat({cast(M1.X,'double'), cast(M1.Y,'double'), ...
                cast(M2.X2,'double'), cast(M2.Y2,'double'), ...
                cast(PeakDist,'double')});
            TranCat(Iobj) = TranCat(Iobj).insertCol( Data, 'SCORE',...
                {'X1', 'Y1', 'X2', 'Y2', 'PEAK_DIST'}, {'','','','',''});
        end

        if Args.includeBitMaskVal
            % get Mask values within cutouts around pos/neg transients candidates
            FLAGS = AD(Iobj).MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
                'or', 'HalfSize',Args.BitCutHalfSize);
            N_FLAGS = AD(Iobj).New.MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
                'or', 'HalfSize',Args.BitCutHalfSize);
            R_FLAGS = AD(Iobj).Ref.MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
                'or', 'HalfSize',Args.BitCutHalfSize);

            FLAGS = cast(FLAGS, 'double');
            N_FLAGS = cast(N_FLAGS, 'double');
            R_FLAGS = cast(R_FLAGS, 'double');

            % Insert results into catalog.
            TranCat(Iobj) = TranCat(Iobj).insertCol( ...
                cell2mat({FLAGS, N_FLAGS, R_FLAGS}), 'SCORE',...
                {'FLAGS', 'N_FLAGS', 'R_FLAGS'}, {'','',''});           
        end

        if Args.includeSkyCoord
            % Get RA/Dec coordinates in radians
            [RA, Dec] = xy2sky(AD(Iobj).New.WCS, LocalMax(:,1), LocalMax(:,2));
            RA = cast(RA,'double');
            Dec = cast(Dec,'double');

            % Insert results into catalog.
            TranCat(Iobj) = TranCat(Iobj).insertCol( ...
                cell2mat({RA, Dec}), 'SCORE',...
                {'RA', 'Dec'}, {'deg','deg'});

        end

        if Args.includeObsTime

            % Get observation times from new image
            [N_JD, ~] = AD(Iobj).New.julday();
    
            ColSize = size(LocalMax(:,3));
            N_JD = N_JD*ones(ColSize);

            % Get observation time from ref image
            [R_JD, ~] = AD(Iobj).Ref.julday();
            R_JD = R_JD*ones(ColSize);

            % Insert results into catalog.
            TranCat(Iobj) = TranCat(Iobj).insertCol( ...
                cell2mat({N_JD, R_JD}), 'SCORE',...
                {'JD', 'R_JD'}, ...
                {'JD','JD'});
            
        end

        if Args.includeGaborSN && ~isempty(AD(Iobj).GaborSN)
            XY = TranCat.getXY('ColX','XPEAK','ColY','YPEAK');
            Size = size(AD(Iobj).GaborSN);
            GaborSN = AD(Iobj).GaborSN(sub2ind(Size,XY(:,2),XY(:,1)));
            TranCat(Iobj) = TranCat(Iobj).insertCol(cast(GaborSN,'double'), ...
                'SCORE', {'SN_GABOR'}, {''});
        end

        if Args.includePVdist

            Score =  TranCat.getCol('SCORE');
            XY =  TranCat.getXY('ColX','XPEAK','ColY','YPEAK');

            Ntran = size(TranCat.Table,1);

            if min(sign(Score)) == max(sign(Score))
                MinDists = NaN(Ntran,1);
            else
                for Itran = Ntran:-1:1
                    Dists = sqrt((XY(Itran,1) - XY(:,1)).^2+(XY(Itran,2) - XY(:,2)).^2);
                    SignFlip = ~(sign(Score(Itran)) == sign(Score));
                    MinDists(Itran,1) = min(Dists(SignFlip));   
                end
            end

            TranCat(Iobj) = TranCat(Iobj).insertCol(cast(MinDists,'double'), ...
                'SCORE', {'PV_DIST'}, {''});
        end

        if Args.includeSkyCoord
            TranCat(Iobj).sortrows('Dec');
        end
  
    end

end