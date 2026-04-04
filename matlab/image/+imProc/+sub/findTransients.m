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
                'include2ndMoments' - Bool whether to derive 2nd moments. 
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
                columns. This might be an eternal TODO.
                .XPEAK - Image x-coordinate of the peak position.
                .YPEAK - Image y-coordinate of the peak position.
                .RA - Sky RA-coordinate of the peak position. In deg.
                .Dec - Sky Dec-coordinate of the peak position. In deg.
                .StarJD - Start of exposure time bin. In JD.
                .MidJD - Center of exposure time bin. In JD.
                .EndJD - End of exposure time bin. In JD.
                .SN - S/N for measurment in difference image, assuming 
                       gain=1 (Poisson errors).
                .PSF_CHI2DOF - Chi2 per degrees of freedom of PSF fit to difference
                       image.
                .NewMaskVal - Array of bit mask values in new image around peak 
                       position within area defined by 'BitCutHalfSize.'
                .RefMaskVal - Array of bit mask values in reference image around
                       peak position within area defined by 'BitCutHalfSize.'
                .Score - Peak value of the threshold image.
                .N_SN - S/N for measurment in new image, assuming 
                       gain=1 (Poisson errors).
                .N_Chi2dof - Chi2 per degrees of freedom of PSF fit to new
                       image.
                .N_Flux - Flux on peak position in new image. In electrons.
                .N_Mag - Magnitude on peak position in new image.
                .R_SN - S/N for measurment in reference image, assuming 
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
        Args.include2ndMoments logical = true;
        Args.AsymThresh             = 0.2;
        Args.Aper_Annulus_min = 5;

        Args.includeGradientDir logical = true;

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

        % Construct AstroCatalog holding transients candidates

        ColNames = {'XPEAK', 'YPEAK', 'SCORE'};
        ColUnits = {'','',''};

        TranCat(Iobj) = AstroCatalog({cast([LocalMax(:,1), LocalMax(:,2), LocalMax(:,3)],'double')},...
            'ColNames', ColNames, 'ColUnits', ColUnits);

        % Skip if no candidates found
        if Nsrc < 1
            continue
        end

        if Args.include2ndMoments || Args.includePsfFit

            N_PSFSize = floor(size(AD(Iobj).New.PSFData.getPSF,2)/2);
            R_PSFSize = floor(size(AD(Iobj).Ref.PSFData.getPSF,2)/2);
            Aper_Annulus_min = min(N_PSFSize,R_PSFSize)-2.0;
            Aper_Annulus_min = max(Args.Aper_Annulus_min, Aper_Annulus_min);
            Aper_Annulus_Max = Aper_Annulus_min + 2 + ceil(min(N_PSFSize,R_PSFSize)/2);
            Aper_Annulus_Max = max(Aper_Annulus_Max, ceil(1.7*AD(Iobj).PSFData.fwhm));

            [M1, M2, Aper] = imUtil.image.moment2(AD(Iobj).Dbs, ...
                LocalMax(:,1), LocalMax(:,2),...
                'MomRadius',1.7*AD(Iobj).PSFData.fwhm, ...
                'Annulus',[Aper_Annulus_min Aper_Annulus_Max]);

            [M1N, ~, ~] = imUtil.image.moment2(AD(Iobj).New.Image, ...
                LocalMax(:,1), LocalMax(:,2),...
                'MomRadius',1.7*AD(Iobj).New.PSFData.fwhm);

            NewPSFHalfSize =  floor(size(AD(Iobj).New.PSFData.getPSF,2)/2)+1;

            % rotate the PSF so that ellipse axes agree with
            % x-y-coordinates
            NewPSF = AD(Iobj).New.PSF;
            PSFbw = imbinarize(NewPSF);
            stats = regionprops(PSFbw, 'Orientation');
            if numel(stats) > 1
                stats = stats([stats.Orientation] ~= 0);
            end
            PSFnew = imrotate(NewPSF, -stats.Orientation, 'bilinear', 'crop');
            [~, M2N, ~] = imUtil.image.moment2(PSFnew, ...
                NewPSFHalfSize, NewPSFHalfSize,...
                'MomRadius',1.7*AD(Iobj).New.PSFData.fwhm);

            % Check for assymetry and update with larger moments if
            % assymetric.

            [Rows, Cols] = size(PSFnew);
            
            % Coordinate grids
            [X, Y] = meshgrid(1:Cols, 1:Rows);
            Xc = X - NewPSFHalfSize;
            Yc = Y - NewPSFHalfSize;
            
            % Masks for halves
            LeftMask   = X <= NewPSFHalfSize;
            RightMask  = X >= NewPSFHalfSize;
            TopMask    = Y <= NewPSFHalfSize;
            BottomMask = Y >= NewPSFHalfSize;
            
            % Second moments for X tails
            M2NX2Left  = sum((Xc(LeftMask).^2)  .* PSFnew(LeftMask));
            M2NX2Right = sum((Xc(RightMask).^2) .* PSFnew(RightMask));
            
            % Second moments for Y tails
            M2NY2Top    = sum((Yc(TopMask).^2)    .* PSFnew(TopMask));
            M2NY2Bottom = sum((Yc(BottomMask).^2) .* PSFnew(BottomMask));
            
            % Compute relative asymmetry
            AsymmetryX = abs(M2NX2Left - M2NX2Right) / max(M2NX2Left, M2NX2Right);
            AsymmetryY = abs(M2NY2Top - M2NY2Bottom) / max(M2NY2Top, M2NY2Bottom);
            
            % X direction
            if AsymmetryX > Args.AsymThresh
                M2N.X2 = max(M2NX2Left, M2NX2Right);
            end
            
            % Y direction
            if AsymmetryY > Args.AsymThresh
                M2N.Y2 = max(M2NY2Top, M2NY2Bottom);
            end

            RefPSFHalfSize =  floor(size(AD(Iobj).Ref.PSFData.getPSF,2)/2)+1;

            % rotate the PSF so that ellipse axes agree with
            % x-y-coordinates
            RefPSF = AD(Iobj).Ref.PSF;
            PSFbw = imbinarize(RefPSF);
            stats = regionprops(PSFbw, 'Orientation');
            PSFref = imrotate(RefPSF, -stats.Orientation, 'bilinear', 'crop');

            [~, M2R, ~] = imUtil.image.moment2(PSFref, ...
                RefPSFHalfSize,RefPSFHalfSize,...
                'MomRadius',1.7*AD(Iobj).Ref.PSFData.fwhm);

            % Check for assymetry and update with larger moments if
            % assymetric.

            [Rows, Cols] = size(PSFref);
            
            % Coordinate grids
            [X, Y] = meshgrid(1:Cols, 1:Rows);
            Xc = X - RefPSFHalfSize;
            Yc = Y - RefPSFHalfSize;
            
            % Masks for halves
            LeftMask   = X <= RefPSFHalfSize;
            RightMask  = X >= RefPSFHalfSize;
            TopMask    = Y <= RefPSFHalfSize;
            BottomMask = Y >= RefPSFHalfSize;
            
            % Second moments for X tails
            M2RX2Left  = sum((Xc(LeftMask).^2)  .* PSFref(LeftMask));
            M2RX2Right = sum((Xc(RightMask).^2) .* PSFref(RightMask));
            
            % Second moments for Y tails
            M2RY2Top    = sum((Yc(TopMask).^2)    .* PSFref(TopMask));
            M2RY2Bottom = sum((Yc(BottomMask).^2) .* PSFref(BottomMask));
            
            % Compute relative asymmetry
            AsymmetryX = abs(M2RX2Left - M2RX2Right) / max(M2RX2Left, M2RX2Right);
            AsymmetryY = abs(M2RY2Top - M2RY2Bottom) / max(M2RY2Top, M2RY2Bottom);
            
            % X direction
            if AsymmetryX > Args.AsymThresh
                M2R.X2 = max(M2RX2Left, M2RX2Right);
            end
            
            % Y direction
            if AsymmetryY > Args.AsymThresh
                M2R.Y2 = max(M2RY2Top, M2RY2Bottom);
            end
          
        end

        if Args.includeGradientDir
            PSFSize = floor(size(AD(Iobj).New.PSFData.getPSF,2)/2);
            % Make a larger cut so we won't have to pad it later.
            CutSize = PSFSize + 1;
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(...
                AD(Iobj).New.Image, LocalMax(:,1), LocalMax(:,2), CutSize);

            FullSizeX = 2*PSFSize + 1;
            FullSizeY = 2*PSFSize + 1;
            CenterX = PSFSize + 1;
            CenterY = PSFSize + 1;
            
            StartX = 2;
            EndX = StartX + FullSizeX - 1;
            StartY = 2;
            EndY = StartY + FullSizeY - 1;

            % Compute expected radial direction
            [Xmesh, Ymesh] = meshgrid(1:FullSizeX, 1:FullSizeY);
            % Flip the X-axis so the convetion agrees with imgradient
            ExpectedAngle = atan2(-(Ymesh - CenterY), -(Xmesh - CenterX));
            ExpectedAngleDeg = rad2deg(ExpectedAngle);

            GDIRCVAR = zeros(Nsrc,1);
            GDIRERROR = zeros(Nsrc,1);
            
            BackThreshold = AD(Iobj).BackN + sqrt(AD(Iobj).VarN);

            CubeList = squeeze(mat2cell( ...
                Cube, FullSizeX+2, FullSizeY+2, ones(1, Nsrc)));

            for ITran=1:Nsrc

                ICube = CubeList{ITran};
                
                MaskBack = (ICube > BackThreshold);
                if sum(MaskBack(:)) < 1
                    MaskBack = ones(size(ICube));
                end
                MaskBack = MaskBack(StartY:EndY, StartX:EndX);
                
                % Assuming ICube is small and fixed-size
                
                % Convolution with Sobel kernels
                % This is the fastest way to do it.
                Gx = ...
                    -1 * ICube(1:end-2, 1:end-2) +  1 * ICube(1:end-2, 3:end) + ...
                    -2 * ICube(2:end-1, 1:end-2) +  2 * ICube(2:end-1, 3:end) + ...
                    -1 * ICube(3:end,   1:end-2) +  1 * ICube(3:end,   3:end);
                
                Gy = ...
                    -1 * ICube(1:end-2, 1:end-2) + -2 * ICube(1:end-2, 2:end-1) + ...
                    -1 * ICube(1:end-2, 3:end)   +  1 * ICube(3:end,   1:end-2) + ...
                     2 * ICube(3:end,   2:end-1) +  1 * ICube(3:end,   3:end);
                
                Gdir = atan2d(Gy, Gx);

                Gdir_rad = deg2rad(Gdir(MaskBack));
                GDIRCVAR(ITran,1) = 1 - abs(mean(exp(1i * Gdir_rad),"all"));

                AngleDiff = abs(Gdir - ExpectedAngleDeg);
                % Correct for wrapping issues (e.g., -179° vs 179° should be close)
                AngleDiff = min(AngleDiff, 360 - AngleDiff);
                % Compute the mean alignment error
                GDIRERROR(ITran,1) = mean(AngleDiff(MaskBack),"all");
            end

            TranCat(Iobj) = TranCat(Iobj).insertCol(cast(GDIRCVAR,'double'), ...
                'SCORE', {'GDIRCVAR'}, {''});
            TranCat(Iobj) = TranCat(Iobj).insertCol(cast(GDIRERROR,'double'), ...
                'SCORE', {'GDIRERROR'}, {''});
        end

        if Args.includePsfFit
            
            ZeroBack = zeros(Nsrc,1);

            % PSF fit all candidates in the D image
            PSFSize = floor(size(AD(Iobj).PSFData.getPSF,2)/2);
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Dbs, M1.RoundX, M1.RoundY, PSFSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            XYind = sub2ind(size(AD(Iobj).Dbs), M1.RoundY, M1.RoundX);
            VarD = AD(Iobj).Var(XYind);
            StdD = sqrt(VarD);
            [ResultD, ~] = imUtil.sources.psfPhotCube(Cube, ...
                'PSF', AD(Iobj).PSFData.getPSF, 'Back', ZeroBack, 'Std', StdD,...
                'ZP', AD(Iobj).ZpD);

            % PSF fit all candidates in the New image
            CutHalfSize =  floor(size(AD(Iobj).New.PSFData.getPSF,2)/2);
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Nbs, M1N.RoundX, M1N.RoundY, CutHalfSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            [ResultN, ~] = imUtil.sources.psfPhotCube(Cube,...
                'PSF', AD(Iobj).New.PSFData.getPSF, 'Back', 0, 'Std', AD(Iobj).SigmaN, ...
                'ZP', AD(Iobj).ZpN, 'MaxIter', 2, 'SmallStep', 0.05, 'MaxStep', 0.1);
            
            % PSF fit all candidates in the Ref image
            CutHalfSize = floor(size(AD(Iobj).Ref.PSFData.getPSF,2)/2);
            [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Rbs, M1N.RoundX, M1N.RoundY, CutHalfSize);
            % Change the sign of negative sources
            Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
            [ResultR, ~] = imUtil.sources.psfPhotCube(Cube, ...
                'PSF', AD(Iobj).Ref.PSFData.getPSF, 'Back', 0, 'Std', AD(Iobj).SigmaR,...
                'ZP', AD(Iobj).ZpR, 'MaxIter', 2, 'SmallStep', 0.05, 'MaxStep', 0.1);

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

            BD_IM = BitDictionary('BitMask.Image.Default');

            MAGPSF_New = AD(Iobj).New.CatData.getCol('MAG_PSF');
            CHI2DOF_New = AD(Iobj).New.CatData.getCol('PSF_CHI2DOF');
    
            NearEdge_New = BD_IM.findBit( ...
                AD(Iobj).New.CatData.getCol('FLAGS'), 'NearEdge');

            MAGPSF_New = MAGPSF_New(~NearEdge_New);
            CHI2DOF_New = CHI2DOF_New(~NearEdge_New);

            MinMag_New = floor(min(MAGPSF_New));
            if AD(Iobj).New.HeaderData.isKeyExist('LIMMAG')
                MaxMag_New = ceil(AD(Iobj).New.HeaderData.getVal('LIMMAG'));
            else
                MaxMag_New = ceil(max(MAGPSF_New));
            end
            binEdges_New = MinMag_New:1.0:MaxMag_New;
            binIndices_New = discretize(MAGPSF_New, binEdges_New);

            ValidMag_New = ~isnan(binIndices_New);
            BinIndicesValid_New = binIndices_New(ValidMag_New);
            ValuesIndices_New = CHI2DOF_New(ValidMag_New);

            MedianValues_New = accumarray(BinIndicesValid_New(:), ...
                ValuesIndices_New(:), [], @median, NaN);

            % Initialize result array
            MedianAtMag_New = NaN(size(ResultN.Mag));

            % Loop through each and assign corresponding median
            for i = 1:numel(ResultN.Mag)
                targetMag = ResultN.Mag(i);
                binIndex = find(targetMag >= binEdges_New(1:end-1) & targetMag < binEdges_New(2:end));
                if ~isempty(binIndex) && (binIndex <= numel(MedianValues_New))
                    MedianAtMag_New(i) = MedianValues_New(binIndex);
                end
            end

            MAGPSF_Ref = AD(Iobj).Ref.CatData.getCol('MAG_PSF');
            CHI2DOF_Ref = AD(Iobj).Ref.CatData.getCol('PSF_CHI2DOF');
          
            NearEdge_Ref = BD_IM.findBit(...
                AD(Iobj).Ref.CatData.getCol('FLAGS'),'NearEdge');

            MAGPSF_Ref = MAGPSF_Ref(~NearEdge_Ref);
            CHI2DOF_Ref = CHI2DOF_Ref(~NearEdge_Ref);

            MinMag_Ref = floor(min(MAGPSF_Ref));

            if AD(Iobj).Ref.HeaderData.isKeyExist('LIMMAG')
                MaxMag_Ref = ceil(AD(Iobj).Ref.HeaderData.getVal('LIMMAG'));
            else
                MaxMag_Ref = ceil(max(MAGPSF_Ref));
            end
            binEdges_Ref = MinMag_Ref:1.0:MaxMag_Ref;
            binIndices_Ref = discretize(MAGPSF_Ref, binEdges_Ref);
            
            ValidMag_Ref = ~isnan(binIndices_Ref);
            BinIndicesValid_Ref = binIndices_Ref(ValidMag_Ref);
            ValuesIndices_Ref = CHI2DOF_Ref(ValidMag_Ref);

            MedianValues_Ref = accumarray(BinIndicesValid_Ref(:), ...
                ValuesIndices_Ref(:), [], @median, NaN);

            % Initialize result array
            MedianAtMag_Ref = NaN(size(ResultR.Mag));

            % Loop through each and assign corresponding median
            for i = 1:numel(ResultR.Mag)
                targetMag = ResultR.Mag(i);
                binIndex = find(targetMag >= binEdges_Ref(1:end-1) & targetMag < binEdges_Ref(2:end));
                if ~isempty(binIndex) && (binIndex <= numel(MedianValues_Ref))
                    MedianAtMag_Ref(i) = MedianValues_Ref(binIndex);
                end
            end

            % Insert results into catalog.
            Data = cell2mat({ResultD.SNm, CHI2DOF, ...
                ResultD.Flux, D_FLUXERR_PSF, ResultD.Mag, D_MAGERR_PSF,...
                ResultN.SNm, ResultN.Chi2./ResultN.Dof, ...
                ResultN.Flux, N_FLUXERR_PSF, ResultN.Mag, N_MAGERR_PSF,...
                ResultR.SNm, ResultR.Chi2./ResultR.Dof, ...
                ResultR.Flux, R_FLUXERR_PSF, ResultR.Mag, R_MAGERR_PSF});
            Data = cast(Data, 'double');
            TranCat(Iobj) = TranCat(Iobj).insertCol( Data, 'SCORE',...
                {'SN', 'PSF_CHI2DOF', ...
                'FLUX_PSF', 'FLUXERR_PSF', 'MAG_PSF', 'MAGERR_PSF',...
                'N_SN', 'N_PSF_CHI2DOF', ...
                'N_FLUX_PSF', 'N_FLUXERR_PSF', 'N_MAG_PSF', 'N_MAGERR_PSF',...
                'R_SN', 'R_PSF_CHI2DOF', ...
                'R_FLUX_PSF', 'R_FLUXERR_PSF', 'R_MAG_PSF', 'R_MAGERR_PSF'},...
                {'','', ...
                'e','e','mag','mag', ...
                '','', ...
                'e','e','mag','mag', ...
                '','', ...
                'e','e','mag','mag'}...
                );

            TranCat(Iobj) = TranCat(Iobj).insertCol(...
                cast(MedianAtMag_New, 'double'), 'N_PSF_CHI2DOF', ...
                {'N_PSF_CHI2DOF_MED'}, {''});
            TranCat(Iobj) = TranCat(Iobj).insertCol(...
                cast(MedianAtMag_Ref, 'double'), 'R_PSF_CHI2DOF', ...
                {'R_PSF_CHI2DOF_MED'}, {''});
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

        if Args.include2ndMoments
            % Get moments

            PeakDist = sqrt((M1N.X-M1.X).^2+(M1N.Y-M1.Y).^2);

            M2NX2 = M2N.X2*ones(Nsrc,1);
            M2NY2 = M2N.Y2*ones(Nsrc,1);
            M2NXY = M2N.XY*ones(Nsrc,1);

            M2RX2 = M2R.X2*ones(Nsrc,1);
            M2RY2 = M2R.Y2*ones(Nsrc,1);
            M2RXY = M2R.XY*ones(Nsrc,1);
            
            Data = cell2mat({cast(M1.X,'double'), cast(M1.Y,'double'), ...
                cast(M2.X2,'double'), cast(M2.Y2,'double'), cast(M2.XY,'double'),...
                cast(M2NX2,'double'), cast(M2NY2,'double'), cast(M2NXY,'double'),...
                cast(M2RX2,'double'), cast(M2RY2,'double'), cast(M2RXY,'double'),...
                cast(PeakDist,'double')});
            TranCat(Iobj) = TranCat(Iobj).insertCol( Data, 'SCORE',...
                {'X1', 'Y1', 'X2', 'Y2', 'XY',...
                'N_X2','N_Y2','N_XY',...
                'R_X2','R_Y2','R_XY',...
                'PEAK_DIST'}, ...
                {'','','','','','','','','','','',''});
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
            [N_JD, ~] = AD(Iobj).New.julday;
    
            ColSize = size(LocalMax(:,3));
            N_JD = N_JD*ones(ColSize);

            % Get observation time from ref image
            [R_JD, ~] = AD(Iobj).Ref.julday;
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