function TranCat=findTransients(AD, Args)
    %{
    Search for positive and negative transients by selecting local
    minima and maxima with an absolute value above a set detection 
    threshold. Results are saved as an AstroCatalog under
    AD.CatData.
    Input   : - An AstroDiff object in which the threshold image is
                populated.
             * ...,key,val,...
            'HalfSizePSF' - Half size of area on transients positions in 
                image. Actual size will be 1+2*HalfSizePSF. Used to cut out 
                an image area to perform PSF photometry on.
                Default is 7.
            'Threshold' - Threshold to be applied to the threshold image. Search
            for local maxima only above this threshold. Default is 5.
            'findLocalMaxArgs' - Args passed into imUtil.sources.findLocalMax()
                when looking for local maxima.
                Default is {}.
            'BitCutHalfSize' - Half size of area on transients positions in 
                image bit masks. Actual size will be 1+2*BitCutHalfSize. Used
                to retrieve bit mask values around transient positions.
                Default is 3.
            'psfPhotCubeArgs' - Args passed into imUtil.sources.psfPhotCube when
                performing PSF photometry on AD, AD.New, and AD.Ref cut outs.
                Default is {}.
    Output  : - An AstroCatalog containing the found transients candidates
                with the following columns;
            .XPEAK - Image x-coordinate of the peak position.
            .YPEAK - Image y-coordinate of the peak position.
            .RA - Sky RA-coordinate of the peak position. In deg.
            .Dec - Sky Dec-coordinate of the peak position. In deg.
            .StarJD - Start of exposure time bin. In JD.
            .MidJD - Center of exposure time bin. In JD.
            .EndJD - End of exposure time bin. In JD.
            .PSF_SNm - S/N for measurment in difference image, assuming 
                gain=1 (Poisson errors).
            .Chi2_D - Chi2 per degrees of freedom of PSF fit to difference
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
    Example : imProc.sub.findTransients(AD)
    %}
    arguments
        AD AstroDiff

        Args.Threshold             = 5;

        Args.HalfSizePSF           = 7;
        Args.HalfSizeTS            = 5;
        Args.findLocalMaxArgs cell = {};
        Args.BitCutHalfSize        = 3;
        Args.psfPhotCubeArgs cell  = {};
    end

        TranCat.ColNames = {'XPEAK', 'YPEAK', 'RA', 'Dec',  ...
            'StartJD', 'MidJD', 'EndJD',...
            'PSF_SNm', 'Chi2_D', 'NewMaskVal', 'RefMaskVal',...
            'Score', ...
            'N_SNm', 'N_Chi2dof', 'N_Flux', 'N_Mag', ...
            'R_SNm', 'R_Chi2dof', 'R_Flux', 'R_Mag',...
            };    
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

        % get Mask values within cutouts around pos/neg transients candidates
        
        NewMaskVal = AD(Iobj).New.MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
            'or', 'HalfSize',Args.BitCutHalfSize);
        RefMaskVal = AD(Iobj).Ref.MaskData.bitwise_cutouts(LocalMax(:,1:2), ...
            'or', 'HalfSize',Args.BitCutHalfSize);

        % PSF fit all candidates in the D image
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Image, LocalMax(:,1), LocalMax(:,2), Args.HalfSizePSF);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        Psf = imUtil.psf.full2stamp(AD(Iobj).PSFData.getPSF, 'StampHalfSize',Args.HalfSizePSF.*ones(1,2));
        [ResultD, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', Psf, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the New image
        CutHalfSize = (size(AD(Iobj).New.PSFData.getPSF,1)-1).*0.5;
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).New.Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultN, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', AD(Iobj).New.PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % PSF fit all candidates in the Ref image
        CutHalfSize = (size(AD(Iobj).Ref.PSFData.getPSF,1)-1).*0.5;
        [Cube, ~, ~, ~, ~] = imUtil.cut.image2cutouts(AD(Iobj).Ref.Image, LocalMax(:,1), LocalMax(:,2), CutHalfSize);
        % Change the sign of negative sources
        Cube = Cube.*reshape(-sign(LocalMax(:,3)), [1 1 Nsrc]);
        [ResultR, ~] = imUtil.sources.psfPhotCube(Cube, 'PSF', AD(Iobj).Ref.PSFData.getPSF, Args.psfPhotCubeArgs{:});
    
        % Get chi2 per degrees of freedom of the PSF fit on the difference
        % image.
        Chi2dof = ResultD.Chi2./ResultD.Dof;

        % Get RA/Dec coordinates in radians
        [RA, Dec] = xy2sky(AD(Iobj).New.WCS, LocalMax(:,1), LocalMax(:,2));
        RA = cast(RA,'double');
        Dec = cast(Dec,'double');

        % Get observation times from new image
        [MidJD, ExpTime] = AD(Iobj).New.julday();

        col_size = size(RA);
        ExpTime_d = ExpTime/3600/24;
        StartJD = MidJD-ExpTime_d/2;
        EndJD = MidJD+ExpTime_d/2;

        MidJD = MidJD*ones(col_size);
        StartJD = StartJD*ones(col_size);
        EndJD = EndJD*ones(col_size);

        % Construct AstroCatalog holding transints candidates
        TranCat = AstroCatalog;
       
        TranCat.ColNames = {'XPEAK', 'YPEAK', 'RA', 'Dec',  ...
            'StartJD', 'MidJD', 'EndJD',...
            'PSF_SNm', 'Chi2_D', 'NewMaskVal', 'RefMaskVal',...
            'Score', ...
            'N_SNm', 'N_Chi2dof', 'N_Flux', 'N_Mag', ...
            'R_SNm', 'R_Chi2dof', 'R_Flux', 'R_Mag',...
            };

        TranCat.Catalog  = table(LocalMax(:,1), LocalMax(:,2), RA, Dec, ...
            StartJD, MidJD, EndJD,...
            ResultD.SNm, Chi2dof,  NewMaskVal,  RefMaskVal, ...
            LocalMax(:,3),...
            ResultN.SNm, ResultN.Chi2./ResultN.Dof, ResultN.Flux, ResultN.Mag, ...
            ResultR.SNm, ResultR.Chi2./ResultR.Dof, ResultR.Flux, ResultR.Mag... 
            );

        TranCat.ColUnits = {'','','deg','deg',...
            'JD','JD','JD',...
            '','','','',...
            '',...
            '','','e','mag',...
            '','','e','mag',...
            };


        TranCat.Catalog.Properties.VariableNames = TranCat.ColNames;
        TranCat.Catalog.Properties.VariableUnits = TranCat.ColUnits;
        
    end

end