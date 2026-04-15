function [M1, M2, Aper, Cube] = moments(Image, Args)
    % Measure weighted first/second moments and aperture photometry in image cutouts.
    % Package: imUtil.sources
    % Description: Given either a 2-D image plus source positions, or a 3-D cube
    %              of stamps, measure source centroids using an iterative weighted
    %              first-moment algorithm, estimate second moments, and optionally
    %              perform aperture photometry. Background is estimated from an
    %              annulus around each stamp and subtracted before the measurements.
    % Input  : - Either:
    %            1) A 2-D image.
    %               In this case, Args.X and Args.Y must be provided, and cutouts
    %               of size (2*HalfSize+1)-by-(2*HalfSize+1) are extracted around
    %               each requested position.
    %            2) A 3-D cube of image stamps of size Ny-by-Nx-by-Nsrc.
    %               In this case, each slice is treated as an individual source stamp.
    %          * ...,key,val,... or named arguments:
    %            'SN' - Vector of S/N values, one per image slice. Used in the
    %                   convergence criterion of the iterative centroiding.
    %                   Convergence is reached when the positional shift between
    %                   iterations is smaller than SigmaWidth./SN.
    %                   Default is [].
    %            'X' - X coordinates in the full image, one per source.
    %                  Required when Image is 2-D.
    %                  Optional when Image is a cube; if not provided, the output
    %                  coordinates M1.X1 will be empty, and StampX1 should be used.
    %                  Default is [].
    %            'Y' - Y coordinates in the full image, one per source.
    %                  Required when Image is 2-D.
    %                  Optional when Image is a cube; if not provided, the output
    %                  coordinates M1.Y1 will be empty, and StampY1 should be used.
    %                  Default is [].
    %            'StampX' - Initial or nominal X position of the source within each
    %                   stamp, in stamp coordinates. If empty and Image is a cube,
    %                   the stamp center is assumed: (Nx+1)/2.
    %                   Default is [].
    %            'StampY' - Initial or nominal Y position of the source within each
    %                   stamp, in stamp coordinates. If empty and Image is a cube,
    %                   the stamp center is assumed: (Ny+1)/2.
    %                   Default is [].
    %            'mexCutout' - Use MEX-based cutout extraction when Image is 2-D.
    %                   Default is true.
    %            'HalfSize' - Half-size of each extracted stamp. Relevant only when
    %                   Image is 2-D. The stamp size is 2*HalfSize+1 pixels.
    %                   Default is 12.
    %            'AperPhotType' - Aperture photometry algorithm.
    %                   Options are:
    %                   'interp' - Interpolated circular aperture photometry.
    %                   'simple' - Simple circular aperture photometry.
    %                   Default is 'interp'.
    %            'AperPhotRadius' - Vector of aperture radii in pixels.
    %                   Default is [2 4 6].
    %            'AnnulusRadii' - Two-element vector [Rin Rout] specifying the
    %                   background annulus radii in pixels.
    %                   Default is [10 12].
    %            'MaxIter' - Maximum number of iterations for the weighted centroid
    %                   calculation.
    %                   Default is 8.
    %            'SigmaWidth' - Width of the Gaussian weight function used in the
    %                   centroid iterations, in pixels.
    %                   If scalar, the same width is used in all iterations.
    %                   If a two-element vector, the first element is used in the
    %                   first iteration and the second in subsequent iterations.
    %                   Default is [3 1.5].
    %            'TruncateSigma' - Truncation factor K for the Gaussian weights.
    %                   Pixels outside +/-K*SigmaWidth may be ignored in the
    %                   weighted-moment calculation.
    %                   Default is 3.
    %            'MaxStepSize' - Two-element vector controlling the maximum centroid
    %                   step size between iterations, in pixels.
    %                   The first element is used for the first iteration and the
    %                   second for all subsequent iterations.
    %                   Default is [0.1 0.1].
    %            'MaxRadiusM2' - Maximum radius, in pixels, used in the second
    %                   moment calculation.
    %                   Default is 6.
    % Output : - M1 structure containing first-moment and centroid information:
    %            .StampX1    - Measured weighted first moment in X, in stamp
    %                          coordinates.
    %            .StampY1    - Measured weighted first moment in Y, in stamp
    %                          coordinates.
    %            .X          - Measured weighted first moment in X, in full-image
    %                          coordinates. If the input is a cube and X was not
    %                          provided, then this field is empty.
    %            .Y          - Measured weighted first moment in Y, in full-image
    %                          coordinates. If the input is a cube and Y was not
    %                          provided, then this field is empty.
    %            .StampInitX - Initial or nominal X coordinate in each stamp.
    %            .StampInitY - Initial or nominal Y coordinate in each stamp.
    %            .Iter       - The number of iterations till convergence.
    %                          NaN for not coverged.
    %          - M2 structure containing second-moment measurements:
    %            .X2         - Second central moment in X.
    %            .Y2         - Second central moment in Y.
    %            .XY         - Mixed second moment.
    %            Returned only if NARGOUT > 1.
    %          - Aper structure containing background and aperture-photometry results:
    %            .BackVec    - Estimated background level per stamp.
    %            .StdVec     - Estimated background standard deviation per stamp.
    %            .NpixBack   - Number of pixels used for the background estimate.
    %            .AperPhot   - Aperture-summed fluxes.
    %            .AperArea   - Effective aperture areas.
    %            .AperRadius - Used aperture radius.
    %            Returned only if NARGOUT > 2.
    %          - Cube of extracted or input image stamps. If Image is a 2-D image,
    %            this is the extracted cutout cube. If Image is already a cube, it
    %            is returned unchanged.
    %
    % Remarks: - For 2-D image input, X and Y are required.
    %          - For cube input, stamp dimensions must be odd.
    %          - Background subtraction is performed using annulus_median prior to
    %            centroid, second-moment, and aperture-photometry measurements.
    %          - If full-image coordinates are needed for cube input, provide X and Y.
    % See also: imUtil.cut.image2cutouts, ...
    %           imUtil.sources.mex.annulus_median, ...
    %           imUtil.sources.mex.moment1_cube, ...
    %           imUtil.sources.mex.mom2_cube, ...
    %           imUtil.sources.mex.aper_phot_cube_interp, ...
    %           imUtil.sources.mex.aper_phot_cube_simple
    % Author : Eran Ofek (2026 Apr)
    % Example:
    %          % Measure moments in a full image around a list of positions:
    %          [M1, M2, Aper, Cube] = imUtil.sources.moments(Image, ...
    %              X=X, Y=Y, HalfSize=12, AperPhotRadius=[2 4 6]);
    %          % Measure moments in a pre-extracted cube of stamps:
    %          [M1, M2] = imUtil.sources.moments(Cube, ...
    %              StampX=[], StampY=[], MaxIter=8);

    arguments
        Image
        Args.SN                = [];
        Args.X                 = []; 
        Args.Y                 = [];
        Args.StampX             = [];
        Args.StampY             = [];
        Args.mexCutout         = true;


        Args.HalfSize          = 12;
        Args.AperPhotMethod    = 'interp';  % 'simple'|'interp'
        Args.AperRadius        = [2 4 6];
        Args.Annulus           = [10 12];
        Args.MaxIter           = 8;
        Args.SigmaWidth        = [3 1.5];
        Args.TruncateSigma     = 2.5;
        Args.MaxStepSize       = [0.1 0.1]; % [first iter, all the rest]
        Args.MaxRadiusM2       = 6;
    end

    if Args.HalfSize<max(max(Args.Annulus), max(Args.AperRadius))
        Args.HalfSize = max(max(Args.Annulus), max(Args.AperRadius));
        %error('HalfSize of PSF must be larger/equal to max Annulus radii/ aper phot radii');
    end


    if ndims(Image)==2
        % Image is 2D
        
        % construct cube of images
        if isempty(Args.X) || isempty(Args.Y)
            error('When Image is 2D image, X and Y must be provided');
        end
        % the stamp size is always HalfSize.*2+1 (so odd number)
        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Image, Args.X, Args.Y, Args.HalfSize, 'mexCutout',Args.mexCutout);
        [Ny, Nx, Nslice] = size(Cube);

        X = RoundX;
        Y = RoundY;

        % X/Y
        StampX = (Args.X-RoundX) + Args.HalfSize + 1;
        StampY = (Args.Y-RoundY) + Args.HalfSize + 1;

    else
        % Image is already a Cube
        Cube = Image;
        [Ny, Nx, Nslice] = size(Cube);
        if mod(Ny,2)==0 || mod(Nx,2)==0
            error('Stamp size must be odd');
        end

       
        % if StampX/StampY are given than these are the PSF position in the
        % cube, otherwise assume the source is in the stamp center
        X = Args.X;
        Y = Args.Y;

        % assume stamp is odd size

        if isempty(Args.StampX)
            StampX = repmat((Nx + 1)./2, Nslice, 1);
        else
            StampX = Args.StampX;
        end
        if isempty(Args.StampY)
            StampY = repmat((Ny + 1)./2, Nslice, 1);
        else
            StampY = Args.StampY;
        end

    end

    [CubeBS, Aper.AnnulusBack, Aper.AnnulusStd, Aper.AnnulusArea] = imUtil.sources.mex.annulus_median(Cube, Args.Annulus, 0);
    B0 = zeros(Nslice, 1);

    % M1.X1S/Y1S are the 1st moment estimated centers relative to the stamp
    % center (not stamp corner - controloed by the meaning of the 7th input
    % argument).
    [M1.StampX1, M1.StampY1, M1.Iter] = imUtil.sources.mex.moment1_cube(CubeBS, B0, Args.SN, Args.MaxIter, Args.SigmaWidth, Args.TruncateSigma, true, Args.MaxStepSize(2), Args.MaxStepSize(1));

    % return X, Y positions to stamp center position
    % Note that if the user didn't define Args.X, Args.Y then these are
    % empty
    % X/Y are rounded coordinates corresponds to the stamp centeral pixel
    % (stamp size is always odd).
    % Note: If the input is a cube and the user didn't provide X/Y then the
    % X1/Y1 (in image coordinates) will be empty, and the use should use
    % StampX1/StampY1
    M1.X = M1.StampX1 + X;  % coordinates in full image
    M1.Y = M1.StampY1 + Y;  % coordinates in full image
    M1.StampInitX = StampX;
    M1.StampInitY = StampY;


    if nargout>1
        
        [M2.X2,M2.Y2,M2.XY] = imUtil.sources.mex.mom2_cube(CubeBS, B0, M1.StampX1, M1.StampY1, Args.MaxRadiusM2);

        
        if nargout>2
            switch Args.AperPhotMethod
                case 'interp'
                    [Aper.AperPhot, Aper.AperArea] = imUtil.sources.mex.aper_phot_cube_interp(CubeBS, B0, M1.StampX1, M1.StampY1, Args.AperRadius);
                case 'simple'
                    [Aper.AperPhot, Aper.AperArea] = imUtil.sources.mex.aper_phot_cube_simple(CubeBS, B0, M1.StampX1, M1.StampY1, Args.AperRadius);
                otherwise
                    error('Unknown AperPhotType option');
            end
            Aper.AperRadius = Args.AperRadius;
        end
    end
end
