function [M1, M2, Aper, Cube] = moments(Image, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    %            'SN' - A vector of S/N per image slice.
    %                   This will be used for the convergence. The calculation is
    %                   converged when the shift between two iterations is smaller than
    %                   SigmaWidth/SN.
    %            'MaxIter' - Max. number of iterations. Default is 8.
    %            'SigmaWidth' sigma-width of the Gaussian weight function.
    %                   If two element vector, then the first is used only in the 1st
    %                   iteration, and the 2nd for all the other iterations.
    %                   Default is 1.5.
    %            'TruncateSigma' - (K) When calculate weights, truncate pixels
    %                   outside (+/- K*SigmaWidth). This may speed up the code.
    %                   Default is 3.
    %            'MaxStepSize' - Max step size in first moment position between
    %                   iterations. This is a two element vector.
    %                   The first element corresponds to the first
    %                   iteraytion, and the second for all the rest.
    %                   A good value is about is 1/(sqrt(2)*MaxIter).
    %                   Default is [0.1 0.1].
    % Output : - 
    % Author : Eran Ofek (2026 Apr) 
    % Example: 

    arguments
        Image
        Args.SN                = [];
        Args.X                 = []; 
        Args.Y                 = [];
        Args.CubeX             = [];
        Args.CubeY             = [];
        Args.mexCutout         = true;


        Args.HalfSize          = 12;
        Args.AperPhotType      = 'interp';  % 'simple'|'interp'
        Args.AperPhotRadius    = [2 4 6];
        Args.AnnulusRadii      = [10 12];
        Args.MaxIter           = 8;
        Args.SigmaWidth        = 1.5;
        Args.TruncateSigma     = 3;
        Args.MaxStepSize       = [0.1 0.1]; % [first iter, all the rest]
        Args.MaxRadiusM2       = 6;
    end

    if Args.HalfSize<max(max(Args.AnnulusRadii), max(Args.AperPhotRadius))
        error('HalfSize of PSF must be larger/equal to max Annulus radii/ aper phot radii');
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
        CubeX = (Args.X-RoundX) + Args.HalfSize + 1;
        CubeY = (Args.Y-RoundY) + Args.HalfSize + 1;

    else
        % Image is already a Cube
        Cube = Image;
        [Ny, Nx, Nslice] = size(Cube);

       
        % if CubeX/CubeY are given than these are the PSF position in the
        % cube, otherwise assume the source is in the stamp center
        X = Args.X;
        Y = Args.Y;

        % assume stamp is odd size

        if isempty(Args.CubeX)
            CubeX = repmat(Args.HalfSize + 1, Nslice,1);
        else
            CubeX = Args.CubeX;
        end
        if isempty(Args.CubeY)
            CubeY = repmat(Args.HalfSize + 1, Nslice, 1);
        else
            CubeY = Args.CubeY;
        end

    end

    [CubeBS, Aper.BackVec, Aper.StdVec, Aper.NpixBack] = imUtil.sources.mex.annulus_median(Cube, Args.AnnulusRadii, 0);


    [M1.X1S, M1.Y1S, M1.Con] = imUtil.sources.mex.moment1_cube(CubeBS, [], Args.SN, Args.MaxIter, Args.SigmaWidth, Args.TruncateSigma, true, Args.MaxStepSize(2), Args.MaxStepSize(1));

    % return X, Y positions to stamp center position
    % Note that if the user didn't define Args.X, Args.Y then these are
    % empty
    M1.X1 = M1.X1S + X;
    M1.Y1 = M1.Y1S + Y;


    if nargout>1
        B0 = zeros(Nslice, 1);
        [M2.X2,M2.Y2,M2.XY] = imUtil.sources.mex.mom2_cube(Cube, B0, X,Y, Args.MaxRadiusM2);

        
        if nargout>2
            switch Args.AperPhotType
                case 'interp'
                    [Aper.AperPhot, Aper.AperArea] = imUtil.sources.mex.aper_phot_cube_interp(CubeBS, B0, M1.X1S, M1.Y1S, Args.AperPhotRadius);
                case 'simple'
                    [Aper.AperPhot, Aper.AperArea] = imUtil.sources.mex.aper_phot_cube_simple(Cube, B0, M1.X1S, M1.Y1S, Args.AperPhotRadius);
                otherwise
                    error('Unknown AperPhotType option');
            end
        end
    end
end
