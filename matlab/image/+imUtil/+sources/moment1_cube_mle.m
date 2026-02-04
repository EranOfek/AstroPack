function [X1,Y1] = moment1_cube_mle(Cube, Back, Args)
    % Poisson-MLE-like Gaussian-weighted centroids for stamps/cubes.
    %   Estimate 1st moments (centroid) for each slice of a 2-D stamp or 3-D cube
    %   assuming Poisson statistics with known background, using an MLE-like update
    %   with a *normalized* Gaussian model.
    %
    % Input  : - (Cube) MxK or MxKxN array, single or double.
    %            Cube should include background.
    %          - (Back) scalar or length-N vector.
    %            Background level per slice (same units as Cube).
    %            Default is 0.
    %          * ...,key,val,...
    %            'SigmaWidth' - Gaussian sigma in pixels.
    %                   Default is 2.
    %            'MaxRadius' - Fixed circular support radius (pixels)
    %                   around the stamp center.
    %                   Default is half stamp size.
    %            'MaxIter' - Maximum number of centroid iterations.
    %                   Default is 10.
    %            'RelToCenter' - If true, output X1,Y1 are relative to the
    %                   stamp center (0,0 at center).
    %                   If false, output X1,Y1 are MATLAB 1-based coordinates
    %                   relative to corner.
    %                   Default is true.
    %
    % Output : - Nx1 vector of X centeroids (relative to stamp corner or
    %            center). See 'RelToCenter' option.
    %          - Nx1 vector of Y centeroids.
    %
    % Algorithm: (per slice)
    %       Model:  lambda_i = B + A * g_i(x0,y0)
    %       where g_i is a *normalized* 2D Gaussian:
    %        g_i = (1/(2*pi*sigma^2)) * exp(-r_i^2/(2*sigma^2))
    %       so A is the integral (total flux) of the Gaussian model.
    %
    %       Initialization: (x0,y0) = stamp center.
    %
    %       Amplitude A: estimated ONCE in iteration 1 by aperture-summing background-subtracted
    %       flux within radius 2*sigma around (x0,y0):
    %        A = sum_{r<=2*sigma} max(n_i - B, 0)
    %
    %       Iterative centroid update (MLE-like weight):
    %        w_i = n_i * g_i / (B + A*g_i)
    %        x_new = sum(w_i * x_i) / sum(w_i)
    %        y_new = sum(w_i * y_i) / sum(w_i)
    %
    %       All sums are restricted to a fixed circular support mask of radius Args.MaxRadius
    %       around the stamp center (fixed center) to avoid edge effects and speed up.
    % Author : ChatGPT + Eran Ofek (Fen 2026)
    % Example: [X1,Y1] = wcentroid_cube_mle(Cube, Back, Args)
    

    arguments
        Cube
        Back = 0;
        Args.SigmaWidth   = 2;
        Args.MaxRadius    = [];
        Args.MaxIter      = 10;
        Args.RelToCenter  = true;
    end



    nd = ndims(Cube);
    if nd ~= 2 && nd ~= 3
        error('Cube must be MxK or MxKxN.');
    end

    [M,K,~] = size(Cube);
    if nd == 2
        N = 1;
        Cube = reshape(Cube, M, K, 1);
    else
        N = size(Cube,3);
    end

    % Back: scalar or N
    if ~isnumeric(Back) || ~isreal(Back)
        error('Back must be real numeric.');
    end
    if isscalar(Back)
        Back = repmat(double(Back), N, 1);
    else
        Back = double(Back(:));
        if numel(Back) ~= N
            error('Back must be scalar or length N.');
        end
    end
    

    sigma = Args.SigmaWidth;
    

    maxIter = Args.MaxIter;
    

    relToCenter = Args.RelToCenter;
    
    % Default MaxRadius: half stamp size
    halfSize = min(M,K) * 0.5;
    if isempty(Args.MaxRadius)
        Args.MaxRadius = halfSize;
    end
    maxR = Args.MaxRadius;
   
    % Stamp center (MATLAB 1-based coordinates)
    xc0 = 0.5*(K+1);
    yc0 = 0.5*(M+1);

    [Xg,Yg] = meshgrid(1:K, 1:M);
    dx0 = Xg - xc0;
    dy0 = Yg - yc0;

    % Fixed circular support mask around stamp center (fixed center)
    maskL = (dx0.^2 + dy0.^2) <= (maxR^2);

    % Extract masked coordinate vectors once
    x_m = double(Xg(maskL));
    y_m = double(Yg(maskL));

    % Output (corner coords for now)
    X1c = zeros(N,1);
    Y1c = zeros(N,1);

    % Gaussian constants
    normG  = 1/(2*pi*sigma*sigma);
    inv2s2 = 1/(2*sigma*sigma);
    R_A2   = (2*sigma)^2;

    for n = 1:N
        img = double(Cube(:,:,n));
        im_m = img(maskL);

        % Initial guess: stamp center
        x0 = xc0;
        y0 = yc0;

        A = 0; % estimated once (iteration 1)

        for it = 1:maxIter
            dx = x_m - x0;
            dy = y_m - y0;
            r2 = dx.*dx + dy.*dy;

            g = normG * exp(-r2 * inv2s2);

            if it == 1
                inA = (r2 <= R_A2);
                sub = im_m(inA) - Back(n);
                sub(sub < 0) = 0;
                A = sum(sub);
            end

            denom = Back(n) + A .* g;

            good = (denom > 0) & isfinite(im_m) & isfinite(denom) & isfinite(g);
            if ~any(good)
                break;
            end

            w = zeros(size(g));
            w(good) = (im_m(good) .* g(good)) ./ denom(good);

            Sw = sum(w);
            if ~(isfinite(Sw) && Sw > 0)
                break;
            end

            x0 = sum(w .* x_m) / Sw;
            y0 = sum(w .* y_m) / Sw;
        end

        X1c(n) = x0;
        Y1c(n) = y0;
    end

    if relToCenter
        X1 = X1c - xc0;
        Y1 = Y1c - yc0;
    else
        X1 = X1c;
        Y1 = Y1c;
    end
end
