function [X1, Y1, sigma_xx, sigma_yy, sigma_xy] = moment2dev(I, X0, Y0, R, sigma_g, Args)
    % moments of an image
    %     Optional detailed description
    % Input  : - a 2D image matrix 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Dec) 
    % Example: 

    arguments
        I
        X0
        Y0
        R
        sigma_g
        Args.MaxIter           = 10;
        Args.Precision         = [];        
    end

% I        : 2D image (double)
% X0, Y0   : initial guess for centroid
% R        : radius for moment computation
% sigma_g  : Gaussian scale used in iterative refinement (e.g. R/2)
% maxIter  : max iterations for centroid refinement (e.g. 20)
% tol      : convergence threshold, e.g. 1e-4

[m, n] = size(I);
[xg, yg] = meshgrid(1:n, 1:m);

% ----- Define circular mask only once -----
dx0 = xg - X0;
dy0 = yg - Y0;
mask = (dx0.^2 + dy0.^2) <= R^2;

% Restrict all operations to fixed aperture
Iwin = I .* mask;

% ----- ITERATIVE CENTROID REFINEMENT -----
Xc = X0;
Yc = Y0;

for k = 1:Args.MaxIter

    dx = xg - Xc;
    dy = yg - Yc;

    % Gaussian weights centered on current estimate
    W = exp(-(dx.^2 + dy.^2) / (2 * sigma_g^2));

    % Use both image and Gaussian as weights
    IW = Iwin .* W;

    M0 = sum(IW(:));
    if M0 == 0
        error('Zero total weight (image may be empty inside R).');
    end

    newX = sum(xg(:) .* IW(:)) / M0;
    newY = sum(yg(:) .* IW(:)) / M0;

    % Check convergence
    if hypot(newX - Xc, newY - Yc) < Args.Precision
        Xc = newX;
        Yc = newY;
        break;
    end

    Xc = newX;
    Yc = newY;
end

X1 = Xc;
Y1 = Yc;

% ----- SECOND MOMENTS -----
dx = xg - X1;
dy = yg - Y1;

% Use the same mask (inside R) and no Gaussian weights for 2nd moments:
J = Iwin;

M0 = sum(J(:));

sigma_xx = sum((dx(:).^2) .* J(:)) / M0;
sigma_yy = sum((dy(:).^2) .* J(:)) / M0;
sigma_xy = sum((dx(:).*dy(:)) .* J(:)) / M0;

end
