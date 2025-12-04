% function [X1, Y1, sigma_xx, sigma_yy, sigma_xy] = moment2dev(I, X0, Y0, R, sigma_g, Args)
function [M1, M2] = moment2dev(I, X0, Y0, Args)
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
        Args.R                 = 8;
        Args.SigmaG            = 4;
        Args.MaxIter           = 10;
        Args.Precision         = 1e-6;        
    end

    [m, n] = size(I);
    [xg, yg] = meshgrid(1:n, 1:m);

    % ----- Define circular mask only once -----
    dx0 = xg - X0;
    dy0 = yg - Y0;
    mask = (dx0.^2 + dy0.^2) <= Args.R^2;

    % Restrict all operations to fixed aperture
    Iwin = I .* mask;

    % ----- ITERATIVE CENTROID REFINEMENT -----
    Xc = X0;
    Yc = Y0;

    for k = 1:Args.MaxIter

        dx = xg - Xc;
        dy = yg - Yc;

        % Gaussian weights centered on current estimate
        W = exp(-(dx.^2 + dy.^2) / (2 * Args.SigmaG^2));

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

    M1.X = Xc;
    M1.Y = Yc;

    % ----- SECOND MOMENTS -----
    dx = xg - M1.X;
    dy = yg - M1.Y;

    % Use the same mask (inside R) and no Gaussian weights for the 2nd moments:
    J = Iwin;

    M0 = sum(J(:));

    M2.X2 = sum((dx(:).^2) .* J(:)) / M0;
    M2.Y2 = sum((dy(:).^2) .* J(:)) / M0;
    M2.XY = sum((dx(:).*dy(:)) .* J(:)) / M0;
end
