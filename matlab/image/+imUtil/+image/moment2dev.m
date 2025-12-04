function [M1, M2] = moment2dev(I, X0, Y0, Args)
    % moments of an image
    %     Optional detailed description
    % Input  : - a 2D image matrix 
    %          - X0 initial guess of the centroid
    %          - Y0 initial guess of the centroid
    %          * ...,key,val,... 
    % Output : - first moment structure:
    %          .X
    %          .Y
    %          .Niter
    %          - second moment structure:
    %          .X2
    %          .Y2
    %          .XY 
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: 

    arguments
        I
        X0
        Y0
        Args.R                 = 8;
        Args.SigmaG            = 4;
        Args.MaxIter           = 30;
        Args.Precision         = 1e-6;        
    end
    
    [M, N] = size(I);
    [Xg, Yg] = meshgrid(1:N, 1:M);

    % Define a circular mask and restrict all the operations to the fixed aperture
    Dx0 = Xg - X0;
    Dy0 = Yg - Y0;
    Mask = (Dx0.^2 + Dy0.^2) <= Args.R^2;
    Iwin = I .* Mask;

    % Find the first moment iteratively
    Xc = X0;
    Yc = Y0;

    for Iter = 1:Args.MaxIter

        Dx = Xg - Xc;
        Dy = Yg - Yc;

        % Gaussian weights centered on the current estimate
        W = exp(-(Dx.^2 + Dy.^2) / (2 * Args.SigmaG^2));

        % Use both the image and the Gaussian as weights
        IW = Iwin .* W;

        M0 = sum(IW(:));
        if M0 == 0
            error('Zero total weight: the image may be empty inside R.');
        end
        
        NewX = sum(Xg(:) .* IW(:)) / M0;
        NewY = sum(Yg(:) .* IW(:)) / M0;

        % Check convergence
        if hypot(NewX - Xc, NewY - Yc) < Args.Precision
            Xc = NewX;
            Yc = NewY;
            break;
        end

        Xc = NewX;
        Yc = NewY;
    end

    M1.X = Xc;
    M1.Y = Yc;
    M1.Niter = Iter;

    % calculate the second moments relative to the found centroid: 
    
    Dx = Xg - M1.X;
    Dy = Yg - M1.Y;

    % use the same mask (inside R) and no Gaussian weights for the 2nd moments:
    J = Iwin;

    M0 = sum(J(:));

    M2.X2 = sum((Dx(:).^2) .* J(:)) / M0;
    M2.Y2 = sum((Dy(:).^2) .* J(:)) / M0;
    M2.XY = sum((Dx(:).*Dy(:)) .* J(:)) / M0;
end
