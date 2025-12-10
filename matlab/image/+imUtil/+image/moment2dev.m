function [M1, M2] = moment2dev(Image, X0, Y0, Args)
    % moments of an image
    %     Optional detailed description
    % Input  : - a 2D image matrix 
    %          - X0 initial guess of the centroid (may be a vector of positions)
    %          - Y0 initial guess of the centroid (may be a vector of positions) 
    %          * ...,key,val,... 
    %          'MomRadius'- Radius around position in which to calculate the
    %                        moments. Recomended ~1.7 FWHM. Default is 8.
    %          'SigmaG' - Sigma of the 'filetring' Gaussian, can be
    %                      up to MomRadius/2
    %          'MaxIter' - maximal number of iterations for M1
    %          'Precision' - precision of M1.X and M1.Y position
    %          'SubtractBack' - whether to measure and subtract the background 
    %                          (Default is true)
    %          'WeightedM2' - whether to multiply the Image by additional gaussian weight for M2 calculation
    %                          (Default is true)
    % Output : - first moment structure:
    %          .X
    %          .Y
    %          .Niter
    %          - second moment structure:
    %          .X2
    %          .Y2
    %          .XY 
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: Im1 = imUtil.kernel2.gauss(2, [31 31]);
    %          [M1,M2]=imUtil.image.moment2dev(Im1,16,16)
    %          Im2 = 10+Im1*100;
    %          [M1,M2]=imUtil.image.moment2dev(Im2,16,16)
    %          Im3 = Im2 + rand(size(Im2));    
    %          [M1,M2]=imUtil.image.moment2dev(Im3,16,16)
    arguments
        Image
        X0
        Y0
        Args.MomRadius         = 8;
        Args.SigmaG            = 2;
        Args.MaxIter           = 30;
        Args.Precision         = 1e-4;   
        Args.SubtractBack      = 'true';
        Args.WeightedM2        = 'true';
    end
    %
    [M, N]   = size(Image);
    [Xg, Yg] = meshgrid(1:N, 1:M);
    
    Nsrc = numel(X0);
    
    % Measure and subtract a global background if requested
    if Args.SubtractBack
        Back  = imUtil.background.background(Image,'SubSizeXY','full');
        Image = Image - Back;
    end

    % Define a circular mask and cut the image outside it     
    Dx0 = Xg - reshape(X0, 1, 1, Nsrc);
    Dy0 = Yg - reshape(Y0, 1, 1, Nsrc);
    Mask = (Dx0.^2 + Dy0.^2) <= Args.MomRadius^2;
    Iwin = Image .* Mask;

    % Find the first moment iteratively
    Xc = X0;
    Yc = Y0;

    InvTwoSig2 = 1/(2 * Args.SigmaG^2);
    
    for Iter = 1:Args.MaxIter
        
        Dx = Xg - reshape(Xc,1,1,Nsrc);
        Dy = Yg - reshape(Yc,1,1,Nsrc);

        W = exp(-(Dx.^2 + Dy.^2).*InvTwoSig2);
        IW = Iwin .* W;

        M0 = squeeze(sum(sum(IW,1),2));   % 1×Nsrc

        NewX = squeeze(sum(sum(IW .* Xg,1),2)) ./ M0;
        NewY = squeeze(sum(sum(IW .* Yg,1),2)) ./ M0;
        
%         M0 = sum(IW(:));              
%         NewX = sum(Xg(:) .* IW(:)) / M0;
%         NewY = sum(Yg(:) .* IW(:)) / M0;
    
        % Check convergence

        d = hypot(NewX - Xc, NewY - Yc);

        Xc = NewX;
        Yc = NewY;

        if all(d < Args.Precision)
            break;
        end
        
%         if hypot(NewX - Xc, NewY - Yc) < Args.Precision
%             Xc = NewX;
%             Yc = NewY;
%             break;
%         end
%         Xc = NewX;
%         Yc = NewY;
%                 
    end

    M1.X = Xc;
    M1.Y = Yc;
    M1.Niter = Iter;

    % calculate the second moments relative to the found centroid: 
%     Dx = Xg - M1.X;
%     Dy = Yg - M1.Y;
%     
    Dx = Xg - reshape(Xc,1,1,Nsrc);
    Dy = Yg - reshape(Yc,1,1,Nsrc);

    % use the same mask (inside MomRadius) with or w/o weights for the 2nd moments:    
    J = Iwin; 
    
    if Args.WeightedM2
        W = exp(-(Dx.^2 + Dy.^2) / (2 * Args.SigmaG^2));
        J = J.*W;
    end

    M0 = squeeze(sum(sum(J,1),2));

    M2.X2 = squeeze(sum(sum(J .* (Dx.^2),1),2)) ./ M0;
    M2.Y2 = squeeze(sum(sum(J .* (Dy.^2),1),2)) ./ M0;
    M2.XY = squeeze(sum(sum(J .* (Dx.*Dy),1),2)) ./ M0;
    
%     M0 = sum(J(:));
% 
%     M2.X2 = sum((Dx(:).^2) .* J(:)) / M0;
%     M2.Y2 = sum((Dy(:).^2) .* J(:)) / M0;
%     M2.XY = sum((Dx(:).*Dy(:)) .* J(:)) / M0;
end
