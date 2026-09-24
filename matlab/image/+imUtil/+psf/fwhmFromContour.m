function [Result] = fwhmFromContour(PSF, Args)
    % Compute FWHM of a PSF image from contour
    %        For complex PSF shapes 
    % Input  : - 2D PSF image, background-free, normalized to 1
    %          * ...,key,val,... 
    %         'Thresh' - threshold, usually 0.5 (half-maximum)
    % Output : - a structure containing the major and minor axis estimates
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: P = imUtil.kernel2.gauss([2 3 0]); 
    %          Res = imUtil.psf.fwhmFromContour(P,'Thresh',0.5) 
    arguments
        PSF        
        Args.Thresh = 0.5; % half-maximum
    end           
    % --- 1) Threshold at half maximum ---
    Halfmax = Args.Thresh * max(PSF(:));
    [ys, xs] = find(PSF >= Halfmax);   % coordinates of half-max contour region

    if numel(xs) < 3
        error('Not enough pixels in the half-max region.');
    end
    
    % --- 2) Compute the geometric center of the region ---
    cx = mean(xs);
    cy = mean(ys);

    % Shift the coordinates to the center
    X = xs - cx;
    Y = ys - cy;
    coords = [X, Y];

    % --- 3) PCA to get major/minor axes ---
    % Covariance matrix
    C = cov(coords);

    % Eigen decomposition
    [V, D] = eig(C);

    % Sort eigenvalues so that major axis = largest eigenvalue
    [~, order] = sort(diag(D), 'descend');
    V = V(:, order);  % principal directions

    % Project contour points onto the principal axes
    proj = coords * V;
    major_proj = proj(:,1);
    minor_proj = proj(:,2);
    
    % --- 4) FWHM from the contour extents ---
    major_fwhm = max(major_proj) - min(major_proj);
    minor_fwhm = max(minor_proj) - min(minor_proj);

    % --- 5) Orientation angle of major axis ---
    % Angle of first principal vector (V(:,1))
    theta = atan2(V(2,1), V(1,1));   % radians

    % --- 6) Build output structure ---
    Result.Center = [cx, cy];
    Result.Major_fwhm = major_fwhm;
    Result.Minor_fwhm = minor_fwhm;
    Result.Average_fwhm = (major_fwhm+minor_fwhm)/2;
    Result.Theta_rad = theta;
end 