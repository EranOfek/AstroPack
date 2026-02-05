function [X1,Y1,X2,Y2,XY] = mom12_cube_once(Cube, BackEstimator, Args)
    % Single-pass (no iteration) intensity moments on a stamp or cube of stamps
    %   Computes (per slice) intensity-weighted centroid (1st moments) and optionally
    %   the central 2nd moments, using a *fixed circular support mask* centered on the
    %   stamp geometric center. No external weight (no Gaussian).
    %
    % Input  : - (Cube) MxK or MxKxN, real single/double.
    %          - (BackEstimator) scalar or N-vector (background per slice). Subtracted first.
    %          * ...,key,val,...
    %            'MaxRadius' -  scalar radius.
    %                   Default: half-stamp size (inscribed circle).
    %            'RelToCenter' - if true, return X1,Y1 relative to stamp center (0,0).
    %                   if false, return MATLAB 1-based corner coords.
    %                   Default is true.
    % Output : - (X1) Vector of X 1st moment.
    %          - (Y1) Vector of Y 1st moment.
    %          - (X2) Vector of X2 2nd moment.
    %          - (Y2) Vector of Y2 2nd moment.
    %          - (XY) Vector of XY 2nd moment.
    % Notes : 
    %   - No loop over stamps (vectorized over 3rd dimension).
    %   - NaN/Inf pixels are ignored (treated as 0 contribution).
    %   - If sum(I)==0 for a slice, outputs for that slice are NaN.
    % Author : ChatGPT + Eran Ofek (Fen 2026)
    % Example: [X1,Y1,X2,Y2,XY] = mom12_cube_once(Cube, BackEstimator)
    
    
    arguments
        Cube
        BackEstimator
        Args.MaxRadius (1,1) double = NaN
        Args.RelToCenter (1,1) logical = true;
    end
    
    % ---- normalize input to MxKxN ----
    if ~(isnumeric(Cube) && isreal(Cube))
        error('Cube must be a real numeric array.');
    end
    if ndims(Cube) == 2
        Cube = reshape(Cube, size(Cube,1), size(Cube,2), 1);
    elseif ndims(Cube) ~= 3
        error('Cube must be MxK or MxKxN.');
    end
    [M,K,N] = size(Cube);
    
    % ---- background vector ----
    if ~(isnumeric(BackEstimator) && isreal(BackEstimator))
        error('BackEstimator must be real numeric.');
    end
    nBE = numel(BackEstimator);
    if ~(nBE==1 || nBE==N)
        error('BackEstimator must be scalar or length N.');
    end
    BE = double(BackEstimator(:));
    if nBE==1
        BE = repmat(BE, N, 1);
    end
    
    % ---- geometry ----
    xC = (K+1)/2;
    yC = (M+1)/2;
    
    % ---- MaxRadius default: max inscribed circle ----
    if isnan(Args.MaxRadius)
        MaxRadius = min((K-1)/2, (M-1)/2);
    else
        MaxRadius = double(Args.MaxRadius);
        if ~isfinite(MaxRadius) || MaxRadius < 0
            error('MaxRadius must be finite >=0 (or omitted/default).');
        end
    end
    R2 = MaxRadius^2;
    
    % ---- fixed circular support mask about stamp center ----
    [Xg,Yg] = meshgrid(1:K, 1:M);                          % corner coords
    Mask = ((Xg - xC).^2 + (Yg - yC).^2) <= R2;             % MxK logical
    W = double(Mask);
    
    % ---- background subtraction + masking ----
    I = double(Cube) - reshape(BE, 1, 1, N);
    I(~isfinite(I)) = 0;
    I = I .* W;
    
    % ---- coordinate system ----
    if Args.RelToCenter
        Xcoord = Xg - xC;
        Ycoord = Yg - yC;
    else
        Xcoord = Xg;
        Ycoord = Yg;
    end
    
    % ---- 1st moments ----
    sumI  = squeeze(sum(sum(I,1),2));                       % Nx1
    sumIX = squeeze(sum(sum(I .* Xcoord,1),2));             % Nx1
    sumIY = squeeze(sum(sum(I .* Ycoord,1),2));             % Nx1
    
    X1 = sumIX ./ sumI;
    Y1 = sumIY ./ sumI;
    
    % ---- handle bad denominator early ----
    bad = ~(isfinite(sumI) & (sumI ~= 0));
    X1(bad) = NaN;
    Y1(bad) = NaN;
    
    % ---- 2nd moments only if requested ----
    if nargout > 2
        DX = Xcoord - reshape(X1, 1, 1, N);
        DY = Ycoord - reshape(Y1, 1, 1, N);
    
        X2 = squeeze(sum(sum(I .* (DX.^2), 1),2)) ./ sumI;
        Y2 = squeeze(sum(sum(I .* (DY.^2), 1),2)) ./ sumI;
        XY = squeeze(sum(sum(I .* (DX .* DY),1),2)) ./ sumI;
    
        X2(bad) = NaN; Y2(bad) = NaN; XY(bad) = NaN;
    else
        X2 = []; Y2 = []; XY = [];
    end
    
    % ensure column vectors
    X1 = X1(:); Y1 = Y1(:);
    if nargout > 2
        X2 = X2(:); Y2 = Y2(:); XY = XY(:);
    end
end
