function Coeffs = zernikeMoments(Image, MaxOrder, R, Xc, Yc)
% Zernike moments up to a given radial order (default MaxOrder = 8).
% Vectorized (no loops): works on a single image HxW or a cube HxWxN.
% Output: Coeffs is [NumModes x N] with columns per image and rows in Noll order.
%
% Example:
%   Coeffs = imUtil.psf.zernikeMoments(AI(1).PSFData.Data);            % HxW
%   Coeffs = imUtil.psf.zernikeMoments(ImageCube, 8, R, Xc, Yc);        % HxWxN

    arguments
        Image
        MaxOrder (1,1) double {mustBeNonnegative, mustBeInteger} = 8
        R   = []
        Xc  = []
        Yc  = []
    end

    % --- Sizes & defaults ---
    SizeIm = size(Image);
    if numel(SizeIm) < 3, SizeIm(3) = 1; end
    H = SizeIm(1);  W = SizeIm(2);  Nimg = SizeIm(3);

    if isempty(Xc), Xc = floor((W+1)*0.5); end
    if isempty(Yc), Yc = floor((H+1)*0.5); end
    if isempty(R)
        R = max(1, floor(min([Xc-1, W-Xc, Yc-1, H-Yc])));
        if ~isfinite(R) || R<=0, R = floor(min(H,W)/2); end
    end

    % --- Geometry mapped to unit disk ---
    [Xg, Yg] = meshgrid(1:W, 1:H);
    Xp = (Xg - Xc) / R;
    Yp = (Yg - Yc) / R;
    Rho   = hypot(Xp, Yp);
    Theta = atan2(Yp, Xp);
    MaskGeom = Rho <= 1;              % geometry mask only (same for all images)

    % Keep only masked pixels (vectorized view)
    Idx = find(MaskGeom);
    NP  = numel(Idx);
    Rv  = Rho(Idx);                    % [NP x 1]
    Tv  = Theta(Idx);                  % [NP x 1]

    % --- Build Noll-ordered (n,m) list up to MaxOrder (no loops) ---
    Mcells = arrayfun(@(N) -N:2:N, 0:MaxOrder, 'UniformOutput', false);
    Mvec   = cell2mat(Mcells);                                       % 1 x NumModes
    Ncells = arrayfun(@(N) N*ones(1, numel(-N:2:N)), 0:MaxOrder, 'UniformOutput', false);
    Nvec   = cell2mat(Ncells);                                       % 1 x NumModes
    NumModes = numel(Mvec);

    % --- Normalization per mode ---
    NormVec = sqrt(Nvec+1).*(Mvec==0) + sqrt(2*(Nvec+1)).*(Mvec~=0); % 1 x NumModes

    % --- Radial polynomials via coefficient matrix (no loops) ---
    % R_n^{|m|}(rho) = sum_{e valid} C(e,j) * rho.^e
    E = (0:MaxOrder)';                               % exponents 0..MaxOrder   [Elen x 1]
    Elen = numel(E);
    Erep = E*ones(1, NumModes);                      % [Elen x NumModes]
    Nrep = ones(Elen,1)*Nvec;                        % [Elen x NumModes]
    Mrep = ones(Elen,1)*abs(Mvec);                   % [Elen x NumModes]

    ValidCoeff = (Erep <= Nrep) & (Erep >= Mrep) & (mod(Nrep - Erep, 2) == 0);
    S = (Nrep - Erep)/2;                              % s = (n - e)/2

    % Use gammaln for stability: n! = exp(gammaln(n+1))
    Coeff = (-1).^S .* exp( gammaln(Nrep - S + 1) ...
        - ( gammaln(S + 1) + gammaln((Nrep + Mrep)/2 - S + 1) + gammaln((Nrep - Mrep)/2 - S + 1) ) );
    Coeff(~ValidCoeff) = 0;

    % Powers of rho for all exponents
    Pows = Rv .^ (0:MaxOrder);                        % [NP x Elen] via implicit expansion

    % Radial part for all modes
    Rpoly = Pows * Coeff;                             % [NP x NumModes]

    % --- Angular part for all modes (no loops) ---
    T = zeros(NP, NumModes);
    IdxZero = (Mvec == 0);
    IdxPos  = (Mvec > 0);
    IdxNeg  = (Mvec < 0);
    if any(IdxZero), T(:,IdxZero) = 1; end
    if any(IdxPos),  T(:,IdxPos)  = cos( Tv .* Mvec(IdxPos) ); end
    if any(IdxNeg),  T(:,IdxNeg)  = sin( Tv .* abs(Mvec(IdxNeg)) ); end

    % --- Full basis on masked pixels: Z = Norm * R_n^m(rho) * trig ---
    Z = Rpoly .* T .* (ones(NP,1) * NormVec);         % [NP x NumModes]

    % --- Project images (no loops): A = Z' * I, B = (Z.^2)' * Valid ---
    Ivec = reshape(Image, H*W, Nimg);                 % [HW x Nimg]
    Imsk = Ivec(Idx, :);                              % [NP x Nimg]
    Valid = ~isnan(Imsk);                             % [NP x Nimg]
    Imsk(~Valid) = 0;                                 % zero-out NaNs

    Numer = Z.' * Imsk;                               % [NumModes x Nimg]
    Denom = (Z.^2).' * double(Valid);                 % [NumModes x Nimg]

    Coeffs = Numer ./ max(Denom, eps);
end


function Coeffs = zernikeMoments1(Image, R, Xc, Yc)
% Zernike moments up to Noll j=11 (piston..spherical)
% Output: Coeffs is a NUM_MODES x 1 column vector in Noll order (j=1..11)
% Example: Coeffs = imUtil.psf.zernikeMoments(AI(1).PSFData.Data)

    arguments
        Image
        R   = []
        Xc  = []
        Yc  = []
    end

    % --- Defaults & geometry ---
    SizeIm = size(Image);
    if numel(SizeIm) < 2, SizeIm(2) = 1; end
    H = SizeIm(1);  W = SizeIm(2);

    if isempty(Xc), Xc = floor((W+1)*0.5); end
    if isempty(Yc), Yc = floor((H+1)*0.5); end
    if isempty(R)
        % Default radius: fit inside image, centered at (Xc,Yc)
        R = max(1, floor(min([Xc-1, W-Xc, Yc-1, H-Yc])));
        if ~isfinite(R) || R<=0
            R = floor(min(H,W)/2);
        end
    end

    % --- Coordinates mapped to unit disk ---
    [Xg, Yg] = meshgrid(1:W, 1:H);
    Xp = (Xg - Xc) / R;   % NOTE: [] on LHS, not {}
    Yp = (Yg - Yc) / R;
    Rho   = hypot(Xp, Yp);
    Theta = atan2(Yp, Xp);

    Mask = (Rho <= 1) & ~isnan(Image);
    I  = Image(Mask);
    Rv = Rho(Mask);
    Tv = Theta(Mask);

    % --- Radial polynomial R_n^m ---
    Rnm = @(N, M, Rvec) arrayfun(@(RR) Radial(N, abs(M), RR), Rvec);
    function Val = Radial(N, M, RR)
        if mod(N - M, 2) ~= 0
            Val = 0; return;
        end
        Val = 0;
        for K = 0:((N - M)/2)
            Val = Val + (-1)^K * factorial(N - K) / ...
                ( factorial(K) * factorial((N + M)/2 - K) * factorial((N - M)/2 - K) ) ...
                * RR.^(N - 2*K);
        end
    end

    % --- Orthonormalization on unit disk ---
    Norm = @(N, M) (sqrt(2*(N+1)))*(M~=0) + sqrt(N+1)*(M==0);

    % --- Noll j=1..11 mapping to (n,m) ---
    Modes = [0 0; 1 -1; 1 1; 2 0; 2 -2; 2 2; 3 -1; 3 1; 3 -3; 3 3; 4 0];

    NumModes = size(Modes,1);
    Coeffs = zeros(NumModes, 1);   % <<< column vector output

    for J = 1:NumModes
        N = Modes(J,1);  M = Modes(J,2);
        Zr = Norm(N, abs(M)) * Rnm(N, M, Rv);
        if M == 0
            Z = Zr;
        elseif M > 0
            Z = Zr .* cos(M * Tv);
        else
            Z = Zr .* sin(abs(M) * Tv);
        end
        A = sum(I .* Z);
        B = sum(Z .* Z);
        Coeffs(J) = A / max(B, eps);
    end
end
