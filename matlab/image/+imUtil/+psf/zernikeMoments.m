function [Coeffs, Basis] = zernikeMoments(Image, MaxOrder, R, Xc, Yc, Basis)
% Fast Zernike moments up to radial order MaxOrder (default 8).
% Works on HxW or HxWxN image cube. Fully vectorized across pixels/images.
% SPEED: Precomputes & caches the Zernike basis (geometry + modes). Reuse by
% passing the returned Basis back into subsequent calls with the same
% geometry/order, or let the function auto-cache via a persistent.
%
% Output Coeffs: [NumModes x N] (columns per image) in Noll order.
%
%   [C,B] = imUtil.psf.zernikeMoments(Im, 4);       % build & cache basis
%   C2    = imUtil.psf.zernikeMoments(Cube, 4, [], [], [], B);  % reuse

    arguments
        Image
        MaxOrder (1,1) double {mustBeNonnegative, mustBeInteger} = 8
        R   = []
        Xc  = []
        Yc  = []
        Basis = []     % optional precomputed basis
    end

    % --- Sizes ---
    SizeIm = size(Image);
    if numel(SizeIm) < 3, SizeIm(3) = 1; end
    H = SizeIm(1);  W = SizeIm(2);  Nimg = SizeIm(3);

    % --- Defaults ---
    if isempty(Xc), Xc = floor((W+1)*0.5); end
    if isempty(Yc), Yc = floor((H+1)*0.5); end
    if isempty(R)
        R = max(1, floor(min([Xc-1, W-Xc, Yc-1, H-Yc])));
        if ~isfinite(R) || R<=0, R = floor(min(H,W)/2); end
    end

    % --- Persistent cache ---
    persistent BasisCache KeyCache
    UseCache = false;
    Key = sprintf('H%d_W%d_R%d_Xc%d_Yc%d_N%d', H, W, R, Xc, Yc, MaxOrder);
    if isempty(Basis) && ~isempty(KeyCache) && strcmp(Key, KeyCache)
        Basis = BasisCache; UseCache = true;
    end

    % --- Build basis if needed ---
    if isempty(Basis) || ~isfield(Basis, 'Z')
        Basis = zernikeBasis(H, W, MaxOrder, R, Xc, Yc);
        BasisCache = Basis; KeyCache = Key; UseCache = false; %#ok<NASGU>
    end

    % --- Project images (vectorized) ---
    Ivec = reshape(Image, H*W, Nimg);                 % [HW x Nimg]
    Imsk = Ivec(Basis.MaskIdx, :);                    % [NP x Nimg]

    % Zero NaNs (we'll fix the denominator accordingly)
    Valid = ~isnan(Imsk);
    Imsk(~Valid) = 0;

    % Numerator: Z' * I
    Numer = Basis.Z.' * Imsk;                         % [NumModes x Nimg]

    % Denominator: per-image energy of each mode over valid pixels
    if Basis.NoNaNs && all(Valid(:))
        Denom = Basis.BaseDenom;                      % [NumModes x 1], broadcast
        Denom = Denom .* ones(1, Nimg);
    else
        Denom = (Basis.Z2).' * double(Valid);         % [NumModes x Nimg]
    end

    Coeffs = Numer ./ max(Denom, eps);
end

% ================================================================
function Basis = zernikeBasis(H, W, MaxOrder, R, Xc, Yc)
% Build & return the Zernike basis and geometry for given params.
    [Xg, Yg] = meshgrid(1:W, 1:H);
    Xp = (Xg - Xc) / R;
    Yp = (Yg - Yc) / R;
    Rho   = hypot(Xp, Yp);
    Theta = atan2(Yp, Xp);
    MaskGeom = Rho <= 1;

    MaskIdx = find(MaskGeom);
    NP  = numel(MaskIdx);
    Rv  = Rho(MaskIdx);                    % [NP x 1]
    Tv  = Theta(MaskIdx);                  % [NP x 1]

    % -- (n,m) up to MaxOrder, then Noll order --
    Mcells = arrayfun(@(N) -N:2:N, 0:MaxOrder, 'UniformOutput', false);
    Mvec   = cell2mat(Mcells);
    Ncells = arrayfun(@(N) N*ones(1, numel(-N:2:N)), 0:MaxOrder, 'UniformOutput', false);
    Nvec   = cell2mat(Ncells);

    Mabs = abs(Mvec);  IsEvenN = mod(Nvec,2) == 0;  IsOddN = ~IsEvenN;
    Rank = zeros(size(Mvec));
    IdxE0   = IsEvenN & (Mabs == 0);
    IdxENeg = IsEvenN & (Mvec < 0) & (Mabs > 0);
    IdxEPos = IsEvenN & (Mvec > 0);
    Rank(IdxE0)   = 0;
    Rank(IdxENeg) = 2*(Mabs(IdxENeg)/2) - 1;
    Rank(IdxEPos) = 2*(Mabs(IdxEPos)/2);
    IdxONeg = IsOddN & (Mvec < 0);
    IdxOPos = IsOddN & (Mvec > 0);
    Rank(IdxONeg) = 2*((Mabs(IdxONeg)+1)/2) - 2;
    Rank(IdxOPos) = 2*((Mabs(IdxOPos)+1)/2) - 1;
    [~, Ord] = sortrows([Nvec(:), Rank(:)], [1 2]);
    Nvec = Nvec(Ord);  Mvec = Mvec(Ord);
    NumModes = numel(Mvec);

    % -- Normalization
    NormVec = sqrt(Nvec+1).*(Mvec==0) + sqrt(2*(Nvec+1)).*(Mvec~=0); % [1 x NumModes]

    % -- Radial polynomials via coefficient matrix
    E = (0:MaxOrder)';  Elen = numel(E);
    Erep = E*ones(1, NumModes);
    Nrep = ones(Elen,1)*Nvec;
    Mrep = ones(Elen,1)*abs(Mvec);
    ValidCoeff = (Erep <= Nrep) & (Erep >= Mrep) & (mod(Nrep - Erep, 2) == 0);
    S = (Nrep - Erep)/2;
    Coeff = zeros(Elen, NumModes);
    if any(ValidCoeff(:))
        A  = Nrep - S + 1;
        B1 = S + 1;
        B2 = (Nrep + Mrep)/2 - S + 1;  % = (M+E)/2 + 1
        B3 = (Nrep - Mrep)/2 - S + 1;  % = (E-M)/2 + 1
        IdxV = ValidCoeff;
        Coeff(IdxV) = (-1).^S(IdxV) .* exp( gammaln(A(IdxV)) - (gammaln(B1(IdxV)) + gammaln(B2(IdxV)) + gammaln(B3(IdxV))) );
    end

    Pows = Rv .^ (0:MaxOrder);                 % [NP x Elen]
    Rpoly = Pows * Coeff;                       % [NP x NumModes]

    % -- Angular part: compute sin/cos using one trig call each, then recurrences
    % This avoids NP x NumModes trig calls.
    MabsMax = max(abs(Mvec));
    C1 = cos(Tv);                                % [NP x 1]
    S1 = sin(Tv);                                % [NP x 1]
    % Build cos(k*theta), sin(k*theta) for k=0..MabsMax via recurrence
    Ck = zeros(NP, MabsMax+1);  Sk = zeros(NP, MabsMax+1);
    Ck(:,1) = 1;           % cos(0*theta)
    Sk(:,1) = 0;           % sin(0*theta)
    if MabsMax >= 1
        Ck(:,2) = C1;      % cos(theta)
        Sk(:,2) = S1;      % sin(theta)
        for K = 2:MabsMax  % K indexes "k"; generates k=2..MabsMax
            % cos((k)θ) = cosθ cos((k-1)θ) - sinθ sin((k-1)θ)
            % sin((k)θ) = sinθ cos((k-1)θ) + cosθ sin((k-1)θ)
            Ck(:,K+1) = C1.*Ck(:,K) - S1.*Sk(:,K);
            Sk(:,K+1) = S1.*Ck(:,K) + C1.*Sk(:,K);
        end
    end

    % Map to required m values (vectorized gather)
    IdxZero = (Mvec == 0);
    IdxPos  = (Mvec > 0);
    IdxNeg  = (Mvec < 0);
    T = zeros(NP, NumModes);
    if any(IdxZero), T(:,IdxZero) = 1; end
    if any(IdxPos),  T(:,IdxPos)  = Ck(:, Mabs(IdxPos)+1); end    % cos(|m|θ)
    if any(IdxNeg),  T(:,IdxNeg)  = Sk(:, Mabs(IdxNeg)+1); end    % sin(|m|θ)

    % -- Full basis and denominators
    Z = (Rpoly .* T) .* (ones(NP,1) * NormVec);        % [NP x NumModes]

    Basis.MaskIdx   = MaskIdx;
    Basis.Nvec      = Nvec;
    Basis.Mvec      = Mvec;
    Basis.NumModes  = NumModes;
    Basis.NormVec   = NormVec;
    Basis.Z         = Z;
    Basis.Z2        = Z.^2;
    Basis.BaseDenom = sum(Basis.Z2, 1).';             % [NumModes x 1]
    Basis.NoNaNs    = true;                            % geometry has no NaNs
end



function Coeffs = zernikeMoments2(Image, MaxOrder, R, Xc, Yc)
% Zernike moments up to a given radial order (default MaxOrder = 8).
% Vectorized (no loops): works on a single image HxW or a cube HxWxN.
% Output: Coeffs is [NumModes x N] with columns per image and rows in Noll order.
%
% Example:
%   Coeffs = imUtil.psf.zernikeMoments(AI(1).PSFData.Data);            % HxW
%   Coeffs = imUtil.psf.zernikeMoments(ImageCube, 8, R, Xc, Yc);        % HxWxN

    arguments
        Image
        MaxOrder = 3;
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

    % --- Build (n,m) list up to MaxOrder, then reorder into Noll order (no loops) ---
    Mcells = arrayfun(@(N) -N:2:N, 0:MaxOrder, 'UniformOutput', false);
    Mvec   = cell2mat(Mcells);                                       % 1 x NumModes (raw)
    Ncells = arrayfun(@(N) N*ones(1, numel(-N:2:N)), 0:MaxOrder, 'UniformOutput', false);
    Nvec   = cell2mat(Ncells);                                       % 1 x NumModes (raw)

    % Noll ordering within each n:
    %  - if n is even:  m = 0, -2, +2, -4, +4, ...
    %  - if n is odd:   m = -1, +1, -3, +3, ...
    Mabs = abs(Mvec);
    IsEvenN = mod(Nvec,2) == 0;

    Rank = zeros(size(Mvec));
    % even n
    IdxE0   = IsEvenN & (Mabs == 0);
    IdxENeg = IsEvenN & (Mvec < 0) & (Mabs > 0);
    IdxEPos = IsEvenN & (Mvec > 0);
    Rank(IdxE0)   = 0;
    Rank(IdxENeg) = 2*(Mabs(IdxENeg)/2) - 1;   % 1,3,5,... for -2,-4,-6,...
    Rank(IdxEPos) = 2*(Mabs(IdxEPos)/2);       % 2,4,6,... for +2,+4,+6,...

    % odd n
    IsOddN  = ~IsEvenN;
    IdxONeg = IsOddN & (Mvec < 0);
    IdxOPos = IsOddN & (Mvec > 0);
    Rank(IdxONeg) = 2*((Mabs(IdxONeg)+1)/2) - 2; % 0,2,4,... for -1,-3,-5,...
    Rank(IdxOPos) = 2*((Mabs(IdxOPos)+1)/2) - 1; % 1,3,5,... for +1,+3,+5,...

    % sort by n then rank
    [~, Ord] = sortrows([Nvec(:), Rank(:)], [1 2]);
    Nvec = Nvec(Ord);
    Mvec = Mvec(Ord);
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

    % Compute only on valid entries to avoid gammaln of negatives
    Coeff = zeros(Elen, NumModes);
    if any(ValidCoeff(:))
        A  = Nrep - S + 1;
        B1 = S + 1;
        B2 = (Nrep + Mrep)/2 - S + 1;  % = (M+E)/2 + 1
        B3 = (Nrep - Mrep)/2 - S + 1;  % = (E-M)/2 + 1
        IdxV = ValidCoeff;
        Coeff(IdxV) = (-1).^S(IdxV) .* exp( gammaln(A(IdxV)) - (gammaln(B1(IdxV)) + gammaln(B2(IdxV)) + gammaln(B3(IdxV))) );
    end;

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
