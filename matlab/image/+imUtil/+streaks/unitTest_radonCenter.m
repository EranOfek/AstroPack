function unitTest_radonCenter(DoPlot)
% Validate radonCenter (MEX) against a vectorized MATLAB
% reference, demonstrate faint-trail detection, and time it.
%   Compile first, e.g.:  mex -O radonCenter.cpp   (see the .cpp header for OpenMP flags)
%   test_radonCenter          % with plot
%   test_radonCenter(false)   % no plot

if nargin < 1, DoPlot = true; end

%% 1. Exact agreement with an independent reference implementation
Sizes  = [21 21; 20 20; 15 24; 33 18; 1 7; 64 64];
Widths = [1 3 5 9];
MaxErr = 0;
for Is = 1:size(Sizes,1)
    for W = Widths
        Cube         = randn([Sizes(Is,:) 3]);
        [T,  S,  A ] = imUtil.streaks.mex.radonCenter(Cube, W);
        [Tr, Sr, Ar] = radonCenterRef(Cube, W);
        assert(isequal(T, Tr), 'TanRot mismatch');
        MaxErr = max([MaxErr; abs(S(:)-Sr(:)); abs(A-Ar)]);
    end
end
fprintf('1) max |mex - reference|            = %.2e\n', MaxErr);
assert(MaxErr < 1e-9, 'Mismatch with reference');

% classes: single and uint16 must agree with double
Cube = round(1000*rand(31,31,5));
[~, Sd] = imUtil.streaks.mex.radonCenter(Cube);
[~, Ss] = imUtil.streaks.mex.radonCenter(single(Cube));
[~, Su] = imUtil.streaks.mex.radonCenter(uint16(Cube));
assert(isa(Ss,'single') && isa(Su,'double'));
fprintf('   single / uint16 rel. difference   = %.2e / %.2e\n', ...
        max(abs(double(Ss(:))-Sd(:))./abs(Sd(:))), max(abs(Su(:)-Sd(:))));

%% 2. Constant image: RadonSums == B*Area; angles strictly increasing
[T, S, A] = imUtil.streaks.mex.radonCenter(7*ones(25,25), 3);
Theta     = mod(atand(T), 180);
fprintf('2) constant image max|S-7*Area|      = %.2e,  Nang = %d,  Theta in [%g, %g]\n', ...
        max(abs(S-7*A)), numel(T), min(Theta), max(Theta));
assert(all(diff(Theta) > 0));

%% 3. Detection demo: faint trail (0.7 sigma per pixel) through the center
N      = 65;
Theta0 = 33.7;                                     % true angle [deg]
[X, Y] = meshgrid(1:N, 1:N);
Xc     = (N+1)/2;   Yc = (N+1)/2;
Dperp  = -(X-Xc)*sind(Theta0) + (Y-Yc)*cosd(Theta0);  % distance from the line
Im     = 0.7*exp(-0.5*Dperp.^2) + randn(N);           % noise sigma = 1, B = 0
[T, S, A] = imUtil.streaks.mex.radonCenter(Im, 3);
Z         = S ./ sqrt(A);         % approx. S/N for white noise (sigma=1, B=0)
[Zmax, I] = max(Z);
Theta     = mod(atand(T), 180);
fprintf('3) trail at %.1f deg: detected at %.2f deg with S/N = %.1f\n', Theta0, Theta(I), Zmax);

if DoPlot
    figure;
    subplot(1,2,1); imagesc(Im); axis image xy; colorbar; title('Stamp (axis xy)');
    subplot(1,2,2); plot(Theta, Z, '.-'); grid on;
    xlabel('\theta [deg]'); ylabel('S / \surd{Area}'); xlim([0 180]);
end

%% 4. Timing
Cube = randn(64, 64, 1e4, 'single');
tic; [~, S] = imUtil.streaks.mex.radonCenter(Cube); Dt = toc;  %#ok<ASGLU>
fprintf('4) %d stamps of 64x64 (W=3): %.3f s  (%.2f us/stamp)\n', ...
        size(Cube,3), Dt, 1e6*Dt/size(Cube,3));
end


function [TanRot, Sums, Area] = radonCenterRef(Cube, W)
% Brute-force reference: explicit weight map per angle (same definition).
[Ny, Nx, Nim] = size(Cube);
Yc  = (Ny-1)/2;   Xc = (Nx-1)/2;                 % 0-based center
R   = max(1, floor(max(Ny,Nx)/2));
KA1 = (0:R)';  KB = (R-1:-1:-(R-1))';  KA2 = (-R:-1)';
Slope  = [KA1/R; KB/R; KA2/R];
Steep  = [false(R+1,1); true(2*R-1,1); false(R,1)];
TanRot = Slope;  TanRot(Steep) = R ./ KB;
Nang   = numel(Slope);
Sums   = zeros(Nang, Nim);
Area   = zeros(Nang, 1);
Mat    = reshape(Cube, Ny*Nx, Nim);
Yv     = (0:Ny-1)';   Xv = 0:Nx-1;
for Ia = 1:Nang
    H = W/2*sqrt(1 + Slope(Ia)^2);
    if ~Steep(Ia)                        % integrate along y in each column
        V0 = Yc + (Xv - Xc)*Slope(Ia);   % 1 x Nx
        Wt = max(0, min(Yv+0.5, V0+H) - max(Yv-0.5, V0-H));
    else                                 % integrate along x in each row
        V0 = Xc + (Yv - Yc)*Slope(Ia);   % Ny x 1
        Wt = max(0, min(Xv+0.5, V0+H) - max(Xv-0.5, V0-H));
    end
    Sums(Ia,:) = Wt(:).' * Mat;
    Area(Ia)   = sum(Wt(:));
end
end
