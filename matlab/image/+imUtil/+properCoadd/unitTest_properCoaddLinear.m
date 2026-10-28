function Result = unitTest_properCoaddLinear(Fun, Seed)
    % Unit test for imUtil.properCoadd.properCoaddLinear.
    %   Runs 27 numerical checks of the linear-system proper coadd:
    %   equivalence with the Fourier closed form, noise and PSF properties,
    %   masks, invariances, regularization, robust rejection (including the
    %   M-estimator score equation), and the source-noise model (Eq. C2).
    %   All checks are run, a summary table is printed, and an error is
    %   thrown if any check fails.
    % Input  : - Handle of the function to test.
    %            Default is @imUtil.properCoadd.properCoaddLinear.
    %          - Random-number seed. Default is 1.
    % Output : - true if all tests passed.
    % Author : Claude + Eran Ofek (Oct 2026)
    % Example: imUtil.properCoadd.unitTest_properCoaddLinear

    if nargin<1 || isempty(Fun)
        Fun = @imUtil.properCoadd.properCoaddLinear;
    end
    if nargin<2
        Seed = 1;
    end

    rng(Seed);
    S   = simData();
    Nim = S.Nim;
    Ny  = S.Ny;
    Nx  = S.Nx;
    T   = struct('Name',{},'Value',{},'Limit',{},'Pass',{});

    % noisy images, a mask, and a direct (uniform-weight) reference coadd
    M   = S.M0 + reshape(S.Sig,1,1,Nim).*randn(Ny,Nx,Nim);
    W   = double(rand(Ny,Nx,Nim) > 0.02);
    W(28:38, 32:44, 1) = 0;                          % block mask on a star, sharpest image
    [R, P_R, Info] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F);

    % ---- direct solution equals the Fourier closed form Shat/Dhat
    Rf = fourierCoadd(M, S.OTF, S.F, S.Sig);
    T  = check(T, 'Direct solution = Fourier closed form (rel. err.)', relErr(R,Rf), 1e-10);

    % ---- Info for the direct solution
    Ok = strcmp(Info.Method,'direct') && isempty(Info.CGiter) && Info.BandFrac==1 && ...
         abs(Info.F_R - sqrt(sum(S.F.^2./S.Sig.^2)))/Info.F_R < 1e-12;
    T  = checkTrue(T, 'Info: Method, CGiter, BandFrac, F_R', Ok);

    % ---- uniform weights: white noise with unit variance
    Rn  = Fun(reshape(S.Sig,1,1,Nim).*randn(Ny,Nx,Nim), S.P, S.Sig, 1, 'Flux',S.F);
    T   = check(T, 'Noise-only coadd: |std(R) - 1|', abs(std(Rn(:)) - 1), 0.06);
    Cx  = corrcoef(reshape(Rn(:,1:end-1),[],1), reshape(Rn(:,2:end),[],1));
    Cy  = corrcoef(reshape(Rn(1:end-1,:),[],1), reshape(Rn(2:end,:),[],1));
    T   = check(T, 'Noise-only coadd: max |lag-1 correlation|', max(abs([Cx(1,2) Cy(1,2)])), 0.075);

    % ---- coadd PSF: unit sum, peak at the center, PsfOutSize crop
    Cy0  = floor(Ny/2) + 1;
    Cx0  = floor(Nx/2) + 1;
    [~, Imax] = max(P_R(:));
    [Iy, Ix]  = ind2sub(size(P_R), Imax);
    [~, P_Rc] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F, 'PsfOutSize',[15 15]);
    Ok = abs(sum(P_R(:)) - 1) < 1e-10 && Iy==Cy0 && Ix==Cx0 && ...
         isequal(P_Rc, P_R(Cy0-7:Cy0+7, Cx0-7:Cx0+7));
    T  = checkTrue(T, 'P_R: unit sum, centered peak, PsfOutSize crop', Ok);

    % ---- CG with uniform per-pixel weights reproduces the direct solution
    [Rcg, ~, Icg] = Fun(M, S.P, S.Sig, ones(Ny,Nx,Nim), 'Flux',S.F);
    T  = check(T, 'CG, uniform per-pixel weights = direct (rel. err.)', relErr(Rcg,R), 1e-8);
    T  = checkTrue(T, 'CG, uniform per-pixel weights: Method cg, 1 CG iteration', ...
                   strcmp(Icg.Method,'cg') && isequal(Icg.CGiter,1));

    % ---- masked pixels are ignored (with and without robust rejection)
    Mbad = M;
    Mbad(W==0) = 1e6;
    Ra = Fun(M,    S.P, S.Sig, W, 'Flux',S.F);
    Rb = Fun(Mbad, S.P, S.Sig, W, 'Flux',S.F);
    Rc = Fun(M,    S.P, S.Sig, W, 'Flux',S.F, 'Robust',true);
    Rd = Fun(Mbad, S.P, S.Sig, W, 'Flux',S.F, 'Robust',true);
    T  = check(T, 'Masked pixels (=1e6) have no effect (rel. err.)', max(relErr(Rb,Ra), relErr(Rd,Rc)), 1e-12);

    % ---- masks preserve the PSF: noiseless data give F_R*(P_R conv Sky), with
    %        the P_R and F_R returned for these weights (omegaBar depends on W)
    [R0m, P_Rm, I0m] = Fun(S.M0, S.P, S.Sig, W, 'Flux',S.F, 'CGTol',1e-10, 'CGMaxIter',2000);
    R0  = I0m.F_R * real(ifft2( fft2(ifftshift(P_Rm)) .* fft2(S.Sky) ));
    T   = check(T, 'Noiseless data + mask give F_R*(P_R conv Sky) (rel. err.)', relErr(R0m,R0), 1e-7);
    F_Rexp = sqrt(sum(S.F.^2 .* reshape(mean(mean(W,1),2),1,[]) ./ S.Sig.^2));
    T   = check(T, 'Masks: F_R uses omegaBar_j = mean(W_j)/sigma_j^2 (rel. err.)', ...
                abs(I0m.F_R - F_Rexp)/F_Rexp, 1e-12);

    % ---- invariance to rescaling an image together with its F_j and sigma_j
    A  = linspace(0.5, 2, Nim);
    Ma = M .* reshape(A,1,1,Nim);
    E1 = relErr(Fun(Ma, S.P, S.Sig.*A, 1, 'Flux',S.F.*A), R);
    E2 = relErr(Fun(Ma, S.P, S.Sig.*A, W, 'Flux',S.F.*A), Ra);
    T  = check(T, 'Invariance to image rescaling (rel. err.)', max(E1,E2), 1e-8);

    % ---- PSF normalization: R independent of the stamps' scale
    Rp = Fun(M, S.P.*reshape(1:Nim,1,1,Nim), S.Sig, 1, 'Flux',S.F);
    T  = check(T, 'NormPSF: invariance to PSF-stamp scale (rel. err.)', relErr(Rp,R), 1e-12);

    % ---- shift equivariance
    Rs = Fun(circshift(M,[5 -3 0]), S.P, S.Sig, 1, 'Flux',S.F);
    T  = check(T, 'Shift equivariance (rel. err.)', relErr(Rs, circshift(R,[5 -3])), 1e-10);

    % ---- Tikhonov regularization: closed form for uniform weights
    Lambda = 0.05;
    [Rt, ~, It] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F, 'RegParam',Lambda, 'CGTol',1e-12);
    Dsq = sum(reshape(S.F.^2./S.Sig.^2,1,1,Nim) .* abs(S.OTF).^2, 3);
    Rtc = real(ifft2( fft2(Rf) ./ (1 + Lambda./Dsq) ));
    T   = check(T, 'RegParam = closed form dhat/(1+lambda/Dhat^2) (rel. err.)', relErr(Rt,Rtc), 1e-8);
    T   = checkTrue(T, 'RegParam: exact preconditioner, <=2 CG iterations', It.CGiter <= 2);

    % ---- SigmaIsVariance
    V  = reshape(S.Sig.^2,1,1,Nim) .* (1 + 0.2*rand(Ny,Nx,Nim));
    Rv = Fun(M, S.P, V,       1, 'Flux',S.F, 'SigmaIsVariance',true, 'CGTol',1e-10);
    Rq = Fun(M, S.P, sqrt(V), 1, 'Flux',S.F, 'CGTol',1e-10);
    T  = check(T, 'SigmaIsVariance: variance input = std input (rel. err.)', relErr(Rv,Rq), 1e-8);

    % ---- robust with a very large tuning constant = non-robust
    [Rh, ~, Ih] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F, 'Robust',true, 'RobustPar',1e6);
    T  = check(T, 'Robust, c = 1e6, equals non-robust (rel. err.)', relErr(Rh,R), 1e-8);
    T  = checkTrue(T, 'Robust, c = 1e6: warm-started CG needs 0 iterations, stops after 1', ...
                   isequal(Ih.CGiter(2:end), 0) && Ih.NiterRobust==1);

    % ---- robust rejection of cosmic rays (Tukey defaults; Huber)
    CR  = [12 30 1 3000; 20 20 2 2000; 45 50 5 5000; 30 60 7 1000];   % [y x image amplitude]
    Mc  = M;
    Box = false(Ny,Nx);
    for Icr = 1:size(CR,1)
        Mc(CR(Icr,1), CR(Icr,2), CR(Icr,3)) = Mc(CR(Icr,1), CR(Icr,2), CR(Icr,3)) + CR(Icr,4);
        Box(CR(Icr,1)-4:CR(Icr,1)+4, CR(Icr,2)-4:CR(Icr,2)+4) = true;
    end
    Rnr  = Fun(Mc, S.P, S.Sig, 1, 'Flux',S.F);
    Rtk  = Fun(Mc, S.P, S.Sig, 1, 'Flux',S.F, 'Robust',true);
    Rhb  = Fun(Mc, S.P, S.Sig, 1, 'Flux',S.F, 'Robust',true, 'RobustFun','Huber', 'MaxIter',10);
    RmsNr = rms0(Rnr(Box) - R(Box));
    T  = check(T, 'Tukey: CR residual rms / non-robust rms', rms0(Rtk(Box) - R(Box))/RmsNr, 0.05);
    T  = check(T, 'Huber: CR residual rms / non-robust rms', rms0(Rhb(Box) - R(Box))/RmsNr, 0.02);

    % ---- Huber fixed point solves the M-estimator equation sum_j B_j^T psi(u_j)/s_j = 0
    Ch = 1.345;
    [Rhc, P_Rh, Ihc] = Fun(Mc, S.P, S.Sig, 1, 'Flux',S.F, 'Robust',true, 'RobustFun','Huber', ...
                           'MaxIter',100, 'Tol',1e-12, 'CGTol',1e-12, 'CGMaxIter',1000);
    Bhat = forwardOp(P_Rh, Ihc.F_R, S.OTF, S.F);
    Sig3 = reshape(S.Sig,1,1,Nim);
    U    = (Mc - real(ifft2(Bhat .* fft2(Rhc)))) ./ Sig3;
    T    = check(T, 'Huber: score equation sum_j B_j^T psi(u_j)/s_j = 0 (rel.)', ...
                 scoreRatio(Bhat, min(max(U,-Ch),Ch)./Sig3), 1e-6);

    % ---- SourceNoise with all source terms switched off = weighted solution
    [Rz, ~, Iz] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F, 'SourceNoise',true, ...
                      'Gain',Inf, 'ModelErr',0, 'AstErr',0);
    T  = check(T, 'SourceNoise, zero source terms = direct (rel. err.)', relErr(Rz,R), 1e-6);

    % ---- SourceNoise variance model: reduced chi^2 for source-noise data
    %         (background noise ~10 data units, bright sources, gain 2)
    Gain = 2;
    Sb   = 10*S.Sig;
    Mb   = 500*S.M0;
    Ms   = Mb + sqrt(reshape(Sb.^2,1,1,Nim) + max(Mb,0)/Gain).*randn(Ny,Nx,Nim);
    [~, ~, Ip] = Fun(Ms, S.P, Sb, 1, 'Flux',S.F);
    [~, ~, Is] = Fun(Ms, S.P, Sb, 1, 'Flux',S.F, 'SourceNoise',true, ...
                     'Gain',Gain, 'ModelErr',0, 'AstErr',0);
    Chi2Exp = 1 - Is.BandFrac/Nim;                     % (Ndata - Nparam)/Ndata
    T  = check(T, 'SourceNoise: |chi2_red - (1-1/Nim)|', abs(Is.ReducedChi2 - Chi2Exp), 0.035);
    T  = checkTrue(T, 'SourceNoise: chi2_red without source noise is larger', ...
                   Ip.ReducedChi2 > Is.ReducedChi2 + 0.1);

    % ---- SourceNoise fixed point solves sum_j B_j^T r_j/s_j^2 = 0 with s_j^2 of
    %      Eq. C2 evaluated independently here from the returned model
    Eps = 0.02;
    Ast = 0.05;
    [Rsn, P_Rs, Isn] = Fun(Ms, S.P, Sb, 1, 'Flux',S.F, 'SourceNoise',true, 'Gain',Gain, ...
                           'ModelErr',Eps, 'AstErr',Ast, 'MaxIter',100, 'Tol',1e-12, ...
                           'CGTol',1e-12, 'CGMaxIter',1000);
    Bhat = forwardOp(P_Rs, Isn.F_R, S.OTF, S.F);
    Mhat = Bhat .* fft2(Rsn);
    Mod  = real(ifft2(Mhat));
    Kx   = 2*pi*nyqZero(Nx);                        % spectral derivative, Nyquist term = 0
    Ky   = 2*pi*nyqZero(Ny).';
    Grad2 = real(ifft2(Mhat.*(1i*Kx))).^2 + real(ifft2(Mhat.*(1i*Ky))).^2;
    S2   = reshape(Sb.^2,1,1,Nim) + max(Mod,0)/Gain + Eps^2*Mod.^2 + Ast^2*Grad2;
    T    = check(T, 'SourceNoise: score equation with Eq. C2 variances = 0 (rel.)', ...
                 scoreRatio(Bhat, (Ms - Mod)./S2), 1e-6);

    % ---- Robust + SourceNoise: residuals standardized by the Eq. C2 scale
    %      (Huber fixed point: sum_j B_j^T psi(r_j/s_j)/s_j = 0)
    Ch = 2;
    [Rhs, P_Rhs, Ihs] = Fun(Ms, S.P, Sb, 1, 'Flux',S.F, 'Robust',true, 'RobustFun','Huber', ...
                            'RobustPar',Ch, 'SourceNoise',true, 'Gain',Gain, 'ModelErr',Eps, ...
                            'AstErr',Ast, 'MaxIter',100, 'Tol',1e-12, 'CGTol',1e-12, 'CGMaxIter',1000);
    Bhat  = forwardOp(P_Rhs, Ihs.F_R, S.OTF, S.F);
    Mhat  = Bhat .* fft2(Rhs);
    Mod   = real(ifft2(Mhat));
    Grad2 = real(ifft2(Mhat.*(1i*Kx))).^2 + real(ifft2(Mhat.*(1i*Ky))).^2;
    Sn    = sqrt(reshape(Sb.^2,1,1,Nim) + max(Mod,0)/Gain + Eps^2*Mod.^2 + Ast^2*Grad2);
    T     = check(T, 'Huber + SourceNoise: score equation with Eq. C2 scale = 0 (rel.)', ...
                  scoreRatio(Bhat, min(max((Ms - Mod)./Sn,-Ch),Ch)./Sn), 1e-6);

    % ---- EtaBand: modes outside the retained band are zero
    Eta = 1e-3;
    [Re, ~, Ie] = Fun(M, S.P, S.Sig, 1, 'Flux',S.F, 'EtaBand',Eta);
    Band = Dsq > Eta*max(Dsq(:));
    Rhat = abs(fft2(Re));
    Ok   = abs(Ie.BandFrac - mean(Band(:))) < 1e-12 && Ie.BandFrac < 1 && ...
           max(Rhat(~Band)) < 1e-10*max(Rhat(:));
    T  = checkTrue(T, 'EtaBand: BandFrac and zero power outside the band', Ok);

    % ------------------------------------------------------------ summary
    fprintf('\nunitTest_properCoaddLinear (seed %d)\n', Seed);
    for It = 1:numel(T)
        if T(It).Pass, Str = 'PASS'; else, Str = 'FAIL'; end
        if isempty(T(It).Limit)
            fprintf('  %s  %2d. %s\n', Str, It, T(It).Name);
        else
            fprintf('  %s  %2d. %-66s %9.2e  (limit %.2g)\n', Str, It, T(It).Name, ...
                    T(It).Value, T(It).Limit);
        end
    end
    Result = all([T.Pass]);
    fprintf('  %d/%d tests passed\n', sum([T.Pass]), numel(T));
    if ~Result
        error('unitTest_properCoaddLinear: %d test(s) failed', sum(~[T.Pass]));
    end
end

% ======================================================================= locals
function S = simData()
    % Noiseless test images: 8 asymmetric PSFs (Gaussian core plus an offset
    % secondary component, so the OTFs are complex) with different widths,
    % transparencies and noise levels; three point sources.
    S.Ny  = 64;
    S.Nx  = 72;
    S.Nim = 8;
    S.F   = linspace(0.8, 1.2, S.Nim);
    S.Sig = linspace(1.4, 0.7, S.Nim);
    Width = linspace(1.0, 2.0, S.Nim);
    Sky   = zeros(S.Ny, S.Nx);
    Sky(33,37) = 500;
    Sky(15,55) = 200;
    Sky(50,15) = 80;
    S.Sky = Sky;
    [Xx, Yy] = meshgrid(-8:8, -8:8);
    S.P   = zeros(17, 17, S.Nim);
    S.OTF = complex(zeros(S.Ny, S.Nx, S.Nim));
    S.M0  = zeros(S.Ny, S.Nx, S.Nim);
    for J = 1:S.Nim
        G = exp(-(Xx.^2 + Yy.^2)/(2*Width(J)^2)) + ...
            0.3*exp(-((Xx-1.5).^2 + (Yy-0.5).^2)/(2*Width(J)^2));
        G = G./sum(G(:));
        S.P(:,:,J)   = G;
        Pad          = zeros(S.Ny, S.Nx);
        Pad(1:17,1:17) = G;
        S.OTF(:,:,J) = fft2(circshift(Pad, [-8 -8]));
        S.M0(:,:,J)  = S.F(J) * real(ifft2(fft2(Sky) .* S.OTF(:,:,J)));
    end
end

function R = fourierCoadd(M, OTF, F, Sig)
    % Zackay & Ofek (2017) proper coadd: Rhat = Shat/Dhat (independent implementation)
    Nim  = numel(F);
    F    = reshape(F,   1, 1, Nim);
    Om   = reshape(1./Sig.^2, 1, 1, Nim);
    Shat = sum(F.*Om .* conj(OTF) .* fft2(M), 3);
    Dhat = sqrt(sum(F.^2.*Om .* abs(OTF).^2, 3));
    R    = real(ifft2(Shat./Dhat));
end

function Bhat = forwardOp(P_R, F_R, OTF, F)
    % Coadd-to-image operators B_j = F_j P_j / D, with D = F_R*P_R (all modes retained)
    Dhat = F_R * real(fft2(ifftshift(P_R)));
    Bhat = reshape(F,1,1,[]) .* OTF ./ Dhat;
end

function Ratio = scoreRatio(Bhat, X)
    % ||sum_j B_j^T X_j|| / sqrt(sum_j ||B_j^T X_j||^2): ~0 at a solution of the
    % normal equations, ~1 for unrelated terms
    Sj    = real(ifft2(conj(Bhat) .* fft2(X)));
    Tot   = sum(Sj, 3);
    Ratio = norm(Tot(:)) / sqrt(sum(Sj(:).^2));
end

function F = nyqZero(N)
    % DFT frequencies [cycles/pixel] in FFT order, with the Nyquist term set to 0
    F = [0:ceil(N/2)-1, -floor(N/2):-1] / N;
    if mod(N,2)==0
        F(N/2+1) = 0;
    end
end

function E = relErr(A, B)
    E = max(abs(A(:) - B(:))) / max(abs(B(:)));
end

function Y = rms0(X)
    Y = sqrt(mean(X(:).^2));
end

function T = check(T, Name, Value, Limit)
    T(end+1) = struct('Name',Name, 'Value',Value, 'Limit',Limit, 'Pass',Value <= Limit);
end

function T = checkTrue(T, Name, Cond)
    T(end+1) = struct('Name',Name, 'Value',[], 'Limit',[], 'Pass',logical(Cond));
end
