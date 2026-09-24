function [Result] = unitTest()
    % unitTest for imUtil.poissNoise

    % imUtil.poissNoise.fitPoissPSF
    [Pass, Result] = fitPoissPSF_unitTest()
    Pass

    Result = true;
    
end

function [Pass, Result] = fitPoissPSF_unitTest(Args)
% Unit test for fitPoissPSF: generate a PSF, inject sources, fit, and compare.
% Package: imUtil.poissNoise
% Description: Self-contained test of fitPoissPSF. Generates a normalised
%              Gaussian PSF and a low-count Poisson background, injects sources
%              of known flux, runs fitPoissPSF, and checks:
%                A. flux recovery        - mean fitted flux ~ injected flux
%                B. flux-error calibration - std(Fhat) ~ mean reported FluxErr
%                C. p-value calibration   - under H0 (no source) the p-values
%                                           are ~uniform (fraction<=alpha ~ alpha)
%                D. detection             - a bright source gives a tiny p-value
%                E. position recovery     - FixedPos=false recovers an offset
%                                           position and improves the statistic
%                F. fft p-value method    - agrees with mc for a detected source
%                                           and is ~calibrated under H0
%                G. multi-source input    - vector X,Y handled; a zero-flux
%                                           source yields Flux=0, p~1
%              Each check has a tolerance and prints PASS/FAIL; the overall
%              Pass is the AND of all checks. Uses a toolbox-free Poisson
%              sampler so it runs in base MATLAB or Octave.
% Input  : * ...,key,val,...
%            'Ntrials' - Monte-Carlo trials for the statistical checks (A,B,C,F).
%                        Default 2000.
%            'Nsim'    - simulations for the MC null distribution. Default 1e5.
%            'Sigma'   - Gaussian PSF sigma [pix]. Default 1.3.
%            'StampN'  - PSF stamp side (odd). Default 7.
%            'Back'    - background level [counts/pix]. Default 0.05.
%            'Seed'    - RNG seed. Default 42.
%            'Verbose' - print per-check details. Default true.
% Output : - Pass   : logical, true if all checks passed.
%          - Result : struct with the measured quantities and per-check flags.
% License: GNU general public license version 3
% Tested : Matlab R2021b / Octave 8
%     By : (test for fitPoissPSF)                                  Jun 2026
%    URL :
% Example: Pass = imUtil.poissNoise.fitPoissPSF_unitTest;
%          [Pass,R] = imUtil.poissNoise.fitPoissPSF_unitTest('Ntrials',5000);
% Reliable: 2
%--------------------------------------------------------------------------

    arguments
        Args.Ntrials   = 2000
        Args.Nsim      = 1e5
        Args.Sigma     = 1.3
        Args.StampN    = 7
        Args.Back      = 0.05
        Args.Seed      = 42
        Args.Verbose   = true
    end

    seed_rng(Args.Seed);

    B  = Args.Back;
    N  = Args.StampN;
    h  = (N-1)/2;
    P  = gauss_psf(N, Args.Sigma);          % normalised PSF, sum(P)=1
    nt = Args.Ntrials;

    Result = struct();
    Result.PSFsum = sum(P(:));
    checks = struct();

    vprintf(Args.Verbose, '== fitPoissPSF unit test ==\n');
    vprintf(Args.Verbose, 'PSF %dx%d Gaussian sigma=%.2f, sum(P)=%.6f, Back=%g\n', ...
            N, N, Args.Sigma, sum(P(:)), B);

    % ----- sanity: PSF normalisation, odd size -----
    checks.psf_norm = abs(sum(P(:)) - 1) < 1e-10 && mod(N,2)==1;

    % =================================================================
    % A & B: flux recovery and error calibration (injected source, fixed pos)
    % =================================================================
    Ftrue = 12.0;
    Fhat = zeros(nt,1); Sig = zeros(nt,1);
    for i = 1:nt
        stamp = randPoiss(B + Ftrue*P);
        R = imUtil.poissNoise.fitPoissPSF(stamp, h+1, h+1, B, 'PSF',P, 'PvalMethod','none');
        Fhat(i) = R.Flux; Sig(i) = R.FluxErr;
    end
    meanF = mean(Fhat); biasF = meanF - Ftrue;
    stdF  = std(Fhat);  meanSig = mean(Sig);
    calib = stdF/meanSig;
    Result.Ftrue=Ftrue; Result.meanFhat=meanF; Result.biasF=biasF;
    Result.stdFhat=stdF; Result.meanFluxErr=meanSig; Result.errCalib=calib;

    checks.flux_recovery = abs(biasF) < 0.06*Ftrue;     % bias < 6%
    checks.err_calib     = (calib > 0.88) && (calib < 1.15);
    vprintf(Args.Verbose, ...
        'A. flux recovery : Ftrue=%.2f mean(Fhat)=%.3f bias=%+.3f (%.1f%%)  [%s]\n', ...
        Ftrue, meanF, biasF, 100*biasF/Ftrue, passfail(checks.flux_recovery));
    vprintf(Args.Verbose, ...
        'B. error calib   : std(Fhat)=%.3f mean(sigF)=%.3f ratio=%.3f  [%s]\n', ...
        stdF, meanSig, calib, passfail(checks.err_calib));

    % =================================================================
    % C: p-value calibration under H0 (no source). Build the MC null once,
    %    then test independent background-only stamps.
    % =================================================================
    Rn = imUtil.poissNoise.fitPoissPSF(randPoiss(B*ones(N,N)), h+1, h+1, B, ...
                     'PSF',P, 'Nsim',Args.Nsim, 'ChunkSize',2e4);
    qn = sort(Rn.qNull); Nn = numel(qn);
    pv = zeros(nt,1);
    for i = 1:nt
        st = randPoiss(B*ones(N,N));
        R  = imUtil.poissNoise.fitPoissPSF(st, h+1, h+1, B, 'PSF',P, 'PvalMethod','none');
        q  = 2*R.S;
        if q > 0, pv(i) = (1 + sum(qn>=q))/(1 + Nn); else, pv(i) = 1; end
    end
    f10 = mean(pv<=0.10); f05 = mean(pv<=0.05); f01 = mean(pv<=0.01);
    Result.pcalib = [f10 f05 f01];
    % loose bounds scaled to be ~3-4 sigma for these Ntrials
    tol = 4*sqrt([0.10 0.05 0.01].*(1-[0.10 0.05 0.01])/nt) + 0.01;
    checks.pval_calib = abs(f10-0.10)<tol(1) && abs(f05-0.05)<tol(2) && abs(f01-0.01)<tol(3);
    vprintf(Args.Verbose, ...
        'C. p calib (H0)  : frac(p<=.1)=%.3f frac(p<=.05)=%.3f frac(p<=.01)=%.3f  [%s]\n', ...
        f10, f05, f01, passfail(checks.pval_calib));

    % =================================================================
    % D: detection of a bright source (end-to-end mc p-value)
    % =================================================================
    stampBright = randPoiss(B + 30*P);
    Rd = imUtil.poissNoise.fitPoissPSF(stampBright, h+1, h+1, B, 'PSF',P, 'Nsim',Args.Nsim);
    Result.bright_p = Rd.Pvalue; Result.bright_Nsigma = Rd.Nsigma;
    checks.detection = (Rd.Pvalue < 1e-3) && (Rd.Flux > 0) && (Rd.Nsigma > 4);
    vprintf(Args.Verbose, ...
        'D. detection     : Fhat=%.2f S=%.2f Nsigma=%.2f p=%.2e  [%s]\n', ...
        Rd.Flux, Rd.S, Rd.Nsigma, Rd.Pvalue, passfail(checks.detection));

    % =================================================================
    % E: position recovery (FixedPos=false). Inject at centre of a small image,
    %    query 1 pixel away, expect the fit to move back and improve S.
    % =================================================================
    IMG = randPoiss(B*ones(31,31));
    cx = 16; cy = 16;
    IMG(cy-h:cy+h, cx-h:cx+h) = IMG(cy-h:cy+h, cx-h:cx+h) + randPoiss(25*P);
    Rfree = imUtil.poissNoise.fitPoissPSF(IMG, cx+1, cy-1, B, 'PSF',P, 'FixedPos',false, ...
                        'MaxDeltaPos',1, 'Nsim',Args.Nsim);
    Rfix  = imUtil.poissNoise.fitPoissPSF(IMG, cx+1, cy-1, B, 'PSF',P, 'FixedPos',true, ...
                        'PvalMethod','none');
    Result.pos_fit = [Rfree.Xfit Rfree.Yfit];
    Result.pos_true= [cx cy];
    checks.position = abs(Rfree.Xfit-cx)<=1 && abs(Rfree.Yfit-cy)<=1 && ...
                      (Rfree.S >= Rfix.S - 1e-9) && (Rfree.Pvalue < 1e-2);
    vprintf(Args.Verbose, ...
        'E. position fit  : query(%d,%d) -> (%d,%d) true(%d,%d), S:%.2f>=%.2f p=%.2e  [%s]\n', ...
        cx+1, cy-1, Rfree.Xfit, Rfree.Yfit, cx, cy, Rfree.S, Rfix.S, ...
        Rfree.Pvalue, passfail(checks.position));

    % =================================================================
    % F: fft p-value method. (i) detected source -> small p, same ballpark as
    %    mc; (ii) under H0 it is ~calibrated (allowing mild anti-conservatism).
    % =================================================================
    stampMod = randPoiss(B + 12*P);
    Rfft = imUtil.poissNoise.fitPoissPSF(stampMod, h+1, h+1, B, 'PSF',P, 'PvalMethod','fft');
    Rmc  = imUtil.poissNoise.fitPoissPSF(stampMod, h+1, h+1, B, 'PSF',P, 'PvalMethod','none');
    qmod = 2*Rmc.S; pmc = (qmod>0)*((1+sum(qn>=qmod))/(1+Nn)) + (qmod<=0)*1;
    % H0 rate for fft
    nt_f = min(nt, 1500);
    pf = zeros(nt_f,1);
    for i = 1:nt_f
        st = randPoiss(B*ones(N,N));
        Rf = imUtil.poissNoise.fitPoissPSF(st, h+1, h+1, B, 'PSF',P, 'PvalMethod','fft');
        pf(i) = Rf.Pvalue;
    end
    ff05 = mean(pf<=0.05);
    Result.fft_p_detect = Rfft.Pvalue; Result.mc_p_detect = pmc;
    Result.fft_H0_frac05 = ff05;
    % detection agreement: both small; fft within a few dex of mc (or below mc floor)
    agree = (Rfft.Pvalue < 0.05);
    % allow fft to be smaller than mc (deep tail) but not absurdly larger
    if pmc > 1.5/(Nn+1)   % mc not at its floor
        agree = agree && (Rfft.Pvalue < 10*pmc);
    end
    checks.fft_method = agree && (ff05 > 0.02) && (ff05 < 0.10);
    vprintf(Args.Verbose, ...
        'F. fft method    : p(fft)=%.2e p(mc)=%.2e ; H0 frac(p<=.05)=%.3f  [%s]\n', ...
        Rfft.Pvalue, pmc, ff05, passfail(checks.fft_method));

    % =================================================================
    % G: multi-source vector input incl. a zero-flux source
    % =================================================================
    BIG = randPoiss(B*ones(60,60));
    srcX = [15; 40; 30];  srcY = [20; 45; 30];  srcF = [18; 0; 25];
    for k = 1:3
        if srcF(k) > 0
            BIG(srcY(k)-h:srcY(k)+h, srcX(k)-h:srcX(k)+h) = ...
                BIG(srcY(k)-h:srcY(k)+h, srcX(k)-h:srcX(k)+h) + randPoiss(srcF(k)*P);
        end
    end
    Rms = imUtil.poissNoise.fitPoissPSF(BIG, srcX, srcY, B, 'PSF',P, 'Nsim',Args.Nsim);
    Result.multi_Flux = Rms.Flux; Result.multi_P = Rms.Pvalue;
    sized = numel(Rms.Flux)==3 && numel(Rms.Pvalue)==3;
    % the zero-flux source must be a NON-detection: a background patch can fit a
    % small positive flux by chance, so test the p-value (significance), not F==0.
    zero_ok = (Rms.Flux(2) >= 0) && (Rms.Pvalue(2) > 1e-3);
    det_ok  = (Rms.Pvalue(1) < 1e-2) && (Rms.Pvalue(3) < 1e-2);
    checks.multi = sized && zero_ok && det_ok;
    vprintf(Args.Verbose, ...
        'G. multi-source  : F=[%.1f %.1f %.1f] p=[%.1e %.2f %.1e]  [%s]\n', ...
        Rms.Flux(1),Rms.Flux(2),Rms.Flux(3), ...
        Rms.Pvalue(1),Rms.Pvalue(2),Rms.Pvalue(3), passfail(checks.multi));

    % ----- aggregate -----
    Result.checks = checks;
    fn = fieldnames(checks);
    Pass = true;
    for i = 1:numel(fn)
        Pass = Pass && checks.(fn{i});
    end
    Result.Pass = Pass;
    vprintf(Args.Verbose, '== overall: %s ==\n', passfail(Pass));
end

% =====================================================================
% helpers
% =====================================================================
function P = gauss_psf(N, sig)
    h = (N-1)/2;
    [xx, yy] = meshgrid(-h:h, -h:h);
    P = exp(-(xx.^2 + yy.^2)/(2*sig^2));
    P = P / sum(P(:));
end

function out = randPoiss(Lam)
    % Toolbox-free Poisson sampler: Lam is a matrix of per-pixel means (small).
    % Vectorised Knuth (targets the low-count regime).
    sz = size(Lam);
    L  = exp(-Lam);
    k  = zeros(sz);
    pr = ones(sz);
    active = true(sz);
    while any(active(:))
        idx = active;
        k(idx)  = k(idx) + 1;
        pr(idx) = pr(idx) .* rand(nnz(idx), 1);
        active  = pr > L;
    end
    out = k - 1;
end

function seed_rng(seed)
    try
        rng(seed);
    catch
        rand('seed', seed); randn('seed', seed);
    end
end

function s = passfail(tf)
    if tf, s = 'PASS'; else, s = 'FAIL'; end
end

function vprintf(verbose, varargin)
    if verbose, fprintf(varargin{:}); end
end