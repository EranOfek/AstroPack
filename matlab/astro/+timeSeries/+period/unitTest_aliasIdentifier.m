function [Pass, Report] = unitTest_aliasIdentifier(Args)
% Numerical unit tests of aliasIdentifier, with a Markdown report.
%   Runs a suite of tests: analytic results of Ofek (2026), invariances,
%   the spectral-window relations, peak selection, sine fits, a Monte
%   Carlo calibration of the misidentification probability, the
%   decisions, the input modes, the plots, the input validation, and
%   timing. Each test runs inside try/catch, so a failure does not stop
%   the suite. Each check records the expected value, the measured value,
%   and the tolerance. The report (Markdown), a .mat file with all
%   results, and the plots are written to Args.OutDir.
% Input  : * ...,key,val,...
%            'OutDir' - Output directory.
%                   Default is fullfile(pwd,'aliasIdentifier_unitTest').
%            'Nsim' - Number of Monte Carlo realizations. Default is 500.
%            'Seed' - Random seed. Default is 1.
%            'SavePlots' - Save the diagnostic plots. Default is true.
%            'KeepPlots' - Leave the four figures of the plotting test open.
%                   Figures created by the other tests are always closed;
%                   figures that existed before the call are never touched.
%                   Default is true.
%            'Verbose' - Print progress. Default is true.
% Output : - True if all tests passed.
%          - Struct array with one element per test.
% Author : Eran Ofek (Oct 2026)
% Example: [Pass, Report] = unitTest_aliasIdentifier;
%          % then send aliasIdentifier_unitTest/report.md

arguments
    Args.OutDir                                          = fullfile(pwd, 'aliasIdentifier_unitTest')
    Args.Nsim       (1,1) {mustBeInteger, mustBePositive} = 500
    Args.Seed       (1,1) {mustBeInteger}                 = 1
    Args.SavePlots  (1,1) logical                         = true
    Args.KeepPlots  (1,1) logical                         = true
    Args.Verbose    (1,1) logical                         = true
end

if ~isfolder(Args.OutDir)
    mkdir(Args.OutDir);
end

Tests = {
    'Exact daily alias, two candidates',          @testExactPair
    'Exact daily alias, three candidates',        @testExactTriple
    'Joint degeneracy in sparse data (paper)',    @testSparse
    'Exact null modes when N < 2K',               @testNullModes
    'Invariance to the time origin',              @testOrigin
    'Invariance to the error scale',              @testErrorScaleInvariance
    'Invariance to the candidate order',          @testOrder
    'Timing jitter: r_min = pi*sigma_t etc.',     @testJitter
    'Window approximation of cos(theta_1)',       @testWindowApprox
    'Within-frequency ratio vs window',           @testWithinFreq
    'Peak selection: Npeak, MinPeak, MinSep',     @testPeakSelection
    'Noiseless fit: amplitude, phase, Dchi2',     @testFitNoiseless
    'Frequency refinement',                       @testRefine
    'Error rescaling with equal weights',         @testErrorRescale
    'Monte Carlo calibration of P_wrong',         @testMonteCarlo
    'Decisions',                                  @testDecisions
    'Coordinate search = exhaustive search',      @testCoordinateSearch
    'Nuisance models',                            @testNuisance
    'Rho and PowerIsChi2 modes',                  @testRhoModes
    'Dropping degenerate candidates',             @testDropDegenerate
    'Input validation (arguments block)',         @testValidation
    'Plots',                                      @testPlots
    'Timing (N=5000, K=10)',                      @testTiming
    };

Ntest  = size(Tests,1);
Report = struct('Name', {}, 'Status', {}, 'Time', {}, 'Checks', {}, 'Message', {}, 'Stack', {}, 'Extra', {});
Ttot   = tic;
for I = 1:Ntest
    rng(Args.Seed + I);
    if Args.Verbose
        fprintf('[%2d/%2d] %-45s ', I, Ntest, Tests{I,1});
    end
    R.Name    = Tests{I,1};
    R.Checks  = [];
    R.Message = '';
    R.Stack   = '';
    R.Extra   = [];
    Before = findobj(0, 'Type', 'figure');
    T0 = tic;
    try
        [R.Checks, R.Extra] = Tests{I,2}(Args);
        if all([R.Checks.Pass])
            R.Status = 'PASS';
        else
            R.Status = 'FAIL';
        end
    catch ME
        R.Status  = 'ERROR';
        R.Message = ME.message;
        R.Stack   = stackText(ME);
    end
    R.Time = toc(T0);
    Report(I) = R;
    if Args.Verbose
        fprintf('%-5s (%.2f s)\n', R.Status, R.Time);
    end
    if ~(Args.KeepPlots && strcmp(R.Name, 'Plots'))
        closeNewFigures(Before);
    end
end
Ttot = toc(Ttot);
Pass = all(strcmp({Report.Status}, 'PASS'));

Env = envInfo();
save(fullfile(Args.OutDir, 'results.mat'), 'Report', 'Env', 'Args');
File = fullfile(Args.OutDir, 'report.md');
writeReport(File, Report, Env, Args, Ttot);
if Args.Verbose
    fprintf('%d/%d tests passed (%.1f s). Report: %s\n', sum(strcmp({Report.Status}, 'PASS')), Ntest, Ttot, File);
end
end


% =====================================================================
% Tests. Each returns a struct array of checks and an optional Extra.
% =====================================================================
function [C, X] = testExactPair(~)
    X  = [];
    T  = (1:120).';  F0 = 0.173;
    I  = timeSeries.period.aliasIdentifier(T, [], [F0; F0+1], 'Plot', false);
    C  = [check('mu_1, mu_2', I.SingularValues(1:2).', sqrt(2)*[1 1], 1e-10, 'abs')
          check('mu_3, mu_4', I.SingularValues(3:4).', [0 0], 1e-10, 'abs')
          check('D_k', I.NullParticipation.', [1 1], 1e-10, 'abs')
          check('C_12', I.CoParticipation(1,2), 1/sqrt(2), 1e-10, 'abs')
          check('cos(theta_1)', I.PairCos1(1,2), 1, 1e-10, 'abs')
          check('n_ind', I.NumIndependent, 1, 1e-12, 'abs')
          check('number of families', I.NumFamilies, 1, 0, 'abs')];
end

function [C, X] = testExactTriple(~)
    X  = [];
    T  = (1:120).';  F0 = 0.173;
    I  = timeSeries.period.aliasIdentifier(T, [], [F0; F0+1; F0+2], 'Plot', false);
    C  = [check('mu_1, mu_2', I.SingularValues(1:2).', sqrt(3)*[1 1], 1e-10, 'abs')
          check('mu_3..mu_6', I.SingularValues(3:6).', [0 0 0 0], 1e-10, 'abs')
          check('D_k', I.NullParticipation.', 4/3*[1 1 1], 1e-10, 'abs')
          check('C_12, C_13, C_23', [I.CoParticipation(1,2), I.CoParticipation(1,3), I.CoParticipation(2,3)], ...
                sqrt(2)/3*[1 1 1], 1e-10, 'abs')
          check('n_ind', I.NumIndependent, 1, 1e-12, 'abs')];
end

function [C, X] = testSparse(~)
    X  = [];
    T  = [2.905 7.914 10.072 17.046 19.069 26.922 31.064 38.078 48.054 57.926].';
    F  = [0.193 0.283 0.405 0.446].';
    I  = timeSeries.period.aliasIdentifier(T, [], F, 'Plot', false);
    R  = I.RelativeSingularValues;
    Pr = I.PairRatio(~eye(4));
    C  = [check('r_min (paper: 0.0055)', R(end), 0.0055, 5e-4, 'abs')
          check('r_{2K-1}/r_min (paper: 75)', R(end-1)/R(end), 75, 2, 'abs')
          check('D_k (paper: 0.18 0.36 0.17 0.28)', I.NullParticipation.', [0.18 0.36 0.17 0.28], 0.01, 'abs')
          check('min pairwise r_kl (paper: 0.48)', min(Pr), 0.48, 0.01, 'abs')
          check('max pairwise r_kl (paper: 0.67)', max(Pr), 0.67, 0.01, 'abs')
          check('number of families', I.NumFamilies, 1, 0, 'abs')];
end

function [C, X] = testNullModes(~)
    X  = [];
    T  = [2.905 7.914 10.072 17.046 19.069 26.922 31.064].';
    F  = [0.193 0.283 0.405 0.446].';
    I  = timeSeries.period.aliasIdentifier(T, [], F, 'Plot', false);
    C  = [check('number of singular values', numel(I.SingularValues), 8, 0, 'abs')
          check('number of r_j < 1e-10', sum(I.RelativeSingularValues < 1e-10), 2, 0, 'abs')
          check('size of V', size(I.RightSingularVectors), [8 8], 0, 'abs')
          check('n_ind', I.NumIndependent, 3, 1e-12, 'abs')];
end

function [C, X] = testOrigin(~)
    X  = [];
    T  = sort(rand(200,1))*500;  E = 0.5 + rand(200,1);  F = [0.21; 0.49; 0.79; 1.21];
    A  = timeSeries.period.aliasIdentifier(T, E, F, 'Plot', false);
    B  = timeSeries.period.aliasIdentifier(T + 123.456, E, F, 'Plot', false);
    J  = timeSeries.period.aliasIdentifier(T + 2460000.5, E, F, 'Plot', false);
    C  = [check('max|dmu|, shift 123.456', max(abs(A.SingularValues - B.SingularValues)), 1e-10, [], 'lt')
          check('max|dcos1|, shift 123.456', max(abs(A.PairCos1(:) - B.PairCos1(:))), 1e-10, [], 'lt')
          check('max|dD_k|, shift 123.456', max(abs(A.NullParticipation - B.NullParticipation)), 1e-10, [], 'lt')
          check('max|dmu|, shift 2460000.5 (JD rounding)', max(abs(A.SingularValues - J.SingularValues)), 1e-8, [], 'lt')
          check('max|dcos1|, shift 2460000.5', max(abs(A.PairCos1(:) - J.PairCos1(:))), 1e-8, [], 'lt')];
end

function [C, X] = testErrorScaleInvariance(~)
    X  = [];
    T  = sort(rand(300,1))*400;  E = 0.3 + rand(300,1);  F = [0.11; 0.37; 0.63; 1.37];
    A  = timeSeries.period.aliasIdentifier(T, E, F, 'Plot', false);
    B  = timeSeries.period.aliasIdentifier(T, 3*E, F, 'Plot', false);
    U  = timeSeries.period.aliasIdentifier(T, [], F, 'Plot', false);
    C  = [check('max|dmu|, errors x3', max(abs(A.SingularValues - B.SingularValues)), 1e-12, [], 'lt')
          check('max|dcos1|, errors x3', max(abs(A.PairCos1(:) - B.PairCos1(:))), 1e-12, [], 'lt')
          check('weights matter: max|dmu| weighted vs unweighted', max(abs(A.SingularValues - U.SingularValues)), 1e-3, [], 'gt')];
end

function [C, X] = testOrder(~)
    X  = [];
    T  = sort(rand(150,1))*200;  F = [0.12; 1.12; 0.88; 0.31; 0.33; 0.71];
    P  = [4 2 6 1 5 3];
    A  = timeSeries.period.aliasIdentifier(T, [], F, 'Plot', false);
    B  = timeSeries.period.aliasIdentifier(T, [], F(P), 'Plot', false);
    CoA = A.Families == A.Families.';
    CoB = B.Families == B.Families.';
    C  = [check('max|dmu| after permutation', max(abs(A.SingularValues - B.SingularValues)), 1e-12, [], 'lt')
          check('same family partition', isequal(CoA(P,P), CoB), true, 0, 'eq')
          check('max|dcos1| after permutation', max(max(abs(A.PairCos1(P,P) - B.PairCos1))), 1e-12, [], 'lt')];
end

function [C, X] = testJitter(~)
    Sig = 0.03;  F0 = 0.173;
    T   = (1:4000).' + Sig*randn(4000,1);
    I2  = timeSeries.period.aliasIdentifier(T, [], [F0; F0+1], 'Plot', false);
    I3  = timeSeries.period.aliasIdentifier(T, [], [F0; F0+1; F0+2], 'Plot', false);
    R2  = I2.RelativeSingularValues(end);
    R3  = I3.RelativeSingularValues(end);
    X   = struct('r2', R2, 'r3', R3, 'sin1', I2.PairSin1(1,2));
    C   = [check('r_min/sqrt(tanh(pi^2 sig^2)), K=2', R2/sqrt(tanh(pi^2*Sig^2)), 1, 0.05, 'abs')
           check('sin(theta_1)/(2 pi sig)', I2.PairSin1(1,2)/(2*pi*Sig), 1, 0.05, 'abs')
           check('r_min/[(4/3)(pi sig)^2], K=3', R3/(4/3*(pi*Sig)^2), 1, 0.10, 'abs')
           check('pairwise r_12 unchanged by f0+2', I3.PairRatio(1,2)/I2.PairRatio(1,2), 1, 0.02, 'abs')];
end

function [C, X] = testWindowApprox(~)
    X   = [];
    Sig = 0.1;  F0 = 0.173;
    T   = (1:400).' + Sig*randn(400,1);
    I   = timeSeries.period.aliasIdentifier(T, [], [F0; F0+1], 'Plot', false);
    % tolerances reflect the N^-1/2 fluctuations of the window for N=400
    C   = [check('cos1 vs exp(-2 pi^2 sig^2)', I.PairCos1(1,2), exp(-2*pi^2*Sig^2), 0.05, 'abs')
           check('|WindowCos1 - PairCos1|', abs(I.WindowCos1(1,2) - I.PairCos1(1,2)), 0.06, [], 'lt')
           check('|WindowCos2 - PairCos2|', abs(I.WindowCos2(1,2) - I.PairCos2(1,2)), 0.08, [], 'lt')];
end

function [C, X] = testWithinFreq(~)
    X   = [];
    T   = (1:300).' + 0.03*randn(300,1);
    I   = timeSeries.period.aliasIdentifier(T, [], [0.49; 0.2], 'Plot', false);
    W2  = abs(mean(exp(-2i*pi*2*0.49*T)));
    Exp = sqrt((1 - W2)/(1 + W2));
    C   = [check('r_in(0.49) vs window formula', I.WithinFrequencyRatio(1), Exp, 0.02, 'abs')
           check('r_in(0.2) close to 1', I.WithinFrequencyRatio(2), 0.9, [], 'gt')];
end

function [C, X] = testPeakSelection(~)
    X    = [];
    T    = sort(rand(200,1))*100;                     % 2/range(T) ~ 0.02
    F    = (0:1e-3:1).';
    Cen  = [0.10 0.30 0.50 0.51 0.70 0.90];
    Hgt  = [5    9    7    6    3    8];
    P    = zeros(size(F));
    for K = 1:numel(Cen)
        P = P + Hgt(K)*exp(-0.5*((F - Cen(K))/0.002).^2);
    end
    A  = timeSeries.period.aliasIdentifier(T, [], F, P, 'Npeak', 3, 'Plot', false);
    B  = timeSeries.period.aliasIdentifier(T, [], F, P, 'Npeak', 3, 'MinPeak', 4, 'Plot', false);
    D  = timeSeries.period.aliasIdentifier(T, [], F, P, 'MinPeak', 4, 'MinSep', 0.005, 'Plot', false);
    C  = [check('Npeak=3', sort(A.Freq).', [0.30 0.50 0.90], 1e-9, 'abs')
          check('MinPeak=4 overrides Npeak (0.51 within MinSep)', sort(B.Freq).', [0.10 0.30 0.50 0.90], 1e-9, 'abs')
          check('MinPeak=4, MinSep=0.005', sort(D.Freq).', [0.10 0.30 0.50 0.51 0.90], 1e-9, 'abs')
          check('PeakPower of Npeak=3 (sorted by freq)', sortBy(A.PeakPower, A.Freq).', [9 7 8], 1e-3, 'abs')];
end

function [C, X] = testFitNoiseless(~)
    X  = [];
    T  = sort(rand(300,1))*200;  E = 0.2*ones(300,1);
    F  = 0.3713;  A = 2.5;  Ph = 0.7;
    Y  = A*cos(2*pi*F*T - Ph);
    I  = timeSeries.period.aliasIdentifier(T, E, [F; 1-F; 1+F], [], 'Mag', Y, 'RefTime', 0, 'RefineFreq', false, 'Plot', false);
    Sw = 1./E;  Yw = Y.*Sw;
    Exp = sum((Yw - Sw*(Sw.'*Yw)/(Sw.'*Sw)).^2);   % Delta chi^2 after fitting a constant
    C  = [check('Amp', I.Fit.Amp(1), A, 1e-8, 'abs')
          check('Phase', I.Fit.Phase(1), Ph, 1e-8, 'abs')
          check('DeltaChi2 = |projected signal|^2', I.Fit.DeltaChi2(1)/Exp, 1, 1e-10, 'abs')
          check('ErrorScale (errors given)', I.Fit.ErrorScale, 1, 0, 'abs')
          check('best member', I.Family(1).BestFreq, F, 1e-12, 'abs')];
end

function [C, X] = testRefine(~)
    % The refined frequency must be the local chi^2 minimum: compare with a
    % brute-force fine grid (noisy data) and with the true frequency
    % (noiseless data). Noise shifts the minimum by ~1/(T*rho), so the
    % noisy case is not compared with the true frequency.
    X  = [];
    T  = sort(rand(300,1))*200;  Tspan = max(T) - min(T);  E = 0.2*ones(300,1);
    F  = 0.3713;
    Y0 = 2.5*cos(2*pi*F*T - 0.7);
    Y  = Y0 + E.*randn(300,1);
    I  = timeSeries.period.aliasIdentifier(T, E, F + 0.2/Tspan, [], 'Mag', Y, 'Plot', false);
    G  = F + (-0.3:1e-4:0.6).'/Tspan;
    [~, Im] = max(periodogramChi2(T, E, Y, G));
    I0 = aliasIdentifier(T, E, F + 0.2/Tspan, [], 'Mag', Y0, 'Plot', false);
    C  = [check('noisy: |refined f - grid maximum|*T', abs(I.Freq - G(Im))*Tspan, 2e-3, [], 'lt')
          check('noiseless: |refined f - f|*T', abs(I0.Freq - F)*Tspan, 1e-3, [], 'lt')];
end

function [C, X] = testErrorRescale(~)
    X   = [];
    T   = sort(rand(500,1))*300;  Sig = 0.5;  F = 0.2371;
    Y   = 1.0*cos(2*pi*F*T) + Sig*randn(500,1);
    I   = timeSeries.period.aliasIdentifier(T, [], [F; 1-F], [], 'Mag', Y, 'Plot', false);
    J   = timeSeries.period.aliasIdentifier(T, Sig*ones(500,1), [F; 1-F], [], 'Mag', Y, 'Plot', false);
    C   = [check('ErrorScale vs true sigma', I.Fit.ErrorScale/Sig, 1, 0.1, 'abs')
           check('RescaledErrors flag (MagErr empty)', I.Fit.RescaledErrors, true, 0, 'eq')
           check('DeltaChi2 equal-weights vs true errors', I.Fit.DeltaChi2(1)/J.Fit.DeltaChi2(1), 1, 0.2, 'abs')];
end

function [C, X] = testMonteCarlo(Args)
    % Signal at f0, alias at f0+1, timing jitter 0.03 d, random phase.
    % Compare the rate of preferring the alias with the prediction of the
    % paper (Section 3.2), and the distribution of Delta chi^2.
    N    = 150;  F0 = 0.173;  Rho = 14;
    T    = (1:N).' + 0.03*randn(N,1);
    E    = ones(N,1);
    Amp  = Rho*sqrt(2/N);
    Cand = [F0; F0+1];
    Info0 = timeSeries.period.aliasIdentifier(T, E, Cand, 'Plot', false);
    S2    = 2 - Info0.PairCos1(1,2)^2 - Info0.PairCos2(1,2)^2;
    Nsim  = Args.Nsim;
    Wrong = false(Nsim,1);  Ppred = zeros(Nsim,1);  Z = zeros(Nsim,1);
    for I = 1:Nsim
        Ph  = 2*pi*rand;
        Sgn = Amp*cos(2*pi*F0*T + Ph);
        In  = timeSeries.period.aliasIdentifier(T, E, Cand, [], 'Mag', Sgn, 'RefineFreq', false, 'Plot', false);
        D2  = In.Fit.DeltaChi2(1) - In.Fit.DeltaChi2(2);        % noiseless: rho^2 (1 - |Q_l' u|^2)
        Sd  = 2*sqrt(D2 + S2);
        Ppred(I) = normCdf(-D2/Sd);
        Iy  = timeSeries.period.aliasIdentifier(T, E, Cand, [], 'Mag', Sgn + randn(N,1), 'RefineFreq', false, 'Plot', false);
        Obs = Iy.Fit.DeltaChi2(1) - Iy.Fit.DeltaChi2(2);        % chi2(alias) - chi2(true)
        Wrong(I) = Obs < 0;
        Z(I) = (Obs - D2)/Sd;
    end
    Rate = mean(Wrong);
    Pm   = mean(Ppred);
    Se   = sqrt(Pm*(1 - Pm)/Nsim);
    X    = struct('Rate', Rate, 'Predicted', Pm, 'BinomialSE', Se, 'MeanZ', mean(Z), 'StdZ', std(Z), ...
                  'sin1', Info0.PairSin1(1,2), 'Nsim', Nsim);
    C    = [check(sprintf('alias rate vs predicted (Nsim=%d)', Nsim), Rate, Pm, 3*Se + 0.02, 'abs')
            check('mean of z=(obs-pred)/sd', mean(Z), 0, 4/sqrt(Nsim) + 0.05, 'abs')
            check('std of z', std(Z), 1, 0.1, 'abs')];
end

function [C, X] = testDecisions(~)
    X  = [];
    T  = groundTimes(3);  N = numel(T);  E = ones(N,1);
    Fa = 0.3713;  Fb = 0.0613;  Ca = [Fa; 1-Fa; 1+Fa];
    % strong signal: unique and correct
    Is = timeSeries.period.aliasIdentifier(T, E, Ca, [], 'Mag', 1.2*cos(2*pi*Fa*T + 1) + randn(N,1), 'Plot', false);
    % weak signal: not unique
    Iw = timeSeries.period.aliasIdentifier(T, E, Ca, [], 'Mag', 0.15*cos(2*pi*Fa*T + 1) + randn(N,1), 'Plot', false);
    % noise only, fixed candidates: not significant
    In = timeSeries.period.aliasIdentifier(T, E, [0.21; 0.47; 0.83], [], 'Mag', randn(N,1), 'SignifPvalue', 1e-4, 'Plot', false);
    % two signals in two families
    Y2 = 1.0*cos(2*pi*Fa*T + 1) + 0.8*sin(2*pi*Fb*T) + randn(N,1);
    I2 = timeSeries.period.aliasIdentifier(T, E, [Ca; Fb; 1-Fb; 1+Fb], [], 'Mag', Y2, 'Plot', false);
    Co = I2.Families == I2.Families.';
    CoExp = blkdiag(ones(3), ones(3)) == 1;
    % real signals at f and at its mirror alias, adding constructively
    Ye = 1.5*cos(2*pi*Fa*T + 1) + 1.5*cos(2*pi*(1-Fa)*T - 1) + randn(N,1);
    Ie = timeSeries.period.aliasIdentifier(T, E, Ca, [], 'Mag', Ye, 'Plot', false);
    C  = [check('strong: decision', Is.Family(1).Decision, 'unique', 0, 'eq')
          check('strong: best member', Is.Family(1).BestFreq, Fa, 1e-3, 'abs')
          check('weak: decision is not unique', ~strcmp(Iw.Family(1).Decision, 'unique'), true, 0, 'eq')
          check('noise: all families not significant', all(strcmp({In.Family.Decision}, 'not significant')), true, 0, 'eq')
          check('noise: best model is empty', numel(In.Fit.BestModel), 0, 0, 'abs')
          check('two signals: family partition', isequal(Co, CoExp), true, 0, 'eq')
          check('two signals: best model', sort(I2.Fit.BestModelFreq).', [Fb Fa], 1e-3, 'abs')
          check('f and 1-f: additional signal flagged', ~isempty(strfind(Ie.Family(1).Decision, 'additional')), true, 0, 'eq')];
end

function [C, X] = testCoordinateSearch(~)
    X  = [];
    T  = groundTimes(3);  N = numel(T);  E = ones(N,1);
    Fa = 0.3713;  Fb = 0.0613;  Fc = 0.2211;
    Cand = [Fa; 1-Fa; 1+Fa; Fb; 1-Fb; 1+Fb; Fc; 1-Fc; 1+Fc];
    Y  = 0.8*cos(2*pi*Fa*T) + 0.6*sin(2*pi*Fb*T) + 0.5*cos(2*pi*Fc*T + 2) + randn(N,1);
    A  = timeSeries.period.aliasIdentifier(T, E, Cand, [], 'Mag', Y, 'RefineFreq', false, 'Plot', false);
    B  = timeSeries.period.aliasIdentifier(T, E, Cand, [], 'Mag', Y, 'RefineFreq', false, 'MaxModels', 1, 'Plot', false);
    C  = [check('same best model', isequal(sort(A.Fit.BestModel), sort(B.Fit.BestModel)), true, 0, 'eq')
          check('same chi2 of best model', B.Fit.Chi2Best/A.Fit.Chi2Best, 1, 1e-10, 'abs')];
end

function [C, X] = testNuisance(~)
    X  = [];
    T  = sort(rand(400,1))*300;  E = 0.5*ones(400,1);  F = 0.2371;  A = 0.6;
    Y  = 0.01*T + A*cos(2*pi*F*T) + E.*randn(400,1);
    Il = timeSeries.period.aliasIdentifier(T, E, [F; 1-F], [], 'Mag', Y, 'Nuisance', 'linear', 'RefineFreq', false, 'Plot', false);
    Im = timeSeries.period.aliasIdentifier(T, E, [F; 1-F], [], 'Mag', Y, 'Nuisance', T, 'RefineFreq', false, 'Plot', false);
    Iq = timeSeries.period.aliasIdentifier(T, E, [F; 1-F], [], 'Mag', Y, 'Nuisance', 'quad', 'RefineFreq', false, 'Plot', false);
    SeA = 0.5*sqrt(2/400);
    C  = [check('linear: Amp within 5 SE', abs(Il.Fit.Amp(1) - A)/SeA, 5, [], 'lt')
          check('''linear'' = matrix [T]: DeltaChi2', Im.Fit.DeltaChi2(1)/Il.Fit.DeltaChi2(1), 1, 1e-8, 'abs')
          check('NumNuisance const/linear/quad', [aliasIdentifier(T, E, F, 'Plot', false).NumNuisance, ...
                Il.NumNuisance, Iq.NumNuisance], [1 2 3], 0, 'abs')];
end

function [C, X] = testRhoModes(~)
    X   = [];
    T   = sort(rand(300,1))*200;  F = [0.31; 0.69; 1.31];  Rho = [12; 8; 5];
    I   = timeSeries.period.aliasIdentifier(T, [], F, 'Rho', Rho, 'Plot', false);
    Exp = normCdf(-0.5*Rho.*I.PairSin1);
    M   = ~eye(3);
    Fg  = (0.2:1e-3:1.4).';
    Pw  = 2 + 100*exp(-0.5*((Fg - 0.31)/0.002).^2) + 50*exp(-0.5*((Fg - 0.69)/0.002).^2);
    J   = timeSeries.period.aliasIdentifier(T, [], Fg, Pw, 'Npeak', 2, 'PowerIsChi2', true, 'Plot', false);
    C   = [check('MisidentificationWorst = Phi(-rho sin1/2)', max(abs(I.MisidentificationWorst(M) - Exp(M))), 1e-12, [], 'lt')
           check('PowerIsChi2: Rho = sqrt(Power-2)', sortBy(J.Rho, J.Freq).', [10 sqrt(50)], 1e-6, 'abs')];
end

function [C, X] = testDropDegenerate(~)
    X  = [];
    T  = (1:100).';
    S  = warning('on', 'aliasIdentifier:dropped');     % lastwarn is not set for disabled warnings
    lastwarn('');
    Txt = evalc('I = aliasIdentifier(T, [], [0.173; 1.0], ''Plot'', false);'); %#ok<NASGU>  (hides the intentional warning)
    [~, Id] = lastwarn;
    warning(S);
    C  = [check('remaining candidates', I.Freq.', 0.173, 1e-12, 'abs')
          check('warning id', Id, 'aliasIdentifier:dropped', 0, 'eq')];
end

function [C, X] = testValidation(~)
    X  = [];
    T  = sort(rand(50,1))*100;  F = [0.1; 0.3];
    C  = [check('Power omitted (3 inputs)', runs(@() closeFigures(timeSeries.period.aliasIdentifier(T, [], F))), true, 0, 'eq')
          check('Power = [] with key/val', runs(@() timeSeries.period.aliasIdentifier(T, [], F, [], 'Plot', false)), true, 0, 'eq')
          check('unknown key errors', ~runs(@() timeSeries.period.aliasIdentifier(T, [], F, [], 'Foo', 1, 'Plot', false)), true, 0, 'eq')
          check('Npeak=2.5 errors', ~runs(@() timeSeries.period.aliasIdentifier(T, [], F, [], 'Npeak', 2.5, 'Plot', false)), true, 0, 'eq')
          check('MagErr size mismatch errors', ~runs(@() timeSeries.period.aliasIdentifier(T, ones(3,1), F, [], 'Plot', false)), true, 0, 'eq')
          check('Freq/Power size mismatch errors', ~runs(@() timeSeries.period.aliasIdentifier(T, [], F, [1 2 3], 'Plot', false)), true, 0, 'eq')
          check('Mag size mismatch errors', ~runs(@() timeSeries.period.aliasIdentifier(T, [], F, [], 'Mag', 1:3, 'Plot', false)), true, 0, 'eq')];
end

function [C, X] = testPlots(Args)
    % Four separate figures, with and without the measurements.
    X  = struct('Files', {{}});
    T  = groundTimes(3);  N = numel(T);  E = 0.8 + 0.7*rand(N,1);
    Y  = 0.9*cos(2*pi*0.3713*T + 1) + 0.7*sin(2*pi*0.0613*T) + E.*randn(N,1);
    F  = (0.005:5e-5:2.5).';
    P  = periodogramChi2(T, E, Y, F);
    Names = {'candidates', 'coupling_matrix', 'coupling_to_best', 'singular_values'};
    I  = timeSeries.period.aliasIdentifier(T, E, F, P, 'Mag', Y, 'Npeak', 10, 'Plot', true);
    J  = timeSeries.period.aliasIdentifier(T, E, F, P, 'Npeak', 10, 'Plot', true);
    if Args.SavePlots
        for K = 1:numel(I.Figures)
            X.Files{end+1} = savePlot(Args.OutDir, sprintf('plot_two_signals_%d_%s.png', K, Names{K}), I.Figures(K));
        end
        for K = 1:numel(J.Figures)
            X.Files{end+1} = savePlot(Args.OutDir, sprintf('plot_times_only_%d_%s.png', K, Names{K}), J.Figures(K));
        end
    end
    C  = [check('four figures (with Mag)', numel(I.Figures), 4, 0, 'abs')
          check('four figures (times only)', numel(J.Figures), 4, 0, 'abs')
          check('figures are valid handles', all(ishghandle([I.Figures(:); J.Figures(:)])), true, 0, 'eq')
          check('no figures when Plot=false', isempty(timeSeries.period.aliasIdentifier(T, E, [0.3713; 0.6287], 'Plot', false).Figures), true, 0, 'eq')
          check('at least two families (two signals)', I.NumFamilies >= 2, true, 0, 'eq')];
    closeFigures(J);                    % keep only the figures of the fit (closed by the caller unless KeepPlots)
end

function [C, X] = testTiming(~)
    T  = sort(rand(5000,1))*1000;  E = ones(5000,1);
    F  = sort(0.05 + 2*rand(10,1));
    Y  = cos(2*pi*F(3)*T) + randn(5000,1);
    T0 = tic;
    timeSeries.period.aliasIdentifier(T, E, F, [], 'Mag', Y, 'Plot', false);
    Dt = toc(T0);
    X  = struct('Seconds', Dt);
    C  = check('run time [s] (N=5000, K=10, with fits)', Dt, 60, [], 'lt');
end


% =====================================================================
% Helpers
% =====================================================================
function C = check(Quantity, Measured, Expected, Tol, Mode)
    % One check: Mode is 'abs' (|m-e|<=tol), 'lt' (m<e), 'gt' (m>e), or 'eq'
    switch Mode
        case 'abs'
            Ok  = isequal(size(Measured(:)), size(Expected(:))) && all(abs(Measured(:) - Expected(:)) <= Tol);
            Ex  = sprintf('%s +- %s', fmt(Expected), fmt(Tol));
        case 'lt'
            Ok  = all(Measured(:) < Expected);
            Ex  = sprintf('< %s', fmt(Expected));
        case 'gt'
            Ok  = all(Measured(:) > Expected);
            Ex  = sprintf('> %s', fmt(Expected));
        case 'eq'
            Ok  = isequal(Measured, Expected);
            Ex  = fmt(Expected);
        otherwise
            error('unknown check mode %s', Mode);
    end
    C = struct('Quantity', Quantity, 'Expected', Ex, 'Measured', fmt(Measured), 'Pass', logical(Ok));
end

function S = fmt(V)
    % Compact text for a value
    if ischar(V)
        S = V;
    elseif islogical(V)
        if isscalar(V)
            S = mat2str(V);
        else
            S = mat2str(double(V));
        end
    elseif isempty(V)
        S = '[]';
    elseif isscalar(V)
        S = num2str(V, 6);
    else
        S = mat2str(V, 5);
    end
end

function Ok = runs(Fun)
    % True if Fun() runs without error
    try
        Fun();
        Ok = true;
    catch
        Ok = false;
    end
end

function V = sortBy(V, Key)
    [~, I] = sort(Key);
    V = V(I);
end

function P = normCdf(X)
    P = 0.5*erfc(-X./sqrt(2));
end

function T = groundTimes(Nseason)
    % Ground-based sampling: 240-night seasons, 60% clear, 1-2 epochs per night
    T = [];
    for S = 0:Nseason-1
        Night = S*365 + (0:239).';
        Night = Night(rand(240,1) < 0.6);
        Two   = rand(size(Night)) < 0.3;
        T = [T; Night + 0.24*(rand(size(Night)) - 0.5); Night(Two) + 0.24*(rand(sum(Two),1) - 0.5)]; %#ok<AGROW>
    end
    T = sort(T);
end

function P = periodogramChi2(T, E, Y, F)
    % Delta chi^2 periodogram with a floating mean
    Sw = 1./E;
    Qc = Sw/norm(Sw);
    Yt = Y.*Sw;  Yt = Yt - Qc*(Qc.'*Yt);
    P  = zeros(size(F));
    for K = 1:numel(F)
        X = [cos(2*pi*F(K)*T), sin(2*pi*F(K)*T)].*Sw;
        X = X - Qc*(Qc.'*X);
        B = X.'*Yt;
        P(K) = B.'*((X.'*X)\B);
    end
end

function File = savePlot(Dir, Name, Fig)
    File = fullfile(Dir, Name);
    set(Fig, 'PaperPositionMode', 'manual', 'PaperUnits', 'inches', 'PaperPosition', [0 0 7 5]);
    print(Fig, '-dpng', '-r100', File);
end

function closeFigures(Info)
    % Close the figures returned by aliasIdentifier
    if isstruct(Info) && isfield(Info, 'Figures') && ~isempty(Info.Figures)
        H = Info.Figures(ishghandle(Info.Figures));
        if ~isempty(H)
            close(H);
        end
    end
end

function closeNewFigures(Before)
    % Close the figures that did not exist before (never the user's figures)
    After = findobj(0, 'Type', 'figure');
    for K = 1:numel(After)
        if ~any(After(K) == Before)
            close(After(K));
        end
    end
end

function S = stackText(ME)
    % Error stack as text
    S = '';
    for K = 1:numel(ME.stack)
        S = sprintf('%s    at %s (line %d)\n', S, ME.stack(K).name, ME.stack(K).line);
    end
end

function Env = envInfo()
    Env.Date     = datestr(now, 'yyyy-mm-dd HH:MM:SS');
    Env.Version  = version;
    Env.Computer = computer;
    Env.Path     = which('aliasIdentifier');
    D = dir(Env.Path);
    if isempty(D)
        Env.FileDate  = '';
        Env.FileBytes = NaN;
    else
        Env.FileDate  = D(1).date;
        Env.FileBytes = D(1).bytes;
    end
end

function writeReport(File, Report, Env, Args, Ttot)
    % Markdown report
    Fid = fopen(File, 'w');
    if Fid < 0
        error('Cannot open %s for writing', File);
    end
    Status = {Report.Status};
    fprintf(Fid, '# aliasIdentifier unit-test report\n\n');
    fprintf(Fid, '- Date: %s\n- MATLAB: %s (%s)\n', Env.Date, Env.Version, Env.Computer);
    fprintf(Fid, '- aliasIdentifier: `%s` (%s, %d bytes)\n', Env.Path, Env.FileDate, Env.FileBytes);
    fprintf(Fid, '- Settings: Nsim = %d, Seed = %d\n', Args.Nsim, Args.Seed);
    fprintf(Fid, '- Result: **%d passed, %d failed, %d errors** of %d tests (%.1f s)\n\n', ...
            sum(strcmp(Status, 'PASS')), sum(strcmp(Status, 'FAIL')), sum(strcmp(Status, 'ERROR')), numel(Report), Ttot);

    fprintf(Fid, '## Summary\n\n| # | Test | Status | Time [s] |\n|---|---|---|---|\n');
    for I = 1:numel(Report)
        fprintf(Fid, '| %d | %s | %s | %.2f |\n', I, md(Report(I).Name), Report(I).Status, Report(I).Time);
    end

    fprintf(Fid, '\n## Checks\n\n| # | Test | Check | Expected | Measured | OK |\n|---|---|---|---|---|---|\n');
    for I = 1:numel(Report)
        for K = 1:numel(Report(I).Checks)
            Ck = Report(I).Checks(K);
            fprintf(Fid, '| %d | %s | %s | %s | %s | %s |\n', I, md(Report(I).Name), md(Ck.Quantity), ...
                    md(Ck.Expected), md(Ck.Measured), yesNo(Ck.Pass));
        end
    end

    Bad = find(~strcmp(Status, 'PASS'));
    fprintf(Fid, '\n## Failures and errors\n\n');
    if isempty(Bad)
        fprintf(Fid, 'None.\n');
    end
    for I = Bad
        fprintf(Fid, '### %d. %s (%s)\n\n', I, Report(I).Name, Report(I).Status);
        if strcmp(Report(I).Status, 'ERROR')
            fprintf(Fid, '```\n%s\n%s```\n\n', Report(I).Message, Report(I).Stack);
        else
            for K = find(~[Report(I).Checks.Pass])
                Ck = Report(I).Checks(K);
                fprintf(Fid, '- %s: expected %s, measured %s\n', Ck.Quantity, Ck.Expected, Ck.Measured);
            end
            fprintf(Fid, '\n');
        end
    end

    fprintf(Fid, '\n## Details\n\n');
    for I = 1:numel(Report)
        if ~isempty(Report(I).Extra) && isstruct(Report(I).Extra)
            Fn = fieldnames(Report(I).Extra);
            Txt = cell(1, numel(Fn));
            for K = 1:numel(Fn)
                V = Report(I).Extra.(Fn{K});
                if iscell(V)
                    Txt{K} = sprintf('%s = %s', Fn{K}, strjoin(V, ', '));
                else
                    Txt{K} = sprintf('%s = %s', Fn{K}, fmt(V));
                end
            end
            fprintf(Fid, '- %s: %s\n', Report(I).Name, strjoin(Txt, '; '));
        end
    end
    fclose(Fid);
end

function S = md(S)
    % Escape characters that break a Markdown table
    S = strrep(S, '|', '/');
    S = strrep(S, sprintf('\n'), ' ');
end

function S = yesNo(B)
    if B
        S = 'yes';
    else
        S = '**NO**';
    end
end
