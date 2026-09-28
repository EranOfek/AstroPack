function Result = unitTest_poissThreshold(Args)
    % Unit test for the Poisson-noise false-alarm threshold functions.
    % Description: Test imUtil.poissNoise.poissThresholdSimulations,
    %              imUtil.poissNoise.poissThresholdFFT and
    %              imUtil.poissNoise.poissThresholdPanjer:
    %              1. Top-hat kernel: S = N ~ Poisson(Lambda), so the FFT and
    %                 Panjer PMFs must equal the Poisson PMF.
    %              2. Optimal Poisson matched filter K = log(1+F*P/B) (as in
    %                 Fig. 2 of Soumagnac et al.): FFT, Panjer and simulations
    %                 must give consistent thresholds, and a figure of the
    %                 false-alarm probability vs. S is produced (similar to the
    %                 bottom-right panel of Fig. 2).
    %              3. Kernel with negative values (Mexican hat): the FFT must
    %                 agree with simulations, and Panjer must throw an error.
    %              4. Spatially varying background: FFT, Panjer and
    %                 simulations must agree.
    % Input  : * Arbitrary number of pairs of arguments: ...,keyword,value,...
    %            where keyword are one of the followings:
    %            'Nsim'      - Number of simulations for test 2.
    %                          Default is 1e7.
    %            'NsimOther' - Number of simulations for tests 3 and 4.
    %                          Default is 1e6.
    %            'Gamma'     - False-alarm probability used for the threshold
    %                          in the figure and in the comparison with
    %                          simulations. Default is 3.1671e-5 (4 sigma).
    %            'Back'      - Background [counts/pixel]. Default is 7.3e-3.
    %            'Flux'      - Flux of the matched filter. Default is 3.
    %            'PSFSigma'  - Gaussian PSF sigma [pix]. Default is 1.
    %            'StampSize' - PSF stamp size [pix]. Default is 5.
    %            'RelTol'    - Relative tolerance on the thresholds between
    %                          simulations and the analytical methods.
    %                          Default is 0.03.
    %            'Plot'      - Produce the figure. Default is true.
    %            'Seed'      - Random number seed. Default is 1.
    % Output : - Result : true if all tests passed (otherwise an error is
    %                     thrown).
    % Author : Claude + Eran Ofek (Sep 2026)
    % Example: Result = unitTest_poissThreshold;
    %          Result = unitTest_poissThreshold('Nsim',1e6);

    arguments
        Args.Nsim          = 1e7
        Args.NsimOther     = 1e6
        Args.Gamma         = 3.1671e-5
        Args.Back          = 7.3e-3
        Args.Flux          = 3
        Args.PSFSigma      = 1
        Args.StampSize     = 5
        Args.RelTol        = 0.03
        Args.Plot logical  = true
        Args.Seed          = 1
    end

    %% Test 1: top-hat kernel -> S ~ Poisson(Lambda)
    Kth    = ones(5,5);
    Bth    = 0.2;
    Lambda = numel(Kth).*Bth;
    [~, Rf] = imUtil.poissNoise.poissThresholdFFT(Kth, Bth, 'DeltaS',1, 'Validate',false);
    [~, Rp] = imUtil.poissNoise.poissThresholdPanjer(Kth, Bth, 'DeltaS',1, 'Validate',false);
    N      = (0:min([40, max(Rf.S), max(Rp.S)])).';
    Ppois  = exp(N.*log(Lambda) - Lambda - gammaln(N + 1));
    [~, If] = ismember(N, round(Rf.S));
    Pfft   = Rf.PMF(If);
    Ppan   = Rp.PMF(N + 1);
    assert(max(abs(Pfft - Ppois)) < 1e-14, 'FFT PMF does not match Poisson for a top-hat kernel');
    Fn     = Ppois > 1e-250;
    assert(max(abs(Ppan(Fn)./Ppois(Fn) - 1)) < 1e-10, 'Panjer PMF does not match Poisson for a top-hat kernel');

    %% Test 2: optimal Poisson matched filter (Fig. 2)
    Half   = (Args.StampSize - 1)./2;
    [X, Y] = meshgrid(-Half:Half);
    PSF    = exp(-((X - 0.3).^2 + (Y + 0.2).^2)./(2.*Args.PSFSigma.^2));
    PSF    = PSF./sum(PSF(:));
    K      = log(1 + Args.Flux.*PSF./Args.Back);

    GammaVec = [Args.Gamma, 1e-3, 1e-6, 1e-9];

    [SthF, ResF] = imUtil.poissNoise.poissThresholdFFT(K, Args.Back, 'Gamma',GammaVec);
    [SthP, ResP] = imUtil.poissNoise.poissThresholdPanjer(K, Args.Back, 'Gamma',[GammaVec 1e-20]);
    [SthS, ResS] = imUtil.poissNoise.poissThresholdSimulations(K, Args.Back, 'Gamma',Args.Gamma, ...
                                                               'Nsim',Args.Nsim, 'Seed',Args.Seed);

    assert(ResF.Converged, 'FFT convergence checks failed');
    assert(ResP.Converged, 'Panjer convergence checks failed');
    EpsS = 2.*max(ResF.DeltaS, ResP.DeltaS);
    assert(all(abs(SthF - SthP(1:numel(GammaVec))) <= EpsS + 1e-12), 'FFT and Panjer thresholds disagree');
    assert(abs(SthS - SthP(1))./SthP(1) < Args.RelTol, 'Simulation and Panjer thresholds disagree');
    % the analytical SF must be consistent with the simulated SF at the threshold
    Is  = find(ResS.S >= SthP(1) - ResS.DeltaS./2, 1, 'first');
    SFp = interp1(ResP.S - ResP.DeltaS./2, ResP.SF, ResS.S(Is) - ResS.DeltaS./2);
    assert(abs(ResS.SF(Is) - SFp) < 5.*ResS.SFErr(Is) + 0.05.*SFp, 'Simulated SF inconsistent with Panjer');

    fprintf('Test 2 (gamma=%g): Sth: Simulations=%.3f  FFT=%.3f  Panjer=%.3f\n', Args.Gamma, SthS, SthF(1), SthP(1));
    fprintf('        Run time [s]: Simulations=%.3f  FFT=%.3f  Panjer=%.3f\n', ResS.Time, ResF.Time, ResP.Time);
    fprintf('        Panjer Sth(gamma=1e-20)=%.3f\n', SthP(end));

    %% Test 3: Mexican-hat kernel (negative values)
    R2    = X.^2 + Y.^2;
    Kmh   = (1 - R2./2).*exp(-R2./2);
    Bmh   = 0.5;
    Gmh   = 1e-3;
    SthFmh = imUtil.poissNoise.poissThresholdFFT(Kmh, Bmh, 'Gamma',Gmh);
    SthSmh = imUtil.poissNoise.poissThresholdSimulations(Kmh, Bmh, 'Gamma',Gmh, 'Nsim',Args.NsimOther, 'Seed',Args.Seed);
    assert(abs(SthFmh - SthSmh)./abs(SthFmh) < Args.RelTol, 'FFT and simulations disagree for a Mexican-hat kernel');
    Failed = false;
    try
        imUtil.poissNoise.poissThresholdPanjer(Kmh, Bmh, 'Gamma',Gmh);
    catch
        Failed = true;
    end
    assert(Failed, 'Panjer should fail for a kernel with negative values');
    fprintf('Test 3 (Mexican hat, gamma=%g): Sth: Simulations=%.3f  FFT=%.3f\n', Gmh, SthSmh, SthFmh);

    %% Test 4: spatially varying background
    Bvar  = Args.Back.*10.*(1 + 0.5.*X./Half);
    Gvar  = 1e-3;
    SthFv = imUtil.poissNoise.poissThresholdFFT(K, Bvar, 'Gamma',Gvar);
    SthPv = imUtil.poissNoise.poissThresholdPanjer(K, Bvar, 'Gamma',Gvar);
    SthSv = imUtil.poissNoise.poissThresholdSimulations(K, Bvar, 'Gamma',Gvar, 'Nsim',Args.NsimOther, 'Seed',Args.Seed);
    assert(abs(SthFv - SthPv) <= EpsS + 1e-12, 'FFT and Panjer disagree for a variable background');
    assert(abs(SthSv - SthPv)./SthPv < Args.RelTol, 'Simulations and Panjer disagree for a variable background');
    fprintf('Test 4 (variable background, gamma=%g): Sth: Simulations=%.3f  FFT=%.3f  Panjer=%.3f\n', Gvar, SthSv, SthFv, SthPv);

    %% Figure (similar to Fig. 2, bottom right)
    if Args.Plot
        figure;
        Ax1 = axes;
        Fs  = ResS.SF > 0;
        H1  = semilogy(ResS.S(Fs), ResS.SF(Fs), '-', 'Color',[0.12 0.47 0.71], 'LineWidth',1.5);
        hold on;
        H2  = semilogy(ResF.S, max(ResF.SF, realmin), '-', 'Color',[0.17 0.63 0.17], 'LineWidth',1.5);
        H3  = semilogy(ResP.S, ResP.SF, '-', 'Color',[0.84 0.15 0.16], 'LineWidth',1.2);
        H4  = plot([SthP(1) SthP(1)], [1e-30 10], 'r--', 'LineWidth',1);
        H5  = plot([ResP.S(1) ResP.S(end)], [Args.Gamma Args.Gamma], 'k:', 'LineWidth',1);
        Xlim = [ResP.S(1), SthP(end).*1.05];
        Ylim = [1e-25 10];
        set(Ax1, 'XLim',Xlim, 'YLim',Ylim, 'YMinorTick','off', 'Box','on');
        grid on;
        xlabel('S');
        ylabel('\gamma = P(S \geq s | H_0)');
        title('False Alarm Probability \gamma');
        legend([H1 H2 H3 H4 H5], {sprintf('Simulations (%g)', Args.Nsim), 'FFT', 'Panjer recursion', ...
                                  'S_{thresh}', sprintf('\\gamma=%.3g', Args.Gamma)}, 'Location','northeast');

        % right axis: equivalent one-sided Gaussian significance
        Sigma  = (1:10);
        GamSig = 0.5.*erfc(Sigma./sqrt(2));
        Ax2 = axes('Position',get(Ax1,'Position'), 'Color','none', 'YAxisLocation','right', ...
                   'XTick',[], 'YScale','log', 'YLim',Ylim, 'XLim',Xlim, ...
                   'YTick',fliplr(GamSig), 'YTickLabel',arrayfun(@num2str, fliplr(Sigma), 'UniformOutput',false), ...
                   'YMinorTick','off');
        ylabel(Ax2, '\sigma (one-sided Gaussian)');
        set(gcf, 'CurrentAxes',Ax1);
    end

    Result = true;
end
