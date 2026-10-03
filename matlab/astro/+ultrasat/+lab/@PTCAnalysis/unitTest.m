function Result = unitTest()
    % unitTest for ultrasat.lab.PTCAnalysis
    %   Builds a synthetic device directory whose high-gain half follows
    %   ADU = Zero + Intercept + Gain*Poisson(Slope*X/Gain) (+ read noise), with
    %   pixel-dependent slope and intercept (the low-gain half is 1/15 of it),
    %   written as [low high] TIFFs so that the default readPTC (high gain,
    %   DESY orientation) returns the model maps. Checks that the region and
    %   the full (streamed) modes recover slope, intercept, Nused and gain.
    % Example: ultrasat.lab.PTCAnalysis.unitTest
    io.msgStyle(LogLevel.Test, '@start', 'ultrasat.lab.PTCAnalysis test started');

    % masked regression on a known line
    X = [1 2 3 4 5];
    Y = reshape(3.*X + 10, 1, 1, []);
    Fit = ultrasat.lab.PTCAnalysis.fitMasked(Y, X, [0 100]);
    assert(abs(Fit.Slope-3)<1e-9 && abs(Fit.Intercept-10)<1e-9 && Fit.Nused==5 && Fit.ResidRMS<1e-9);
    Fit = ultrasat.lab.PTCAnalysis.fitMasked(Y, X, [15 20]);       % only X=2,3 inside
    assert(Fit.Nused==2 && abs(Fit.Slope-3)<1e-9 && isequal(squeeze(Fit.Used).', logical([0 1 1 0 0])));
    Fit = ultrasat.lab.PTCAnalysis.fitMasked(Y, X, [15 17]);       % 1 point -> NaN
    assert(Fit.Nused==1 && isnan(Fit.Slope));
    % accumulate in two parts equals one pass
    S1 = ultrasat.lab.PTCAnalysis.accumulate([], Y(:,:,1:2), X(1:2), [0 100]);
    S1 = ultrasat.lab.PTCAnalysis.accumulate(S1, Y(:,:,3:5), X(3:5), [0 100]);
    F1 = ultrasat.lab.PTCAnalysis.solve(S1);
    assert(abs(F1.Slope-3)<1e-9 && F1.Nused==5);

    % synthetic device
    rng(1);
    Ny = 24;  Nx = 32;
    GainTrue  = 1.2;                                   % ADU/e-
    ZeroLevel = 400;
    DarkExp   = [15 30 60 120 180 240 360 480 600];
    BrightInt = [1e-5 2e-5 4e-5 6e-5 9e-5 1.4e-4 1.8e-4 2.2e-4 2.7e-4 3.6e-4];   % config units; 'int' = x1000
    IntScale  = 1000;
    ExpSen    = 15;
    [Xg, Yg]  = meshgrid(1:Nx, 1:Ny);
    SlopeD    = 6 + 0.02.*Xg;                          % ADU/s per pixel
    SlopeD(2:2:end, :) = 1.1.*SlopeD(2:2:end, :);      % even DESY rows = odd raw-TIFF columns: 10% more dark current
    InterD    = -140 + 1.*Yg;                          % ADU per pixel
    SlopeB    = 1.3e4 + 20.*Yg;                        % ADU/int
    InterB    = -20 + 0.5.*Xg;                         % ADU
    Nd = 3;  Nb = 3;  Nz = 5;

    TmpDir = tempname;
    Base   = 'Ultrasat_BSI_L_TH00002_W04_D07';
    Dev    = fullfile(TmpDir, 'LOT_TH00002_W04_D07');
    Test   = 'PTC_int_hr';
    mkdir(fullfile(Dev, Test));
    Cleanup = onCleanup(@() rmdir(TmpDir, 's'));

    Fid = fopen(fullfile(Dev, [Base, '_Result.txt']), 'w');
    fprintf(Fid, 'Product Name\tUltrasat_BSI\t\t\t\t\t\n');
    fprintf(Fid, 'LOT ID\tTH00002\t\t\t\t\t\n');
    fprintf(Fid, 'Wafer ID\t04\t\t\t\t\t\n');
    fprintf(Fid, 'Device No\t00007\t\t\t\t\t\n');
    fprintf(Fid, 'Tester Temperature\t-50,0\t\t\t\t\t\n');
    fprintf(Fid, '1200\tVDD_GND_Shorts_VDDD_U [V]\t8\t1\t-0,300\t-0,249\t-0,200\n');
    fprintf(Fid, 'Soft Bin\t1\t\t\t\t\t\n');
    fprintf(Fid, 'Pass\t1\t\t\t\t\t\n');
    fclose(Fid);
    Config = {'Time', '2026.08.27 06:33:07';
              'Chuck Temperature', '-50,0';
              'PTC_ExpTime', sprintf('[1]{%d}', ExpSen);
              'Dark_ExpTime', sprintf('[%d]{%s}', numel(DarkExp), strjoin(string(DarkExp), ';'));
              'Bright_Intensity', sprintf('[%d]{%s}', numel(BrightInt), strjoin(strrep(compose('%.1E', BrightInt), '.', ','), ';'));
              'Dark_Frames', Nd;
              'Bright_Frames', Nb;
              'ZeroExp_Frames', Nz};
    writecell(Config, fullfile(Dev, Test, 'PTC_Config.xlsx'));

    writeFrames(Dev, Base, Test, 'ZE', 1, Nz, zeros(Ny,Nx), zeros(Ny,Nx), ZeroLevel, GainTrue);
    for Is=1:1:numel(DarkExp)
        writeFrames(Dev, Base, Test, 'D', Is, Nd, SlopeD.*DarkExp(Is), InterD, ZeroLevel, GainTrue);
    end
    for Is=1:1:numel(BrightInt)
        writeFrames(Dev, Base, Test, 'B', Is, Nb, SlopeB.*BrightInt(Is).*IntScale, InterB, ZeroLevel, GainTrue);
    end

    % region mode, wide fit window: every step used
    P = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Nx 1 Ny], 'FitRange',[-1e9 1e9], 'GainRange',[100 1e5]);
    P.run;
    assert(strcmp(P.Mode,'region') && P.NZero==Nz && P.ExpSen==ExpSen);
    assert(max(abs(P.Bright.X - BrightInt.*IntScale))<1e-9);
    assert(abs(mean(P.Zero(:))-ZeroLevel)<1);
    Z = P.ZeroStats;                                                % ReadNoise = 2 ADU in writeFrames
    assert(abs(Z.BiasLevel-ZeroLevel)<1 && abs(Z.BiasMean-ZeroLevel)<1 && Z.BiasStd<2 && Z.Nframes==Nz && Z.Npix==Ny*Nx);
    assert(abs(Z.ReadNoiseTemporal-2)<0.4 && abs(Z.ReadNoiseDiff-2)<0.2 && abs(Z.ReadNoiseSpatial-2)<0.3);
    assert(Z.ReadNoiseTemporalStd>0 && Z.ReadNoiseTemporalStd<1.5);
    assert(isequal(P.Dark.X, DarkExp) && isequal(size(P.Dark.Mean), [Ny Nx numel(DarkExp)]));
    assert(P.DarkFit.NusedMode==numel(DarkExp) && P.BrightFit.NusedMode==numel(BrightInt));
    assert(abs(P.DarkFit.MedianSlope - median(SlopeD(:)))<0.05);
    assert(abs(P.DarkFit.MedianIntercept - median(InterD(:)))<8);
    assert(abs(P.BrightFit.MedianSlope - median(SlopeB(:)))/median(SlopeB(:))<0.02);
    assert(max(abs(P.DarkFit.Slope(:) - SlopeD(:)))<0.5);        % per pixel
    for E = {'temporal','diff'}
        G = P.PTC.Fit.(E{1}).Gain;
        assert(abs(G-GainTrue)/GainTrue<0.1, 'gain (%s) = %.3f, expected %.2f', E{1}, G, GainTrue);
    end
    assert(P.PTC.Fit.spatial.Gain > P.PTC.Fit.temporal.Gain);        % PRNU adds to the spatial variance
    assert(strncmp(P.PTC.GainSource, 'measured', 8) && P.PTC.GainUsed==P.PTC.Fit.temporal.Gain);
    T = P.Threshold;
    assert(abs(T.MedianDarkADU + P.DarkFit.MedianIntercept)<1e-9);
    assert(abs(T.MedianLightADU - (P.DarkFit.MedianSlope*ExpSen - P.BrightFit.MedianIntercept))<1e-9);
    assert(abs(T.MedianDarkE - T.MedianDarkADU/P.PTC.GainUsed)<1e-9);
    S = P.summary;
    assert(strcmp(S.Lot,'TH00002') && S.Wafer==4 && S.Device==7 && S.DarkFit.Npix==Ny*Nx);
    assert(abs(S.BiasLevel-ZeroLevel)<1 && abs(S.ReadNoiseE - S.ReadNoiseTemporalRMS/S.GainUsed)<1e-9 && S.NZero==Nz && abs(S.ReadNoiseTemporalRMS-2)<0.3);
    assert(isfinite(S.ReadNoiseFromOffset));

    % conversion-gain override
    P.GainADU = 1.05;
    P.fitGain;  P.threshold;
    assert(P.PTC.GainUsed==1.05 && strcmp(P.PTC.GainSource,'override'));
    assert(abs(P.Threshold.MedianDarkE - P.Threshold.MedianDarkADU/1.05)<1e-9);

    % narrow window: DESY-style point selection
    P2 = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Nx 1 Ny], 'FitRange',[1000 2500]);
    P2.run;
    ExpUsed = sum(median(SlopeD(:)).*DarkExp + median(InterD(:)) >= 1000 & median(SlopeD(:)).*DarkExp + median(InterD(:)) <= 2500);
    assert(P2.DarkFit.NusedMode==ExpUsed && ExpUsed<numel(DarkExp));
    assert(abs(P2.DarkFit.MedianSlope - median(SlopeD(:)))<0.1);
    assert(isequal(P2.DarkFit.FitRange, [1000 2500]) && isempty(P2.DarkFit.FitSteps));
    % per-type window and explicit steps
    P2.FitRange = [1000 2500; 500 3000];
    P2.fitResponse('B');
    assert(isequal(P2.BrightFit.FitRange, [500 3000]));
    P2.FitSteps.D = [4 5 6 7];
    P2.fitResponse('D');
    assert(P2.DarkFit.NusedMode==4 && all(P2.DarkFit.Nused(:)==4) && isequal(P2.DarkFit.FitSteps, [4 5 6 7]));
    assert(isequal(squeeze(any(any(P2.DarkFit.Used,1),2)).', logical([0 0 0 1 1 1 1 0 0])));
    assert(abs(P2.DarkFit.MedianSlope - median(SlopeD(:)))<0.1);

    % 'auto' selection: window when it holds >= 3 steps, else the top >= 15% of the maximum
    Pa = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Nx 1 Ny], 'FitSteps',struct('D','auto','B','auto'), 'FitRange',[1000 2500]);
    Pa.run;
    MedD = median(SlopeD(:)).*DarkExp + median(InterD(:));
    ExpD = find(MedD>=1000 & MedD<=2500);
    assert(isequal(Pa.DarkFit.FitSteps, ExpD) && Pa.DarkFit.NusedMode==numel(ExpD) && numel(ExpD)>=3);
    Pa.FitRange = [1e5 2e5];  Pa.fitResponse('D');                  % window empty -> fallback
    ExpD = find(MedD>=0.15*max(MedD));
    assert(isequal(Pa.DarkFit.FitSteps, ExpD) && abs(Pa.DarkFit.MedianSlope - median(SlopeD(:)))<0.1);
    Pa.AutoMinFrac = 0.99;  Pa.fitResponse('D');                    % fallback too strict -> top 3 steps
    assert(isequal(Pa.DarkFit.FitSteps, 7:9));
    Pa.AutoMinFrac = 0.15;
    Pa.FitRange = [MedD(5)-1 MedD(6)+1];  Pa.fitResponse('D');       % two steps in the window -> nearest neighbour added, not the top
    Near = [4 7];  [~, Im] = min([MedD(5)-MedD(4), MedD(7)-MedD(6)]);
    assert(isequal(Pa.DarkFit.FitSteps, sort([5 6 Near(Im)])));
    Pa.FitRange = [MedD(2)-1 MedD(2)+1];  Pa.fitResponse('D');       % one step -> the two nearest (1 and 3)
    assert(isequal(Pa.DarkFit.FitSteps, 1:3));

    % median combiner and a sub-region
    P3 = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[5 20 3 12], 'Combiner','median', 'FitRange',[-1e9 1e9]);
    P3.run;
    assert(isequal(size(P3.Zero), [10 16]) && abs(P3.DarkFit.MedianSlope - median(reshape(SlopeD(3:12,5:20),[],1)))<0.1);

    % full (streamed) mode equals region mode on the same pixels
    F = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'FitRange',[-1e9 1e9], 'GainRange',[100 1e5]);
    F.run;
    assert(strcmp(F.Mode,'full') && ~isfield(F.Dark, 'Mean'));
    assert(max(abs(F.DarkFit.Slope(:) - P.DarkFit.Slope(:)))<1e-3);
    assert(max(abs(F.BrightFit.Intercept(:) - P.BrightFit.Intercept(:)))<1e-2);
    assert(isequal(F.DarkFit.Nused, P.DarkFit.Nused));
    assert(max(abs(F.PTC.Mean - P.PTC.Mean))<1e-3 && abs(F.PTC.Fit.temporal.Gain - P.PTC.Fit.temporal.Gain)<1e-6);
    F2 = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'FitSteps',struct('D',[4 5 6 7], 'B',[]), 'FitRange',[1000 2500; 500 3000]);
    F2.run;
    assert(max(abs(F2.DarkFit.Slope(:) - P2.DarkFit.Slope(:)))<1e-3 && isequal(F2.BrightFit.Nused, P2.BrightFit.Nused));

    % full mode honours Gain and Orient: the streamed reader used to fall back to
    % the readPTC defaults, so a low-gain or tiff-oriented full run silently
    % returned the high-gain DESY half
    Rl = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Nx 1 Ny], 'Gain','low', 'FitRange',[-1e9 1e9]);
    Rl.read;  Rl.subtractZero;  Rl.combineSteps;
    Fl = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'Gain','low', 'FitRange',[-1e9 1e9]);
    Fl.read;  Fl.subtractZero;  Fl.combineSteps;
    assert(isequal(Fl.Zero, Rl.Zero) && max(abs(Fl.Dark.RegionMean - Rl.Dark.RegionMean))<1e-3);
    assert(Fl.Dark.RegionMean(end) < 0.2.*F.Dark.RegionMean(end));   % the /15 half, not the high-gain one
    Ft = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'Orient','tiff', 'FitRange',[-1e9 1e9]);
    Ft.read;  Ft.subtractZero;
    assert(isequal(size(Ft.Zero), [Nx Ny]) && isequal(size(F.Zero), [Ny Nx]));

    % raw-column parity: map orientation and detection of the 10% slope difference
    Pp = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Nx 1 Ny], 'FitRange',[-1e9 1e9], 'GainRange',[100 1e5], 'Parity','rawcol');
    Pp.run;
    assert(isequal(size(Pp.ParityMap), [Ny Nx]) && all(Pp.ParityMap(2,:)) && ~any(Pp.ParityMap(1,:)));
    T = Pp.parityTable;
    assert(any(strcmp(T.Quantity, 'BiasLevel')) && any(strcmp(T.Quantity, 'ReadNoiseTemporal')));
    assert(abs(T.RelDiff(strcmp(T.Quantity, 'BiasLevel'))) < 0.01);
    R = T(strcmp(T.Quantity, 'DarkSlope'), :);
    assert(abs(R.Odd/R.Even - 1.1) < 0.02 && R.DiffOverSE < -20);
    R = T(strcmp(T.Quantity, 'BrightSlope'), :);
    assert(abs(R.RelDiff) < 0.02);
    assert(abs(Pp.PTC.Parity.Even.Fit.temporal.Gain - GainTrue)/GainTrue < 0.15);
    assert(isfield(Pp.Threshold.Parity.Even, 'MedianLightE') && numel(Pp.Dark.Parity.Odd.RegionMean)==numel(DarkExp));
    S = Pp.summary;  assert(istable(S.ParityTable) && strcmp(S.Parity, 'rawcol'));
    Pt = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[1 Ny 1 Nx], 'Orient','tiff', 'Parity','rawcol', 'FitRange',[-1e9 1e9]);
    Pt.run;                                                       % tiff orientation: parity along columns
    assert(isequal(size(Pt.ParityMap), [Nx Ny]) && all(Pt.ParityMap(:,1)) && ~any(Pt.ParityMap(:,2)));
    Tt = Pt.parityTable;  assert(abs(Tt.Odd(strcmp(Tt.Quantity,'DarkSlope'))/Tt.Even(strcmp(Tt.Quantity,'DarkSlope')) - 1.1) < 0.02);
    Fp = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'Parity','rawcol', 'FitRange',[-1e9 1e9], 'GainRange',[100 1e5]);
    Fp.run;                                                       % streamed mode carries the parity split too
    Tf = Fp.parityTable;
    assert(max(abs(Tf.Even - T.Even)) < 1e-6 && max(abs(Tf.Odd - T.Odd)) < 1e-6);

    % --- individual-pixel statistics -------------------------------------
    % varSpread: chi2 sampling scatter removed (chi2(2K)/(2K) = mean of K exponentials)
    rng(7);
    Ex   = @(N,K) mean(-log(rand(N,K)), 2);
    Nsim = 2e5;
    Sv   = ultrasat.lab.PTCAnalysis.varSpread(100.*Ex(Nsim,1), 2);      % identical pixels
    assert(abs(Sv.MeanVar-100)<2 && abs(Sv.StdObs-100)<3 && abs(Sv.Sigma)<5 && Sv.RelUL95>Sv.RelIntr);
    Sv   = ultrasat.lab.PTCAnalysis.varSpread(max(100.*(1+0.3.*randn(Nsim,1)),1).*Ex(Nsim,1), 2);
    assert(abs(Sv.RelIntr-0.3)<0.03 && Sv.Sigma>20);
    Sv   = ultrasat.lab.PTCAnalysis.varSpread(100.*Ex(Nsim,2), 4);      % Dof=4: less scatter
    assert(abs(Sv.StdObs-100.*sqrt(2/4))<3 && abs(Sv.Sigma)<5);
    % paramSpread: analytic fit noise removed from the observed spread
    Sp = ultrasat.lab.PTCAnalysis.paramSpread(10 + 2.*randn(Nsim,1) + 3.*randn(Nsim,1), 9.*ones(Nsim,1), 'Robust',false);
    assert(abs(Sp.StdFit-3)<1e-9 && abs(Sp.StdIntr-2)<0.1 && Sp.Sigma>20);
    Sp = ultrasat.lab.PTCAnalysis.paramSpread(10 + 3.*randn(Nsim,1), 9.*ones(Nsim,1), 'Robust',false);
    assert(Sp.StdIntr<0.5 && Sp.Sigma<5);

    % rawColGeom agrees with the parity map in both orientations
    Gd = Pp.rawColGeom;
    assert(Gd.Dim==1 && Gd.Ny==Ny && Gd.Nx==Nx && isequal(mod(Gd.RawCol,2)==1, Pp.ParityMap(:,1)));
    Gt = Pt.rawColGeom;
    assert(Gt.Dim==2 && isequal(mod(Gt.RawCol,2)==1, Pt.ParityMap(1,:)));

    % badColumns: nothing to flag on the synthetic device, one injected column found
    Bc = Pp.badColumns;
    assert(Bc.Nbad==0 && all(Bc.GoodMask(:)) && numel(Bc.RawCol)==Ny && numel(Bc.NoiseProfile)==Ny);
    ZN = Pp.ZeroNoise;
    Pp.ZeroNoise(3,:) = 50.*ZN(3,:);
    Bc2 = Pp.badColumns;
    assert(Bc2.Nbad==1 && ~any(Bc2.GoodMask(3,:)) && all(Bc2.GoodMask(4,:)) && Bc2.BadNoise(3));
    Pp.ZeroNoise = ZN;

    % zeroNoiseStats: uniform synthetic read noise, no fixed pattern
    Zn = Pp.zeroNoiseStats;
    assert(Zn.Nframes==Nz && Zn.Dof==Nz-1 && numel(Zn.CommonMode.Levels)==Nz);
    assert(abs(Zn.All.BiasLevel-ZeroLevel)<1 && Zn.All.Npix==Ny*Nx);
    assert(abs(Zn.All.ReadNoiseMedian-2)<0.5 && abs(Zn.All.ReadNoiseRobust-2)<0.5 && abs(Zn.All.ReadNoiseRMS-2)<0.5);
    assert(Zn.All.FixedPatternRMS < 0.6 && Zn.All.Spread.Sigma < 5);   % neither is present in the model
    assert(Zn.Even.Npix+Zn.Odd.Npix==Zn.All.Npix && Zn.Structure.ReadoutDim==1);
    assert(numel(Zn.Structure.LagDim1)==3 && all(abs(Zn.Structure.LagDim1)<0.3));
    Zm = Pp.zeroNoiseStats('Mask',Bc2.GoodMask);
    assert(Zm.All.Npix==(Ny-1)*Nx);

    % perPixelFits: every step below the limit, per-pixel truth recovered
    Fd = Pp.perPixelFits('D', 'Select','linlimit', 'LinLimit',1e9, 'Robust',false);
    assert(numel(Fd.Steps)==numel(DarkExp) && strcmp(Fd.Weights,'measured') && all(Fd.Nused(:)==numel(DarkExp)));
    assert(all(isfinite(Fd.VarStep)) && issorted(Fd.VarStep));        % measured step variances grow with signal
    assert(max(abs(Fd.Slope(:)-SlopeD(:)))<0.5 && abs(median(Fd.Intercept(:))-median(InterD(:)))<8);
    assert(abs(median(Fd.Chi2Dof(:),'omitnan')-1)<0.5);               % weights are the true variances
    assert(abs(Fd.Odd.SlopeSpread.Median./Fd.Even.SlopeSpread.Median - 1.1)<0.02);
    ExpStd = std(InterD(:));                                          % fit noise must be deconvolved out
    Is = Fd.All.InterceptSpread;
    assert(abs(Is.StdIntr-ExpStd)./ExpStd < 0.3, 'intr %.3f vs %.3f', Is.StdIntr, ExpStd);
    assert(Fd.All.InterceptSpread.StdObs > Fd.All.InterceptSpread.StdIntr);
    Fm = Pp.perPixelFits('D', 'Select','linlimit', 'LinLimit',1e9, 'Weights','model', 'Robust',false);
    assert(Fm.All.InterceptSpread.StdFit < Is.StdFit);                % the modelled weight misses g*T
    Fl = Pp.perPixelFits('D', 'Select','linlimit', 'LinLimit',1500, 'Robust',false);       % truncation shortens the lever arm
    assert(numel(Fl.Steps)<numel(DarkExp) && Fl.All.InterceptSpread.StdFit > Is.StdFit);
    Fo = Pp.perPixelFits('D', 'Select','linlimit', 'LinLimit',1e9, 'Weighted',false, 'Robust',false);
    assert(strcmp(Fo.Weights,'none') && Fo.All.InterceptSpread.StdFit > Is.StdFit);   % OLS is less efficient

    % default selection reproduces fitResponse exactly (same steps, OLS)
    Fr = Pp.perPixelFits('D', 'Weighted',false);
    assert(strcmp(Fr.Select,'fitrange') && isequal(Fr.Steps, Pp.Dark.Step(any(any(Pp.DarkFit.Used,1),2))));
    assert(max(abs(Fr.Slope(:)-Pp.DarkFit.Slope(:)))<1e-9 && max(abs(Fr.Intercept(:)-Pp.DarkFit.Intercept(:)))<1e-9);
    assert(isequal(Fr.Nused, Pp.DarkFit.Nused));

    % perPixelThreshold: both methods and the error propagation
    Th = Pp.perPixelThreshold('Select','linlimit', 'LinLimit',1e9, 'DarkFit',Fd);
    assert(max(abs(Th.DarkADU(:) + Fd.Intercept(:)))<1e-9 && isequal(Th.VarDarkADU, Fd.VarIntercept));
    assert(abs(median(Th.DarkADU(:)) + median(InterD(:)))<8);
    assert(abs(Th.All.MedianDarkE - Th.All.DarkSpread.Median./Pp.PTC.GainUsed)<1e-9);
    assert(abs(median(Th.LightADU(:)) - (median(SlopeD(:)).*ExpSen - median(InterB(:))))<25);
    assert(all(Th.VarLightADU(:) >= Th.VarDarkADU(:)*0));             % finite and non-negative
    assert(isfinite(Th.All.PRNU) && Th.All.PRNU_UL95 >= Th.All.PRNU);
    assert(isfinite(Th.PatternStep) && isfinite(Th.PatternX));

    % stepFixedPattern: the PRNU built into the synthetic device (1.05%)
    Pb = Pp.stepFixedPattern('B', 'Robust',false);
    ExpPRNU = std(SlopeB(:))./median(SlopeB(:));
    assert(numel(Pb.All.RelFixed)==numel(BrightInt) && all(Pb.All.StdNoise>0));
    assert(abs(Pb.All.RelFixed(end)-ExpPRNU)./ExpPRNU < 0.2, 'PRNU %.4f vs %.4f', Pb.All.RelFixed(end), ExpPRNU);
    assert(Pb.All.RelUL95(end) >= Pb.All.RelFixed(end) && Pb.All.Sigma(end) > 10);
    Top = Pb.All.Median > 0.5.*max(Pb.All.Median);                    % multiplicative: relative value stable
    assert(max(Pb.All.RelFixed(Top))./min(Pb.All.RelFixed(Top)) < 1.5);
    Pr = Pp.stepFixedPattern('B');                                    % MAD on a uniform spread reads ~1.28x
    assert(Pr.All.RelFixed(end) > Pb.All.RelFixed(end));
    assert(isfield(Pb,'Even') && numel(Pb.Even.Median)==numel(BrightInt));
    % additive (+) multiplicative decomposition recovers the built-in PRNU
    assert(Pb.All.PatternNsteps>=3 && isfinite(Pb.All.Multiplicative) && isfinite(Pb.All.Additive));
    assert(abs(Pb.All.Multiplicative-ExpPRNU)./ExpPRNU < 0.25, 'b %.4f vs %.4f', Pb.All.Multiplicative, ExpPRNU);
    ExpAdd = std(InterB(:));                                          % additive offset pattern [ADU]
    assert(Pb.All.Additive < 5.*max(ExpAdd,1));                       % small, and not confused with b
    % the threshold summary takes its PRNU from that fit, not the slope spread
    assert(abs(Th.All.PRNU - Pr.All.Multiplicative) < 1e-12 && Th.PatternStep==Pb.Step(end));
    assert(isfield(Th.All, 'PRNU_slope') && isfinite(Th.All.OffsetFPN_e));
    assert(abs(Th.All.OffsetFPN_e - Th.All.OffsetFPN_ADU./Pp.PTC.GainUsed)<1e-12);

    % --- streamed per-pixel fits: accumulateFit / solveFit and full == region
    % the sums reproduce a weighted straight line and its covariance
    Xs = [1 2 4 8].';
    Ys = 3 + 2.*Xs + [0.1; -0.2; 0.05; 0.3];
    Ws = [1 2 0.5 4].';
    Sm = [];
    for Is=1:1:numel(Xs)
        Sm = ultrasat.lab.PTCAnalysis.accumulateFit(Sm, Ys(Is).*ones(2,2), Xs(Is), Ws(Is).*ones(2,2), [-Inf Inf]);
    end
    Fs = ultrasat.lab.PTCAnalysis.solveFit(Sm);
    Ad = [ones(numel(Xs),1), Xs];
    Cd = (Ad.'*(Ws.*Ad))\(Ad.'*(Ws.*Ys));
    Vd = inv(Ad.'*(Ws.*Ad));
    assert(max(abs(Fs.Slope(:)-Cd(2)))<1e-12 && max(abs(Fs.Intercept(:)-Cd(1)))<1e-12);
    assert(abs(Fs.VarSlope(1)-Vd(2,2))<1e-12 && abs(Fs.VarIntercept(1)-Vd(1,1))<1e-12);
    Rd = Ys - Ad*Cd;
    assert(abs(Fs.Chi2Dof(1) - sum(Ws.*Rd.^2)./(numel(Xs)-2))<1e-10);
    assert(abs(Fs.ResidRMS(1) - sqrt(mean(Rd.^2)))<1e-10 && all(Fs.Nused(:)==numel(Xs)));
    Sw2 = ultrasat.lab.PTCAnalysis.accumulateFit([], [1 2; 3 4], 1, [1 0; -1 NaN], [-Inf Inf]);
    assert(isequal(Sw2.Nok, [1 0; 0 0]) && isequal(Sw2.Nr, ones(2,2)));   % bad weights drop from the fit only

    % full (streamed) mode reproduces the region-mode per-pixel fit exactly
    Fs2 = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'FitRange',[-1e9 1e9], 'Parity','rawcol');
    Fs2.read;  Fs2.subtractZero;                      % no combineSteps: the inventory is enough
    Ff = Fs2.perPixelFits('D', 'Robust',false);
    Fg = Pp.perPixelFits('D', 'Robust',false);
    assert(strcmp(Ff.Mode,'full') && isequal(Ff.Steps, Fg.Steps) && max(abs(Ff.VarStep-Fg.VarStep))<1e-9);
    assert(max(abs(Ff.Slope(:)-Fg.Slope(:)))<1e-9 && max(abs(Ff.Intercept(:)-Fg.Intercept(:)))<1e-9);
    assert(max(abs(Ff.VarIntercept(:)-Fg.VarIntercept(:)))<1e-9 && isequal(Ff.Nused, Fg.Nused));
    assert(max(abs(Ff.ResidRMS(:)-Fg.ResidRMS(:)))<1e-9 && max(abs(Ff.Chi2Dof(:)-Fg.Chi2Dof(:)))<1e-9);
    assert(abs(Ff.All.InterceptSpread.StdIntr-Fg.All.InterceptSpread.StdIntr)<1e-9);
    assert(abs(Ff.Odd.SlopeSpread.Median-Fg.Odd.SlopeSpread.Median)<1e-9);
    Fn = Fs2.perPixelFits('D', 'Weighted',false, 'Robust',false);       % OLS branch too
    Gn2 = Pp.perPixelFits('D', 'Weighted',false, 'Robust',false);
    assert(max(abs(Fn.VarSlope(:)-Gn2.VarSlope(:)))<1e-9);
    try
        Fs2.perPixelFits('D', 'Select','linlimit');                    % needs the step levels
        error('unitTest:nothrow', 'linlimit must fail in full mode without combineSteps');
    catch Me
        assert(strcmp(Me.identifier, 'ultrasat:lab:PTCAnalysis:order'));
    end

    % full mode reproduces the per-step fixed pattern as well
    Sf = Fs2.stepFixedPattern('B', 'Robust',false);
    Sg = Pp.stepFixedPattern('B', 'Robust',false);
    assert(strcmp(Sf.Mode,'full') && isequal(Sf.Step, Sg.Step) && isequal(Sf.Nframes, Sg.Nframes));
    assert(max(abs(Sf.All.StdFixed-Sg.All.StdFixed))<1e-9 && max(abs(Sf.All.Median-Sg.All.Median))<1e-9);
    assert(abs(Sf.All.Multiplicative-Sg.All.Multiplicative)<1e-9 && Sf.All.PatternNsteps==Sg.All.PatternNsteps);
    assert(max(abs(Sf.Even.RelFixed-Sg.Even.RelFixed))<1e-9 && max(abs(Sf.Odd.Sigma-Sg.Odd.Sigma))<1e-9);

    % budgetCurve: only a POSITIVE threshold removes charge
    Bp = ultrasat.lab.PTCAnalysis.budgetCurve([10 100 1000], struct('RN_e',2, 'Threshold_e',50));
    assert(isequal(Bp.Qc, [0 50 950]) && Bp.SNR_cal(1)==0);
    Bn = ultrasat.lab.PTCAnalysis.budgetCurve([10 100 1000], struct('RN_e',2, 'Threshold_e',-50));
    assert(isequal(Bn.Qc, [10 100 1000]));            % charge at zero signal is an offset
    assert(all(Bn.SNR_cal <= [10 100 1000]./sqrt(4 + [10 100 1000])) + 1e-12 > 0);
    B0 = ultrasat.lab.PTCAnalysis.budgetCurve([10 100 1000], struct('RN_e',2));
    assert(isequal(B0.Qc, Bn.Qc));                    % no threshold = negative threshold
    assert(max(abs(B0.SigmaEff_cal - sqrt(4 + [10 100 1000])))<1e-12);

    % noiseBudget: electrons, gain direction and the threshold dead zone
    Nb = Pp.noiseBudget('Threshold',Th, 'Zero',Zn, 'Method','none', 'Q',[1 10 100 1000]);
    assert(abs(Nb.RN_e - Zn.All.ReadNoiseMedian./Pp.PTC.GainUsed)<1e-9);
    assert(Nb.RN_e < Zn.All.ReadNoiseMedian);                         % Gain > 1 ADU/e-
    Expect = sqrt(Nb.RN_e.^2 + Nb.Q + Nb.DC_e.*Nb.ExpTime);
    assert(max(abs(Nb.SigmaEff_cal - Expect))<1e-9 && isequal(Nb.Qc, Nb.Q));
    assert(all(Nb.SigmaEff_raw >= Nb.SigmaEff_cal) && all(Nb.SNR_raw <= Nb.SNR_cal));
    assert(isfinite(Nb.Qlim_cal) && Nb.Qlim_cal>Nb.RN_e);
    Nl = Pp.noiseBudget('Threshold',Th, 'Zero',Zn, 'Method','light');
    assert(Nl.Threshold_e>0 && Nl.Qlim_cal > Nl.Threshold_e && Nl.Qlim_cal > Nb.Qlim_cal);
    assert(all(Nl.Qc(Nl.Q<=Nl.Threshold_e)==0) && all(Nl.SNR_cal(Nl.Q<=Nl.Threshold_e)==0));

    % FITS export through the class
    FitsDir = fullfile(TmpDir, 'fits');
    Files = P.writeFITS(FitsDir, 'FrameType','ZE', 'FrameIndex',1);
    assert(numel(Files)==2 && all(cellfun(@isfile, Files)));
    A = AstroImage(Files{1});
    assert(isequal(A.Image, P.AI(find(strcmp(P.Frames.FrameType,'ZE'),1)).Image));
    assert(strcmp(A.HeaderData.getVal('GAINSEL'),'high') && A.HeaderData.getVal('SATURATE')==16383);

    % plots run without error
    Fig = figure('Visible','off');
    P.plotPTC('Axes',axes(Fig));
    P.plotResponse('D', 'Axes',axes(Fig), 'Npix',20);
    Pp.plotResponse('D', 'Axes',axes(Fig), 'Npix',20, 'Parity',true);
    Pp.plotPTC('Axes',axes(Fig), 'Estimator','temporal', 'Parity',true);
    close(Fig);
    H = P.plotHistograms('B');  close(ancestor(H(1), 'figure'));
    H = F.plotMaps('D');        close(ancestor(H(1), 'figure'));

    io.msgStyle(LogLevel.Test, '@passed', 'ultrasat.lab.PTCAnalysis test passed');
    Result = true;
end

function writeFrames(Dev, Base, Test, Type, Step, Nframes, Signal, Offset, ZeroLevel, Gain)
    % write Nframes TIFFs whose high-gain half (in the DESY orientation) is
    % ADU = ZeroLevel + Offset + Gain*Poisson(Signal/Gain) + read noise, and
    % whose low-gain half is the same signal divided by 15
    ReadNoise = 2;   % ADU
    for If=1:1:Nframes
        Ne   = poissrnd(max(Signal, 0)./Gain);
        High = ZeroLevel + Offset + Gain.*Ne + ReadNoise.*randn(size(Signal));
        Low  = ZeroLevel + (Offset + Gain.*Ne)./15 + ReadNoise.*randn(size(Signal));
        % readPTC returns rot90(Half.', 2); invert that for the stored halves, and
        % prepend the two counter columns of every TIFF row
        Hh   = size(Signal, 2);                                  % TIFF rows = model columns
        Ctr  = [(1:Hh).', 32768 + (1:Hh).'];
        Im   = [Ctr, rot90(Low, 2).', rot90(High, 2).'];
        Name = sprintf('%s_%s_#%02d_%s_%04d.tif', Base, Test, Step, Type, If);
        imwrite(uint16(round(Im)), fullfile(Dev, Test, Name), 'tif', 'Compression','none');
    end
end
