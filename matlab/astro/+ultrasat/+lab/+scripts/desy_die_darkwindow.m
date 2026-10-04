% Stage 2a of the single-die chain: AUTOMATIC CHOICE OF THE DARK FIT WINDOW.
%   The dark ladder is not a straight line over its whole length, and the part
%   that is straight is not the same part in every run. The two bias-board
%   setups of lot TH02954 differ by a factor 22 in dark current, so at the same
%   nine exposures run 32's ladder spans -3..172 ADU and run 31's spans
%   7..3500 ADU: one fixed list of steps cannot be the straight part of both.
%   Both ends bend, for different reasons:
%     low signal  a knee -- the charge threshold eats the first electrons, so
%                 the measured signal sits ABOVE the extrapolated line at the
%                 shortest exposures (run 32: +11.6, +9.9, +7.2, +3.4 ADU at
%                 steps 1-4 against the top-five-step line)
%     high signal the integral non-linearity above ~2900 ADU, which only a
%                 high-dark-current run reaches (run 31 step 9 at -39 ADU)
%   so the window has to be chosen from BOTH sides, per die, by how well the
%   straight line actually fits.
%   The rule: of every contiguous window of at least 3 steps whose highest
%   step median is below LinLimit, keep those whose per-pixel goodness of fit
%   is within DieDarkTol of its expectation, and take the widest; ties go to
%   the longest lever arm. The goodness of fit is the MEDIAN over the 22.5 M
%   pixels of chi2/dof from the same weighted fit stage 2 will run, compared
%   with the median of the chi2/dof distribution itself
%   (2*gammaincinv(0.5,dof/2)/dof, 0.693 for 1 dof, 0.839 for 2, ...), which
%   is what a correctly weighted fit to a straight line gives.
%   Every step is read once; the mean maps are then held in memory and the
%   whole window grid is solved from them, so the scan costs one extra pass
%   over the dark ladder, not one pass per window.
%   Settings: ultrasat.lab.scripts.desy_die_config. Output in DieOut:
%   darkwindow.json -- the chosen step list, the full scan, and the tolerance
%   it was chosen under. desy_die_config reads that file and asserts the
%   tolerance matches, so a stale scan cannot silently pin a window.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 2a (dark window): scanning the dark ladder\n', DieTag);
% FitSteps.D is deliberately left empty: this stage decides it.
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', ...
                             'FitSteps',struct('D',[], 'B',DieFitStepsB), ...
                             'GainADU',DieGainADU, 'Verbosity',0);
P.read;
P.subtractZero;
RN2    = double(P.ZeroNoise).^2;
RN2med = median(RN2(isfinite(RN2)), 'omitnan');
fprintf('  %d ZE frames, %d x %d pixels, RN median %.4f ADU, %.0f s\n', ...
    P.NZero, size(P.Zero,1), size(P.Zero,2), sqrt(RN2med), toc(T0));

% --- one pass over the ladder: mean map, median signal and the weight of every step
Inv   = P.stepInventory('D');
Ns    = numel(Inv.Step);
Mean  = cell(1, Ns);
Med   = nan(1, Ns);
VarSt = nan(1, Ns);
Nrep  = nan(1, Ns);
for I = 1:1:Ns
    [Mi, Vi, Nf] = P.stepMaps('D', Inv.Step(I));
    Mean{I}  = single(Mi);
    Nrep(I)  = Nf;
    Med(I)   = median(Mi(isfinite(Mi)), 'omitnan');
    Dof      = max(Nf-1, 1);
    Vv       = double(Vi(isfinite(Vi)));
    VarSt(I) = median(Vv).*Dof./(2.*gammaincinv(0.5, Dof./2));   % chi2 median bias removed
    fprintf('  step %d: %6.1f s, median %9.2f ADU, var %8.3f ADU^2, %d frames\n', ...
        Inv.Step(I), Inv.X(I), Med(I), VarSt(I), Nf);
end
clear Vi Mi
X = double(Inv.X);

% --- the window grid
LinLimit = DieLinLimit;          % [ADU] measured INL still <0.5 % below this
Allowed  = isfinite(Med) & Med<min(LinLimit, P.SatLevel);
Scan = struct('Lo',{}, 'Hi',{}, 'Nsteps',{}, 'Steps',{}, 'Median',{}, 'Lever',{}, ...
              'Chi2Dof',{}, 'Chi2Exp',{}, 'Ratio',{}, 'DC',{}, 'Tdark',{}, ...
              'SpreadDC',{}, 'SpreadT',{}, 'FitNoiseDC',{}, 'FitNoiseT',{});
for Hi = Ns:-1:3
    if ~Allowed(Hi)
        continue                 % a window topped by an INL step is not a candidate
    end
    for Lo = (Hi-2):-1:1
        Idx  = Lo:Hi;
        Sums = [];
        for I = Idx
            Wi   = Nrep(I)./max(VarSt(I) + RN2 - RN2med, 0.25.*VarSt(I));
            Sums = ultrasat.lab.PTCAnalysis.accumulateFit(Sums, Mean{I}, X(I), Wi, [-Inf Inf]);
        end
        F    = ultrasat.lab.PTCAnalysis.solveFit(Sums);
        clear Sums
        Dof  = numel(Idx) - 2;
        Cexp = 2.*gammaincinv(0.5, Dof./2)./Dof;
        Ok   = isfinite(F.Slope) & isfinite(F.Intercept);
        Sl   = ultrasat.lab.PTCAnalysis.paramSpread(F.Slope(Ok),     F.VarSlope(Ok),     'Robust',true);
        In   = ultrasat.lab.PTCAnalysis.paramSpread(F.Intercept(Ok), F.VarIntercept(Ok), 'Robust',true);
        Cm   = median(F.Chi2Dof(Ok), 'omitnan');
        Scan(end+1) = struct('Lo',Lo, 'Hi',Hi, 'Nsteps',numel(Idx), 'Steps',Inv.Step(Idx), ...
            'Median',Med(Idx), 'Lever',X(Hi)-X(Lo), 'Chi2Dof',Cm, 'Chi2Exp',Cexp, ...
            'Ratio',Cm./Cexp, 'DC',Sl.Median, 'Tdark',-In.Median, ...
            'SpreadDC',Sl.StdIntr, 'SpreadT',In.StdIntr, ...
            'FitNoiseDC',Sl.StdFit, 'FitNoiseT',In.StdFit);   %#ok<SAGROW>
        clear F
    end
end
clear Mean
if isempty(Scan)
    error('ultrasat:lab:scripts:darkwindow', ...
          'no candidate window: %d of %d dark steps are below the %g ADU linearity limit', ...
          nnz(Allowed), Ns, LinLimit);
end

% --- the choice: widest window inside the tolerance, ties to the longest lever arm
Ratio = [Scan.Ratio];
Nst   = [Scan.Nsteps];
Lev   = [Scan.Lever];
Pass  = Ratio<=DieDarkTol;
if any(Pass)
    Cand = find(Pass);
    Key  = Nst(Cand).*1e6 + Lev(Cand);
    [~, J] = max(Key);
    Pick = Cand(J);
    Why  = sprintf('widest window with chi2/dof within %.0f %% of expectation', 100.*(DieDarkTol-1));
else
    % Nothing qualifies (a ladder bent everywhere): take the best fit there is.
    [~, Pick] = min(Ratio);
    Why = 'no window inside the tolerance: the best goodness of fit';
    fprintf('  WARNING: no window reaches the tolerance (best ratio %.3f)\n', min(Ratio));
end

fprintf('\n%-18s %6s %8s %9s %8s %9s %10s %9s\n', ...
    'steps', 'n', 'lever', 'chi2/dof', 'exp', 'ratio', 'DC [ADU/s]', 'T [ADU]');
[~, Order] = sortrows([-Nst(:), -Lev(:)]);
for K = Order(:).'
    Mark = ' ';
    if K==Pick, Mark = '*'; end
    fprintf('%s %-16s %6d %8.0f %9.4f %8.4f %9.3f %10.4f %9.2f\n', Mark, ...
        ['[', strtrim(sprintf('%d ', Scan(K).Steps)), ']'], Scan(K).Nsteps, Scan(K).Lever, ...
        Scan(K).Chi2Dof, Scan(K).Chi2Exp, Scan(K).Ratio, Scan(K).DC, Scan(K).Tdark);
end

Out = struct('Stage','2a', 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
             'Size',size(P.Zero), 'Tol',DieDarkTol, 'LinLimit',LinLimit, ...
             'Rule','widest contiguous window of >=3 steps, top step below LinLimit, median chi2/dof within Tol of expectation; ties to the longest lever arm', ...
             'Chosen',Scan(Pick).Steps, 'ChosenMedian',Scan(Pick).Median, ...
             'ChosenRatio',Scan(Pick).Ratio, 'Why',Why, ...
             'Step',Inv.Step, 'X',X, 'StepMedian',Med, 'VarStep',VarSt, 'Nframes',Nrep, ...
             'Allowed',Allowed, 'Scan',Scan);
Fid = fopen(fullfile(DieOut, 'darkwindow.json'), 'w');
fwrite(Fid, jsonencode(Out));
fclose(Fid);

fprintf('\nchosen dark window: [%s] (medians %s ADU), %s\n', ...
    strtrim(sprintf('%d ', Scan(Pick).Steps)), strtrim(sprintf('%.1f ', Scan(Pick).Median)), Why);
fprintf('[%4.0f s] DARKWINDOW DONE -> %s\n', toc(T0), fullfile(DieOut, 'darkwindow.json'));
