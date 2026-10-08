% Stage 2a (signal mode): ONE SIGNAL WINDOW FOR BOTH LADDERS AND BOTH FITS.
%   An alternative to desy_die_darkwindow. Instead of choosing each ladder's fit
%   window by goodness of fit, it takes every step whose MEAN signal falls in
%   [DieSigLo DieSigHi] -- the same window for the dark and the bright ladder --
%   and that one list is then used by the response fit AND the photon-transfer
%   fit of that ladder, so the two measure the same charge over the same points.
%   The mean signal is computed exactly as the exported PTC points define it:
%   the mean over the pixels outside the top 0.1 % of the temporal variance,
%   which is where the cosmic rays are.
%   If the window leaves fewer than three steps the fit has no degree of freedom
%   left to judge it by, and solveFit rejects it. Rather than fail, the FLOOR is
%   lowered to the next step below it, one step at a time, until three steps are
%   in -- and the stage says loudly that it did, with the signal of the step it
%   had to reach for. The ceiling is never raised: above it the ladder leaves the
%   linear range, which is a different kind of error from being a little dim.
%   Measured on lot TH02954: the bright ladder gives four steps in 100-1000 ADU
%   on every die, while the dark ladder gives only two on both of the dies tried
%   -- run 31 W04_D07 needs the floor at 90.8 ADU to reach three, run 32 W04_D07
%   at 99.9 ADU, where the third step misses 100 by a tenth of an ADU.
%   Settings: ultrasat.lab.scripts.desy_die_config. Output in DieOut:
%   fitwindow.json. desy_die_config reads it when DieWindowMode is 'signal'.
DieWindowBootstrap = true;      % this stage writes the file the config reads
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 2a (signal window): %g-%g ADU, both ladders, both fits\n', ...
    DieTag, DieSigLo, DieSigHi);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', 'ExpTimeOffset',DieExpOffset, ...
                             'FitSteps',struct('D',[], 'B',[]), 'Verbosity',0);
P.read;
P.subtractZero;
fprintf('  %d ZE frames, %d x %d pixels, %.0f s\n', P.NZero, size(P.Zero,1), size(P.Zero,2), toc(T0));

Out = struct('Stage','2a-signal', 'Tag',DieTag, 'ExpTimeOffset',DieExpOffset, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
             'Size',size(P.Zero), 'SigLo',DieSigLo, 'SigHi',DieSigHi, ...
             'Rule',['every step whose mean signal is inside [SigLo SigHi]; the same list is used ' ...
                     'by the response fit and the PTC fit of that ladder. If fewer than 3 steps ' ...
                     'qualify the floor is lowered one step at a time until 3 do; the ceiling is ' ...
                     'never raised.']);

for Ty = {'D', 'B'}
    Type = Ty{1};
    Inv  = P.stepInventory(Type);
    Ns   = numel(Inv.Step);
    Sig  = nan(1, Ns);
    for I = 1:1:Ns
        [M, V] = P.stepMaps(Type, Inv.Step(I));
        Mm = double(M);  Vv = double(V);
        Keep = isfinite(Mm) & isfinite(Vv) & Vv <= quantile(Vv(:), 1-1e-3);
        Sig(I) = mean(Mm(Keep));
        clear M V Mm Vv Keep
        % the bright ladder runs to saturation over 34 steps; once it is well
        % past the ceiling nothing below can still be in the window
        if strcmp(Type,'B') && Sig(I) > 2.*DieSigHi && nnz(Sig<=DieSigHi & Sig>=DieSigLo)>=3
            Sig = Sig(1:I);  Inv.Step = Inv.Step(1:I);  Inv.X = Inv.X(1:I);
            Inv.Nframes = Inv.Nframes(1:I);
            break
        end
    end
    Ns = numel(Sig);

    Lo  = DieSigLo;
    Sel = Sig>=Lo & Sig<=DieSigHi;
    Reached = {};
    while nnz(Sel)<3
        Below = find(Sig<Lo & isfinite(Sig));
        if isempty(Below)
            error('ultrasat:lab:scripts:fitwindow', ...
                  ['%s ladder: only %d steps in %g-%g ADU and no step below the floor to reach ' ...
                   'for. Highest signal is %.1f ADU.'], ...
                  Type, nnz(Sel), Lo, DieSigHi, max(Sig));
        end
        [~, J] = max(Sig(Below));          % the nearest step below the floor
        Lo = Sig(Below(J));
        Reached{end+1} = sprintf('step %d at %.1f ADU', Inv.Step(Below(J)), Lo);  %#ok<SAGROW>
        Sel = Sig>=Lo & Sig<=DieSigHi;
    end

    Steps = Inv.Step(Sel);
    fprintf('\n  %s ladder: %d steps measured, %d in %g-%g ADU\n', ...
        Type, Ns, nnz(Sig>=DieSigLo & Sig<=DieSigHi), DieSigLo, DieSigHi);
    if ~isempty(Reached)
        fprintf('    FLOOR LOWERED to %.1f ADU to reach three points (took %s)\n', ...
            Lo, strjoin(Reached, ', '));
    end
    fprintf('    chosen steps [%s], mean signals %s ADU\n', ...
        strtrim(sprintf('%d ', Steps)), strtrim(sprintf('%.1f ', Sig(Sel))));
    for I = 1:1:Ns
        fprintf('      step %2d  %10.1f ADU  %s\n', Inv.Step(I), Sig(I), ...
            char("  " + string(repmat('*', 1, Sel(I)))));
    end

    Out.(Type) = struct('Step',Inv.Step, 'X',Inv.X, 'Nframes',Inv.Nframes, 'SignalMean',Sig, ...
                        'Chosen',Steps, 'ChosenSignal',Sig(Sel), 'Floor',Lo, ...
                        'FloorLowered',~isempty(Reached), 'Reached',{Reached}, ...
                        'NinWindow',nnz(Sig>=DieSigLo & Sig<=DieSigHi));
end

Fid = fopen(fullfile(DieOut, 'fitwindow.json'), 'w');
fwrite(Fid, jsonencode(Out));
fclose(Fid);
fprintf('\n[%4.0f s] FITWINDOW DONE -> %s\n', toc(T0), fullfile(DieOut, 'fitwindow.json'));
