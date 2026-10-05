% Stage 10: the four routes to the gain and the charge threshold, with errors.
%   a) dark response    S = DC*t + I_D            -> T = -I_D            (no gain)
%   b) light response   S = R*int + I_B           -> T = DC*ExpSen - I_B (no gain)
%   c) PTC, dark ladder   Var - RN^2 = g*(S + T) -> g, and T = intercept/g
%   d) PTC, bright ladder Var - RN^2 = g*(S + T) -> g, and T = intercept/g
%   The photon-transfer routes are fitted on Var - RN^2, the pixel's own read
%   noise taken out before the fit rather than subtracted from the intercept
%   afterwards. The term is small here (3.5 of 150 ADU^2 at the lowest bright
%   step) but it makes the intercept mean one thing only, g*T, and it removes a
%   whole-die constant from a quantity that varies pixel to pixel.
%   Only the two photon-transfer routes measure a gain: a response curve has no
%   noise in it. All four give a threshold, and they disagree, which is the point
%   of putting them in one table.
%   Every number carries two errors, and the second is much the larger.
%     stat - the scatter between independent parts of the die. The die is split
%            into NBlock x NBlock blocks, each route is run inside each block, and
%            the error is the standard error over blocks. A formal error from the
%            pixel count would read 1e-5 and mean nothing; this one includes the
%            spatial structure, which is what makes "the gain of this die" uncertain.
%     syst - the fit window. Each route is refitted over every defensible window
%            and the error is half the full range. On a curved ladder this
%            dominates: both ladders here are convex, so each window extrapolates
%            its own local tangent to zero and lands somewhere different.
%   Averages inside a block are MEANS of both the signal and the variance over one
%   common set of pixels (those outside the top 0.1 % of the variance): Var = g*S+c
%   holds per pixel, so a median on one axis and a mean on the other is not a point
%   on any curve.
%   Output in DieOut: methods.json.
ultrasat.lab.scripts.desy_die_config;
if ~exist('DieNBlock', 'var')
    DieNBlock = 8;                 % the die is split into DieNBlock^2 blocks
end

T0 = tic;
fprintf('%s stage 10: four routes to the gain and the threshold\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol');
P.read;
P.subtractZero;
G    = P.rawColGeom;
Siz  = [G.Ny G.Nx];
RN2  = double(readBin(fullfile(DieStage1,'rn.bin'), Siz, 'stage 1')).^2;
Nb   = DieNBlock;
By   = floor(Siz(1)./Nb);
Bx   = floor(Siz(2)./Nb);
fprintf('  %d x %d blocks of %d x %d pixels, %.0f s\n', Nb, Nb, By, Bx, toc(T0));

% ---- per step and per block: mean signal, mean variance, over a common mask
% The dark ladder is capped at the linearity limit, not left open: on the
% high-dark-current setup its longest exposures reach 3829 ADU, above both the
% INL limit and the start of the PTC variance dip, and a route fitted there is
% not measuring the same straight line as the rest of the chain.
Lad = struct('Type',{'D','B'}, 'Max',{DieLinLimit, 1200});
Dat = struct();
for Il = 1:1:numel(Lad)
    Ty    = Lad(Il).Type;
    Flag  = strcmp(P.Frames.FrameType, Ty);
    Steps = unique(P.Frames.Step(Flag)).';
    Sid=[]; Xv=[]; Sm=[]; Vm=[]; Nf=[]; Sb=[]; Vb=[]; Rb=[]; Rm=[];
    for Is = 1:1:numel(Steps)
        A = ultrasat.lab.readPTC(DieDev, 'Test',P.Test, 'FrameType',Ty, 'Step',Steps(Is), 'Gain',DieGain);
        C = zeros([size(A(1).Image), numel(A)], 'single');
        for Ii = 1:1:numel(A), C(:,:,Ii) = single(A(Ii).Image); end
        M  = double(mean(C,3)) - double(P.Zero);
        if median(M(:),'omitnan') > Lad(Il).Max
            clear C M
            continue
        end
        V  = var(double(C), 0, 3);
        clear C
        Keep = isfinite(M) & isfinite(V) & V <= quantile(V(:), 1-1e-3);
        Row  = find(Flag & P.Frames.Step==Steps(Is), 1);
        Sid(end+1) = Steps(Is);                                            %#ok<SAGROW>
        if strcmp(Ty,'D')
            Xv(end+1) = P.Frames.ExpTime(Row);                             %#ok<SAGROW>
        else
            Xv(end+1) = P.Frames.Intensity(Row).*P.IntensityScale;         %#ok<SAGROW>
        end
        Nf(end+1) = numel(A);                                              %#ok<SAGROW>
        Sm(end+1) = mean(M(Keep));                                         %#ok<SAGROW>
        Vm(end+1) = mean(V(Keep));                                         %#ok<SAGROW>
        Rm(end+1) = mean(RN2(Keep));                                       %#ok<SAGROW>
        Sb(:,end+1) = local_blocks(M, Keep, Nb, By, Bx);                   %#ok<SAGROW>
        Vb(:,end+1) = local_blocks(V, Keep, Nb, By, Bx);                   %#ok<SAGROW>
        Rb(:,end+1) = local_blocks(RN2, Keep, Nb, By, Bx);                 %#ok<SAGROW>
        clear M V Keep
    end
    % the quantity the PTC routes fit: the variance the signal added
    Dat.(Ty) = struct('Step',Sid, 'X',Xv, 'Signal',Sm, 'Var',Vm, 'RN2',Rm, ...
                      'Excess',Vm-Rm, 'Nframes',Nf, 'SignalBlock',Sb, 'VarBlock',Vb, ...
                      'RN2Block',Rb, 'ExcessBlock',Vb-Rb);
    fprintf('  %s ladder: %d steps, signals %.1f .. %.1f ADU, %.0f s\n', ...
        Ty, numel(Sid), min(Sm), max(Sm), toc(T0));
end
RN2b = mean(Dat.D.RN2Block, 2);
RN2m = mean(RN2(:), 'omitnan');

% ---- the four routes, each over its nominal window and over alternatives
% The windows cannot be fixed step numbers: the two bias-board setups put their
% dark ladders in signal ranges that barely overlap, so step 7 is 94 ADU on one
% and 2277 on the other. The dark response starts from the window stage 2a chose
% by goodness of fit and varies it; the dark PTC takes the whole linear part of
% the ladder and trims it. Both are intersected with the steps actually read, so
% a window can never reach outside the linear range.
Win = struct();
AvD   = Dat.D.Step;
if exist('DieStepsExplicit','var') && DieStepsExplicit
    % 'signal' mode: one list per ladder, used by the response route and the PTC
    % route of that ladder alike, so a) and c) fit the same dark points and b)
    % and d) the same bright ones. The window systematic is then the sub-windows
    % of that list, not other windows of the ladder: varying it further would
    % leave the signal range the mode exists to fix.
    Win.a = local_vary(DieFitStepsD, AvD);
    Win.c = Win.a;
    Win.b = local_vary(DieFitStepsB, Dat.B.Step);
    Win.d = Win.b;
else
    Win.a = local_vary(DieFitStepsD, AvD);
    Win.c = local_trim(AvD);
    Win.b = {[1 2 3 4], [2 3 4], [1 2 3], [3 4 5]};               % light response
    Win.d = {[1 2 3 4], [2 3 4], [1 2 3 4 5], [1 2 3]};           % PTC light
end
% The bright ladder is the same optical setup in every run, so its step numbers
% are stable -- but not guaranteed, and a step that drifted out of the PTC signal
% window would silently be fitted by the response route and dropped by the PTC
% one, which is exactly the "one signal range" property the config claims.
BmedB = Dat.B.Signal(ismember(Dat.B.Step, DieFitStepsB));
if ~(exist('DieStepsExplicit','var') && DieStepsExplicit) && ...
        (numel(BmedB)~=numel(DieFitStepsB) || any(BmedB<DieGainRange(1)) || any(BmedB>DieGainRange(2)))
    error('ultrasat:lab:scripts:brightwindow', ...
          ['the bright response window [%s] has medians %s ADU, which do not all lie inside the ' ...
           'PTC window %g-%g ADU: the two routes would no longer share a signal range'], ...
          strtrim(sprintf('%d ', DieFitStepsB)), strtrim(sprintf('%.0f ', BmedB)), ...
          DieGainRange(1), DieGainRange(2));
end
fprintf('  dark windows: nominal [%s], %d variants; dark PTC over %d linear steps (<= %g ADU)\n', ...
    strtrim(sprintf('%d ', Win.a{1})), numel(Win.a)-1, numel(AvD), DieLinLimit);

R = struct();
R.a = local_route('a', 'dark response',  Dat.D, Win.a, [], RN2b, RN2m, P.ExpSen);
R.c = local_route('c', 'PTC dark',       Dat.D, Win.c, [], RN2b, RN2m, P.ExpSen);
R.d = local_route('d', 'PTC bright',     Dat.B, Win.d, [], RN2b, RN2m, P.ExpSen);
R.b = local_route('b', 'light response', Dat.B, Win.b, R.a, RN2b, RN2m, P.ExpSen);

% ---- what if the gain is the same on both ladders?
% Force g = g_bright on the dark points and fit only the offset. If the two
% ladders really share a gain, the residuals are flat and the implied threshold is
% the dark one; if they do not, the constrained fit has to absorb the difference
% in its offset and leaves a trend behind.
Sel  = find(ismember(Dat.D.Step, Win.c{1}));
Gb   = R.d.Gain;
Off  = mean(Dat.D.Excess(Sel) - Gb.*Dat.D.Signal(Sel));
Res  = Dat.D.Excess(Sel) - (Gb.*Dat.D.Signal(Sel) + Off);
Free = R.c;
Con  = struct('GainImposed',Gb, 'Offset',Off, 'Threshold',Off./Gb, ...
              'Signal',Dat.D.Signal(Sel), 'Residual',Res, ...
              'ResidRMS',sqrt(mean(Res.^2)), ...
              'FreeGain',Free.Gain, 'FreeOffset',Free.Intercept, ...
              'FreeResidRMS',sqrt(mean((Dat.D.Excess(Sel) - ...
                   (Free.Gain.*Dat.D.Signal(Sel) + Free.Intercept)).^2)));
R.constrained = Con;

% threshold in electrons needs a gain: use the bright PTC, and propagate its error
Gn = R.d.Gain;  Ge = hypot(R.d.GainStat, R.d.GainSyst);
for Rt = {'a','b','c','d'}
    Q = R.(Rt{1});
    Q.Threshold_e     = Q.Threshold./Gn;
    Q.Threshold_e_err = abs(Q.Threshold_e).*hypot(hypot(Q.ThresholdStat, Q.ThresholdSyst)./max(abs(Q.Threshold),eps), Ge./Gn);
    R.(Rt{1}) = Q;
end

S = struct('Stage',10, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, 'Size',Siz, ...
           'NBlock',Nb, 'BlockSize',[By Bx], 'RN2',RN2m, 'ExpSen',P.ExpSen, ...
           'GainForElectrons',Gn, 'GainForElectronsErr',Ge, 'Routes',R, ...
           'Windows',struct('a',{Win.a}, 'b',{Win.b}, 'c',{Win.c}, 'd',{Win.d}));
Fid = fopen(fullfile(DieOut, 'methods.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\n%-16s %22s %26s\n', '', 'gain [ADU/e-]', 'charge threshold');
fprintf('%-16s %10s %5s %5s %12s %5s %5s %9s\n', 'route', 'value', 'stat', 'syst', ...
    'value [ADU]', 'stat', 'syst', 'T [e-]');
for Rt = {'a','b','c','d'}
    Q = R.(Rt{1});
    if isnan(Q.Gain)
        Gs = sprintf('%10s %5s %5s', '-', '-', '-');
    else
        Gs = sprintf('%10.4f %5.4f %5.4f', Q.Gain, Q.GainStat, Q.GainSyst);
    end
    fprintf('%-16s %s %12.2f %5.2f %5.2f %5.1f+-%4.1f\n', [Rt{1} ') ' Q.Name], Gs, ...
        Q.Threshold, Q.ThresholdStat, Q.ThresholdSyst, Q.Threshold_e, Q.Threshold_e_err);
end
fprintf('\nother fitted parameters of the response routes:\n');
fprintf('  a) dark current %.4f +- %.4f (stat) +- %.4f (syst) ADU/s\n', ...
    R.a.Slope, R.a.SlopeStat, R.a.SlopeSyst);
fprintf('  b) response     %.1f +- %.1f (stat) +- %.1f (syst) ADU per intensity unit\n', ...
    R.b.Slope, R.b.SlopeStat, R.b.SlopeSyst);
fprintf('\nforcing the bright gain %.4f on the dark ladder: offset %+.2f ADU^2 -> T = %+.2f ADU,\n', ...
    Con.GainImposed, Con.Offset, Con.Threshold);
fprintf('  residual rms %.2f ADU^2 against %.2f when the gain is free; residuals by step:\n    ', ...
    Con.ResidRMS, Con.FreeResidRMS);
for I = 1:1:numel(Con.Signal)
    fprintf('%.0f:%+.1f  ', Con.Signal(I), Con.Residual(I));
end
fprintf('\n');
fprintf('\nthe PTC routes also carry an estimator systematic: fitting the same points weighted by\n');
fprintf('1/Var^2, as stage 9 does, gives g = %.4f (dark) and %.4f (bright) instead of %.4f and %.4f.\n', ...
    R.c.GainWeighted, R.d.GainWeighted, R.c.Gain, R.d.Gain);
fprintf('\nthe systematic is the fit window and it dominates everywhere; the statistical error is\n');
fprintf('the scatter between %d independent blocks of the die, which already includes its structure.\n', Nb.^2);
fprintf('[%4.0f s] METHODS DONE -> %s\n', toc(T0), DieOut);

function W = local_vary(Sd, Avail)
    % The nominal dark-response window and the defensible variations of it: drop
    % the lowest step, drop the highest, add the next lower one. Each is kept
    % only if it still has three steps -- two leave no degree of freedom to
    % judge a fit by -- and lies inside the steps that were read.
    Sd = sort(Sd(ismember(Sd, Avail)));
    if numel(Sd)<3
        error('ultrasat:lab:scripts:window', ...
              'the chosen dark window has only %d of its steps inside the linear range', numel(Sd));
    end
    C = {Sd, Sd(2:end), Sd(1:end-1), unique([Sd(1)-1, Sd])};
    W = local_keep(C, Avail);
end

function W = local_trim(Avail)
    % The whole linear dark ladder and shorter versions of it, which is what the
    % fixed {1:9, 3:9, 5:9, 2:8} did for a nine-step ladder.
    Avail = sort(Avail);
    N = numel(Avail);
    C = {Avail, Avail(min(3, max(N-2,1)):end), Avail(min(5, max(N-2,1)):end), Avail(2:max(N-1,3))};
    W = local_keep(C, Avail);
end

function W = local_keep(C, Avail)
    % drop the candidates that fall outside the ladder, are too short, or repeat
    W = {};
    for I = 1:1:numel(C)
        V = sort(C{I}(ismember(C{I}, Avail)));
        if numel(V)>=3 && (isempty(W) || ~any(cellfun(@(U) isequal(U, V), W)))
            W{end+1} = V;                                                  %#ok<AGROW>
        end
    end
    if isempty(W)
        error('ultrasat:lab:scripts:window', 'no window of 3 steps inside the linear range');
    end
end

function B = local_blocks(A, Keep, Nb, By, Bx)
    % mean of A over the kept pixels of each block, as a column of Nb^2 values
    B = nan(Nb.^2, 1);
    for Iy = 1:1:Nb
        for Ix = 1:1:Nb
            Ry = (Iy-1).*By + (1:By);
            Rx = (Ix-1).*Bx + (1:Bx);
            Sub = A(Ry, Rx);  Kp = Keep(Ry, Rx);
            B((Iy-1).*Nb + Ix) = mean(Sub(Kp));
        end
    end
end

function Q = local_route(Tag, Name, L, Wins, Dark, RN2b, RN2m, ExpSen)
    % Tag a/b: response (signal vs X).  Tag c/d: PTC (variance vs signal).
    IsPTC = any(strcmp(Tag, {'c','d'}));
    Vals  = nan(numel(Wins), 2);                      % [slope intercept] per window, ensemble
    for Iw = 1:1:numel(Wins)
        Sel = find(ismember(L.Step, Wins{Iw}));
        if numel(Sel)<2, continue; end
        if IsPTC
            Vals(Iw,:) = local_fit(L.Signal(Sel), L.Excess(Sel));
        else
            Vals(Iw,:) = local_fit(L.X(Sel), L.Signal(Sel));
        end
    end
    Nom = Vals(1,:);
    % statistical error: the same fit inside each block
    Sel = find(ismember(L.Step, Wins{1}));
    Nbk = size(L.SignalBlock, 1);
    Bv  = nan(Nbk, 2);
    for Ib = 1:1:Nbk
        if IsPTC
            Bv(Ib,:) = local_fit(L.SignalBlock(Ib,Sel), L.ExcessBlock(Ib,Sel));
        else
            Bv(Ib,:) = local_fit(L.X(Sel), L.SignalBlock(Ib,Sel));
        end
    end
    Se = std(Bv, 0, 1, 'omitnan')./sqrt(sum(isfinite(Bv(:,1))));
    Sy = (max(Vals,[],1,'omitnan') - min(Vals,[],1,'omitnan'))./2;

    Q = struct('Tag',Tag, 'Name',Name, 'Windows',{Wins}, 'WindowValues',Vals, ...
               'Slope',Nom(1), 'SlopeStat',Se(1), 'SlopeSyst',Sy(1), ...
               'Intercept',Nom(2), 'InterceptStat',Se(2), 'InterceptSyst',Sy(2), ...
               'Gain',NaN, 'GainStat',NaN, 'GainSyst',NaN);
    switch Tag
        case 'a'                                   % T = -I_D
            Q.Threshold     = -Nom(2);
            Q.ThresholdStat = Se(2);
            Q.ThresholdSyst = Sy(2);
        case 'b'                                   % T = DC*ExpSen - I_B
            Q.Threshold     = Dark.Slope.*ExpSen - Nom(2);
            Q.ThresholdStat = hypot(Dark.SlopeStat.*ExpSen, Se(2));
            Q.ThresholdSyst = hypot(Dark.SlopeSyst.*ExpSen, Sy(2));
        otherwise                                  % PTC: g, and T = (c - RN^2)/g
            % Estimator systematic. The nominal fit is unweighted on the MEAN
            % variance against the MEAN signal, which is the unbiased estimator of
            % the ensemble relation. Stage 9 instead weights by 1/Var^2, which on a
            % slightly curved PTC lets the low-signal points set the slope. The two
            % answers differ by about a per cent and neither is wrong, so half the
            % difference joins the systematic.
            Sel2 = find(ismember(L.Step, Wins{1}));
            Dof2 = max(L.Nframes(Sel2)-1, 1);
            Wv   = (Dof2./(2.*L.Var(Sel2).^2)).';
            Am   = [ones(numel(Sel2),1), L.Signal(Sel2).'];
            Cw   = (Am.'*(Wv.*Am))\(Am.'*(Wv.*L.Var(Sel2).'));
            Q.GainWeighted      = Cw(2);
            Q.InterceptWeighted = Cw(1);
            Q.GainEstimatorSyst = abs(Cw(2) - Nom(1))./2;
            Q.Gain = Nom(1);  Q.GainStat = Se(1);
            Q.GainSyst = hypot(Sy(1), Q.GainEstimatorSyst);
            Q.Threshold     = Nom(2)./Nom(1);          % the fit is on Var - RN^2
            Bt = Bv(:,2)./Bv(:,1);
            Q.ThresholdStat = std(Bt, 'omitnan')./sqrt(sum(isfinite(Bt)));
            Tw = Vals(:,2)./Vals(:,1);
            Tweight = Cw(1)./Cw(2);
            Q.ThresholdWeighted = Tweight;
            Q.ThresholdSyst = hypot((max(Tw)-min(Tw))./2, abs(Tweight - Q.Threshold)./2);
    end
end

function C = local_fit(X, Y)
    X = X(:);  Y = Y(:);
    Ok = isfinite(X) & isfinite(Y);
    if nnz(Ok)<2
        C = [NaN NaN];
        return
    end
    A = [ones(nnz(Ok),1), X(Ok)];
    B = A\Y(Ok);
    C = [B(2) B(1)];
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 10 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
