% Stage 10: the four routes to the gain and the charge threshold, with errors.
%   a) dark response    S = DC*t + I_D            -> T = -I_D            (no gain)
%   b) light response   S = R*int + I_B           -> T = DC*ExpSen - I_B (no gain)
%   c) PTC, dark ladder   Var = g*S + c           -> g, and T = (c-RN^2)/g
%   d) PTC, bright ladder Var = g*S + c           -> g, and T = (c-RN^2)/g
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
Lad = struct('Type',{'D','B'}, 'Max',{Inf, 1200});
Dat = struct();
for Il = 1:1:numel(Lad)
    Ty    = Lad(Il).Type;
    Flag  = strcmp(P.Frames.FrameType, Ty);
    Steps = unique(P.Frames.Step(Flag)).';
    Sid=[]; Xv=[]; Sm=[]; Vm=[]; Nf=[]; Sb=[]; Vb=[]; Rb=[];
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
        Sb(:,end+1) = local_blocks(M, Keep, Nb, By, Bx);                   %#ok<SAGROW>
        Vb(:,end+1) = local_blocks(V, Keep, Nb, By, Bx);                   %#ok<SAGROW>
        Rb(:,end+1) = local_blocks(RN2, Keep, Nb, By, Bx);                 %#ok<SAGROW>
        clear M V Keep
    end
    Dat.(Ty) = struct('Step',Sid, 'X',Xv, 'Signal',Sm, 'Var',Vm, 'Nframes',Nf, ...
                      'SignalBlock',Sb, 'VarBlock',Vb, 'RN2Block',Rb);
    fprintf('  %s ladder: %d steps, signals %.1f .. %.1f ADU, %.0f s\n', ...
        Ty, numel(Sid), min(Sm), max(Sm), toc(T0));
end
RN2b = mean(Dat.D.RN2Block, 2);
RN2m = mean(RN2(:), 'omitnan');

% ---- the four routes, each over its nominal window and over alternatives
Win = struct();
Win.a = {[7 8 9], [6 7 8 9], [5 6 7 8 9], [8 9]};                 % dark response
Win.b = {[1 2 3 4], [2 3 4], [1 2 3], [3 4 5]};                   % light response
Win.c = {1:9, 3:9, 5:9, 2:8};                                     % PTC dark
Win.d = {[1 2 3 4], [2 3 4], [1 2 3 4 5], [1 2 3]};               % PTC light

R = struct();
R.a = local_route('a', 'dark response',  Dat.D, Win.a, [], RN2b, RN2m, P.ExpSen);
R.c = local_route('c', 'PTC dark',       Dat.D, Win.c, [], RN2b, RN2m, P.ExpSen);
R.d = local_route('d', 'PTC bright',     Dat.B, Win.d, [], RN2b, RN2m, P.ExpSen);
R.b = local_route('b', 'light response', Dat.B, Win.b, R.a, RN2b, RN2m, P.ExpSen);

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
fprintf('\nthe PTC routes also carry an estimator systematic: fitting the same points weighted by\n');
fprintf('1/Var^2, as stage 9 does, gives g = %.4f (dark) and %.4f (bright) instead of %.4f and %.4f.\n', ...
    R.c.GainWeighted, R.d.GainWeighted, R.c.Gain, R.d.Gain);
fprintf('\nthe systematic is the fit window and it dominates everywhere; the statistical error is\n');
fprintf('the scatter between %d independent blocks of the die, which already includes its structure.\n', Nb.^2);
fprintf('[%4.0f s] METHODS DONE -> %s\n', toc(T0), DieOut);

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
            Vals(Iw,:) = local_fit(L.Signal(Sel), L.Var(Sel));
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
            Bv(Ib,:) = local_fit(L.SignalBlock(Ib,Sel), L.VarBlock(Ib,Sel));
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
            Q.Threshold     = (Nom(2) - RN2m)./Nom(1);
            Bt = (Bv(:,2) - RN2b)./Bv(:,1);
            Q.ThresholdStat = std(Bt, 'omitnan')./sqrt(sum(isfinite(Bt)));
            Tw = (Vals(:,2) - RN2m)./Vals(:,1);
            Tweight = (Cw(1) - RN2m)./Cw(2);
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
