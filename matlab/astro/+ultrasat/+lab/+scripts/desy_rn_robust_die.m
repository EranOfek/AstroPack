% Read noise of one die-half from the ZE frames, with robust per-frame estimators.
%   The chain measures the read noise of a pixel as the standard deviation of its
%   readings over the zero-exposure frames (zeroNoiseStats). With ten frames that
%   estimate carries nine degrees of freedom and is not robust: one outlying
%   reading - a cosmic ray, a telegraph switch - moves it as much as genuine noise
%   does. This script measures the same pixels with three robust estimators beside
%   the standard deviation, so the die figures can be re-derived and the movement
%   read off:
%       SD     the plain standard deviation, the chain's own definition, kept as
%              the control: it must reproduce rn.bin pixel for pixel
%       MAD    median absolute deviation from the per-pixel median, no factor
%       QN     the Rousseeuw-Croux Qn: the k-th order statistic of the pairwise
%              absolute differences, k = C(h,2) with h = floor(Nf/2)+1, no factor
%       CLIP   the standard deviation after the single most deviant reading of the
%              pixel is dropped - nine readings left, eight degrees of freedom
%   All four maps are written RAW, in ADU, with no consistency factor, because at
%   ten readings no textbook factor applies. The readings are integers, so every
%   estimator is quantised: MAD in steps of half an ADU, Qn in steps of one, and
%   SD and CLIP in steps of 1/90 and 1/72 of an ADU SQUARED (for integer readings
%   10*sum(x-mean)^2 and 9*sum(x-mean)^2 are themselves integers). A plain die
%   median therefore lands on a tooth of that comb and would report the same read
%   noise for dies that differ - measured, not assumed: the first calibration run
%   returned 1.5000, 1.5811 = sqrt(2.5), 1.7321 = sqrt(3) and 2.0000 on adjacent
%   points of the sigma grid.
%
%   Two things follow, and this script does both.
%   1. The die-level statistic of every estimator is the GROUPED median: the tooth
%      the median falls in is read as an interval of its own width and the median
%      is interpolated inside it, which is continuous in sigma where the plain
%      median is a staircase. For SD and CLIP the grouping is done on the SQUARE,
%      where the teeth are evenly spaced, and the result is taken back by sqrt.
%   2. The scale is fixed by a Monte-Carlo of the same estimators, through the
%      same code, on the same kind of data: ten readings, the phase of the bias
%      level uniform, the sum rounded to integers, this die's own per-frame common
%      mode taken out as the chain takes it, and the pixels given an intrinsic
%      spread of sigma. That gives, for each estimator, the curve of its die-level
%      statistic against the true sigma, to be inverted at the measured value.
%
%   Set before running (the same DieSelect the chain stages take):
%       DieSelect   struct with Run, Folder, Die, Gain and optionally Lot, Tag,
%                   Source, Test, CCDSEC - one die-half
%       RobustArgs  optional struct:
%           .McOnly      true to build a common-mode-free calibration and stop,
%                        reading no frames (for the method, not for a die)
%           .OutRoot     where to write (default /home/sasha/claude/desy_rn_robust)
%           .RnDir       the chain's directory of this half, for the SD check
%           .Overwrite   true to replace an existing output of the same tag
%           .McNpix      pixels per sigma of the Monte-Carlo (default 200000)
%           .McSigma     the sigma grid in ADU (default 0.8:0.05:3.2)
%           .McSpread    intrinsic pixel-to-pixel spread of sigma, as a fraction
%                        (default [0 0.20]: none, and the measured one). The grid
%                        value is the MEDIAN of the pixel sigmas, which is what a
%                        die-level median estimates.
%           .Chunk       rows of the die per chunk (default 150: the Qn step holds
%                        45 pairwise differences of every pixel of the chunk)
%
%   Writes into <OutRoot>/<Tag>/: rn_sd.bin, rn_mad.bin, rn_qn.bin, rn_clip.bin
%   (single, [Ny Nx], column-major, ADU, raw), calibration.json (this die's own)
%   and robust_stats.json, which carries the die-level statistic of each estimator
%   and the sigma it inverts to. The chain's desy_rn/<tag>/ is never written to.
%   The searched-pixel medians and the electrons are taken downstream, with the
%   same border and the same gain the figures use.
%
% Author: A.M. Krassilchtchikov

%% ---------------------------------------------------------------- arguments
if ~exist('RobustArgs', 'var') || ~isstruct(RobustArgs)
    RobustArgs = struct;
end
RA = RobustArgs;
if ~isfield(RA, 'McOnly'),    RA.McOnly    = false;   end
if ~isfield(RA, 'OutRoot'),   RA.OutRoot   = '/home/sasha/claude/desy_rn_robust'; end
if ~isfield(RA, 'RnDir'),     RA.RnDir     = '';      end
if ~isfield(RA, 'Overwrite'), RA.Overwrite = false;   end
if ~isfield(RA, 'McNpix'),    RA.McNpix    = 200000;  end
if ~isfield(RA, 'McSigma'),   RA.McSigma   = 0.8:0.05:3.2; end
if ~isfield(RA, 'McSpread'),  RA.McSpread  = [0 0.20]; end
if ~isfield(RA, 'Chunk'),     RA.Chunk     = 150;     end
if ~isfolder(RA.OutRoot), mkdir(RA.OutRoot); end

NF     = 10;                                 % the frames of run 41; checked below
QNK    = nchoosek(floor(NF/2)+1, 2);         % 15 of the 45 pairwise differences
ENAMES = {'SD','MAD','QN','CLIP'};
% the comb of each estimator: the power the teeth are even in, and their width
TOOTH  = struct('SD',   struct('Pow',2, 'W',1./90), ...
                'MAD',  struct('Pow',1, 'W',0.5), ...
                'QN',   struct('Pow',1, 'W',1.0), ...
                'CLIP', struct('Pow',2, 'W',1./72));

%% --------------------------------------------------- the common-mode-free run
if RA.McOnly
    Mc = local_mc(RA, zeros(1, NF), NF, QNK, ENAMES, TOOTH);
    Fid = fopen(fullfile(RA.OutRoot, 'calibration_nocm.json'), 'w');
    fwrite(Fid, jsonencode(Mc));  fclose(Fid);    % R2020b has no PrettyPrint
    fprintf('McOnly: wrote %s, no frame read\n', fullfile(RA.OutRoot, 'calibration_nocm.json'));
    return
end

%% ------------------------------------------------------------------ the die
if ~exist('DieSelect', 'var') || ~isstruct(DieSelect)
    error('ultrasat:lab:scripts:noDieSelect', 'Define DieSelect before running this script');
end
Run    = DieSelect.Run;
Folder = DieSelect.Folder;
Die    = DieSelect.Die;
Gain   = DieSelect.Gain;
Lot    = 'TH02954';
if isfield(DieSelect, 'Lot') && ~isempty(DieSelect.Lot), Lot = DieSelect.Lot; end
Source = 'aspect';  Test = 'PTC_int_hr';  Sec = [];
if isfield(DieSelect, 'Source') && ~isempty(DieSelect.Source), Source = DieSelect.Source; end
if isfield(DieSelect, 'Test')   && ~isempty(DieSelect.Test),   Test   = DieSelect.Test;   end
if isfield(DieSelect, 'CCDSEC'), Sec = DieSelect.CCDSEC; end
if isfield(DieSelect, 'Tag') && ~isempty(DieSelect.Tag)
    Tag = DieSelect.Tag;
elseif strcmp(Lot, 'TH02954')
    Tag = sprintf('run%s_%s_%s', Run, Die, Gain);
else
    Tag = sprintf('run%s_%s_%s_%s', Run, Lot, Die, Gain);
end
OutDir = fullfile(RA.OutRoot, Tag);
if isfile(fullfile(OutDir, 'robust_stats.json')) && ~RA.Overwrite
    error('ultrasat:lab:scripts:exists', '%s already holds a result; set RobustArgs.Overwrite', OutDir);
end
if ~isfolder(OutDir), mkdir(OutDir); end

Root = '/Data1/DESY';
if ~isfolder(Root), Root = '/bigdata3/projects/ultrasat/DESY'; end
if strcmpi(Source, 'ucfits')
    Dev = DieSelect.DieDir;
else
    Dev = fullfile(Root, Folder, ['LOT_', Lot, '_', Die]);
end

T0 = tic;
fprintf('%s: reading the ZE frames of the whole die\n', Tag);
P = ultrasat.lab.PTCAnalysis(Dev, 'Test',Test, 'CCDSEC',Sec, 'Gain',Gain, 'Parity','rawcol');
P.read;
% the same cube zeroNoiseStats measures, through the accessor that selects the
% frames of one run when the device holds a run pair
Cube = P.zeroCube;
[Ny, Nx, Nf] = size(Cube);
fprintf('  %d ZE frames, %d x %d pixels, %.0f s\n', Nf, Ny, Nx, toc(T0));
if Nf ~= NF
    error('ultrasat:lab:scripts:nframes', '%d ZE frames, the comb was worked out for %d', Nf, NF);
end
% the readings must be integers before anything is taken out of them: the comb
% widths above and the rounding in the Monte-Carlo both rest on it
Frac = max(max(max(abs(Cube - round(Cube)))));
fprintf('  largest departure of a reading from an integer: %.6g ADU\n', Frac);
if Frac > 1e-3
    error('ultrasat:lab:scripts:notinteger', 'the readings are not integers (%.6g ADU)', Frac);
end

% the per-frame common mode, exactly as zeroNoiseStats takes it: a 5-sigma
% clipped mean a frame, of which only the deviation from the mean is removed
C2 = reshape(Cube, [], Nf);
CM = zeros(1, Nf);
for If = 1:1:Nf
    V  = C2(:, If);
    V  = V(isfinite(V));
    Mv = median(V);
    Sv = 1.4826.*median(abs(V - Mv));
    if Sv>0
        V = V(abs(V - Mv) <= 5.*Sv);
    end
    CM(If) = mean(V);
end
clear C2
CmDev = CM - mean(CM);
Cube  = Cube - reshape(CmDev, 1, 1, Nf);
fprintf('  common mode removed, levels %.4f to %.4f ADU (std %.6f), %.0f s\n', ...
        min(CM), max(CM), std(CM), toc(T0));

% this die's own calibration: the same estimators, the same die-level statistic,
% the same common mode, through the same code
Mc  = local_mc(RA, CmDev, NF, QNK, ENAMES, TOOTH);
Fid = fopen(fullfile(OutDir, 'calibration.json'), 'w');
fwrite(Fid, jsonencode(Mc));  fclose(Fid);
fprintf('  calibration done, %.0f s\n', toc(T0));

% the four maps, chunk by chunk over the readout columns
Maps = struct();
for Ie = 1:1:numel(ENAMES)
    Maps.(ENAMES{Ie}) = zeros(Ny, Nx, 'single');
end
for I0 = 1:RA.Chunk:Ny
    I1 = min(I0+RA.Chunk-1, Ny);
    X  = double(reshape(Cube(I0:I1, :, :), [], Nf));
    E  = local_estimate(X, QNK);
    for Ie = 1:1:numel(ENAMES)
        Maps.(ENAMES{Ie})(I0:I1, :) = single(reshape(E.(ENAMES{Ie}), I1-I0+1, Nx));
    end
    if mod(I0-1, 10.*RA.Chunk)==0
        fprintf('    rows %d-%d of %d, %.0f s\n', I0, I1, Ny, toc(T0));
    end
end
clear Cube X E
fprintf('  four estimators done, %.0f s\n', toc(T0));

%% ------------------------------------------------------------- what came out
S = struct('Tag',Tag, 'Run',Run, 'Lot',Lot, 'Die',Die, 'Gain',Gain, 'Nframes',Nf, ...
           'Size',[Ny Nx], 'QnK',QNK, 'Tooth',TOOTH, 'IntegerDeparture',Frac, ...
           'CommonMode',struct('Levels',CM, 'Mean',mean(CM), 'Std',std(CM)));
% the control: the SD map must be the chain's own rn.bin
if ~isempty(RA.RnDir) && isfile(fullfile(RA.RnDir, 'rn.bin'))
    Fid = fopen(fullfile(RA.RnDir, 'rn.bin'), 'r');
    Ref = reshape(fread(Fid, Ny.*Nx, 'single=>single'), Ny, Nx);  fclose(Fid);
    D   = abs(double(Maps.SD) - double(Ref));
    S.SdCheck = struct('RnDir',RA.RnDir, 'MaxAbsDiff',max(D(:)), ...
                       'MedianAbsDiff',median(D(:)), 'MedianRef',median(double(Ref(:))), ...
                       'MedianNew',median(double(Maps.SD(:))));
    fprintf('  SD against the chain rn.bin: largest difference %.3g ADU, median %.3g\n', ...
            S.SdCheck.MaxAbsDiff, S.SdCheck.MedianAbsDiff);
else
    S.SdCheck = [];
    fprintf('  SD not checked: no rn.bin given\n');
end

% whole-die figures; the searched-pixel ones are taken downstream from the maps
for Ie = 1:1:numel(ENAMES)
    Nm = ENAMES{Ie};
    V  = double(Maps.(Nm)(:));
    St = local_dieStat(V, TOOTH.(Nm).Pow, TOOTH.(Nm).W);
    S.WholeDie.(Nm) = struct('DieStat',St, 'Median',median(V), 'Mean',mean(V), ...
                             'RobustSigma',1.4826.*median(abs(V - median(V))));
    for Is = 1:1:numel(RA.McSpread)
        Fn = sprintf('Spread%d', round(100.*RA.McSpread(Is)));
        S.WholeDie.(Nm).(Fn) = local_invert(Mc.Curve.(Fn).(Nm), Mc.Sigma, St);
    end
    local_writeBin(fullfile(OutDir, ['rn_', lower(Nm), '.bin']), Maps.(Nm));
end
Fid = fopen(fullfile(OutDir, 'robust_stats.json'), 'w');
fwrite(Fid, jsonencode(S));  fclose(Fid);
fprintf('%s: done, %.0f s -> %s\n', Tag, toc(T0), OutDir);

%% ------------------------------------------------------------ local functions
function Mc = local_mc(RA, CmDev, NF, QNK, ENAMES, TOOTH)
    % The calibration: for each sigma of the grid, a population of pixels read ten
    % times, rounded to integers as the detector reads them, with the given
    % per-frame common mode taken out of them as the chain takes it out, put
    % through the same estimators and the same die-level statistic as the data.
    Tmc = tic;
    fprintf('  Monte-Carlo: %d sigmas x %d pixels x %d readings, spreads [%s], CM std %.5f\n', ...
            numel(RA.McSigma), RA.McNpix, NF, num2str(RA.McSpread), std(CmDev));
    Mc = struct('Nframes',NF, 'Npix',RA.McNpix, 'Sigma',RA.McSigma, 'Spread',RA.McSpread, ...
                'QnK',QNK, 'CmDev',CmDev, 'CmStd',std(CmDev));
    rng(20261127, 'twister');
    for Is = 1:1:numel(RA.McSpread)
        Sp = RA.McSpread(Is);
        Fn = sprintf('Spread%d', round(100.*Sp));
        for Ie = 1:1:numel(ENAMES)
            Mc.Curve.(Fn).(ENAMES{Ie}) = zeros(1, numel(RA.McSigma));
        end
        for Ig = 1:1:numel(RA.McSigma)
            Sg  = RA.McSigma(Ig);
            Sig = Sg.*ones(RA.McNpix, 1);
            if Sp>0
                % Lognormal, so sigma stays positive and the spread is relative.
                % The grid value is the MEDIAN of the pixel sigmas, not their mean,
                % because the die-level statistic estimates a median: a lognormal
                % written to preserve the mean would put its median a factor
                % sqrt(1+Sp^2) lower - 2 % at Sp = 0.2 - and the inversion would
                % return that much too high a read noise.
                Sig = Sg.*exp(sqrt(log(1+Sp.^2)).*randn(RA.McNpix,1));
            end
            Mu = rand(RA.McNpix, 1);                       % phase of the bias level
            X  = round(Mu + Sig.*randn(RA.McNpix, NF)) - CmDev;
            E  = local_estimate(X, QNK);
            for Ie = 1:1:numel(ENAMES)
                Nm = ENAMES{Ie};
                Mc.Curve.(Fn).(Nm)(Ig) = local_dieStat(E.(Nm), TOOTH.(Nm).Pow, TOOTH.(Nm).W);
            end
        end
    end
    Mc.Seconds = toc(Tmc);
end

function E = local_estimate(X, QnK)
    % The four estimators of one population, X being [Npix x Nframes] of readings.
    % Raw: no consistency factor is applied here, the calibration carries it.
    Nf  = size(X, 2);
    E.SD  = std(X, 0, 2);
    Med   = median(X, 2);
    Dev   = abs(X - Med);
    E.MAD = median(Dev, 2);
    % Qn: the QnK-th smallest of the Nf*(Nf-1)/2 pairwise absolute differences
    Np = Nf.*(Nf-1)./2;
    Dp = zeros(size(X,1), Np);
    Ip = 0;
    for Ia = 1:1:Nf-1
        for Ib = Ia+1:1:Nf
            Ip = Ip + 1;
            Dp(:, Ip) = abs(X(:,Ia) - X(:,Ib));
        end
    end
    Sm = mink(Dp, QnK, 2);
    E.QN = Sm(:, end);
    % CLIP: drop the single most deviant reading (the first of a tie) and take the
    % standard deviation of the nine that are left. Done in closed form from the
    % sums, which avoids building a second copy of the chunk: with S and Q the sum
    % and the sum of squares of the ten, dropping x gives
    %   sum  = S - x,  sumsq = Q - x^2,  SS = (Q - x^2) - (S - x)^2/(Nf-1)
    [~, Iw] = max(Dev, [], 2);
    Xw = X(sub2ind(size(X), (1:size(X,1)).', Iw));
    Sx = sum(X, 2);
    Qx = sum(X.^2, 2);
    Ss = (Qx - Xw.^2) - ((Sx - Xw).^2)./(Nf-1);
    E.CLIP = sqrt(max(Ss, 0)./(Nf-2));
end

function M = local_dieStat(V, Pow, W)
    % The die-level statistic of an estimator over a population: the GROUPED
    % median, the median interpolated across the tooth of the comb it falls in,
    % which is continuous in sigma where the plain median is a staircase. Pow = 2
    % groups on the square, where SD and CLIP have evenly spaced teeth, and takes
    % the result back by sqrt.
    V = V(isfinite(V));
    if isempty(V)
        M = NaN;  return
    end
    if Pow==2
        V = V.^2;
    end
    % The lattice is not fully occupied - for the SD of ten integer readings the
    % attainable values of 90*sigma^2 near the median are 284, 285, 289, 296, 300,
    % ... with gaps of 1, 3, 4 and 7 - so a tooth cannot be given the width of the
    % quantum. Each occupied value is given the interval reaching midway to its
    % occupied neighbours, and the median is interpolated inside the interval it
    % falls in; where the lattice IS evenly occupied, as it is for MAD and Qn,
    % this is the ordinary grouped median.
    T = round(V./W);
    [U, ~, Ib] = unique(T);
    Cn = accumarray(Ib, 1);
    Cu = cumsum(Cn);
    N2 = numel(V)./2;
    K  = find(Cu >= N2, 1, 'first');
    if numel(U)<2
        M = U(1).*W;
    else
        if K==1
            ELo = U(1) - 0.5.*(U(2) - U(1));
        else
            ELo = 0.5.*(U(K-1) + U(K));
        end
        if K==numel(U)
            EHi = U(end) + 0.5.*(U(end) - U(end-1));
        else
            EHi = 0.5.*(U(K) + U(K+1));
        end
        Be = 0;
        if K>1, Be = Cu(K-1); end
        M = (ELo + (EHi - ELo).*(N2 - Be)./Cn(K)).*W;
    end
    if Pow==2
        M = sqrt(max(M, 0));
    end
end

function local_writeBin(Path, A)
    % One map as single, column-major, the format the chain's own maps use.
    Fid = fopen(Path, 'w');
    fwrite(Fid, single(A), 'single');
    fclose(Fid);
end

function Sg = local_invert(Curve, Sigma, Stat)
    % The sigma whose die-level statistic is Stat, by interpolation of the
    % calibration curve. NaN if the measurement falls outside the grid, which is
    % a refusal to extrapolate rather than a guess.
    Curve = Curve(:);  Sigma = Sigma(:);
    [C, I] = unique(Curve);
    S = Sigma(I);
    if Stat < min(C) || Stat > max(C)
        Sg = NaN;
    else
        Sg = interp1(C, S, Stat, 'linear');
    end
end
