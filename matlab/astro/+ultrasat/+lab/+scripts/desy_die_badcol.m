% Stage 4 of the single-die chain: BAD READOUT COLUMNS.
%   Reads no frames. The three per-pixel maps the earlier stages measured are
%   enough -- read noise (stage 1), dark current (stage 2) and photo-response
%   (stage 3) -- and the whole-die response map costs a full ladder read, so
%   recomputing it here would be wasteful.
%   Two criteria decide, both per raw readout column (image rows in the DESY
%   orientation): excess read noise and a dead or weak light response, each
%   as a robust-sigma outlier of the column profile and as a plain ratio to
%   its median (see ultrasat.lab.PTCAnalysis.badColumns). A plain ratio alone
%   finds nothing -- the column-to-column spread is a few per cent, so a
%   column twice as noisy is a 60-sigma outlier but only 2x the median.
%   The dark-current profile is reported as well but does NOT mask: a column
%   with more leakage is still a working column, and the dark map of this
%   device has a 2:1 ramp that no column-level cut can separate from a defect.
%   Output in DieOut: mask.bin (uint8, 1 = good pixel, the mask stages 5 and 6
%   use) and badcol.json with the three profiles and the flags.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 4 (bad columns): from the stage 1-3 maps, no frames read\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', 'ExpTimeOffset',DieExpOffset);
P.read;
G   = P.rawColGeom;
Siz = [G.Ny G.Nx];

RN   = readBin(fullfile(DieStage1, 'rn.bin'),  Siz, 'stage 1 (desy_rn_single_die)');
DC   = readBin(fullfile(DieOut,    'dc.bin'),  Siz, 'stage 2 (desy_die_dark)');
RESP = readBin(fullfile(DieOut,    'resp.bin'),Siz, 'stage 3 (desy_die_light)');
BIAS = readBin(fullfile(DieStage1, 'bias.bin'),Siz, 'stage 1');
TDRK = readBin(fullfile(DieOut,    'tdark.bin'),Siz, 'stage 2 (desy_die_dark)');

B = P.badColumns('NoiseMap',RN, 'RespMap',RESP, 'NoiseSigma',DieNoiseSigma);
Red = 3 - G.Dim;
DcProfile = squeeze(median(double(DC), Red, 'omitnan'));
BiasProfile  = squeeze(median(double(BIAS), Red, 'omitnan'));
TdarkProfile = squeeze(median(double(TDRK), Red, 'omitnan'));
% The dark current is not flat across the die: it rises monotonically along the
% readout-column direction. Summarise the two ends and the middle so the report
% can describe it without re-reading the frames. Bands are in RAW COLUMN order,
% so band 1 is the columns read out first.
[~, Ord] = sort(G.RawCol);
Nc3 = floor(numel(Ord)./3);
Grad = struct('BandRawCol',nan(1,3), 'DC',nan(1,3), 'Bias',nan(1,3), 'RN',nan(1,3), ...
              'Resp',nan(1,3), 'Tdark',nan(1,3));
for Ib = 1:1:3
    Sel = Ord((Ib-1).*Nc3 + (1:Nc3));
    Grad.BandRawCol(Ib) = median(G.RawCol(Sel));
    Grad.DC(Ib)    = median(DcProfile(Sel), 'omitnan');
    Grad.Bias(Ib)  = median(BiasProfile(Sel), 'omitnan');
    Grad.RN(Ib)    = median(B.NoiseProfile(Sel), 'omitnan');
    Grad.Resp(Ib)  = median(B.RespProfile(Sel), 'omitnan');
    Grad.Tdark(Ib) = median(TdarkProfile(Sel), 'omitnan');
end
Grad.DCRatio   = Grad.DC(1)./Grad.DC(3);
Grad.RespRatio = Grad.Resp(1)./Grad.Resp(3);
Grad.RNRatio   = Grad.RN(1)./Grad.RN(3);
Grad.BiasDiff  = Grad.Bias(1) - Grad.Bias(3);
Grad.TdarkDiff = Grad.Tdark(1) - Grad.Tdark(3);
Grad.DCDiff    = Grad.DC(1) - Grad.DC(3);
Grad.CrossTime = Grad.TdarkDiff./max(Grad.DCDiff, eps);   % excess(t) = dDC*t - dT
DcMed     = median(DcProfile(isfinite(DcProfile)));
DcSig     = 1.4826.*median(abs(DcProfile(isfinite(DcProfile)) - DcMed));

writeBin(fullfile(DieOut, 'mask.bin'), uint8(B.GoodMask), 'uint8');

% do the flagged columns come in (2k-1, 2k) readout pairs, as the read noise does?
Bad     = ismember(G.RawCol, B.BadRawCol);
Partner = G.RawCol + (-1).^(mod(G.RawCol,2)+1);            % odd 2k-1 <-> even 2k
[~, Loc] = ismember(Partner, G.RawCol);
Has      = Loc>0;
BothBad  = false(size(Bad));
BothBad(Has) = Bad(Has) & Bad(Loc(Has));
Npair    = nnz(BothBad);

S = struct('Stage',4, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Size',Siz, 'ReadoutDim',G.Dim, 'RawCol',G.RawCol, ...
           'NoiseProfile',B.NoiseProfile, 'NoiseMedian',B.NoiseMedian, 'NoiseSigma',B.NoiseSigma, ...
           'RespProfile',B.RespProfile, 'RespMedian',B.RespMedian, 'RespSigma',B.RespSigma, ...
           'DcProfile',DcProfile, 'DcMedian',DcMed, 'DcSigma',DcSig, ...
           'BiasProfile',BiasProfile, 'TdarkProfile',TdarkProfile, 'Gradient',Grad, ...
           'BadNoise',B.BadNoise, 'BadResp',B.BadResp, 'BadRawCol',B.BadRawCol, ...
           'Nbad',B.Nbad, 'Nrawcol',B.Nrawcol, 'NbadInPairs',Npair, ...
           'NoiseSigmaCut',B.NoiseSigmaCut, 'NoiseFactor',B.NoiseFactor, ...
           'RespSigmaCut',B.RespSigmaCut, 'RespFactor',B.RespFactor, ...
           'GoodFraction',nnz(B.GoodMask)./numel(B.GoodMask));
Good = B.GoodMask;
S.Effect = struct('RNall',median(double(RN(:)),'omitnan'), 'RNgood',median(double(RN(Good)),'omitnan'), ...
                  'RespAll',median(double(RESP(:)),'omitnan'), 'RespGood',median(double(RESP(Good)),'omitnan'), ...
                  'DCall',median(double(DC(:)),'omitnan'), 'DCgood',median(double(DC(Good)),'omitnan'));
fprintf('  %d of %d raw readout columns flagged (%.3f %% of the pixels), %d of them in complete pairs\n', ...
    B.Nbad, B.Nrawcol, 100.*(1-S.GoodFraction), Npair);
fprintf('  noise    median %.4f ADU, robust sigma %.4f -> cut at %.4f (%g sigma) or %.4f (%gx)\n', ...
    B.NoiseMedian, B.NoiseSigma, B.NoiseMedian + B.NoiseSigmaCut.*B.NoiseSigma, B.NoiseSigmaCut, ...
    B.NoiseFactor.*B.NoiseMedian, B.NoiseFactor);
fprintf('  response median %.1f ADU/int, robust sigma %.1f -> cut at %.1f (%g sigma) or %.1f (%gx)\n', ...
    B.RespMedian, B.RespSigma, B.RespMedian - B.RespSigmaCut.*B.RespSigma, B.RespSigmaCut, ...
    B.RespFactor.*B.RespMedian, B.RespFactor);
fprintf('  dark     median %.4f ADU/s, robust sigma %.4f (reported only, never masked)\n', DcMed, DcSig);

Nshow = 20;
fprintf('\nworst %d of the %d flagged columns:\n', min(Nshow,B.Nbad), B.Nbad);
fprintf('%-8s %10s %10s %12s %10s %8s\n', 'rawcol', 'RN [ADU]', 'sigma', 'R [ADU/int]', 'sigma', 'why');
Flag = find(ismember(G.RawCol, B.BadRawCol));
Key  = max((B.NoiseProfile(Flag)-B.NoiseMedian)./B.NoiseSigma, ...
           (B.RespMedian-B.RespProfile(Flag))./B.RespSigma);
[~, Ord] = sort(Key, 'descend');
Ord = Ord(1:min(Nshow, numel(Ord)));
for Ii = Ord(:).'
    I = Flag(Ii);
    Why = '';
    if B.BadNoise(I), Why = [Why, 'noise ']; end
    if B.BadResp(I),  Why = [Why, 'response']; end
    fprintf('%-8d %10.4f %10.1f %12.1f %10.1f  %s\n', G.RawCol(I), ...
        B.NoiseProfile(I), (B.NoiseProfile(I)-B.NoiseMedian)./B.NoiseSigma, ...
        B.RespProfile(I), (B.RespProfile(I)-B.RespMedian)./B.RespSigma, Why);
end
Cuts = [3 5 8 10 20 50];
Nc   = arrayfun(@(C) nnz(B.NoiseProfile > B.NoiseMedian + C.*B.NoiseSigma), Cuts);
S.CutScan = struct('Sigma',Cuts, 'Nbad',Nc);
fprintf('\nhow sharp is the cut: columns above the noise median by\n  ');
for Ic = 1:1:numel(Cuts)
    fprintf('%g sigma: %d   ', Cuts(Ic), Nc(Ic));
end
fprintf('\n');

Fid = fopen(fullfile(DieOut, 'badcol.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\nmedians with and without the flagged columns:\n');
fprintf('  read noise %.4f -> %.4f ADU,  response %.1f -> %.1f ADU/int,  dark %.4f -> %.4f ADU/s\n', ...
    S.Effect.RNall, S.Effect.RNgood, S.Effect.RespAll, S.Effect.RespGood, S.Effect.DCall, S.Effect.DCgood);
Dz = (DcProfile - DcMed)./DcSig;
[~, Od] = sort(Dz, 'descend');
fprintf('dark-current outliers (not masked): %s\n', ...
    strjoin(arrayfun(@(I) sprintf('%d:%+.1fs', G.RawCol(I), Dz(I)), Od(1:6).', 'UniformOutput',false), ' '));
fprintf(['\ngradient along the readout direction (thirds, band 1 = read out first):\n', ...
    '  raw column   %8.0f %8.0f %8.0f\n', ...
    '  dark current %8.4f %8.4f %8.4f ADU/s   (first/last %.2f)\n', ...
    '  bias         %8.2f %8.2f %8.2f ADU      (difference %+.2f)\n', ...
    '  read noise   %8.3f %8.3f %8.3f ADU      (ratio %.3f)\n', ...
    '  response     %8.0f %8.0f %8.0f ADU/int  (ratio %.4f)\n', ...
    '  T_dark       %8.2f %8.2f %8.2f ADU      (difference %+.2f)\n', ...
    '  the two ends cross at t = %.0f s; below it the first-read columns are the darker\n'], ...
    Grad.BandRawCol, Grad.DC, Grad.DCRatio, Grad.Bias, Grad.BiasDiff, ...
    Grad.RN, Grad.RNRatio, Grad.Resp, Grad.RespRatio, Grad.Tdark, Grad.TdarkDiff, Grad.CrossTime);
fprintf('[%4.0f s] BADCOL DONE -> %s\n', toc(T0), DieOut);

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 4 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
