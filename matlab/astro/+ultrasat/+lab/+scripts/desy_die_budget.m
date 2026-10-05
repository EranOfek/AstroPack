% Stage 6 of the single-die chain: the NOISE BUDGET in electrons.
%   Reads no frames: the five maps the earlier stages measured are enough.
%   For an incident charge Q the pixel collects Qc = Q - max(T,0) and
%     sigma_eff^2(Q) = RN^2 + Qc + DC*t
%                      + [(1-f)*sigma_T]^2       offset fixed pattern
%                      + [(1-f)*sigma_DC*t]^2    DSNU
%                      + [(1-f)*PRNU*Qc]^2       photo-response FPN
%   with f = 1 once the fixed patterns are calibrated out and f = 0 for a
%   single raw frame (ultrasat.lab.PTCAnalysis.budgetCurve). SNR = Qc/sigma_eff
%   and the limiting signal is where it reaches SNRdet.
%   Every fixed-pattern term is the LOCAL (block-detrended) spread, not the
%   spread over the die: a budget asks what varies between neighbouring
%   pixels, and the die-wide figures of stages 2 and 3 are dominated by
%   large-scale structure that any flat field removes.
%   The charge threshold is the one quantity the chain does not determine. The
%   three routes disagree by a factor four -- 7.7 e- from the shot noise
%   (stage 5, the only route that extrapolates nothing), 17.1 e- from the dark
%   response and 29.0 e- from the light response -- and the difference is most
%   of the signal range this test is about, so ALL THREE are carried through
%   to the end and none is preferred here.
%   Also carried: the mask (every number with and without the stage 4 bad
%   columns) and the gain's 2.9 % window systematic, which scales every
%   electron-unit quantity at once.
%   Output in DieOut: budget.json.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 6 (noise budget): from the stage 1-5 maps\n', DieTag);
Dark  = jsondecode(fileread(local_need(DieOut,    'dark.json',  'stage 2')));
Light = jsondecode(fileread(local_need(DieOut,    'light.json', 'stage 3')));
Ptc   = jsondecode(fileread(local_need(DieOut,    'ptc.json',   'stage 5')));
Siz   = Ptc.Size(:).';
RN    = readBin(fullfile(DieStage1, 'rn.bin'),  Siz, 'stage 1');
DC    = readBin(fullfile(DieOut,    'dc.bin'),  Siz, 'stage 2');
TD    = readBin(fullfile(DieOut,    'tdark.bin'),  Siz, 'stage 2');
TL    = readBin(fullfile(DieOut,    'tlight.bin'), Siz, 'stage 3');
RS    = readBin(fullfile(DieOut,    'resp.bin'),   Siz, 'stage 3');
Mask  = [];
if isfile(fullfile(DieOut, 'mask.bin'))
    Fid  = fopen(fullfile(DieOut, 'mask.bin'), 'r');
    Mask = logical(fread(Fid, Siz, 'uint8'));
    fclose(Fid);
end

Qgrid = logspace(0, 3.3, 160);
Tt    = Light.ExpSen;
Sdet  = [5 3];
Names = {'PTC','Dark','Light'};
Tadu  = [Ptc.Thresholds.PTC_ADU, Ptc.Thresholds.Dark_ADU, Ptc.Thresholds.Light_ADU];
% the offset fixed pattern is measured once, from the best-determined map: the
% dark threshold map pulls 2.7 ADU out of an observed 6.5, the light map 9.0
% out of 52.2. The light variant uses its own map; the PTC borrows the dark
% one, having none of its own, and the sensitivity to that is reported.
SigTsrc = {'dark','dark','light'};

B = struct('Stage',6, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',Ptc.Lot, 'Wafer',Ptc.Wafer, 'Device',Ptc.Device, 'Size',Siz, ...
           'ExpTime',Tt, 'Qgrid',Qgrid, 'SNRdet',Sdet, 'Block',DieBlock, ...
           'ThresholdNames',{Names}, 'ThresholdADU',Tadu, 'SigmaTSource',{SigTsrc}, ...
           'GainSystematic',Ptc.GainSystematic);

for Mk = {'Unmasked','Masked'}
    Use = true(Siz);
    if strcmp(Mk{1}, 'Masked')
        if isempty(Mask), continue; end
        Use = Mask;
    end
    Gm = Ptc.(Mk{1}).All.GainMean;
    In = struct('GainADU',Gm, 'ExpTime',Tt, ...
                'RN_ADU',   median(double(RN(Use)), 'omitnan'), ...
                'DC_ADU',   median(double(DC(Use)), 'omitnan'));
    % local (pixel-to-pixel) fixed patterns, recomputed under this mask
    Ldc = ultrasat.lab.PTCAnalysis.localSpread(DC, 'Block',DieBlock, 'Mask',Use, ...
              'StdFit',Dark.Fit.All.SlopeSpread.StdFitRobust);
    Ltd = ultrasat.lab.PTCAnalysis.localSpread(TD, 'Block',DieBlock, 'Mask',Use, ...
              'StdFit',Dark.Fit.All.InterceptSpread.StdFitRobust);
    Ltl = ultrasat.lab.PTCAnalysis.localSpread(TL, 'Block',DieBlock, 'Mask',Use, ...
              'StdFit',Light.Threshold.StdFitRobust);
    Lrs = ultrasat.lab.PTCAnalysis.localSpread(RS, 'Block',DieBlock, 'Mask',Use, ...
              'StdFit',Light.Fit.All.SlopeSpread.StdFitRobust);
    In.SigmaDC_ADU = Ldc.StdIntr;
    In.SigmaTdark_ADU = Ltd.StdIntr;
    In.SigmaTlight_ADU = Ltl.StdIntr;
    In.PRNU = Lrs.RelIntr;
    In.Npix = nnz(Use);
    B.(Mk{1}).Inputs = In;
    for It = 1:1:numel(Names)
        Sig = In.SigmaTdark_ADU;
        if strcmp(SigTsrc{It}, 'light')
            Sig = In.SigmaTlight_ADU;
        end
        Arg = struct('RN_e',In.RN_ADU./Gm, 'DC_e',In.DC_ADU./Gm, 'ExpTime',Tt, ...
                     'SigmaT_e',Sig./Gm, 'SigmaDC_e',In.SigmaDC_ADU./Gm, ...
                     'PRNU',In.PRNU, 'Threshold_e',Tadu(It)./Gm);
        for Is = 1:1:numel(Sdet)
            Arg.SNRdet = Sdet(Is);
            C = ultrasat.lab.PTCAnalysis.budgetCurve(Qgrid, Arg);
            if Is==1
                B.(Mk{1}).(Names{It}) = C;
            end
            B.(Mk{1}).(Names{It}).(sprintf('Qlim_cal_%d', Sdet(Is))) = C.Qlim_cal;
            B.(Mk{1}).(Names{It}).(sprintf('Qlim_raw_%d', Sdet(Is))) = C.Qlim_raw;
        end
        % sensitivity of the uncalibrated curve to the other offset pattern
        Alt = Arg;
        Alt.SigmaT_e = In.SigmaTlight_ADU./Gm;
        if strcmp(SigTsrc{It}, 'light')
            Alt.SigmaT_e = In.SigmaTdark_ADU./Gm;
        end
        Alt.SNRdet = Sdet(1);
        Ca = ultrasat.lab.PTCAnalysis.budgetCurve(Qgrid, Alt);
        B.(Mk{1}).(Names{It}).Qlim_raw_altFPN = Ca.Qlim_raw;
        % gain systematic: the whole budget scales with g. Indexed rather than
        % compared by value: when the gain scan holds a single window the two
        % ends are equal, and `if Gg==Min` would then take the same branch twice
        % and leave Qlim_cal_gmax undefined.
        Gends = [Ptc.GainSystematic.Min Ptc.GainSystematic.Max];
        for Ig = 1:1:2
            Gg = Gends(Ig);
            Ag = struct('RN_e',In.RN_ADU./Gg, 'DC_e',In.DC_ADU./Gg, 'ExpTime',Tt, ...
                        'SigmaT_e',Sig./Gg, 'SigmaDC_e',In.SigmaDC_ADU./Gg, ...
                        'PRNU',In.PRNU, 'Threshold_e',Tadu(It)./Gg, 'SNRdet',Sdet(1));
            Cg = ultrasat.lab.PTCAnalysis.budgetCurve(Qgrid, Ag);
            if Ig==1
                B.(Mk{1}).(Names{It}).Qlim_cal_gmin = Cg.Qlim_cal;
            else
                B.(Mk{1}).(Names{It}).Qlim_cal_gmax = Cg.Qlim_cal;
            end
        end
    end
end

Fid = fopen(fullfile(DieOut, 'budget.json'), 'w');
fwrite(Fid, jsonencode(B));
fclose(Fid);

for Mk = {'Unmasked','Masked'}
    if ~isfield(B, Mk{1}), continue; end
    In = B.(Mk{1}).Inputs;
    fprintf('\n--- %s (%.1f M pixels), gain %.4f ADU/e-, exposure %g s\n', ...
        Mk{1}, In.Npix./1e6, In.GainADU, Tt);
    fprintf('  read noise %.3f e-   dark current %.4f e-/s (%.2f e- in %g s)\n', ...
        In.RN_ADU./In.GainADU, In.DC_ADU./In.GainADU, In.DC_ADU.*Tt./In.GainADU, Tt);
    fprintf('  pixel-to-pixel patterns: DSNU %.4f e-/s (%.2f e- in %g s), PRNU %.3f %%, offset %.2f e- (dark map) or %.2f e- (light map)\n', ...
        In.SigmaDC_ADU./In.GainADU, In.SigmaDC_ADU.*Tt./In.GainADU, Tt, 100.*In.PRNU, ...
        In.SigmaTdark_ADU./In.GainADU, In.SigmaTlight_ADU./In.GainADU);
    fprintf('\n  %-7s %9s %11s %11s %11s %11s %13s\n', ...
        'T from', 'T [e-]', 'Qlim5 cal', 'Qlim5 raw', 'Qlim3 cal', 'Qlim3 raw', 'Qlim5 cal (g)');
    for It = 1:1:numel(Names)
        C = B.(Mk{1}).(Names{It});
        fprintf('  %-7s %9.1f %11.1f %11.1f %11.1f %11.1f %6.1f-%-6.1f\n', Names{It}, ...
            C.Threshold_e, C.Qlim_cal_5, C.Qlim_raw_5, C.Qlim_cal_3, C.Qlim_raw_3, ...
            C.Qlim_cal_gmax, C.Qlim_cal_gmin);
    end
end

fprintf('\nwhich term dominates (unmasked, PTC threshold, uncalibrated):\n');
C = B.Unmasked.PTC;
fprintf('  %-10s %10s %10s %10s %10s %10s %10s\n', 'Q [e-]', 'RN^2', 'shot', 'dark', 'offset', 'DSNU', 'PRNU');
for Qv = [10 30 100 300 1000]
    [~, Iq] = min(abs(C.Q - Qv));
    Tm = C.Terms;
    Tot = Tm.RN(Iq)+Tm.Shot(Iq)+Tm.DarkShot(Iq)+Tm.OffsetFPN(Iq)+Tm.DSNU(Iq)+Tm.PRNU(Iq);
    fprintf('  %-10.0f %9.0f%% %9.0f%% %9.0f%% %9.0f%% %9.0f%% %9.0f%%\n', C.Q(Iq), ...
        100.*Tm.RN(Iq)./Tot, 100.*Tm.Shot(Iq)./Tot, 100.*Tm.DarkShot(Iq)./Tot, ...
        100.*Tm.OffsetFPN(Iq)./Tot, 100.*Tm.DSNU(Iq)./Tot, 100.*Tm.PRNU(Iq)./Tot);
end
fprintf('\nthe threshold is the dominant uncertainty: Qlim(SNR 5, calibrated) spans %.1f to %.1f e-\n', ...
    min([B.Unmasked.PTC.Qlim_cal_5 B.Unmasked.Dark.Qlim_cal_5 B.Unmasked.Light.Qlim_cal_5]), ...
    max([B.Unmasked.PTC.Qlim_cal_5 B.Unmasked.Dark.Qlim_cal_5 B.Unmasked.Light.Qlim_cal_5]));
fprintf('  against %.1f e- of gain systematic and %.1f e- from the bad-column mask\n', ...
    abs(B.Unmasked.PTC.Qlim_cal_gmax - B.Unmasked.PTC.Qlim_cal_gmin), ...
    abs(B.Unmasked.PTC.Qlim_cal_5 - B.Masked.PTC.Qlim_cal_5));
fprintf('[%4.0f s] BUDGET DONE -> %s\n', toc(T0), DieOut);

function P = local_need(Dir, Name, Who)
    P = fullfile(Dir, Name);
    if ~isfile(P)
        error('ultrasat:lab:scripts:stage', 'stage 6 needs %s from %s', P, Who);
    end
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 6 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
