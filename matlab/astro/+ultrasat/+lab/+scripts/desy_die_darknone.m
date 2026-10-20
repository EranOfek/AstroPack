% Stage 2 substitute for a unit that has NO DARK LADDER.
%   The INTERACTIVE runs of the DESY campaign carry a bright ladder and two
%   zero-light references at the SAME sensor exposure, but no ladder of dark
%   exposures, so there is nothing to fit a dark current to. What they do carry
%   is better than a fitted dark current for the purpose stage 3 needs it:
%   their DARK frames have the same 15 s exposure, the same reset setting
%   (20 row-0 resets, 300 us photodiode delay) and the same readout as every
%   light frame, so subtracting them removes the bias, the dark signal and the
%   reset pedestal together. Referenced that way the dark contribution to a
%   bright frame is zero BY CONSTRUCTION, not by assumption.
%
%   This stage therefore writes the maps stage 3 and everything after it expect,
%   with the dark current and its variance identically zero, and marks the dump
%   NoDarkLadder so that no reader mistakes a structural zero for a measurement.
%   The consequences, which the report states:
%     - the dark current, the DSNU and the dark-route threshold are NOT measured
%       by such a unit (use a DC run of the same configuration for those);
%     - the light-route threshold reduces to -Intercept, i.e. the deficit at
%       zero light measured against the matched dark reference, with no
%       DC*ExpSen pedestal to subtract and no dark-ladder extrapolation in it.
%
%   Settings: ultrasat.lab.scripts.desy_die_config. Output in DieOut: dc.bin,
%   tdark.bin, dc_var.bin, chi2.bin, nused.bin, rawcol.bin and dark.json, the
%   same set desy_die_dark writes.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 2 (no dark ladder): the bright ladder is referenced to the run''s own dark frames\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'Test',DieTest, 'CCDSEC',DieCCDSEC, 'Gain',DieGain, 'Parity','rawcol', ...
                             'IntensityScale',DieIntScale, 'ExpTimeOffset',DieExpOffset, 'DropFrames',DieDropFrames, ...
                             'GainADU',DieGainADU, 'Verbosity',0);
P.read;
P.subtractZero;

Nd = sum(strcmp(P.Frames.FrameType, 'D'));
if Nd > 0
    error('ultrasat:lab:scripts:darknone', ...
          ['this unit has %d dark-ladder frames: run desy_die_darkwindow and desy_die_dark ' ...
           'instead of desy_die_darknone'], Nd);
end

G   = P.rawColGeom;
Siz = size(P.Zero);
Z   = zeros(Siz, 'single');
fprintf('  %d reference frames, %d x %d pixels, level %.3f ADU, RN median %.4f ADU\n', ...
    P.NZero, Siz(1), Siz(2), median(P.Zero(:)), median(P.ZeroNoise(:)));

writeBin(fullfile(DieOut, 'dc.bin'),     Z);
writeBin(fullfile(DieOut, 'tdark.bin'),  Z);
writeBin(fullfile(DieOut, 'dc_var.bin'), Z);
writeBin(fullfile(DieOut, 'chi2.bin'),   Z);
writeBin(fullfile(DieOut, 'nused.bin'),  zeros(Siz, 'uint8'), 'uint8');
writeBin(fullfile(DieOut, 'rawcol.bin'), int32(G.RawCol), 'int32');

Zero1 = struct('Npix',numel(Z), 'Median',0, 'Mean',0, 'StdObs',0, 'StdRobust',0, ...
               'StdFit',0, 'StdIntr',0, 'RelIntr',0, 'RelUL95',0, 'Sigma',0, 'StdFitRobust',0);
S = struct('Stage',2, 'Tag',DieTag, 'ExpTimeOffset',DieExpOffset, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, ...
           'Size',Siz, 'ReadoutDim',G.Dim, 'Mode',P.Mode, ...
           'NZero',P.NZero, 'BiasLevel',median(P.Zero(:)), 'RNMedian',median(P.ZeroNoise(:)), ...
           'GainADU',DieGainADU, 'FitSteps',[], 'ExpTime',[], ...
           'StepMedian',[], 'VarStep',[], 'Weights','none', ...
           'NoDarkLadder',true, ...
           'Reference',struct('Mode',refMode(P), 'Nframes',P.NZero, 'Level',median(P.Zero(:))));
for Pn = {'All','Even','Odd'}
    S.Fit.(Pn{1}) = struct('Npix',numel(Z), 'SlopeSpread',Zero1, 'InterceptSpread',Zero1, ...
                           'CorrSlopeIntercept',0, 'MedianChi2Dof',0, 'MedianResidRMS',0);
    S.Pattern.(Pn{1}) = struct('Mean',[], 'Std',[], 'Rel',[]);
end
S.PatternStep = [];  S.PatternX = [];  S.PatternNframes = [];
S.Local = struct('DC',Zero1, 'T',Zero1);
S.Chi2DofExpected = [];
Fid = fopen(fullfile(DieOut, 'dark.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('[%4.0f s] DARKNONE DONE -> %s (dark current and dark-route threshold NOT measured)\n', toc(T0), DieOut);

function M = refMode(P)
    % which image mode the reference frames came from, as the reader recorded it
    M = 'ZE';
    if ~isempty(P.AI) && ~isempty(P.AI(1).HeaderData)
        V = P.AI(1).HeaderData.getVal('IMG-MODE');
        if ~isempty(V) && ischar(V)
            M = strtrim(V);
        end
    end
end

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end
