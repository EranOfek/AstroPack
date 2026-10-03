% Shared settings of the single-die stage tools (desy_die_*.m).
%   One run / wafer / die / gain half at a time. Every stage script starts by
%   running this file, so it is the only place to edit when moving to another
%   dataset; the stages themselves take no arguments.
%   The stages are independent and rerunnable: each writes its own binary
%   maps and stats.json under DieOut and validates the dumps of the earlier
%   stages (run, die, gain half and image size) before using them.
%     stage 1  desy_rn_single_die   bias and read noise from the ZE frames
%     stage 2  desy_die_dark        per-pixel dark current and dark threshold
%     stage 3  desy_die_light       per-pixel response, PRNU, light threshold
%     stage 4  desy_die_badcol      bad readout columns (needs 1 and 3)
%     stage 5  desy_die_ptc         per-pixel variance vs mean, gain
%     stage 6  desy_die_budget      sigma_eff and SNR in electrons
%   The whole die is processed in the STREAMED mode (CCDSEC empty): the
%   per-pixel methods read one step at a time and keep only the running sums,
%   so 22.5 M pixels cost ~3 GB instead of the ~20 GB a cached ladder of the
%   whole die would need.
DieRun       = '32';
DieFolder    = 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27';
Die          = 'W04_D07';
DieGain      = 'high';                 % 'high' | 'low'
DieLot       = 'TH02954';
% Fit windows. The streamed mode needs an explicit step list ('auto' resolves
% the steps from the cached region ladder, which full mode does not build);
% these are the published steps of run 32 -- dark medians 34..151 ADU, bright
% 120..508 ADU. Run 31 (22x the dark current) uses D [5 6 7] instead.
DieFitStepsD = [5 6 7 8 9];
DieFitStepsB = [5 6 7];
DieGainADU   = [];                     % [ADU/e-] for the electron columns; [] = report ADU only
DieRoot      = '/Data1/DESY';
if ~isfolder(DieRoot)
    DieRoot = '/bigdata3/projects/ultrasat/DESY';
end
DieTag    = sprintf('run%s_%s_%s', DieRun, Die, DieGain);
DieDev    = fullfile(DieRoot, DieFolder, ['LOT_', DieLot, '_', Die]);
DieOut    = fullfile('/home/sasha/claude/desy_die', DieTag);
DieStage1 = fullfile('/home/sasha/claude/desy_rn', DieTag);     % desy_rn_single_die output
if ~isfolder(DieOut)
    mkdir(DieOut);
end
