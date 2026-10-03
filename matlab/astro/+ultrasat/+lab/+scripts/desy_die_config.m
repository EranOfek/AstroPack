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
% Which threshold the budget uses. Measured on run 32 W04_D07 (whole die):
% the DARK method moves from 8.9 to 25.0 ADU as the fit window is raised from
% all 9 steps to the top 3, monotonically, because the dark ladder is bent at
% the bottom -- residuals to the published window are +11.6, +9.9, +7.2 and
% +3.4 ADU at steps 1-4 -- and this run's whole dark ladder (39-172 ADU) sits
% inside that knee. The LIGHT method's intercept moves only 25.5 to 29.5 ADU
% over every window inside the linear range of the bright ladder (500-2900
% ADU, where its own residuals are below 2 ADU), and the dark current enters
% it only as DC*ExpSen = 4.7 ADU, so the dark window barely matters. Hence
% 'light'; 'dark' is kept for comparison.
DieThreshold = 'light';                % 'light' | 'dark'
DieBlock     = 32;                     % block side of the pixel-to-pixel (local) spreads
% Bad-column cut (stage 4). This has a bigger effect on the headline numbers
% than the threshold choice: at 5 sigma it masks 275 of 4740 readout columns,
% 5.8 % of the pixels, and improves the read-noise median by 2.7 %. The noisy
% columns are a smooth tail, not a separate population (488 columns at 3
% sigma, 271 at 5, 93 at 10), so this is a choice, not a defect count, and
% every ensemble number from stage 5 on is reported both masked and unmasked.
DieNoiseSigma = 5;
% Signal window of the per-pixel PTC fit (stage 5), [ADU] of the measured
% bright signal. Steps are selected by the ENSEMBLE median, not per pixel: the
% top edge sits only 2.6 % above step 5 (1170 ADU), so a per-pixel test would
% include or drop that step according to the pixel's own response and build a
% selection effect into the gain. The window stays below the 3-5 kADU variance
% dip and the INL above 2900 ADU. It does include the bright ladder's
% low-signal knee, which shifts a PTC intercept but not its slope as long as
% the knee is additive -- DieGainScan tests that.
DieGainRange = [100 1200];
DieGainScan  = {[100 1200], [100 800], [300 1200], [300 2500], [100 2900]};
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
