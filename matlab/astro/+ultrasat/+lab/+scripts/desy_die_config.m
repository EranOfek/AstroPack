% Shared settings of the single-die stage tools (desy_die_*.m).
%   One run / wafer / die / gain half at a time. Every stage script starts by
%   running this file, so it is the only place to edit when moving to another
%   dataset; the stages themselves take no arguments.
%   The stages are independent and rerunnable: each writes its own binary
%   maps and stats.json under DieOut and validates the dumps of the earlier
%   stages (run, die, gain half and image size) before using them.
%     stage 1   desy_rn_single_die   bias and read noise from the ZE frames
%     stage 2a  desy_die_darkwindow  which dark steps are the straight part
%     stage 2   desy_die_dark        per-pixel dark current and dark threshold
%     stage 3   desy_die_light       per-pixel response, PRNU, light threshold
%     stage 4   desy_die_badcol      bad readout columns (needs 1 and 3)
%     stage 5   desy_die_ptc         per-pixel variance vs mean, gain
%     stage 6   desy_die_budget      sigma_eff and SNR in electrons
%     stage 7   desy_die_varspread   spread of the per-pixel variance per step
%     stage 8   desy_die_lowsignal   is that variance explained, pixel by pixel
%     stage 9   desy_die_ptc_perpixel  a gain per pixel on each ladder
%     stage 10  desy_die_methods     four routes to gain and threshold, with errors
%   The whole die is processed in the STREAMED mode (CCDSEC empty): the
%   per-pixel methods read one step at a time and keep only the running sums,
%   so 22.5 M pixels cost ~3 GB instead of the ~20 GB a cached ladder of the
%   whole die would need.
% A driver can select the dataset by defining DieSelect (Run, Folder, Die, Gain)
% before running this file; otherwise the defaults below apply. The stage scripts
% run in the caller's workspace, so nothing else has to change.
if exist('DieSelect', 'var') && isstruct(DieSelect)
    DieRun    = DieSelect.Run;
    DieFolder = DieSelect.Folder;
    Die       = DieSelect.Die;
    DieGain   = DieSelect.Gain;
else
    DieRun    = '32';
    DieFolder = 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27';
    Die       = 'W04_D07';
    DieGain   = 'high';                % 'high' | 'low'
end
DieLot       = 'TH02954';
% Fit windows. The streamed mode needs an explicit step list ('auto' resolves
% the steps from the cached region ladder, which full mode does not build).
% Dark: NOT a setting. stage 2a (desy_die_darkwindow) measures it per die and
% writes darkwindow.json, which this file reads below; the list here is only the
% fallback for a die whose scan has not been run. The reason it cannot be a
% setting is that the two bias-board setups put their dark ladders in signal
% ranges that barely overlap -- at the same nine exposures one reaches 172 ADU
% and the other 3829 -- so the straight part of the ladder is not the same steps
% in both, and the ladder bends at BOTH ends: a charge threshold lifts the
% shortest exposures above the line, and the INL bends the longest ones down.
DieFitStepsD = [7 8 9];
DieDarkTol   = 1.10;      % goodness-of-fit tolerance for the automatic dark window
% Highest step median any fit may use, [ADU]. Above it the measured integral
% non-linearity exceeds 0.5 % and the PTC starts into the 3-5 kADU variance dip,
% so a point there is not on the straight line the fits assume. Only the
% high-dark-current setup reaches it on the dark ladder (3829 ADU at the longest
% exposure against 172 on the low one), which is why it has to be a limit rather
% than a step list.
DieLinLimit  = 2900;
% Bright: every step whose median signal is below 1000 ADU (120, 248, 507 and
% 772 ADU). The bright ladder's knee is at the BOTTOM -- steps 1 and 2 sit
% +15.0 and +9.6 ADU above the line of the published window -- so two of these
% four points are inside it, and this window's intercept, and with it the
% light-route threshold, is not the same quantity the published window
% measures. The slope is barely affected; the intercept is.
DieFitStepsB = [1 2 3 4];
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
% window now covers the same four steps as the bright response fit, so the
% gain and the response are measured over one signal range. The window stays below the 3-5 kADU variance
% dip and the INL above 2900 ADU. It does include the bright ladder's
% low-signal knee, which shifts a PTC intercept but not its slope as long as
% the knee is additive -- DieGainScan tests that.
% The lower edge is 80, not 100, and the reason is a cross-die one. The window
% is meant to hold the same four lowest bright steps as the response fit, so
% that the gain and the response are measured over one signal range. But the
% level of the lowest bright step is not a constant of the test: it follows each
% die's response, and across this lot it runs from 93 to 142 ADU (highest on run
% 31 W04_D05, lowest on run 32 W08_D04). A 100 ADU floor therefore kept step 1
% on six die-runs and silently dropped it on two, so those two would have been
% compared with the rest at a different window -- in exactly the quantities, the
% gain and the shot-noise threshold, that the cross-die comparison is about. At
% 80 every die-run uses steps 1-4 in both routes. Nothing physical happens at
% either number: the floor exists only to keep the saturating upper ladder out.
DieGainRange = [80 1000];
DieGainScan  = {[80 1000], [80 800], [300 1000], [80 1200], [300 2500]};
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
% The dark window is chosen per die by desy_die_darkwindow (goodness of fit),
% because the two bias-board setups put their dark ladders in signal ranges that
% do not overlap: at the same exposures run 31 reaches 3500 ADU and run 32 only
% 172, so one fixed list of steps cannot be right for both. The list above is
% the fallback when that stage has not been run.
if isfile(fullfile(DieOut, 'darkwindow.json'))
    DwJ = jsondecode(fileread(fullfile(DieOut, 'darkwindow.json')));
    if ~isequal(DwJ.Run, DieRun) || ~isequal(DwJ.Die, Die) || ~isequal(DwJ.GainHalf, DieGain)
        error('ultrasat:lab:scripts:darkwindow', ...
              'darkwindow.json in %s is run %s %s %s, not run %s %s %s', ...
              DieOut, DwJ.Run, DwJ.Die, DwJ.GainHalf, DieRun, Die, DieGain);
    end
    if abs(DwJ.Tol - DieDarkTol) > 1e-12
        error('ultrasat:lab:scripts:darkwindow', ...
              'darkwindow.json in %s was scanned at tolerance %.4f, not the %.4f now set: rerun desy_die_darkwindow', ...
              DieOut, DwJ.Tol, DieDarkTol);
    end
    DieFitStepsD = DwJ.Chosen(:).';
end
