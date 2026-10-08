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
DieLotDefault = 'TH02954';
if exist('DieSelect', 'var') && isstruct(DieSelect)
    DieRun    = DieSelect.Run;
    DieFolder = DieSelect.Folder;
    Die       = DieSelect.Die;
    DieGain   = DieSelect.Gain;
    % The lot was fixed while only one was being analysed. It is selectable now
    % that TH02260 is in scope, and defaults to the old value so that every
    % caller written before this still means what it meant.
    if isfield(DieSelect, 'Lot') && ~isempty(DieSelect.Lot)
        DieLot = DieSelect.Lot;
    else
        DieLot = DieLotDefault;
    end
else
    DieRun    = '32';
    DieFolder = 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27';
    Die       = 'W04_D07';
    DieGain   = 'high';                % 'high' | 'low'
    DieLot    = DieLotDefault;
end
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
% How the fit windows are chosen. Two modes, and they answer different questions.
%   'chi2'   (default) each ladder gets the widest window, anchored at the top of
%            its linear range, that still fits a straight line -- so each ladder
%            is measured over as much of itself as is straight, and the two
%            ladders are measured over quite different signal ranges.
%   'signal' both ladders get the SAME signal window, [DieSigLo DieSigHi], and
%            within a ladder the response fit and the PTC fit use exactly the
%            same steps -- so the gain and the response refer to the same charge
%            over the same points, at the price of fewer points and, on a dark
%            ladder that barely reaches the window, of a much shorter lever.
% In 'signal' mode stage 2a is desy_die_fitwindow rather than desy_die_darkwindow,
% and stages 5, 9 and 10 take their steps from the lists rather than re-selecting
% by signal range, which is what makes "exactly the same points" true rather than
% nearly true (stage 5 selected on the per-step median, stage 9 on the mean).
if ~exist('DieWindowMode', 'var')
    DieWindowMode = 'chi2';
end
DieSigLo = 100;           % [ADU] 'signal' mode window, on the MEAN signal
DieSigHi = 1000;
% Highest step median any fit may use, [ADU]. Above it the measured integral
% non-linearity exceeds 0.5 % and the PTC starts into the 3-5 kADU variance dip,
% so a point there is not on the straight line the fits assume. Only the
% high-dark-current setup reaches it on the dark ladder (3829 ADU at the longest
% exposure against 172 on the low one), which is why it has to be a limit rather
% than a step list.
% The commanded exposure of this tester is t_exp = RO_time + Reset_delay, with
% RO_time the full-die readout time: the configuration records it as
% zDUT_ExpTimeOffset = 12 and DESY give RO_time = 2 x 4742 rows x 1.3 ms =
% 12.3292 s (the factor 2 because odd and even columns share an ADC block). The
% CHARGE-COLLECTING interval is therefore t_exp - RO_time, and so is the sensor
% exposure of the bright frames. DieExpOffset = 0 keeps the commanded value,
% which is what every result before October 2026 used; set it to 12.3292 to work
% in collecting time. Results are written to a separate directory either way.
if ~exist('DieExpOffset', 'var')
    DieExpOffset = 0;              % [s] subtracted from the B and D exposures
end
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
% Die names repeat between lots -- TH02260 and TH02954 both have a W07_D06, and
% both were measured in run 35 -- so the lot has to be in the tag or the two
% would write over each other. It is left out for the default lot, which keeps
% the directories of everything analysed so far exactly as they are.
if strcmp(DieLot, DieLotDefault)
    DieTag = sprintf('run%s_%s_%s', DieRun, Die, DieGain);
else
    DieTag = sprintf('run%s_%s_%s_%s', DieRun, DieLot, Die, DieGain);
end
DieDev    = fullfile(DieRoot, DieFolder, ['LOT_', DieLot, '_', Die]);
% 'signal' mode writes beside the default results rather than over them, so the
% two ways of choosing the window can be compared on the same die. Stage 1 is
% shared: bias and read noise do not depend on any fit window.
DieOut    = fullfile('/home/sasha/claude/desy_die', DieTag);
if strcmpi(DieWindowMode, 'signal')
    DieOut = [DieOut, '_sig'];
end
if DieExpOffset ~= 0
    DieOut = [DieOut, '_off'];     % collecting-time results, beside the others
end
DieStage1 = fullfile('/home/sasha/claude/desy_rn', DieTag);     % desy_rn_single_die output
if ~isfolder(DieOut)
    mkdir(DieOut);
end
% The dark window is chosen per die by desy_die_darkwindow (goodness of fit),
% because the two bias-board setups put their dark ladders in signal ranges that
% do not overlap: at the same exposures run 31 reaches 3500 ADU and run 32 only
% 172, so one fixed list of steps cannot be right for both. The list above is
% the fallback when that stage has not been run.
DieStepsExplicit = false;        % do stages 5/9/10 take the step lists verbatim?
DieGainScanSteps = {};           % 'signal' mode: the gain scan as step lists
if strcmpi(DieWindowMode, 'signal')
    FwPath = fullfile(DieOut, 'fitwindow.json');
    % desy_die_fitwindow is the stage that WRITES that file, so it runs this
    % config before the file can exist; it sets DieWindowBootstrap to say so.
    Boot = exist('DieWindowBootstrap', 'var') && DieWindowBootstrap;
    if ~isfile(FwPath) && ~Boot
        error('ultrasat:lab:scripts:fitwindow', ...
              'DieWindowMode is ''signal'' but %s does not exist: run desy_die_fitwindow first', FwPath);
    end
end
if strcmpi(DieWindowMode, 'signal') && isfile(fullfile(DieOut, 'fitwindow.json'))
    FwJ = jsondecode(fileread(fullfile(DieOut, 'fitwindow.json')));
    if ~isequal(FwJ.Run, DieRun) || ~isequal(FwJ.Die, Die) || ~isequal(FwJ.GainHalf, DieGain)
        error('ultrasat:lab:scripts:fitwindow', ...
              'fitwindow.json in %s is run %s %s %s, not run %s %s %s', ...
              DieOut, FwJ.Run, FwJ.Die, FwJ.GainHalf, DieRun, Die, DieGain);
    end
    if abs(FwJ.SigLo-DieSigLo)>1e-9 || abs(FwJ.SigHi-DieSigHi)>1e-9
        error('ultrasat:lab:scripts:fitwindow', ...
              'fitwindow.json was built for %g-%g ADU, not the %g-%g now set: rerun desy_die_fitwindow', ...
              FwJ.SigLo, FwJ.SigHi, DieSigLo, DieSigHi);
    end
    DieFitStepsD     = FwJ.D.Chosen(:).';
    DieFitStepsB     = FwJ.B.Chosen(:).';
    DieGainRange     = [min(FwJ.B.ChosenSignal) max(FwJ.B.ChosenSignal)].*[0.999 1.001];
    % The window systematic inside this mode is the SUB-windows of the chosen
    % list, not other windows of the ladder: varying it further would leave the
    % signal range the mode exists to fix. A single-entry scan would instead
    % report a zero systematic, which is not the same as having measured one.
    FwBst = sort(DieFitStepsB);
    DieGainScan      = {DieGainRange};
    DieGainScanSteps = {FwBst};
    if numel(FwBst)>=4
        DieGainScanSteps{end+1} = FwBst(2:end);       % drop the lowest point
        DieGainScanSteps{end+1} = FwBst(1:end-1);     % drop the highest point
    end
    DieStepsExplicit = true;
elseif isfile(fullfile(DieOut, 'darkwindow.json'))
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
