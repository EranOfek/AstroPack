function [MCMCResult, PhotCalib] = fitMCMC_PhotCalibTrans(Obj, Args)
    % Staged MCMC parameter uncertainty estimation for transmission-based photometric calibration
    % Description: Runs MCMC sampling stage-by-stage following the optimization sequence
    %              from fitPhotCalibTrans. Each stage samples only its parameters while
    %              keeping others fixed. 
    % Input  : - Obj - One of:
    %                  (1) PhotCalibTrans object (already calibrated)
    %                  (2) AstroImage object (will run fitPhotCalibTrans first)
    %          * ...,key,val,...
    %            Calibration (if AstroImage input):
    %            'RunCalibFirst' - Run fitPhotCalibTrans before MCMC. Default is true.
    %            'CalibArgs' - Cell array of args for fitPhotCalibTrans. Default is {}.
    %            MCMC sampling:
    %            'IncludeTran2D' - Include position parameters in MCMC. Default is false.
    %            'Nsimu' - Number of MCMC samples per stage. Default is 10000.
    %            'BurnIn' - Burn-in samples to discard. Default is [] (20%).
    %            'Method' - MCMC method: 'dram', 'am', 'dr', 'mh', 'ram'.
    %                   Default is 'dram'.
    %            'AdaptInt' - Adaptation interval. Default is 100.
    %            Output:
    %            'PlotResults' - Generate diagnostic plots. Default is false.
    %            'SaveChain' - Save chain to file. Default is false.
    %            'ChainFile' - Output filename for chain. Default is 'mcmc_chain.mat'.
    %            General:
    %            'Verbosity' - mcmcstat verbosity (0-2). Default is 0.
    %            'WaitBar' - Show progress waitbar. Default is false.
    %            'Verbose' - Enable verbose output. Default is true.
    % Output : - MCMCResult - Structure with staged MCMC results:
    %                   .Stages - Array of per-stage results
    %                   .Combined - Combined results across all stages
    %                   .Diagnostics - Comparison with analytical solutions
    %          - PhotCalib - PhotCalibTrans object used for MCMC
    % Author : D. Kovaleva (Jan 2026)
    % Reference: mcmcstat toolbox by Marko Laine (2003)
    % Example: [MCMCRes, PC] = imProc.calib.fitMCMC_PhotCalibTrans(PC);

    arguments
        Obj  % PhotCalibTrans or AstroImage

        % Calibration options (for AstroImage input)
        Args.RunCalibFirst logical = true
        Args.CalibArgs cell = {}

        % MCMC sampling options
        Args.IncludeTran2D logical = false
        Args.Nsimu = 10000
        Args.BurnIn = []
        Args.Method = 'dram'
        Args.AdaptInt = 100

        % Output options
        Args.PlotResults logical = false
        Args.SaveChain logical = false
        Args.ChainFile = 'mcmc_chain.mat'

        % General options
        Args.Verbosity = 0
        Args.WaitBar logical = false
        Args.Verbose logical = true
    end

    % ====================================================================
    % STEP 1: GET OR CREATE PHOTCALIBTRANS OBJECT
    % ====================================================================

    if isa(Obj, 'PhotCalibTrans')
        PC = Obj;
        if ~PC.Success
            error('imProc:calib:fitMCMC_PhotCalibTrans:NotCalibrated', ...
                'PhotCalibTrans object has Success=false. Run calibration first.');
        end
    elseif isa(Obj, 'AstroImage')
        if Args.RunCalibFirst
            if Args.Verbose
                fprintf('Running fitPhotCalibTrans on AstroImage...\n');
            end
            [~, PC] = imProc.calib.fitPhotCalibTrans(Obj, Args.CalibArgs{:});
            if ~PC.Success
                error('imProc:calib:fitMCMC_PhotCalibTrans:CalibFailed', ...
                    'Photometric calibration failed. Cannot run MCMC.');
            end
        else
            error('imProc:calib:fitMCMC_PhotCalibTrans:NoCalib', ...
                'AstroImage input requires RunCalibFirst=true or pre-calibrated PhotCalibTrans.');
        end
    else
        error('imProc:calib:fitMCMC_PhotCalibTrans:InvalidInput', ...
            'Input must be PhotCalibTrans or AstroImage object.');
    end

    PhotCalib = PC;

    % ====================================================================
    % STEP 2: VALIDATE CALIBRATION DATA
    % ====================================================================

    if isempty(PC.TransModel)
        error('imProc:calib:fitMCMC_PhotCalibTrans:NoModel', ...
            'PhotCalibTrans.TransModel is empty. Run calibration first.');
    end

    if isempty(PC.SourceData) || isempty(PC.SpecData)
        error('imProc:calib:fitMCMC_PhotCalibTrans:NoData', ...
            'PhotCalibTrans.SourceData or SpecData is empty.');
    end

    OptSeq = PC.TransModel.OptSeq;
    if isempty(OptSeq)
        error('imProc:calib:fitMCMC_PhotCalibTrans:NoOptSeq', ...
            'No optimization sequence found. Cannot run staged MCMC.');
    end

    % ====================================================================
    % STEP 3: PREPARE COMMON DATA
    % ====================================================================

    Model = PC.TransModel;
    Lambda = PC.TransWvl;
    AllFunPar = Model.getAllFunPar();

    % Get calibrator data
    ObsFlux = PC.SourceData.getCol('Flux');
    X = PC.SourceData.getCol('X');
    Y = PC.SourceData.getCol('Y');

    % Build CostArgs
    ExpTime_eff = PC.ExpTime / PC.NCoadd;
    CostArgs = {'WeightMatrix', PC.SpecData.Spec', ...
                'TransmissionMode', true, ...
                'CalibWavelength', PC.SpecData.SpecWvl, ...
                'ExpTime', ExpTime_eff, ...
                'Aperture_area_m2', PC.Aperture, ...
                'X', X, 'Y', Y};

    % Get calibration fit results for comparison
    FitResults = PC.FitResults;

    if Args.Verbose
        fprintf('\n=== STAGED MCMC PHOTOMETRIC CALIBRATION ===\n');
        fprintf('Calibrators: %d\n', height(PC.SourceData.Table));
        fprintf('Number of stages: %d\n', length(OptSeq));
        fprintf('Samples per stage: %d\n', Args.Nsimu);
        fprintf('Include position parameters: %s\n', string(Args.IncludeTran2D));
    end

    % ====================================================================
    % STEP 4: RUN MCMC FOR EACH STAGE
    % ====================================================================

    NumStages = length(OptSeq);
    StageResults = cell(NumStages, 1);

    for iStage = 1:NumStages
        Stage = OptSeq(iStage);
        StageName = Stage.StageName;
        Method = Stage.Method;
        FreeParams = Stage.FreeParams;

        % Skip field correction stages (empty FreeParams) unless IncludeTran2D
        IsFieldCorrection = isempty(FreeParams);

        if IsFieldCorrection
            if Args.IncludeTran2D && Model.UseTran2D && ~isempty(Model.Tran2DObj)
                % Run MCMC on position parameters
                if Args.Verbose
                    fprintf('\n--- Stage %d: %s (Field Correction) ---\n', iStage, StageName);
                    fprintf('Method: %s\n', Method);
                    fprintf('Parameters: Position coefficients\n');
                end

                StageResults{iStage} = runStageMCMC_Position(Model, Lambda, ObsFlux, ...
                    CostArgs, X, Y, Args, iStage, StageName, Method, FitResults);
            else
                if Args.Verbose
                    fprintf('\n--- Stage %d: %s (Field Correction) ---\n', iStage, StageName);
                    fprintf('Skipping (no position parameters or IncludeTran2D=false)\n');
                end
                StageResults{iStage} = struct('Skipped', true, 'StageName', StageName, ...
                    'Method', Method, 'IsFieldCorrection', true);
            end
            continue;
        end

        % Get parameter names for this stage
        ParamNames = cell(1, length(FreeParams));
        for iFree = 1:length(FreeParams)
            ParamNames{iFree} = FreeParams(iFree).Parameter;
        end

        if Args.Verbose
            fprintf('\n--- Stage %d: %s ---\n', iStage, StageName);
            fprintf('Method: %s\n', Method);
            fprintf('Parameters: %s\n', strjoin(ParamNames, ', '));
        end

        % Find parameter indices in AllFunPar
        ParamIndices = zeros(1, length(ParamNames));
        ParamValues = zeros(1, length(ParamNames));
        ParamMin = zeros(1, length(ParamNames));
        ParamMax = zeros(1, length(ParamNames));

        for iParam = 1:length(ParamNames)
            Idx = find(strcmp(AllFunPar.Name, ParamNames{iParam}), 1);
            if isempty(Idx)
                warning('Parameter "%s" not found in model', ParamNames{iParam});
                continue;
            end
            ParamIndices(iParam) = Idx;
            ParamValues(iParam) = AllFunPar.Val(Idx);
            ParamMin(iParam) = AllFunPar.Min(Idx);
            ParamMax(iParam) = AllFunPar.Max(Idx);
        end

        if Args.Verbose
            fprintf('Initial values:\n');
            for iParam = 1:length(ParamNames)
                fprintf('  %s: %.4g [%.4g, %.4g]\n', ...
                    ParamNames{iParam}, ParamValues(iParam), ...
                    ParamMin(iParam), ParamMax(iParam));
            end
        end

        % Set FitPar flags for only this stage's parameters
        StageFitPar = AllFunPar;
        StageFitPar.FitPar(:) = false;
        for iParam = 1:length(ParamIndices)
            if ParamIndices(iParam) > 0
                StageFitPar.FitPar(ParamIndices(iParam)) = true;
            end
        end
        Model.setAllFunPar(StageFitPar);

        % Run MCMC for this stage
        try
            [~, RawMCMC] = Model.fitMCMC(Lambda, ObsFlux, ...
                'CostArgs', CostArgs, ...
                'X', X, 'Y', Y, ...
                'IncludeTran2D', false, ...  % Only transmission params for this stage
                'Nsimu', Args.Nsimu, ...
                'BurnIn', Args.BurnIn, ...
                'Method', Args.Method, ...
                'InitFromFit', false, ...
                'AdaptInt', Args.AdaptInt, ...
                'Verbosity', Args.Verbosity, ...
                'WaitBar', Args.WaitBar, ...
                'Verbose', false);

            % Compute statistics
            Chain = RawMCMC.Chain;
            NumParams = size(Chain, 2);

            MCMCMedian = RawMCMC.Median;
            MCMCStd = RawMCMC.Std;
            CI68 = prctile(Chain, [16, 84], 1)';
            CI95 = RawMCMC.CI95;

            % Effective sample size
            EffectiveSamples = RawMCMC.NumSamples ./ RawMCMC.Tau;

            % Get analytical values from FitResults for comparison
            AnalyticalValues = ParamValues;  % Current model values are from calibration

            % Build stage result
            StageResult = struct();
            StageResult.StageName = StageName;
            StageResult.Method = Method;
            StageResult.IsFieldCorrection = false;
            StageResult.Skipped = false;
            StageResult.ParamNames = ParamNames;
            StageResult.ParamIndices = ParamIndices;

            % MCMC results
            StageResult.Chain = Chain;
            StageResult.Median = MCMCMedian;
            StageResult.Std = MCMCStd;
            StageResult.CI68 = CI68;
            StageResult.CI95 = CI95;
            StageResult.AcceptRate = RawMCMC.AcceptRate;
            StageResult.Tau = RawMCMC.Tau;
            StageResult.EffectiveSamples = EffectiveSamples;

            % Comparison with analytical/calibrated values
            StageResult.AnalyticalValues = AnalyticalValues;
            StageResult.Difference = MCMCMedian(:) - AnalyticalValues(:);
            StageResult.DiffInSigma = StageResult.Difference ./ MCMCStd(:);

            % Correlation matrix for this stage
            if NumParams > 1
                StageResult.CorrMatrix = corrcoef(Chain);
            else
                StageResult.CorrMatrix = 1;
            end

            StageResults{iStage} = StageResult;

            % Print stage summary
            if Args.Verbose
                fprintf('\nStage %d Results:\n', iStage);
                fprintf('Acceptance rate: %.1f%%\n', RawMCMC.AcceptRate * 100);
                fprintf('Effective samples: %.0f - %.0f\n', ...
                    min(EffectiveSamples), max(EffectiveSamples));

                fprintf('\n%-15s %12s %12s %12s %10s\n', ...
                    'Parameter', 'Analytical', 'MCMC Median', 'MCMC Std', 'Diff/Sigma');
                fprintf('%s\n', repmat('-', 1, 65));
                for iParam = 1:length(ParamNames)
                    Agreement = '';
                    if abs(StageResult.DiffInSigma(iParam)) < 1
                        Agreement = 'OK';
                    elseif abs(StageResult.DiffInSigma(iParam)) < 2
                        Agreement = '~';
                    else
                        Agreement = '!!';
                    end
                    fprintf('%-15s %12.4g %12.4g %12.4g %10.2f %s\n', ...
                        ParamNames{iParam}, ...
                        AnalyticalValues(iParam), ...
                        MCMCMedian(iParam), ...
                        MCMCStd(iParam), ...
                        StageResult.DiffInSigma(iParam), ...
                        Agreement);
                end

                % Show correlations if > 1 parameter
                if NumParams > 1
                    fprintf('\nCorrelation matrix:\n');
                    for i = 1:NumParams
                        fprintf('  ');
                        for j = 1:NumParams
                            fprintf('%6.2f ', StageResult.CorrMatrix(i,j));
                        end
                        fprintf('  %s\n', ParamNames{i});
                    end
                end
            end

        catch ME
            warning('MCMC failed for stage %d (%s): %s', iStage, StageName, ME.message);
            StageResults{iStage} = struct('Skipped', true, 'StageName', StageName, ...
                'Method', Method, 'IsFieldCorrection', false, 'Error', ME.message);
        end

        % Restore original FitPar flags for next stage
        Model.setAllFunPar(AllFunPar);
    end

    % ====================================================================
    % STEP 5: COMBINE RESULTS
    % ====================================================================

    % Collect all non-skipped stages
    ValidStages = cellfun(@(x) ~x.Skipped, StageResults);

    % Build combined parameter list (unique parameters across all stages)
    AllParamNames = {};
    AllMedian = [];
    AllStd = [];
    AllCI68 = [];
    AllAnalytical = [];

    for iStage = 1:NumStages
        if ~ValidStages(iStage)
            continue;
        end
        SR = StageResults{iStage};
        for iParam = 1:length(SR.ParamNames)
            ParamName = SR.ParamNames{iParam};
            % Check if already added (use last occurrence for params in multiple stages)
            ExistIdx = find(strcmp(AllParamNames, ParamName), 1);
            if isempty(ExistIdx)
                % New parameter
                AllParamNames{end+1} = ParamName; %#ok<AGROW>
                AllMedian(end+1) = SR.Median(iParam); %#ok<AGROW>
                AllStd(end+1) = SR.Std(iParam); %#ok<AGROW>
                AllCI68(end+1, :) = SR.CI68(iParam, :); %#ok<AGROW>
                AllAnalytical(end+1) = SR.AnalyticalValues(iParam); %#ok<AGROW>
            else
                % Update with later stage value
                AllMedian(ExistIdx) = SR.Median(iParam);
                AllStd(ExistIdx) = SR.Std(iParam);
                AllCI68(ExistIdx, :) = SR.CI68(iParam, :);
                AllAnalytical(ExistIdx) = SR.AnalyticalValues(iParam);
            end
        end
    end

    % ====================================================================
    % STEP 6: BUILD OUTPUT STRUCTURE
    % ====================================================================

    MCMCResult = struct();

    % Per-stage results
    MCMCResult.Stages = StageResults;
    MCMCResult.NumStages = NumStages;
    MCMCResult.ValidStages = ValidStages;

    % Combined results
    MCMCResult.ParamNames = AllParamNames(:);
    MCMCResult.Median = AllMedian(:);
    MCMCResult.Std = AllStd(:);
    MCMCResult.CI68 = AllCI68;
    MCMCResult.AnalyticalValues = AllAnalytical(:);
    MCMCResult.Difference = AllMedian(:) - AllAnalytical(:);
    MCMCResult.DiffInSigma = MCMCResult.Difference ./ AllStd(:);

    % Metadata
    MCMCResult.Nsimu = Args.Nsimu;
    MCMCResult.Method = Args.Method;
    MCMCResult.IncludeTran2D = Args.IncludeTran2D;
    MCMCResult.NumCalibrators = length(ObsFlux);

    % ====================================================================
    % STEP 7: FINAL SUMMARY
    % ====================================================================

    if Args.Verbose
        fprintf('\n=== STAGED MCMC SUMMARY ===\n');
        fprintf('Stages completed: %d / %d\n', sum(ValidStages), NumStages);

        fprintf('\n%-15s %12s %12s %12s %10s\n', ...
            'Parameter', 'Analytical', 'MCMC Median', 'MCMC Std', 'Agreement');
        fprintf('%s\n', repmat('-', 1, 65));
        for iParam = 1:length(AllParamNames)
            if abs(MCMCResult.DiffInSigma(iParam)) < 1
                Agreement = 'OK';
            elseif abs(MCMCResult.DiffInSigma(iParam)) < 2
                Agreement = 'marginal';
            else
                Agreement = 'DISAGREE';
            end
            fprintf('%-15s %12.4g %12.4g %12.4g %10s\n', ...
                AllParamNames{iParam}, ...
                AllAnalytical(iParam), ...
                AllMedian(iParam), ...
                AllStd(iParam), ...
                Agreement);
        end

        % Summary of linear vs nonlinear stages
        fprintf('\nPer-stage diagnostics:\n');
        for iStage = 1:NumStages
            SR = StageResults{iStage};
            if SR.Skipped
                fprintf('  Stage %d (%s): SKIPPED\n', iStage, SR.StageName);
            else
                if strcmpi(SR.Method, 'linear')
                    MethodTag = '[LINEAR]';
                else
                    MethodTag = '[NONLIN]';
                end
                MaxDiff = max(abs(SR.DiffInSigma));
                if MaxDiff < 1
                    Status = 'OK';
                elseif MaxDiff < 2
                    Status = 'marginal';
                else
                    Status = 'CHECK';
                end
                fprintf('  Stage %d (%s) %s: max|diff/sigma|=%.2f %s\n', ...
                    iStage, SR.StageName, MethodTag, MaxDiff, Status);
            end
        end
    end

    % ====================================================================
    % STEP 8: OPTIONAL PLOTS
    % ====================================================================

    if Args.PlotResults
        plotStagedMCMCDiagnostics(MCMCResult);
    end

    % ====================================================================
    % STEP 9: OPTIONAL SAVE
    % ====================================================================

    if Args.SaveChain
        save(Args.ChainFile, 'MCMCResult', '-v7.3');
        if Args.Verbose
            fprintf('\nResults saved to: %s\n', Args.ChainFile);
        end
    end
end

% ========================================================================
% LOCAL FUNCTION: RUN MCMC FOR POSITION PARAMETERS
% ========================================================================

function StageResult = runStageMCMC_Position(Model, Lambda, ObsFlux, ...
    CostArgs, X, Y, Args, iStage, StageName, Method, FitResults)
    % Run MCMC for position (Tran2D) parameters

    try
        [~, RawMCMC] = Model.fitMCMC(Lambda, ObsFlux, ...
            'CostArgs', CostArgs, ...
            'X', X, 'Y', Y, ...
            'IncludeTran2D', true, ...
            'Nsimu', Args.Nsimu, ...
            'BurnIn', Args.BurnIn, ...
            'Method', Args.Method, ...
            'InitFromFit', false, ...
            'AdaptInt', Args.AdaptInt, ...
            'Verbosity', Args.Verbosity, ...
            'WaitBar', Args.WaitBar, ...
            'Verbose', false);

        Chain = RawMCMC.Chain;
        NumParams = size(Chain, 2);

        StageResult = struct();
        StageResult.StageName = StageName;
        StageResult.Method = Method;
        StageResult.IsFieldCorrection = true;
        StageResult.Skipped = false;
        StageResult.ParamNames = RawMCMC.ParamNames;
        StageResult.ParamIndices = [];

        StageResult.Chain = Chain;
        StageResult.Median = RawMCMC.Median;
        StageResult.Std = RawMCMC.Std;
        StageResult.CI68 = prctile(Chain, [16, 84], 1)';
        StageResult.CI95 = RawMCMC.CI95;
        StageResult.AcceptRate = RawMCMC.AcceptRate;
        StageResult.Tau = RawMCMC.Tau;
        StageResult.EffectiveSamples = RawMCMC.NumSamples ./ RawMCMC.Tau;

        % For position params, analytical values are from Tran2DObj
        StageResult.AnalyticalValues = Model.Tran2DObj.ParX(:)';
        StageResult.Difference = StageResult.Median(:) - StageResult.AnalyticalValues(:);
        StageResult.DiffInSigma = StageResult.Difference ./ StageResult.Std(:);

        if NumParams > 1
            StageResult.CorrMatrix = corrcoef(Chain);
        else
            StageResult.CorrMatrix = 1;
        end

        if Args.Verbose
            fprintf('Acceptance rate: %.1f%%\n', RawMCMC.AcceptRate * 100);
            fprintf('Position parameters: %d\n', NumParams);
        end

    catch ME
        warning('Position MCMC failed: %s', ME.message);
        StageResult = struct('Skipped', true, 'StageName', StageName, ...
            'Method', Method, 'IsFieldCorrection', true, 'Error', ME.message);
    end
end

% ========================================================================
% LOCAL FUNCTION: DIAGNOSTIC PLOTS FOR STAGED MCMC
% ========================================================================

function plotStagedMCMCDiagnostics(MCMCResult)
    % Generate diagnostic plots for staged MCMC results

    NumStages = MCMCResult.NumStages;
    ValidStages = MCMCResult.ValidStages;

    % Figure 1: Per-stage trace plots
    figure('Name', 'Staged MCMC Trace Plots', 'Position', [100 100 1400 800]);

    PlotIdx = 1;
    TotalPlots = sum(ValidStages);

    for iStage = 1:NumStages
        if ~ValidStages(iStage)
            continue;
        end

        SR = MCMCResult.Stages{iStage};
        Chain = SR.Chain;
        Names = SR.ParamNames;
        NumParams = length(Names);

        for iParam = 1:NumParams
            subplot(TotalPlots, max(cellfun(@(x) length(x.ParamNames), ...
                MCMCResult.Stages(ValidStages))), PlotIdx);
            plot(Chain(:, iParam));
            title(sprintf('S%d: %s', iStage, Names{iParam}), 'Interpreter', 'none');
            ylabel('Value');
            if PlotIdx > TotalPlots * (NumParams - 1)
                xlabel('Iteration');
            end
            PlotIdx = PlotIdx + 1;
        end
    end
    sgtitle('Staged MCMC Chain Evolution');

    % Figure 2: Comparison with analytical values
    figure('Name', 'MCMC vs Analytical', 'Position', [150 150 800 600]);

    AllNames = MCMCResult.ParamNames;
    AllMedian = MCMCResult.Median;
    AllStd = MCMCResult.Std;
    AllAnalytical = MCMCResult.AnalyticalValues;
    NumParams = length(AllNames);

    errorbar(1:NumParams, AllMedian, AllStd, 'bo', 'LineWidth', 1.5, ...
        'MarkerSize', 8, 'MarkerFaceColor', 'b');
    hold on;
    plot(1:NumParams, AllAnalytical, 'r^', 'MarkerSize', 10, ...
        'MarkerFaceColor', 'r');
    hold off;

    xticks(1:NumParams);
    xticklabels(AllNames);
    xtickangle(45);
    ylabel('Parameter Value');
    legend('MCMC (median +/- std)', 'Analytical/Calibrated', 'Location', 'best');
    title('MCMC vs Analytical Comparison');
    grid on;

    % Figure 3: Difference in sigma units
    figure('Name', 'Difference Analysis', 'Position', [200 200 800 400]);

    bar(MCMCResult.DiffInSigma);
    hold on;
    yline([-2, -1, 1, 2], '--', {'2σ', '1σ', '1σ', '2σ'});
    hold off;

    xticks(1:NumParams);
    xticklabels(AllNames);
    xtickangle(45);
    ylabel('(MCMC - Analytical) / σ');
    title('Agreement: Difference in Standard Deviations');
    grid on;

    % Figure 4: Per-stage acceptance rates
    figure('Name', 'Stage Diagnostics', 'Position', [250 250 600 400]);

    AcceptRates = nan(NumStages, 1);
    StageLabels = cell(NumStages, 1);
    for iStage = 1:NumStages
        SR = MCMCResult.Stages{iStage};
        StageLabels{iStage} = SR.StageName;
        if ~SR.Skipped && isfield(SR, 'AcceptRate')
            AcceptRates(iStage) = SR.AcceptRate * 100;
        end
    end

    bar(AcceptRates);
    hold on;
    yline([20, 50], '--', {'Min (20%)', 'Ideal (50%)'});
    hold off;

    xticks(1:NumStages);
    xticklabels(StageLabels);
    xtickangle(45);
    ylabel('Acceptance Rate (%)');
    title('MCMC Acceptance Rate per Stage');
    grid on;
    ylim([0 100]);
end
