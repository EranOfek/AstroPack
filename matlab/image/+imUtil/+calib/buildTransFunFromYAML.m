function TransFun = buildTransFunFromYAML(YAMLConfig, Metadata, Args)
    % Build CompositeFun transmission model from YAML configuration
    % Input  : - YAMLConfig - YAML configuration structure (from yaml.ReadYaml) or
    %                   path to YAML file (string/char). Must contain
    %                   DefaultPressure_mbar and TransmissionFunctions fields.
    %          - Metadata - Structure with observation metadata with fields:
    %                   .Airmass - Airmass value (>=1.0)
    %                   .Temperature - Temperature [Celsius]
    %                   Note: Pressure is read from YAML config, not from Metadata.
    %          * ...,key,val,...
    %            'Verbose' - Enable verbose output.
    %                   Default is true.
    % Output : - TransFun - CompositeFun object with transmission functions.
    %                   All FitPar flags initialized to false.
    %                   Atmospheric functions have metadata injected.
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: Metadata = struct('Airmass', 1.2, 'Temperature', 15);
    %          TransFun = imUtil.calib.buildTransFunFromYAML(...
    %                   '~/matlab/AstroPack/config/CalibPhotAB.yml', Metadata);

    arguments
        YAMLConfig  % Structure or path string
        Metadata struct
        Args.Verbose logical = true
    end

    % ====================================================================
    % STEP 1: LOAD YAML CONFIG IF NEEDED
    % ====================================================================

    if ischar(YAMLConfig) || isstring(YAMLConfig)
        % YAMLConfig is a file path - load it
        YAMLFile = YAMLConfig;
        if Args.Verbose
            fprintf('Loading YAML configuration from: %s\n', YAMLFile);
        end

        % Expand ~ to home directory if present
        if startsWith(YAMLFile, '~')
            YAMLFile = strrep(YAMLFile, '~', getenv('HOME'));
        end

        % Load YAML file
        YAMLConfig = yaml.ReadYaml(YAMLFile);
    end

    % Validate YAML structure
    if ~isfield(YAMLConfig, 'TransmissionFunctions')
        error('YAML config missing required field: TransmissionFunctions');
    end
    if ~isfield(YAMLConfig, 'DefaultPressure_mbar')
        error('YAML config missing required field: DefaultPressure_mbar');
    end

    % Extract atmospheric parameters from YAML
    Pressure_mbar = YAMLConfig.DefaultPressure_mbar;

    % ====================================================================
    % STEP 2: CREATE COMPOSITEFUN OBJECT
    % ====================================================================

    TransFun = tools.math.fun.CompositeFun();

    if Args.Verbose
        fprintf('Building CompositeFun from YAML configuration...\n');
    end

    % ====================================================================
    % STEP 3: ADD TRANSMISSION FUNCTIONS
    % ====================================================================

    TransFunctions = YAMLConfig.TransmissionFunctions;
    NumFunctions = length(TransFunctions);

    for i = 1:NumFunctions
        FunSpec = TransFunctions{i};

        % Extract function specification
        FunName = FunSpec.name;
        HandleStr = FunSpec.handle;
        HandleType = FunSpec.handletype;
        Params = FunSpec.params;
        ParamInfo = FunSpec.paraminfo;

        % Ensure Params is a numeric array (YAML may return cell in some cases)
        if iscell(Params)
            Params = cell2mat(Params);
        end
        if ~isnumeric(Params)
            error('Parameters for function %s are not numeric (class: %s)', FunName, class(Params));
        end

        % Convert params to row vector if needed
        if iscolumn(Params)
            Params = Params';
        end

        % Create FitPar array (all false initially)
        NumParams = length(Params);
        FitPar = false(1, NumParams);

        % Build ArgNames structure from paraminfo
        ArgNames = struct([]);
        for j = 1:NumParams
            PInfo = ParamInfo{j};

            ArgNames(j).Name = PInfo.name;
            ArgNames(j).Description = PInfo.description;
            ArgNames(j).Min = PInfo.min;
            ArgNames(j).Max = PInfo.max;
        end

        % Convert string handle to function handle
        try
            if strcmp(HandleType, 'anonymous')
                % Anonymous function - evaluate string
                FunHandle = str2func(HandleStr);
            elseif strcmp(HandleType, 'named')
                % Named function - convert from @package.function format
                % Remove leading @ if present
                if startsWith(HandleStr, '@')
                    HandleStr = HandleStr(2:end);
                end
                FunHandle = str2func(HandleStr);
            else
                error('Unknown handletype: %s (must be "anonymous" or "named")', HandleType);
            end
        catch ME
            error('Failed to convert function handle "%s": %s', HandleStr, ME.message);
        end

        % Add function to CompositeFun with explicit ArgNames
        TransFun.addFun(FunName, FunHandle, ArgNames, ...
                       'Par', Params, ...
                       'FitPar', FitPar);

        if Args.Verbose
            fprintf('  [%d/%d] Added: %s (%d params)\n', ...
                    i, NumFunctions, FunName, NumParams);
        end
    end

    % ====================================================================
    % STEP 4: INJECT METADATA INTO ATMOSPHERIC FUNCTIONS
    % ====================================================================

    % Validate metadata (simple check - error if anything wrong)
    try
        assert(isfield(Metadata, 'Airmass') && ~isnan(Metadata.Airmass), ...
               'Invalid Metadata: Airmass missing or NaN');
        assert(isfield(Metadata, 'Temperature') && ~isnan(Metadata.Temperature), ...
               'Invalid Metadata: Temperature missing or NaN');
    catch ME
        error('Metadata validation failed: %s', ME.message);
    end

    % Calculate zenith angle from airmass
    if Metadata.Airmass < 1.0
        error('Invalid Airmass: %.3f (must be >= 1.0)', Metadata.Airmass);
    end
    ZenithAngle_deg = acosd(1.0 / Metadata.Airmass);

    if Args.Verbose
        fprintf('Injecting metadata and atmospheric parameters:\n');
        fprintf('  Airmass: %.3f -> Zenith angle: %.2f deg\n', ...
                Metadata.Airmass, ZenithAngle_deg);
        fprintf('  Temperature: %.1f C\n', Metadata.Temperature);
        fprintf('  Pressure: %.1f mbar (from YAML config)\n', Pressure_mbar);
    end

    % Find and update atmospheric functions
    for i = 1:NumFunctions
        FunName = TransFunctions{i}.name;
        CurrentParams = TransFun.Funs(i).Par;

        % Ensure CurrentParams is numeric (safety check)
        if iscell(CurrentParams)
            CurrentParams = cell2mat(CurrentParams);
            TransFun.Funs(i).Par = CurrentParams;
        end

        switch FunName
            case 'Rayleigh'
                % Update: [ZenithAngle_deg, Pressure_mbar]
                NewParams = [ZenithAngle_deg, Pressure_mbar];
                TransFun.Funs(i).Par = NewParams;

            case 'Water'
                % Update: [ZenithAngle_deg, PWV_cm, Pressure_mbar]
                % Keep PWV_cm from YAML (will be optimized)
                PWV_cm = CurrentParams(2);
                NewParams = [ZenithAngle_deg, PWV_cm, Pressure_mbar];
                TransFun.Funs(i).Par = NewParams;

            case 'Aerosol'
                % Update: [ZenithAngle_deg, TauAod500, AngstromExp]
                % Keep TauAod500 and AngstromExp from YAML
                TauAod500 = CurrentParams(2);
                AngstromExp = CurrentParams(3);
                NewParams = [ZenithAngle_deg, TauAod500, AngstromExp];
                TransFun.Funs(i).Par = NewParams;
        end
    end

    % ====================================================================
    % STEP 5: VALIDATION
    % ====================================================================

    if length(TransFun.Funs) ~= NumFunctions
        error('CompositeFun has %d functions but expected %d', ...
              length(TransFun.Funs), NumFunctions);
    end

    TotalParams = TransFun.numAllPar();

    if Args.Verbose
        fprintf('CompositeFun built successfully:\n');
        fprintf('  Functions: %d\n', NumFunctions);
        fprintf('  Total parameters: %d\n', TotalParams);
        fprintf('  All FitPar flags initialized to false\n');
        fprintf('  Metadata injected into atmospheric functions\n\n');
    end
end