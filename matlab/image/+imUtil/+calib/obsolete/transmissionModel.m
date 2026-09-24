function Model = transmissionModel(TransFunList, Args)
    % Builds transmission model object from basic transmission functions.
    % Builds tools.math.fun.CompositeFun transmission model from basic function
    % specification list (astro.transmission.* for atmospheric
    % transmission, telescope.detector.* and telescope.optics.* for instrumental transmission). 
    % Uses class tools.math.fun.CompositeFun.
    %
    % Input  : - TransFunList - Struct array of specifications of basic transmission function
    %                   Each element is a struct with fields:
    %                   .name - Function name (string)
    %                   .handle - Function handle string (e.g., '@astro.transmission.ozoneTransmission')
    %                   .handletype - 'named' or 'anonymous'
    %                   .params - Parameter values (numeric array)
    %                   .paraminfo - Struct array of parameter info with fields:
    %                                .name, .min, .max
    %          * ...,key,val,...
    %            'Airmass' - Atmospheric airmass (>= 1.0). Used for metadata injection.
    %                   From observations.
    %            'Temperature' - Temperature [C]. Used for metadata injection.
    %                   From observations.
    %            'Pressure_mbar' - Atmospheric pressure [mbar]. Used for metadata injection.
    %                   Default is 965.
    %            'Verbose' - Enable verbose output.
    %                   Default is false.
    % Output : - Model - CompositeFun object with all transmission
    %                   functions added, and observation-specific metadata (ZenithAngle_deg, 
    %                   Pressure_mbar, Temperature_C) injected.
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Define transmission functions as struct array
    %          TransFunList(1).name = 'Ozone';
    %          TransFunList(1).handle = '@astro.transmission.ozoneTransmission';
    %          TransFunList(1).handletype = 'named';
    %          TransFunList(1).params = [30, 300];
    %          TransFunList(1).paraminfo(1).name = 'ZenithAngle_deg';
    %          TransFunList(1).paraminfo(1).min = 0;
    %          TransFunList(1).paraminfo(1).max = 90;
    %          TransFunList(1).paraminfo(2).name = 'DobsonUnits';
    %          TransFunList(1).paraminfo(2).min = 200;
    %          TransFunList(1).paraminfo(2).max = 400;
    %          TransFunList(2).name = 'Aerosol';
    %          TransFunList(2).handle = '@astro.transmission.aerosolTransmission';
    %          TransFunList(2).handletype = 'named';
    %          TransFunList(2).params = [30, 0.05, 1.2];
    %          TransFunList(2).paraminfo(1).name = 'ZenithAngle_deg';
    %          TransFunList(2).paraminfo(1).min = 0;
    %          TransFunList(2).paraminfo(1).max = 90;
    %          TransFunList(2).paraminfo(2).name = 'TauAod500';
    %          TransFunList(2).paraminfo(2).min = 0.0;
    %          TransFunList(2).paraminfo(2).max = 0.5;
    %          TransFunList(2).paraminfo(3).name = 'Alpha';
    %          TransFunList(2).paraminfo(3).min = 0.5;
    %          TransFunList(2).paraminfo(3).max = 2.5;
    %          % Build model with metadata injection
    %          Model = imUtil.calib.transmissionModel(TransFunList, ...
    %              'Airmass', 1.2, 'Temperature', 15, 'Pressure_mbar', 965);
    %          % Or build model without metadata injection
    %          Model = imUtil.calib.transmissionModel(TransFunList);

    arguments
        TransFunList struct
        Args.Airmass = 1.2       % From observations
        Args.Temperature = 15   % From observations
        Args.Pressure_mbar = 965 % Now default, later: from observations
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS
    % ====================================================================

    % Check if metadata injection is requested
    InjectMetadata = ~isempty(Args.Airmass) || ~isempty(Args.Temperature) || ~isempty(Args.Pressure_mbar);

    % Validate Airmass if provided
    if ~isempty(Args.Airmass) && Args.Airmass < 1.0
        error('Invalid Airmass: %.3f (must be >= 1.0)', Args.Airmass);
    end

    % ====================================================================
    % STEP 2: CREATE COMPOSITEFUN OBJECT
    % ====================================================================

    % Create CompositeFun object
    Model = tools.math.fun.CompositeFun();

    NumFunctions = length(TransFunList);

    if Args.Verbose
        fprintf('=== BUILDING TRANSMISSION MODEL ===\n');
        fprintf('Number of functions: %d\n', NumFunctions);
    end

    % ====================================================================
    % STEP 3: ADD ALL TRANSMISSION FUNCTIONS
    % ====================================================================

    for I = 1:NumFunctions
        FunDef = TransFunList(I);

        % Extract function definition
        FunName = FunDef.name;
        HandleStr = FunDef.handle;
        HandleType = FunDef.handletype;
        Params = FunDef.params;
        ParamInfo = FunDef.paraminfo;

        % Convert params from cell array to numeric array if needed (from YAML)
        if iscell(Params)
            Params = cell2mat(Params);
        end

        % Validate Params is numeric array
        if ~isnumeric(Params)
            error('Parameters for function %s must be numeric array', FunName);
        end

        % Convert paraminfo from cell array to struct array if needed (from YAML)
        if iscell(ParamInfo)
            ParamInfo = [ParamInfo{:}];
        end

        % Convert to row vector
        if iscolumn(Params)
            Params = Params';
        end

        % Create FitPar array (all false initially)
        NumParams = length(Params);
        FitPar = false(1, NumParams);

        % Build ArgNames structure
        % Note: CompositeFun's getAllParStruct().Names returns the Description field,
        % so we put the parameter name in Description to enable lookup by name
        ArgNames = struct([]);
        for J = 1:NumParams
            PInfo = ParamInfo(J);
            ArgNames(J).Name = PInfo.name;  % Actual parameter name (used for lookup)
            ArgNames(J).Description = PInfo.name;  % Put name here so getAllParStruct() finds it
            ArgNames(J).Min = PInfo.min;
            ArgNames(J).Max = PInfo.max;
        end

        % Convert string handle to function handle
        if strcmp(HandleType, 'anonymous')
            FunHandle = str2func(HandleStr);
        elseif strcmp(HandleType, 'named')
            if startsWith(HandleStr, '@')
                HandleStr = HandleStr(2:end);
            end
            FunHandle = str2func(HandleStr);
        else
            error('Unknown handletype: %s', HandleType);
        end

        % Add function to CompositeFun
        Model.addFun(FunName, FunHandle, ArgNames, 'Par', Params, 'FitPar', FitPar);

        if Args.Verbose
            fprintf('  [%d/%d] Added: %s (%d params)\n', I, NumFunctions, FunName, NumParams);
        end
    end

    % ====================================================================
    % STEP 4: INJECT OBSERVATION METADATA (OPTIONAL)
    % ====================================================================

    if InjectMetadata
        % Convert airmass to zenith angle if provided
        if ~isempty(Args.Airmass)
            ZenithAngle_deg = acosd(1.0 / Args.Airmass);
        else
            ZenithAngle_deg = [];
        end

        if Args.Verbose
            fprintf('\nInjecting observation metadata:\n');
            if ~isempty(Args.Airmass)
                fprintf('  Airmass: %.3f -> Zenith angle: %.2f deg\n', Args.Airmass, ZenithAngle_deg);
            end
            if ~isempty(Args.Temperature)
                fprintf('  Temperature: %.1f C\n', Args.Temperature);
            end
            if ~isempty(Args.Pressure_mbar)
                fprintf('  Pressure: %.1f mbar\n', Args.Pressure_mbar);
            end
        end

        % Get all parameters structure from CompositeFun
        AllPar = Model.getAllParStruct();

        % Update parameters by name matching
        for I = 1:length(AllPar.Names)
            ParamName = AllPar.Names{I};

            if strcmp(ParamName, 'ZenithAngle_deg') && ~isempty(ZenithAngle_deg)
                AllPar.Values(I) = ZenithAngle_deg;
                if Args.Verbose
                    fprintf('  Injected %s = %.3f\n', ParamName, ZenithAngle_deg);
                end
            elseif strcmp(ParamName, 'Pressure_mbar') && ~isempty(Args.Pressure_mbar)
                AllPar.Values(I) = Args.Pressure_mbar;
                if Args.Verbose
                    fprintf('  Injected %s = %.3f\n', ParamName, Args.Pressure_mbar);
                end
            elseif strcmp(ParamName, 'Temperature_C') && ~isempty(Args.Temperature)
                AllPar.Values(I) = Args.Temperature;
                if Args.Verbose
                    fprintf('  Injected %s = %.3f\n', ParamName, Args.Temperature);
                end
            end
        end

        % Apply updated parameters back to CompositeFun
        Model.setAllParStruct(AllPar);
    end

    if Args.Verbose
        fprintf('\nCompositeFun built: %d functions, %d total parameters\n', ...
                NumFunctions, Model.numAllPar());
        fprintf('=== TRANSMISSION MODEL COMPLETE ===\n');
    end
end
