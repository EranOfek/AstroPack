function Model = transmissionModel(TransFunList, Airmass, Temperature, Pressure_mbar, Args)
    % Build CompositeFun transmission model from basic function
    % specification list (astro.transmission.* for atmospheric
    % transmission, telescope.detector.* and telescope.optics.* for instrumental transmission). 
    % Uses class tools.math.fun.CompositeFun.
    %
    % Input  : - TransFunList - Cell array of specifications of basic transmission function 
    %                   Each cell is a struct with fields:
    %                   .name - Function name (string)
    %                   .handle - Function handle string (e.g., '@astro.transmission.ozoneTransmission')
    %                   .handletype - 'named' or 'anonymous'
    %                   .params - Parameter values (numeric array or cell array)
    %                   .paraminfo - Cell array of parameter info structs with fields:
    %                                .name, .description, .min, .max
    %          - Airmass - Atmospheric airmass (>= 1.0).
    %          - Temperature - Temperature [C].
    %          - Pressure_mbar - Atmospheric pressure [mbar].
    %          * ...,key,val,...
    %            'Verbose' - Enable verbose output.
    %                   Default is false.
    % Output : - Model - CompositeFun object with all transmission
    %                   functions added, and observation-specific metadata (ZenithAngle_deg, 
    %                   Pressure_mbar, Temperature_C) injected.
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Define transmission functions
    %          TransFunList = cell(2, 1);
    %          TransFunList{1} = struct('name', 'Ozone', ...
    %              'handle', '@astro.transmission.ozoneTransmission', ...
    %              'handletype', 'named', ...
    %              'params', [30, 300], ...
    %              'paraminfo', {{struct('name', 'ZenithAngle_deg', 'min', 0, 'max', 90), ...
    %                             struct('name', 'DobsonUnits', 'min', 200, 'max', 400)}});
    %          TransFunList{2} = struct('name', 'Aerosol', ...
    %              'handle', '@astro.transmission.aerosolTransmission', ...
    %              'handletype', 'named', ...
    %              'params', [30, 0.05, 1.2], ...
    %              'paraminfo', {{struct('name', 'ZenithAngle_deg', 'min', 0, 'max', 90), ...
    %                             struct('name', 'TauAod500', 'min', 0.0, 'max', 0.5), ...
    %                             struct('name', 'Alpha', 'min', 0.5, 'max', 2.5)}});
    %          Model = imUtil.calib.transmissionModel(TransFunList, 1.2, 15, 965);

    arguments
        TransFunList cell
        Airmass = 1.2       % from observations 
        Temperature = 15.0  % from observations
        Pressure_mbar = 965 % by default (or from observations)
        Args.Verbose logical = false
    end

    % ====================================================================
    % STEP 1: VALIDATE INPUTS
    % ====================================================================

    if Airmass < 1.0
        error('Invalid Airmass: %.3f (must be >= 1.0)', Airmass);
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
        FunDef = TransFunList{I};

        % Extract function definition
        FunName = FunDef.name;
        HandleStr = FunDef.handle;
        HandleType = FunDef.handletype;
        Params = FunDef.params;
        ParamInfo = FunDef.paraminfo;

        % Ensure Params is numeric array
        if iscell(Params)
            Params = cell2mat(Params);
        end
        if ~isnumeric(Params)
            error('Parameters for function %s are not numeric', FunName);
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
            PInfo = ParamInfo{J};
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
    % STEP 4: INJECT OBSERVATION METADATA
    % ====================================================================

    % Convert airmass to zenith angle
    ZenithAngle_deg = acosd(1.0 / Airmass);

    if Args.Verbose
        fprintf('\nInjecting observation metadata:\n');
        fprintf('  Airmass: %.3f -> Zenith angle: %.2f deg\n', Airmass, ZenithAngle_deg);
        fprintf('  Temperature: %.1f C\n', Temperature);
        fprintf('  Pressure: %.1f mbar\n', Pressure_mbar);
    end

    % Get all parameters structure from CompositeFun
    AllPar = Model.getAllParStruct();

    % Update parameters by name matching
    for I = 1:length(AllPar.Names)
        ParamName = AllPar.Names{I};

        if strcmp(ParamName, 'ZenithAngle_deg')
            AllPar.Values(I) = ZenithAngle_deg;
            if Args.Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, ZenithAngle_deg);
            end
        elseif strcmp(ParamName, 'Pressure_mbar')
            AllPar.Values(I) = Pressure_mbar;
            if Args.Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, Pressure_mbar);
            end
        elseif strcmp(ParamName, 'Temperature_C')
            AllPar.Values(I) = Temperature;
            if Args.Verbose
                fprintf('  Injected %s = %.3f\n', ParamName, Temperature);
            end
        end
    end

    % Apply updated parameters back to CompositeFun
    Model.setAllParStruct(AllPar);

    if Args.Verbose
        fprintf('\nCompositeFun built: %d functions, %d total parameters\n', ...
                NumFunctions, Model.numAllPar());
        fprintf('=== TRANSMISSION MODEL COMPLETE ===\n');
    end
end
