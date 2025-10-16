classdef TransmissionFunctionComponents < handle
    % TransmissionFunctionComponents - Manages transmission function calculation
    % This class provides a unified interface for computing transmission
    % using multiple components (ozone, aerosol, Rayleigh, water vapor, etc.).
    % Usage:
    %   Lambda = linspace(300, 1100, 401)';
    %   Model = astro.transmission.TransmissionFunctionComponents();
    %   Model.addComponent(@astro.transmission.ozoneTransmission);  % Use all defaults
    %   Model.addComponent(@astro.transmission.aerosolTransmission); 
    %   Trans = Model.evaluate(Lambda, []);
       
    %   Model.addComponent(@astro.transmission.aerosolTransmission, 'TauAod500', NaN);  % Override specific param
    %   Trans = Model.evaluate(Lambda, FittedParameters);
    %   Model.getOptimizationInfo();

    properties (Access = private)
        ParameterRegistry  % Central parameter definitions {name, defaultValue, functions}
        CurrentDefaults    % Current default values (can be modified)
        Components         % Added components {funHandle, paramMatrix, options}
        ParamMapping       % Maps fitted parameters to components
        SharedData         % Pre-loaded absorption data
        Wavelengths        % Default wavelength grid
    end

    properties (Access = public)
        Verbose = false    % Display calculation progress
    end

    methods
        function obj = TransmissionFunctionComponents(Lambda, Args)
            % Create new transmission model with parameter registry and wavelength grid
            % Input  : - Lambda - Wavelength array in nm (column vector).
            %            Default is linspace(300, 1100, 401)'.
            %          * ...,key,val,...
            %            'Verbose' - Display calculation progress. Default is false.
            % Output : - obj - TransmissionFunctionComponents object.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model = astro.transmission.TransmissionFunctionComponents();
            %          Model = astro.transmission.TransmissionFunctionComponents(linspace(300,800,201)', 'Verbose', true);

            arguments
                Lambda = linspace(300, 1100, 401)';
                Args.Verbose = false;
            end

            obj.Components = {};
            obj.ParamMapping = {};
            obj.SharedData = [];
            obj.Wavelengths = Lambda(:);  % Ensure column vector
            obj.Verbose = Args.Verbose;

            % Initialize parameter registry
            obj.initializeParameterRegistry();

            % Copy defaults to current defaults (can be modified)
            obj.CurrentDefaults = containers.Map();
            Keys = keys(obj.ParameterRegistry);
            for i = 1:length(Keys)
                Key = Keys{i};
                if ~strcmp(Key, '_FunctionSignatures')  % Skip special keys
                    ParamDef = obj.ParameterRegistry(Key);
                    obj.CurrentDefaults(Key) = ParamDef.defaultValue;
                end
            end

            if obj.Verbose
                fprintf('TransmissionFunctionComponents initialized with %d parameters in registry\n', ...
                        obj.ParameterRegistry.Count - 1);  % Exclude _FunctionSignatures
            end
        end

        function addComponent(obj, FunHandle, varargin)
            % Add transmission component using parameter registry defaults
            % Input  : - FunHandle - Function handle to transmission function.
            %          - varargin - Parameter overrides as name-value pairs.
            %            Use NaN to make parameter fitted. Only specify
            %            parameters you want to change from registry defaults.
            % Output : None (modifies object).
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.addComponent(@astro.transmission.ozoneTransmission);
            %          Model.addComponent(@astro.transmission.aerosolTransmission, 'TauAod500', NaN);

            % Extract function name
            FunName = obj.extractFunctionName(FunHandle);

            % Parse optional arguments
            if mod(length(varargin), 2) ~= 0
                error('Parameters must be specified as name-value pairs');
            end

            % Convert varargin to overrides map
            Overrides = containers.Map();
            for i = 1:2:length(varargin)
                ParamName = varargin{i};
                ParamValue = varargin{i+1};
                if ~ischar(ParamName) && ~isstring(ParamName)
                    error('Parameter names must be strings');
                end
                Overrides(char(ParamName)) = ParamValue;
            end

            % Get parameters for this function from registry
            RequiredParams = obj.getParametersForFunction(FunName);
            if isempty(RequiredParams)
                warning('Function "%s" not found in registry. Using overrides only.', FunName);
                RequiredParams = keys(Overrides);
            end

            % Build parameter matrix with defaults and overrides
            ParamMatrix = obj.buildParameterMatrix(RequiredParams, Overrides);

            % Create component options
            Options = struct();
            Options.name = FunName;
            Options.funHandle = FunHandle;

            % Store the component
            obj.Components{end+1} = {FunHandle, ParamMatrix, Options};

            % Update parameter mapping
            obj.updateParameterMapping();

            if obj.Verbose
                NumFitted = obj.countFittedInMatrix(ParamMatrix);
                fprintf('Added component "%s" with %d parameters (%d fitted)\n', ...
                        FunName, size(ParamMatrix, 1), NumFitted);
            end
        end

        function setDefaultValue(obj, ParamName, Value)
            % Update default value for parameter affecting future component additions
            % Input  : - ParamName - Parameter name (string).
            %          - Value - New default value (numeric or NaN for fitted).
            % Output : None (modifies object).
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.setDefaultValue('Pressure_mbar', 950);
            %          Model.setDefaultValue('ZenithAngle_deg', NaN);

            if ~obj.CurrentDefaults.isKey(ParamName)
                warning('Parameter "%s" not in registry. Adding it.', ParamName);
            end
            obj.CurrentDefaults(ParamName) = Value;

            if obj.Verbose
                fprintf('Updated default: %s = %g\n', ParamName, Value);
            end
        end

        function Defaults = getDefaultValues(obj)
            % Get current default values
            % Input  : None.
            % Output : - Defaults - Structure with parameter names and current default values.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Defaults = Model.getDefaultValues();
            %          fprintf('ZenithAngle default: %.1f\n', Defaults.ZenithAngle_deg);

            Defaults = struct();
            Keys = keys(obj.CurrentDefaults);
            for i = 1:length(Keys)
                Defaults.(Keys{i}) = obj.CurrentDefaults(Keys{i});
            end
        end

        function ParamList = getParametersForFunction(obj, FunName)
            % Get list of parameters used by a specific function
            % Input  : - FunName - Function name (string).
            % Output : - ParamList - Cell array of parameter names in correct order.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Params = Model.getParametersForFunction('ozoneTransmission');

            FunctionSignatures = obj.ParameterRegistry('_FunctionSignatures');

            if FunctionSignatures.isKey(FunName)
                ParamList = FunctionSignatures(FunName);
            else
                ParamList = {};
            end
        end

        function Info = getParameterInfo(obj, ParamName)
            % Get information about specific parameter or all parameters
            % Input  : - ParamName - Parameter name (string). Default is "" for all parameters.
            % Output : - Info - Structure with parameter information (bounds, description, etc.).
            % Author : D. Kovaleva (Oct 2025)
            % Example: Info = Model.getParameterInfo('TauAod500');
            %          AllInfo = Model.getParameterInfo();

            arguments
                obj
                ParamName string = ""
            end

            if ParamName == ""
                % Return info for all parameters
                Info = struct();
                Keys = keys(obj.ParameterRegistry);
                for i = 1:length(Keys)
                    Info.(Keys{i}) = obj.ParameterRegistry(Keys{i});
                    Info.(Keys{i}).currentDefault = obj.CurrentDefaults(Keys{i});
                end
            else
                % Return info for specific parameter
                if obj.ParameterRegistry.isKey(char(ParamName))
                    Info = obj.ParameterRegistry(char(ParamName));
                    Info.currentDefault = obj.CurrentDefaults(char(ParamName));
                else
                    error('Parameter "%s" not found in registry', ParamName);  % DEBUGGING
                end
            end
        end

        function Trans = evaluate(obj, Lambda, ParamVector, Args)
            % Calculate total transmission by calling all components and multiplying results
            % Input  : - Lambda - Wavelength array in nm (column vector).
            %          - ParamVector - Vector of fitted parameter values.
            %            Length must match parame().
            %          * ...,key,val,...
            %            'Return' - Pre-computed results for caching. Default is [].
            % Output : - Trans - Total transmission vector [0-1].
            % Author : D. Kovaleva (Oct 2025)
            % Example: Lambda = linspace(300, 800, 101)';
            %          FittedParams = [45; 300];
            %          Trans = Model.evaluate(Lambda, FittedParams);

            arguments
                obj
                Lambda 
                ParamVector 
                Args.Return = [];
            end

            % Check for pre-computed results
            if ~isempty(Args.Return)
                Trans = Args.Return;
                return;
            end

            % Validate parameter count
            NumFittedExpected = obj.getNumFittedParameters();
            if length(ParamVector) ~= NumFittedExpected
                error('Expected %d fitted parameters, got %d', NumFittedExpected, length(ParamVector));
            end

            NumWavelengths = length(Lambda);

            % Initialize output
            Trans = ones(NumWavelengths, 1);

            if obj.Verbose
                fprintf('Evaluating transmission with %d fitted parameters...\n', length(ParamVector));
            end

            % Process each component
            for CompIdx = 1:length(obj.Components)
                Component = obj.Components{CompIdx};
                FunHandle = Component{1};
                ParamMatrix = Component{2};
                Options = Component{3};

                % Build numeric parameter array for this component
                ComponentParams = obj.buildComponentParameters(CompIdx, ParamVector);

                % Convert to format expected by transmission functions
                % ParamMatrix for transmission functions: rows are parameter sets
                ComponentParamMatrix = ComponentParams';  % Make it a row vector

                % Call transmission function
                try
                    ComponentTrans = obj.callTransmissionFunction(FunHandle, Lambda, ComponentParamMatrix, Options);
                    Trans = Trans .* ComponentTrans;
                catch ME
                    error('Error in component "%s": %s', Options.name, ME.message);
                end
            end
        end

        function NumParams = getNumFittedParameters(obj)
            % Get total number of fitted parameters
            % Input  : None.
            % Output : - NumParams - Number of fitted parameters (integer).
            % Author : D. Kovaleva (Oct 2025)
            % Example: NumParams = Model.getNumFittedParameters();

            NumParams = size(obj.ParamMapping, 1);
        end

        function Mapping = getFittedParameterMapping(obj)
            % Get mapping of fitted parameters to components
            % Input  : None.
            % Output : - Mapping - Cell array with {paramName, componentIdx, paramIdx}.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Mapping = Model.getFittedParameterMapping();
            %          fprintf('Parameter %s in component %d\n', Mapping{1,1}, Mapping{1,2});

            Mapping = obj.ParamMapping;
        end

        function Bounds = getFittedParameterBounds(obj)
            % Get optimization bounds for all fitted parameters
            % Input  : None.
            % Output : - Bounds - Structure with Lower/Upper bounds vectors and Names.
            %            Fields: .Lower, .Upper, .Names.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Bounds = Model.getFittedParameterBounds();
            %          fprintf('Parameter %s: [%.2f, %.2f]\n', Bounds.Names{1}, Bounds.Lower(1), Bounds.Upper(1));

            NumFitted = obj.getNumFittedParameters();

            if NumFitted == 0
                Bounds = struct('Lower', [], 'Upper', [], 'Names', {{}});
                return;
            end

            Bounds = struct();
            Bounds.Lower = zeros(NumFitted, 1);
            Bounds.Upper = zeros(NumFitted, 1);
            Bounds.Names = cell(NumFitted, 1);

            for i = 1:NumFitted
                ParamName = obj.ParamMapping{i, 1};

                if obj.ParameterRegistry.isKey(ParamName)
                    ParamDef = obj.ParameterRegistry(ParamName);
                    Bounds.Lower(i) = ParamDef.bounds(1);
                    Bounds.Upper(i) = ParamDef.bounds(2);
                    Bounds.Names{i} = ParamName;
                else
                    % Default bounds if parameter not in registry
                    Bounds.Lower(i) = -Inf;
                    Bounds.Upper(i) = Inf;
                    Bounds.Names{i} = ParamName;
                    warning('Parameter "%s" not found in registry, using unbounded', ParamName);
                end
            end
        end

        function Bounds = getParameterBounds(obj, ParamName)
            % Get bounds for a specific parameter
            % Input  : - ParamName - Parameter name (string).
            % Output : - Bounds - [lower, upper] bounds array.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Bounds = Model.getParameterBounds('TauAod500');

            if obj.ParameterRegistry.isKey(char(ParamName))
                ParamDef = obj.ParameterRegistry(char(ParamName));
                if isfield(ParamDef, 'bounds')
                    Bounds = ParamDef.bounds;
                else
                    Bounds = [-Inf, Inf];
                end
            else
                error('Parameter "%s" not found in registry', ParamName);  % DEBUGGING
            end
        end

        function setBounds(obj, ParamName, Bounds)
            % Set bounds for a specific parameter
            % Input  : - ParamName - Parameter name (string).
            %          - Bounds - [lower, upper] bounds array.
            % Output : None.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.setBounds('TauAod500', [0.05, 0.5]);

            if obj.ParameterRegistry.isKey(char(ParamName))
                ParamDef = obj.ParameterRegistry(char(ParamName));
                ParamDef.bounds = Bounds;
                obj.ParameterRegistry(char(ParamName)) = ParamDef;

                if obj.Verbose
                    fprintf('Updated bounds for %s: [%.3f, %.3f]\n', ParamName, Bounds(1), Bounds(2));
                end
            else
                error('Parameter "%s" not found in registry', ParamName);  % DEBUGGING
            end
        end

        function OptInfo = getOptimizationInfo(obj)
            % Get optimization information for fitted parameters ready for optimizers
            % Input  : None.
            % Output : - OptInfo - Structure with LowerBounds, UpperBounds, Names,
            %            NumParameters, and InitialGuess fields.
            % Author : D. Kovaleva (Oct 2025)
            % Example: OptInfo = Model.getOptimizationInfo();
            %          [optimal, fval] = fmincon(@costFun, OptInfo.InitialGuess, [], [], [], [], OptInfo.LowerBounds, OptInfo.UpperBounds);

            Bounds = obj.getFittedParameterBounds();
            NumParams = obj.getNumFittedParameters();

            OptInfo = struct();
            OptInfo.LowerBounds = Bounds.Lower;
            OptInfo.UpperBounds = Bounds.Upper;
            OptInfo.Names = Bounds.Names;
            OptInfo.NumParameters = NumParams;

            % Generate initial guess from current defaults or midpoint of bounds
            OptInfo.InitialGuess = zeros(NumParams, 1);
            for i = 1:NumParams
                ParamName = Bounds.Names{i};
                if obj.CurrentDefaults.isKey(ParamName) && ~isnan(obj.CurrentDefaults(ParamName))
                    % Use current default if available and not NaN
                    OptInfo.InitialGuess(i) = obj.CurrentDefaults(ParamName);
                else
                    % Use midpoint of bounds as initial guess
                    OptInfo.InitialGuess(i) = (Bounds.Lower(i) + Bounds.Upper(i)) / 2;
                end

                % Ensure initial guess is within bounds
                OptInfo.InitialGuess(i) = max(Bounds.Lower(i), min(Bounds.Upper(i), OptInfo.InitialGuess(i)));
            end

            if obj.Verbose
                fprintf('Optimization info for %d fitted parameters:\n', NumParams);
                for i = 1:NumParams
                    fprintf('  %s: bounds [%.3f, %.3f], initial=%.3f\n', ...
                            Bounds.Names{i}, Bounds.Lower(i), Bounds.Upper(i), OptInfo.InitialGuess(i));
                end
            end
        end

        function Summary = listComponents(obj)
            % List all added components and their parameters
            % Input  : None.
            % Output : - Summary - Structure with component information and parameters.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Summary = Model.listComponents();
            %          ComponentNames = fieldnames(Summary);

            Summary = struct();

            for i = 1:length(obj.Components)
                Component = obj.Components{i};
                Options = Component{3};
                ParamMatrix = Component{2};

                Summary.(Options.name) = struct();
                Summary.(Options.name).parameters = ParamMatrix;
                Summary.(Options.name).numFitted = obj.countFittedInMatrix(ParamMatrix);
            end
        end

        function loadAbsorptionData(obj, Args)
            % Pre-load absorption data for all components that need it
            % Input  : * ...,key,val,...
            %            'Verbose' - Display loading progress. Default is obj.Verbose.
            % Output : None.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.loadAbsorptionData();

            arguments
                obj
                Args.Verbose = obj.Verbose;
            end

            if isempty(obj.SharedData)
                obj.SharedData = astro.transmission.loadAbsorptionInterpolants('Verbose', Args.Verbose);
            end
        end

        function exportConfig(obj, FileName)
            % Export current configuration to file
            % Input  : - FileName - Output file name (string).
            % Output : None.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.exportConfig('my_config.mat');

            Config.defaults = obj.getDefaultValues();
            Config.components = obj.listComponents();
            Config.registry = obj.exportRegistry();
            save(FileName, 'Config');

            if obj.Verbose
                fprintf('Configuration exported to %s\n', FileName);
            end
        end

        function importConfig(obj, FileName)
            % Import configuration from file
            % Input  : - FileName - Input file name (string).
            % Output : None.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.importConfig('my_config.mat');

            Load = load(FileName);
            Config = Load.Config;

            % Update defaults
            Fields = fieldnames(Config.defaults);
            for i = 1:length(Fields)
                obj.setDefaultValue(Fields{i}, Config.defaults.(Fields{i}));
            end

            if obj.Verbose
                fprintf('Configuration imported from %s\n', FileName);
            end
        end
    end

    methods (Access = private)
        function initializeParameterRegistry(obj)
            % Initialize the central parameter registry
            % Store function signatures with correct parameter order

            obj.ParameterRegistry = containers.Map();

            % Function signatures - parameter order matters!
            FunctionSignatures = containers.Map();
            FunctionSignatures('ozoneTransmission') = {'ZenithAngle_deg', 'DobsonUnits'};
            FunctionSignatures('aerosolTransmission') = {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'};
            FunctionSignatures('rayleighTransmission') = {'ZenithAngle_deg', 'Pressure_mbar'};
            FunctionSignatures('waterTransmission') = {'ZenithAngle_deg', 'Pressure_mbar', 'Temperature_K', 'PrecipitableWater'};
        
            % Store signatures for parameter ordering
            obj.ParameterRegistry('_FunctionSignatures') = FunctionSignatures;

            % Parameter definitions with bounds (used for fitted parameters only)
            obj.ParameterRegistry('ZenithAngle_deg') = struct(...
                'defaultValue', 30, ...
                'bounds', [0, 90], ...
                'description', 'Solar zenith angle in degrees [0-90]');

            obj.ParameterRegistry('Pressure_mbar') = struct(...
                'defaultValue', 965, ...
                'bounds', [960, 1070], ...
                'description', 'Atmospheric pressure in mbar [960-1070]');

            obj.ParameterRegistry('Temperature_K') = struct(...
                'defaultValue', 288, ...
                'bounds', [283, 308], ...  % 10-35°C converted to Kelvin
                'description', 'Temperature in Kelvin [283-308]');

            obj.ParameterRegistry('DobsonUnits') = struct(...
                'defaultValue', 300, ...
                'bounds', [200, 400], ...
                'description', 'Total ozone column in Dobson units [200-400]');

            obj.ParameterRegistry('TauAod500') = struct(...
                'defaultValue', 0.085, ...
                'bounds', [0.01, 1.0], ...
                'description', 'Aerosol optical depth at 500nm [0.01-1.0]');

            obj.ParameterRegistry('AngstromExponent') = struct(...
                'defaultValue', 0.6, ...
                'bounds', [0.0001, 5.0], ...
                'description', 'Angstrom exponent for aerosol wavelength dependence [0.0001-5.0]');

            obj.ParameterRegistry('PrecipitableWater') = struct(...
                'defaultValue', 1.0, ...
                'bounds', [0.1, 10.0], ...
                'description', 'Precipitable water vapor in cm [0.1-10.0]');

            obj.ParameterRegistry('CO2_ppm') = struct(...
                'defaultValue', 420, ...
                'bounds', [380, 450], ...
                'description', 'CO2 concentration in ppm [380-450]');
        end

        function FunName = extractFunctionName(obj, FunHandle)
            % Extract clean function name from function handle
            FullName = func2str(FunHandle);

            % Remove package prefixes if present
            Parts = strsplit(FullName, '.');
            FunName = Parts{end};

            % Keep full name for registry lookup
            % FunName remains as is for consistency with registry
        end

        function ParamMatrix = buildParameterMatrix(obj, RequiredParams, Overrides)
            % Build parameter matrix for a component
            %
            % Input:
            %   RequiredParams - Cell array of parameter names
            %   Overrides - Map of parameter overrides
            %
            % Output:
            %   ParamMatrix - Cell array {paramName, value; ...}

            ParamMatrix = {};

            for i = 1:length(RequiredParams)
                ParamName = RequiredParams{i};

                % Get value: override > current default > registry default
                if Overrides.isKey(ParamName)
                    Value = Overrides(ParamName);
                elseif obj.CurrentDefaults.isKey(ParamName)
                    Value = obj.CurrentDefaults(ParamName);
                elseif obj.ParameterRegistry.isKey(ParamName)
                    ParamDef = obj.ParameterRegistry(ParamName);
                    Value = ParamDef.defaultValue;
                else
                    warning('Parameter "%s" not found in registry, using NaN', ParamName);  % DEBUGGING
                    Value = NaN;
                end

                ParamMatrix{end+1, 1} = ParamName;
                ParamMatrix{end, 2} = Value;
            end
        end

        function updateParameterMapping(obj)
            % Update mapping of fitted parameters to components
            obj.ParamMapping = {};

            % Track shared parameters
            SharedParams = containers.Map();

            for CompIdx = 1:length(obj.Components)
                Component = obj.Components{CompIdx};
                ParamMatrix = Component{2};

                for ParamIdx = 1:size(ParamMatrix, 1)
                    ParamName = ParamMatrix{ParamIdx, 1};
                    ParamValue = ParamMatrix{ParamIdx, 2};

                    if isnumeric(ParamValue) && isnan(ParamValue)
                        % This parameter needs to be fitted

                        if SharedParams.isKey(ParamName)
                            % Already mapped - this is a shared parameter
                            % Don't add duplicate mapping
                        else
                            % New fitted parameter
                            obj.ParamMapping{end+1, 1} = ParamName;
                            obj.ParamMapping{end, 2} = CompIdx;
                            obj.ParamMapping{end, 3} = ParamIdx;
                            SharedParams(ParamName) = size(obj.ParamMapping, 1);
                        end
                    end
                end
            end
        end

        function ComponentParams = buildComponentParameters(obj, CompIdx, ParamVector)
            % Build numeric parameter array for a specific component
            %
            % Input:
            %   CompIdx - Component index
            %   ParamVector - Vector of fitted parameters
            %
            % Output:
            %   ComponentParams - Numeric array of parameters

            Component = obj.Components{CompIdx};
            ParamMatrix = Component{2};
            NumParams = size(ParamMatrix, 1);

            ComponentParams = zeros(NumParams, 1);

            for ParamIdx = 1:NumParams
                ParamName = ParamMatrix{ParamIdx, 1};
                ParamValue = ParamMatrix{ParamIdx, 2};

                if isnumeric(ParamValue) && ~isnan(ParamValue)
                    % Fixed parameter
                    ComponentParams(ParamIdx) = ParamValue;
                else
                    % Fitted parameter - find in mapping
                    MappingIdx = obj.findInMapping(ParamName);
                    if MappingIdx > 0
                        ComponentParams(ParamIdx) = ParamVector(MappingIdx);
                    else
                        error('Fitted parameter "%s" not found in mapping', ParamName);  % DEBUGGING
                    end
                end
            end
        end

        function MappingIdx = findInMapping(obj, ParamName)
            % Find parameter in the mapping
            MappingIdx = 0;
            for i = 1:size(obj.ParamMapping, 1)
                if strcmp(obj.ParamMapping{i, 1}, ParamName)
                    MappingIdx = i;
                    return;
                end
            end
        end

        function Trans = callTransmissionFunction(obj, FunHandle, Lambda, ParamMatrix, Options)
            % Call individual transmission function with optional caching support

            % Load absorption data once if not already loaded
            if isempty(obj.SharedData)
                obj.loadAbsorptionData();
            end

            % Check if we can use cached result (only if no NaN parameters)
            hasNaN = any(isnan(ParamMatrix));
            if ~hasNaN && isfield(Options, 'cachedResult') && ~isempty(Options.cachedResult)
                % All parameters are fixed, can use cache
                Trans = FunHandle(Lambda, ParamMatrix, 'AbsorptionData', obj.SharedData, 'Return', Options.cachedResult);
            else
                % Has fitted parameters or no cache available
                Trans = FunHandle(Lambda, ParamMatrix, 'AbsorptionData', obj.SharedData);
            end
        end

        function NumFitted = countFittedInMatrix(obj, ParamMatrix)
            % Count number of fitted parameters in a matrix
            NumFitted = 0;
            for i = 1:size(ParamMatrix, 1)
                Value = ParamMatrix{i, 2};
                if isnumeric(Value) && isnan(Value)
                    NumFitted = NumFitted + 1;
                end
            end
        end

        function Registry = exportRegistry(obj)
            % Export parameter registry as struct
            Registry = struct();
            Keys = keys(obj.ParameterRegistry);
            for i = 1:length(Keys)
                Registry.(Keys{i}) = obj.ParameterRegistry(Keys{i});
            end
        end
    end
end
