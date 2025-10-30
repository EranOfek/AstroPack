classdef CompositeFun < handle
    % CompositeFun - Composite function framework for transmission calculations
    % This class provides a unified interface for combining multiple transmission
    % functions (ozone, aerosol, Rayleigh, etc.) with parameter mapping and optimization support.
    %
    % Example - Basic Setup:
    %   % Create composite function
    %   Model = tools.math.fun.CompositeFun();
    %
    %   % Adding functions
    %   % Method 1: Auto-extract ArgNames from function handle(recommended)
    %   Model.addFun('Ozone transmission', @astro.transmission.ozoneTransmission, [], 'Par', [30, 300], 'FitPar', [false, false]);
    %   Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, [], 'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
    %   
    %   % Method 2: Explicit extractArgFuns helper function
    %   OzoneArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission);
    %   Model.addFun('Ozone transmission', @astro.transmission.ozoneTransmission, OzoneArgNames, ...
    %                'Par', [45, 300], 'FitPar', [false, true]);
    %
    %   % Method 3: Manual ArgNames construction
    %   AerosolArgNames = struct('Name', {1, 2, 3}, ...
    %                           'Description', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
    %                           'Min', {0, 0.01, 0.5}, 'Max', {90, 0.5, 2.0});
    %   Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, AerosolArgNames, ...
    %                'Par', [30, 0.05, 1.2], 'FitPar', [false, true, false]);
    %
    %   % Method 4: Direct function call for ArgNames
    %   OzoneArgNames = astro.transmission.ozoneTransmission('GetArgNames', true);
    %
    %   % Adding simple mathematical functions
    %   % Sin function: y = A * sin(x + B)
    %   SinArgNames = struct('Name', {1, 2}, 'Description', {'SinAmplitude', 'Phase'}, 'Min', {0, -pi}, 'Max', {10, pi});
    %   Model.addFun('Sine function', @(x, par) par(1) * sin(x + par(2)), SinArgNames, 'Par', [1, 0], 'FitPar', [true, false]);
    %
    %   % Cos function: y = C * cos(D * x)
    %   CosArgNames = struct('Name', {1, 2}, 'Description', {'CosAmplitude', 'Frequency'}, 'Min', {0, 0}, 'Max', {5, 10});
    %   Model.addFun('Cosine function', @(x, par) par(1) * cos(par(2) * x), CosArgNames, 'Par', [2, 1], 'FitPar', [false, true]);
    %
    %   % After functions are added, can also get ArgNames from model
    %   ArgNames1 = Model.Funs(1).ArgNames;     % Same as OzoneArgNames
    %   ArgNames2 = Model.Funs(2).ArgNames;     % Same as AerosolArgNames
    %
    %   % It is possible to add function(s) without setting parameter values (NaN by default),
    %   % but fixed parameters (FitPar=false) must be set before calculations:
    %   Model.addFun('Aerosol transmission', @astro.transmission.aerosolTransmission, [], 'Par', [], 'FitPar', [false, true, false]);
    %   AllParams = Model.getAllParamStruct();  % Get parameter structure
    %   % AllParams.Names shows parameter names and their global indices
    %   AllParams.Values(1) = 30;   % Set ZenithAngle_deg (fixed parameter)
    %   AllParams.Values(3) = 1.2;  % Set AngstromExponent (fixed parameter)
    %   % Parameter 2 (TauAod500) will be fitted, so can remain NaN initially
    %   Model.setAllParamStruct(AllParams);  % Apply the values 
    %
    % Example - Information Getters:
    %   % Get function summary
    %   FunsNames = Model.namesFuns();
    %   fprintf('Added %d functions\n', size(FunsNames, 1));
    %
    %   % Get parameter information
    %   fprintf('Total parameters: %d\n', Model.numAllParam());
    %   fprintf('Fitted parameters: %d\n', Model.numFittedParam());
    %
    %   AllNames = Model.namesAllParam();
    %   AllValues = Model.valuesAllParam();
    %   FittedNames = Model.namesFittedParam();
    %   FittedInfo = Model.getFittedParamStruct();
    %
    %   % Get detailed function information
    %   AllFuns = Model.allFunsStruct();    % Complete structure with all fields
    %   FunsNames = Model.namesFuns();      % Cell array: {Name, Description}
    %
    % Example - Dynamic Parameter Management:
    %   % Get current parameter structure
    %   AllParams = Model.getAllParamStruct();
    %
    %   % Modify parameters and fit flags for optimization
    %   AllParams.Values(2) = 350;      % Change ozone value
    %   AllParams.FitPar(1) = false;    % Fix zenith angle
    %   AllParams.FitPar(3) = true;     % Fit aerosol parameter
    %   Model.setAllParamStruct(AllParams);  % Handle class - modifies in place
    %
    % Example - Pre-calculation and Evaluation:
    %   % Define wavelength range
    %   Lambda = linspace(300, 1100, 401)';
    %
    %   % Pre-calculate functions with fixed parameters (after setting fit flags)
    %   Model.preCalc(Lambda);
    %
    %   % Method 1: Evaluate with all parameter values (direct input)
    %   NewAllValues = [45, 280, 0.08, 0.6];  % All parameters
    %   Transmission = Model.evaluateAllParamInput(Lambda, NewAllValues);
    %
    %   % Method 2: Evaluate with only fitted parameters (fixed parameters pre-set)
    %   % First set all fixed parameters using setAllParamStruct (if not
    %   % set already)
    %   AllParams = Model.getAllParamStruct();
    %   AllParams(1).Value = 45;    % Set zenith angle (fixed)
    %   AllParams(1).FitPar = false;
    %   AllParams(2).Value = 280;   % Set ozone value (fixed)
    %   AllParams(2).FitPar = false;
    %   AllParams(3).FitPar = true; % Fit aerosol AOD
    %   AllParams(4).FitPar = true; % Fit Angstrom exponent
    %   Model.setAllParamStruct(AllParams);
    %   % Now evaluate with only fitted parameters
    %   FittedValues = [0.08, 0.6];  % Only fitted parameters
    %   Transmission = Model.evaluate(Lambda, FittedValues);
    %
    % Methods:
    %   Constructor: CompositeFun() - Create composite function object
    %   addFun() - Add transmission function with parameters
    %   preCalc() - Pre-calculate functions with fixed parameters
    %   evaluate() - Evaluate composite function 
    %   evaluateAllParamInput() - Evaluate composite function with all-
    %                             parameters input
    %   checkParamConsistency() - Validate parameter consistency across functions
    %
    % Setters:
    %   setAllParamStruct() - Update AllParams (Model.Funs.Par, Model.Funs.FitPar)
    %
    % Getters:
    %   numAllParam() - Count of all parameters (fitted + fixed)
    %   namesAllParam() - Names of all parameters, cell array
    %   valuesAllParam() - Values of all parameters, vector
    %   numFittedParam() - Count of fitted parameters only
    %   namesFittedParam() - Names of fitted parameters only, cell array
    %   namesFuns() - Names and descriptions of added functions as cell array
    %   allFunsStruct() - Complete Funs structure array
    %   getFittedParamStruct() - Comprehensive fitted parameter details,
    %                            structure array
    %   getAllParamStruct() - Get AllParams structure array
    %
    % Internal methods:
    %   extractArgFuns() - Extract argument information from function handles
    %   argMapping() - Map global parameters for newly added functions (builds from Funs structure)
    %


    properties
        Funs        = struct('Name',[], 'Desc','', 'Handle',[], 'Par',[], 'FitPar',[], 'OptionalArgs',{}, ...
                              'ArgNames',[], 'ArgMapping',[], 'PreCalc',[]);
                            % Name - Function name
                            % Desc - Function description
                            % Handle - Function handle to function of the
                            %       form: Y=F(X, Par, OptionaArgs{:})
                            %       X and Y can be arrays of any dim.
                            %       Par is a vector of parameters of the
                            %       function.
                            %       OptionalArgs is a cell array of
                            %       arbitrary number of additional
                            %       arguments.
                            % Par - A vector of parameters.
                            % FitPar - a vector of logical (size is like Par)
                            %       that contains true for a parameter to
                            %       fit and false to parameter to hold
                            %       constant.
                            % OptionalArgs - A cell array of
                            %       arbitrary number of additional
                            %       arguments.
                            % ArgsNames - Structure array of Par information returned by ArgsNames=Fun()
                            %       Contains .Name = number of the parameter in the calling for a given function
                            %                .Description = name of the parameter
                            %                .Min
                            %                .Max
                            % ArgMapping - A vector with size equal to
                            %       size(Par) containing in each entry the
                            %       index of this parameter in the global
                            %       evaluation parameters order.
                            % PreCalc - Pre calculated Y - relevant when
                            %       all(FitPar==false).

        FunOperator = '*'
    end

    methods % Constructor
        function Obj = CompositeFun()
            % Constructor for CompositeFun
            % Input  : None
            % Output : - CompositeFun object.
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model = tools.math.fun.CompositeFun();

            % Initialize properties
            Obj.FunOperator = '*';
            Obj.Funs = [];
        end
    end

    methods % setter/getters

        % All parameters (fitted + fixed)
        function NumParams = numAllParam(Obj)
            % Get total number of all global parameters (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - NumParams - Number of all global parameters.
            % Author : D. Kovaleva (Oct 2025)

            if isempty(Obj.Funs)
                NumParams = 0;
            else
                NumParams = max([Obj.Funs.ArgMapping]);
            end
        end

        function ParamNames = namesAllParam(Obj)
            % Get list of all global parameter names (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - ParamNames - Cell array of all parameter names.
            % Author : D. Kovaleva (Oct 2025)

            NumParams = Obj.numAllParam();
            ParamNames = cell(NumParams, 1);

            % Build names from Funs.ArgNames
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamNames{GlobalIndex} = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                    else
                        ParamNames{GlobalIndex} = sprintf('Param_%d', GlobalIndex);
                    end
                end
            end
        end

        function ParamValues = valuesAllParam(Obj)
            % Get current parameter values for all parameters (fitted + fixed)
            % Input  : - Obj - CompositeFun object.
            % Output : - ParamValues - Column vector of all parameter values.
            % Author : D. Kovaleva (Oct 2025)

            AllParams = getAllParamStruct(Obj);
            ParamValues = AllParams.Values;
        end

        % Fitted parameters only
        function NumFittedParams = numFittedParam(Obj)
            % Get total number of fitted parameters only
            % Input  : - Obj - CompositeFun object.
            % Output : - NumFittedParams - Number of parameters marked for fitting.
            % Author : D. Kovaleva (Oct 2025)

            if isempty(Obj.Funs)
                NumFittedParams = 0;
                return;
            end

            % Vectorized approach: sum all FitPar arrays at once
            NumFittedParams = sum(arrayfun(@(f) sum(f.FitPar), Obj.Funs));
        end

        function FittedNames = namesFittedParam(Obj)
            % Get list of fitted parameter names only
            % Input  : - Obj - CompositeFun object.
            % Output : - FittedNames - Cell array of fitted parameter names.
            % Author : D. Kovaleva (Oct 2025)

            NumAllParams = Obj.numAllParam();
            if NumAllParams == 0
                FittedNames = {};
                return;
            end

            % Get all parameter names
            AllNames = Obj.namesAllParam();

            % Create a logical mask for fitted parameters across all global parameters
            IsFitted = false(NumAllParams, 1);

            % Single loop through all functions to mark fitted parameters
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    if Obj.Funs(Ifun).FitPar(Ipar)
                        AllIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                        if AllIndex > 0 && AllIndex <= NumAllParams
                            IsFitted(AllIndex) = true;
                        end
                    end
                end
            end

            % Extract fitted parameter names 
            FittedNames = AllNames(IsFitted);
        end

        % Function information
        function FunsNames = namesFuns(Obj)
            % Get names and descriptions of added functions as cell array
            % Input  : - Obj - CompositeFun object.
            % Output : - FunsNames - Cell array with columns: {Name, Description}.
            % Author : D. Kovaleva (Oct 2025)

            if isempty(Obj.Funs)
                FunsNames = {};
                return;
            end

            FunsNames = cell(numel(Obj.Funs), 2);
            for Ifun = 1:numel(Obj.Funs)
                FunsNames{Ifun, 1} = Obj.Funs(Ifun).Name;
                FunsNames{Ifun, 2} = Obj.Funs(Ifun).Desc;
            end
        end

        function FunsStruct = allFunsStruct(Obj)
            % Get complete Funs structure: Name, Desc, Handle, Par, FitPar,
            %                              OptionalArgs, ArgNames, ArgMapping, Precalc
            % Input  : - Obj - CompositeFun object.
            % Output : - FunsStruct - Complete Funs structure array.
            % Author : D. Kovaleva (Oct 2025)

            FunsStruct = Obj.Funs;
        end

        function FittedInfo = getFittedParamStruct(Obj)
            % Get comprehensive information about fitted parameters
            % Input  : - Obj - CompositeFun object.
            % Output : - FittedInfo - Structure with TotalFitted, FittedNames, FunctionMapping.
            % Author : D. Kovaleva (Oct 2025)

            FittedInfo.TotalFitted = numFittedParam(Obj);
            FittedInfo.FittedNames = namesFittedParam(Obj);

            % Map which global parameter indices each function uses for fitted params
            FittedInfo.FunctionMapping = {};
            for Ifun = 1:numel(Obj.Funs)
                FittedIndices = Obj.Funs(Ifun).ArgMapping(Obj.Funs(Ifun).FitPar);
                FittedInfo.FunctionMapping{Ifun} = FittedIndices(:)';  % Row vector
            end
        end

        function AllParamsStruct = getAllParamStruct(Obj)
            % Get complete parameter structure for optimization
            % Input  : - Obj - CompositeFun object.
            % Output : - AllParamsStruct - Structure with Names, Values, FitPar, Min, Max.
            % Author : D. Kovaleva (Oct 2025)
            % Example: AllParams = Model.getAllParamStruct();
            %   % Modify parameter values and fit flags as needed
            %   AllParams.Values(2) = 350;  % Change parameter value
            %   AllParams.FitPar(3) = true; % Mark parameter for fitting
            %   % Update the model
            %   Model.setAllParamStruct(AllParams);

            AllParamsStruct = struct();

            % Initialize arrays
            NumAllParams = Obj.numAllParam();
            AllParamsStruct.Names = Obj.namesAllParam();
            AllParamsStruct.Values = zeros(NumAllParams, 1);
            AllParamsStruct.FitPar = false(NumAllParams, 1);
            AllParamsStruct.Min = -inf(NumAllParams, 1);  % Default: no lower bound
            AllParamsStruct.Max = inf(NumAllParams, 1);   % Default: no upper bound

            % Fill values, FitPar flags, and bounds by looking at all functions
            for Ifun = 1:numel(Obj.Funs)
                % Vectorized assignment for values and FitPar
                AllIndices = Obj.Funs(Ifun).ArgMapping;
                AllParamsStruct.Values(AllIndices) = Obj.Funs(Ifun).Par;
                AllParamsStruct.FitPar(AllIndices) = Obj.Funs(Ifun).FitPar;

                % Extract bounds from ArgNames
                if ~isempty(Obj.Funs(Ifun).ArgNames)
                    for Ipar = 1:min(length(Obj.Funs(Ifun).Par), length(Obj.Funs(Ifun).ArgNames))
                        ArgInfo = Obj.Funs(Ifun).ArgNames(Ipar);
                        AllIndex = AllIndices(Ipar);
                        if isfield(ArgInfo, 'Min') && ~isempty(ArgInfo.Min)
                            AllParamsStruct.Min(AllIndex) = ArgInfo.Min;
                        end
                        if isfield(ArgInfo, 'Max') && ~isempty(ArgInfo.Max)
                            AllParamsStruct.Max(AllIndex) = ArgInfo.Max;
                        end
                    end
                end
            end

            % Add metadata
            AllParamsStruct.TotalParams = NumAllParams;
            AllParamsStruct.NumFitted = sum(AllParamsStruct.FitPar);
            AllParamsStruct.NumFixed = sum(~AllParamsStruct.FitPar);
        end

        function setAllParamStruct(Obj, AllParamsStruct)
            % Update all parameter values, FitPar flags, and bounds if provided
            % Input  : - Obj - CompositeFun object.
            %          - AllParamsStruct - Structure with Values and FitPar fields.
            %                            Values: vector of parameter values
            %                            FitPar: logical vector of fit flags
            %                            Min: (optional) vector of lower bounds
            %                            Max: (optional) vector of upper bounds
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Oct 2025)
            % Example:
            %   AllParams = Model.getAllParamStruct();
            %   AllParams.Values(2) = 350;  % Change parameter value
            %   AllParams.FitPar(3) = true; % Mark parameter for fitting
            %   AllParams.Min(2) = 200;     % Set lower bound
            %   AllParams.Max(2) = 500;     % Set upper bound
            %   Model.setAllParamStruct(AllParams);  

            % Validate input structure
            if ~isstruct(AllParamsStruct) || ~isfield(AllParamsStruct, 'Values') || ~isfield(AllParamsStruct, 'FitPar')
                error('CompositeFun:setAllParamsStruct:InvalidInput', 'Input must be structure with Values and FitPar fields');
            end

            NumAllParams = Obj.numAllParam();

            % Validate sizes
            if length(AllParamsStruct.Values) ~= NumAllParams
                error('CompositeFun:setAllParamsStruct:ValuesSizeMismatch', ...
                      'Values has %d elements but %d expected', length(AllParamsStruct.Values), NumAllParams);
            end
            if length(AllParamsStruct.FitPar) ~= NumAllParams
                error('CompositeFun:setAllParamsStruct:FitParSizeMismatch', ...
                      'FitPar has %d elements but %d expected', length(AllParamsStruct.FitPar), NumAllParams);
            end

            % Validate bounds if provided
            UpdateBounds = false;
            if isfield(AllParamsStruct, 'Min') && isfield(AllParamsStruct, 'Max')
                if length(AllParamsStruct.Min) ~= NumAllParams
                    error('CompositeFun:setAllParamsStruct:MinSizeMismatch', ...
                          'Min has %d elements but %d expected', length(AllParamsStruct.Min), NumAllParams);
                end
                if length(AllParamsStruct.Max) ~= NumAllParams
                    error('CompositeFun:setAllParamsStruct:MaxSizeMismatch', ...
                          'Max has %d elements but %d expected', length(AllParamsStruct.Max), NumAllParams);
                end
                UpdateBounds = true;
            end

            % Update all functions with new values, FitPar flags, and bounds
            for Ifun = 1:numel(Obj.Funs)
                % Vectorized assignment for values and FitPar
                AllIndices = Obj.Funs(Ifun).ArgMapping;
                Obj.Funs(Ifun).Par = AllParamsStruct.Values(AllIndices);
                Obj.Funs(Ifun).FitPar = AllParamsStruct.FitPar(AllIndices);

                % Update bounds in ArgNames if provided (still need loop for structure access)
                if UpdateBounds && ~isempty(Obj.Funs(Ifun).ArgNames)
                    for Ipar = 1:min(length(Obj.Funs(Ifun).Par), length(Obj.Funs(Ifun).ArgNames))
                        AllIndex = AllIndices(Ipar);
                        Obj.Funs(Ifun).ArgNames(Ipar).Min = AllParamsStruct.Min(AllIndex);
                        Obj.Funs(Ifun).ArgNames(Ipar).Max = AllParamsStruct.Max(AllIndex);
                    end
                end
            end

            % Clear any pre-calculated values since parameters may have changed
            if ~isempty(Obj.Funs)
                [Obj.Funs.PreCalc] = deal([]);  % Vectorized assignment
            end
        end

    end

    methods % low level utilities

        function checkParamConsistency(Obj)
            % Check for parameter value inconsistencies across functions and NaN fixed parameters
            % Input  : - Obj - CompositeFun object.
            % Output : - None (throws error if inconsistencies found, shows reminder for NaN fixed params).
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.checkParamConsistency();  % Optional validation before calculations

            if isempty(Obj.Funs)
                return;  % Nothing to check
            end

            % Build mapping of global parameter index to {function, local parameter index, value}
            GlobalParamMap = containers.Map('KeyType', 'int32', 'ValueType', 'any');

            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    ParamValue = Obj.Funs(Ifun).Par(Ipar);
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                    else
                        ParamName = sprintf('Param_%d', GlobalIndex);
                    end

                    if isKey(GlobalParamMap, GlobalIndex)
                        % Parameter already seen - check for consistency
                        ExistingEntries = GlobalParamMap(GlobalIndex);

                        % Check if any existing value differs from current value
                        for Ientry = 1:length(ExistingEntries)
                            ExistingValue = ExistingEntries{Ientry}{3};
                            % Check for NaN inconsistencies or numerical differences
                            if isnan(ParamValue) ~= isnan(ExistingValue) || ...
                               (~isnan(ParamValue) && ~isnan(ExistingValue) && abs(ParamValue - ExistingValue) > 1e-12)
                                % Found inconsistency - display detailed information using fprintf
                                fprintf('\n=== PARAMETER INCONSISTENCY DETECTED ===\n');
                                fprintf('Global parameter %d (%s) has different values:\n', GlobalIndex, ParamName);

                                % Display existing conflicting values
                                for Jentry = 1:length(ExistingEntries)
                                    ExistFun = ExistingEntries{Jentry}{1};
                                    ExistVal = ExistingEntries{Jentry}{3};
                                    if isnan(ExistVal)
                                        fprintf('  Function %d (%s): NaN\n', ExistFun, Obj.Funs(ExistFun).Desc);
                                    else
                                        fprintf('  Function %d (%s): %.6g\n', ExistFun, Obj.Funs(ExistFun).Desc, ExistVal);
                                    end
                                end

                                % Display current conflicting value
                                if isnan(ParamValue)
                                    fprintf('  Function %d (%s): NaN\n', Ifun, Obj.Funs(Ifun).Desc);
                                else
                                    fprintf('  Function %d (%s): %.6g\n', Ifun, Obj.Funs(Ifun).Desc, ParamValue);
                                end

                                % Display recipe to fix - prefer non-NaN value
                                SuggestedValue = ExistingValue;
                                if isnan(ExistingValue) && ~isnan(ParamValue)
                                    SuggestedValue = ParamValue;
                                end

                                fprintf('\nRecipe to fix:\n');
                                fprintf('  AllParams = Model.getAllParamStruct();\n');
                                if isnan(SuggestedValue)
                                    fprintf('  AllParams.Values(%d) = 45;  %% Set meaningful value (example)\n', GlobalIndex);
                                else
                                    fprintf('  AllParams.Values(%d) = %.6g;  %% Set consistent value\n', GlobalIndex, SuggestedValue);
                                end
                                fprintf('  Model.setAllParamStruct(AllParams);\n');
                                fprintf('==========================================\n\n');

                                % Throw a concise error
                                error('CompositeFun:checkParamConsistency:Inconsistency', ...
                                      'Parameter inconsistency detected for "%s". See details above.', ParamName);
                            end
                        end

                        % Add current entry to existing list
                        NewEntry = {Ifun, Ipar, ParamValue};
                        ExistingEntries = [ExistingEntries, {NewEntry}];  % Concatenate instead of dynamic indexing
                        GlobalParamMap(GlobalIndex) = ExistingEntries;
                    else
                        % First time seeing this global parameter
                        GlobalParamMap(GlobalIndex) = {{Ifun, Ipar, ParamValue}};
                    end
                end
            end

            % Check for NaN fixed parameters (informational warning, not error)
            % Use cell array to avoid size-changing warnings
            NaNFixedParamsCell = {};
            for Ifun = 1:numel(Obj.Funs)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    if ~Obj.Funs(Ifun).FitPar(Ipar) && isnan(Obj.Funs(Ifun).Par(Ipar))
                        GlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                        if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                            ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                        else
                            ParamName = sprintf('Param_%d', GlobalIndex);
                        end
                        NewEntry = {GlobalIndex, Ifun, Ipar, ParamName, Obj.Funs(Ifun).Desc};
                        NaNFixedParamsCell = [NaNFixedParamsCell, {NewEntry}];
                    end
                end
            end

            % Convert to matrix if there are any NaN fixed params
            if ~isempty(NaNFixedParamsCell)
                NaNFixedParams = zeros(length(NaNFixedParamsCell), 3);
                NaNFixedParamsNames = cell(length(NaNFixedParamsCell), 2);
                for i = 1:length(NaNFixedParamsCell)
                    NaNFixedParams(i,:) = [NaNFixedParamsCell{i}{1:3}];
                    NaNFixedParamsNames{i,1} = NaNFixedParamsCell{i}{4};
                    NaNFixedParamsNames{i,2} = NaNFixedParamsCell{i}{5};
                end
            else
                NaNFixedParams = [];
                NaNFixedParamsNames = {};
            end

            if ~isempty(NaNFixedParams)
                fprintf('\n=== REMINDER: NaN FIXED PARAMETERS DETECTED ===\n');
                fprintf('The following fixed parameters (FitPar=false) have NaN values:\n');
                for i = 1:size(NaNFixedParams, 1)
                    GlobalIndex = NaNFixedParams(i,1);
                    FunIndex = NaNFixedParams(i,2);
                    ParamName = NaNFixedParamsNames{i,1};
                    FunDesc = NaNFixedParamsNames{i,2};
                    fprintf('  Global parameter %d (%s) in function %d (%s)\n', GlobalIndex, ParamName, FunIndex, FunDesc);
                end

                fprintf('\nFixed parameters with NaN values cannot be used in calculations.\n');
                fprintf('Recipe to set values:\n');
                fprintf('  AllParams = Model.getAllParamStruct();\n');

                % Show unique global indices to avoid duplicate settings
                UniqueGlobalIndices = unique(NaNFixedParams(:,1));
                for i = 1:length(UniqueGlobalIndices)
                    GlobalIndex = UniqueGlobalIndices(i);
                    % Find corresponding parameter name
                    ParamRow = find(NaNFixedParams(:,1) == GlobalIndex, 1);
                    ParamName = NaNFixedParamsNames{ParamRow,1};
                    fprintf('  AllParams.Values(%d) = 45;  %% Set meaningful value for %s (example)\n', GlobalIndex, ParamName);
                end

                fprintf('  Model.setAllParamStruct(AllParams);\n');
                fprintf('===============================================\n\n');
            end

            % If we reach here, no inconsistencies found
        end

        function ArgNames = extractArgFuns(~, FunctionHandle)
            % Auto-extract ArgNames structure from function handle
            % Input  : - ~ - CompositeFun object (not used, static-like method).
            %          - FunctionHandle - Function handle to transmission function that supports GetArgNames flag.
            % Output : - ArgNames - Structure array with Name, Description, Min, Max fields.
            % Author : D. Kovaleva (Oct 2025)
            % Example: ArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission);
            
            try
                % Call function with GetArgNames flag to get parameter info
                ArgNames = FunctionHandle('GetArgNames', true);

                if isempty(ArgNames) || ~isstruct(ArgNames)
                    error('Function did not return valid ArgNames structure');
                end

                % Validate structure has required fields
                RequiredFields = {'Name', 'Description', 'Min', 'Max'};
                for i = 1:length(RequiredFields)
                    if ~isfield(ArgNames, RequiredFields{i})
                        error('ArgNames missing required field: %s', RequiredFields{i});
                    end
                end

            catch ME
                error('CompositeFun:extractArgFuns:Failed', ...
                      'Cannot extract ArgNames from function handle: %s', ME.message);
            end
        end

    end

    methods % utilities

        function addFun(Obj, Desc, Handle, ArgNames, varargin)
            % Add a function component to Funs
            % Input  : - Obj - CompositeFun object.
            %          - Desc - Description string (obligatory).
            %          - Handle - Function handle (obligatory).
            %          - ArgNames - Argument names structure array (optional) for ParamMatrix elements.
            %                     For each ParamMatrix element: .Name - consecutive number in ParamMatrix,
            %                                                 .Description - parameter name,
            %                                                 .Min - lower bound,
            %                                                 .Max - upper bound.
            %                     If not submitted, ArgNames will be uploaded from the function.
            %          * ...,key,val,...
            %            'Par' - Parameter values for ParamMatrix (default from Handle).
            %                   Vector corresponding to one row of transmission function's ParamMatrix.
            %            'FitPar' - Logical vector for fitting ParamMatrix elements (default all false).
            %            'OptionalArgs' - Cell array for transmission function's optional arguments.
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Oct 2025)
            % Example: Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], 'Par', [45, 300]);
            %          ArgNames = Model.extractArgFuns(@astro.transmission.ozoneTransmission); % Explicit generation

            % Check obligatory inputs
            if nargin < 3
                error('CompositeFun:addFun:MissingInputs', 'Desc and Handle are obligatory');
            end

            % Auto-extract ArgNames if not provided
            if nargin < 4 || isempty(ArgNames)
                ArgNames = extractArgFuns(Obj, Handle);
            end

            % Parse optional key-value pairs
            Par = [];
            FitPar = logical([]);
            OptionalArgs = {};

            for I = 1:2:length(varargin)
                if I+1 <= length(varargin)
                    switch varargin{I}
                        case 'Par'
                            Par = varargin{I+1};
                        case 'FitPar'
                            FitPar = varargin{I+1};
                        case 'OptionalArgs'
                            OptionalArgs = varargin{I+1};
                    end
                end
            end

            % Handle empty Par - parameters can be provided later
            if isempty(Par)
                % Get number of parameters from ArgNames to set up structure
                NumParams = length(ArgNames);
                Par = NaN(1, NumParams);    % Initialize with NaN as placeholder
            end

            % Set FitPar default if not provided
            if isempty(FitPar)
                FitPar = false(size(Par));  % Already returns logical
            end

            % Validate sizes
            if length(Par) ~= length(FitPar)
                error('CompositeFun:addFun:SizeMismatch', 'Par and FitPar must have the same length');
            end

            % Create an entry for new function 
            NewFun = struct();

            % Name is the function number in order of adding
            if isempty(Obj.Funs)
                NewFun.Name = 1;
            else
                NewFun.Name = numel(Obj.Funs) + 1;
            end

            NewFun.Desc = Desc;
            NewFun.Handle = Handle;
            NewFun.Par = Par(:)';
            NewFun.FitPar = FitPar(:);  
            NewFun.OptionalArgs = OptionalArgs;
            NewFun.ArgNames = ArgNames;  % Store ArgNames structure directly
            NewFun.ArgMapping = [];  % Will be filled by argMapping
            NewFun.PreCalc = [];  % Placeholder - will come from function cache if available

            % Add to Funs array 
            if isempty(Obj.Funs)
                Obj.Funs = NewFun;
            else
                Obj.Funs(end+1) = NewFun;
            end

            % Update ArgMapping for the new function
            argMapping(Obj);
        end

        function argMapping(Obj)
            % Map parameters of the last added function to global parameter indices
            % Input  : - Obj - CompositeFun object.
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Oct 2025)

            if isempty(Obj.Funs)
                return;
            end

            % Process the last added function
            CurrentFun = Obj.Funs(end);
            NumParams = length(CurrentFun.Par);

            % Build current global parameter name list from existing functions
            % Find maximum global index first to pre-allocate
            MaxGlobalIndex = 0;
            for Ifun = 1:(numel(Obj.Funs)-1)
                if ~isempty(Obj.Funs(Ifun).ArgMapping)
                    MaxGlobalIndex = max(MaxGlobalIndex, max(Obj.Funs(Ifun).ArgMapping));
                end
            end

            % Pre-allocate GlobalParamNames
            GlobalParamNames = cell(1, MaxGlobalIndex);
            NextGlobalIndex = MaxGlobalIndex + 1;

            % First, process all existing functions (except the last one) to build current state
            for Ifun = 1:(numel(Obj.Funs)-1)
                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                    ExistingGlobalIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                    % Build parameter name from ArgNames
                    if ~isempty(Obj.Funs(Ifun).ArgNames) && length(Obj.Funs(Ifun).ArgNames) >= Ipar
                        ParamName = Obj.Funs(Ifun).ArgNames(Ipar).Description;
                        GlobalParamNames{ExistingGlobalIndex} = ParamName;
                    end
                end
            end

            % Now process the last function's parameters
            for Ipar = 1:NumParams
                % Validate ArgNames
                if isempty(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:MissingArgNames', 'ArgNames is empty for function %s', CurrentFun.Name);
                end
                if Ipar > length(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:ArgNamesMismatch', 'ArgNames has %d elements but Par has %d elements for function %s', ...
                          length(CurrentFun.ArgNames), NumParams, CurrentFun.Name);
                end

                % Get parameter name (now stored in Description field)
                ParamName = CurrentFun.ArgNames(Ipar).Description;

                % Check if parameter exists in global list
                ExistingIndex = find(strcmp(GlobalParamNames, ParamName), 1);

                if isempty(ExistingIndex)
                    % New parameter, assign next available index
                    GlobalIndex = NextGlobalIndex;
                    NextGlobalIndex = NextGlobalIndex + 1;
                else
                    % Parameter exists, use existing index
                    GlobalIndex = ExistingIndex;
                end

                % Store mapping for this function
                Obj.Funs(end).ArgMapping(Ipar) = GlobalIndex;
            end
        end

        function preCalc(Obj, X)
            % Pre-calculate function values for functions with all fixed parameters
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths).
            % Output : - None (modifies object in-place - handle class).
            % Author : D. Kovaleva (Oct 2025)

            if nargin < 2 || isempty(X)
                return;
            end

            Nfun = numel(Obj.Funs);
            for Ifun = 1:Nfun
                if all(~Obj.Funs(Ifun).FitPar)
                    % Check for NaN parameters before pre-calculation
                    if any(isnan(Obj.Funs(Ifun).Par))
                        error('CompositeFun:preCalc:NaNParameters', ...
                              'Cannot pre-calculate function %d (%s): contains NaN parameter values. Use setAllParamStruct() to set parameter values first.', ...
                              Ifun, Obj.Funs(Ifun).Desc);
                    end

                    % All parameters are fixed - calculate once and store result
                    try
                        % Calculate result once
                        Obj.Funs(Ifun).PreCalc = Obj.Funs(Ifun).Handle(X, Obj.Funs(Ifun).Par, Obj.Funs(Ifun).OptionalArgs{:});

                        % Future calls to evaluate() will use this PreCalc value via 'Return' argument
                        % instead of recalculating

                    catch ME
                        % Configuration error check
                        error('CompositeFun:preCalc:CalculationFailed', ...
                              'Pre-calculation failed for function %d (%s): %s', ...
                              Ifun, Obj.Funs(Ifun).Desc, ME.message);
                    end
                else
                    % Some parameters will be fitted - clear any existing PreCalc
                    Obj.Funs(Ifun).PreCalc = [];
                end
            end
        end
    end

    methods % evaluation
        function Y=evaluateAllParamInput(Obj, X, AllParams)
            % Evaluate the composite function
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths), column vector.
            %          - AllParams - Full parameter matrix (optional).
            %                       If vector: single parameter set.
            %                       If matrix: each row is a parameter set.
            %                       If not provided, uses stored parameter values.
            % Output : - Y - Output values matrix (wavelengths × parameter_sets).
            % Author : D. Kovaleva (Oct 2025)

            if nargin < 3
                AllParams = [];
            end

            % Validate AllParams size if provided
            if ~isempty(AllParams)
                ExpectedSize = Obj.numAllParam();
                if size(AllParams, 2) ~= ExpectedSize
                    error('CompositeFun:evaluate:AllParamsSizeMismatch', ...
                          'AllParams has %d columns but %d expected', ...
                          size(AllParams, 2), ExpectedSize);
                end
                NumParamSets = size(AllParams, 1);
            else
                NumParamSets = 1;
            end

            Nfun = numel(Obj.Funs);

            switch Obj.FunOperator                       % there may be more FunOperator options
                case '*'

                    Y = ones(length(X), NumParamSets);
                    for Ifun=1:1:Nfun
                        % Check if all parameters for this function are fixed and pre-calculated
                        if ~isempty(Obj.Funs(Ifun).PreCalc) && all(~Obj.Funs(Ifun).FitPar)
                            % Use pre-calculated values - replicate for all parameter sets
                            Y = Y .* repmat(Obj.Funs(Ifun).PreCalc(:), 1, NumParamSets);
                        else
                            % Build parameter matrix for this function
                            if ~isempty(AllParams)
                                % Extract parameters for this function from AllParams matrix
                                ParMatrix = zeros(NumParamSets, length(Obj.Funs(Ifun).Par));
                                for Ipar = 1:length(Obj.Funs(Ifun).Par)
                                    AllIndex = Obj.Funs(Ifun).ArgMapping(Ipar);
                                    ParMatrix(:, Ipar) = AllParams(:, AllIndex);
                                end
                            else
                                % Use stored parameter values - single parameter set
                                ParMatrix = Obj.Funs(Ifun).Par(:)';
                            end

                            % Check for NaN parameters
                            if any(isnan(ParMatrix(:)))
                                error('CompositeFun:evaluate:NaNParameters', ...
                                      'Cannot evaluate: some parameters contain NaN values. Use setAllParamsStruct() to set parameter values first.');
                            end

                            FunResult = Obj.Funs(Ifun).Handle(X, ParMatrix, Obj.Funs(Ifun).OptionalArgs{:});
                            Y = Y .* FunResult;
                        end
                    end
                otherwise
                    error('Composite function Operator: %s - not supported yet',Obj.FunOperator);
            end

        end

        function Y=evaluate(Obj, X, FittedParams)
            % Evaluate the composite function using only fitted parameters
            % Input  : - Obj - CompositeFun object.
            %          - X - Input values (e.g., wavelengths), column vector.
            %          - FittedParams - Fitted parameter matrix only.
            %                          If vector: single parameter set.
            %                          If matrix: each row is a parameter set.
            %                          Fixed parameters are taken from stored Obj.Funs.Par values.
            % Output : - Y - Output values matrix (wavelengths × parameter_sets).
            % Author : D. Kovaleva (Oct 2025)

            % Validate FittedParams size
            NumFittedParams = Obj.numFittedParam();
            if size(FittedParams, 2) ~= NumFittedParams
                error('CompositeFun:evaluate:FittedParamsSizeMismatch', ...
                      'FittedParams has %d columns but %d fitted parameters expected', ...
                      size(FittedParams, 2), NumFittedParams);
            end

            NumParamSets = size(FittedParams, 1);
            Nfun = numel(Obj.Funs);

            % Track fitted parameter usage across all functions
            FittedParamIndex = 0;

            switch Obj.FunOperator
                case '*'
                    Y = ones(length(X), NumParamSets);
                    for Ifun=1:1:Nfun
                        % Check if all parameters for this function are fixed and pre-calculated
                        if ~isempty(Obj.Funs(Ifun).PreCalc) && all(~Obj.Funs(Ifun).FitPar)
                            % Use pre-calculated values - replicate for all parameter sets
                            Y = Y .* repmat(Obj.Funs(Ifun).PreCalc(:), 1, NumParamSets);
                        else
                            % Build parameter matrix for this function
                            NumFunParams = length(Obj.Funs(Ifun).Par);
                            ParMatrix = zeros(NumParamSets, NumFunParams);

                            for Ipar = 1:NumFunParams
                                if Obj.Funs(Ifun).FitPar(Ipar)
                                    % This is a fitted parameter - take from FittedParams input
                                    FittedParamIndex = FittedParamIndex + 1;
                                    ParMatrix(:, Ipar) = FittedParams(:, FittedParamIndex);
                                else
                                    % This is a fixed parameter - take from stored Obj.Funs.Par
                                    FixedValue = Obj.Funs(Ifun).Par(Ipar);
                                    if isnan(FixedValue)
                                        error('CompositeFun:evaluate:NaNFixedParameter', ...
                                              'Fixed parameter %d in function %d (%s) has NaN value. Use setAllParamStruct() to set fixed parameters first.', ...
                                              Ipar, Ifun, Obj.Funs(Ifun).Name);
                                    end
                                    ParMatrix(:, Ipar) = FixedValue;
                                end
                            end

                            FunResult = Obj.Funs(Ifun).Handle(X, ParMatrix, Obj.Funs(Ifun).OptionalArgs{:});
                            Y = Y .* FunResult;
                        end
                    end
                otherwise
                    error('Composite function Operator: %s - not supported yet',Obj.FunOperator);
            end
        end

    end

end