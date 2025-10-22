classdef CompositeFun < Component
    % CompositeFun - Composite function framework for transmission calculations
    % This class provides a unified interface for combining multiple transmission
    % functions (ozone, aerosol, Rayleigh, etc.) with parameter mapping and optimization support.
    %
    % Usage Example - Basic Setup:
    %   % Create composite function
    %   Model = astro.transmission.CompositeFun();
    %
    %   % Adding functions
    %   % Method 1: Auto-generate ArgNames (recommended)
    %   Model.addFun('Aerosol scattering', @astro.transmission.aerosolTransmission, [], 'Par', [], 'FitPar', [false, true, false]);
    %   Model.addFun('Ozone absorption', @astro.transmission.ozoneTransmission, [], 'Par', [45, 300], 'FitPar', [false, false]);
    %
    %   % Method 2: Explicit generateArgNames helper function
    %   OzoneArgNames = Model.generateArgNames(@astro.transmission.ozoneTransmission);
    %   Model.addFun('Ozone absorption', @astro.transmission.ozoneTransmission, OzoneArgNames, ...
    %                'Par', [45, 300], 'FitPar', [false, true]);
    %
    %   % Method 3: Manual ArgNames construction
    %   AerosolArgNames = struct('Name', {1, 2, 3}, ...
    %                           'Description', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
    %                           'Min', {0, 0.01, 0.5}, 'Max', {90, 0.5, 2.0});
    %   Model.addFun('Aerosol scattering', @astro.transmission.aerosolTransmission, AerosolArgNames, ...
    %                'Par', [30, 0.05, 1.2], 'FitPar', [true, false, false]);
    %
    %   % Method 4: Direct function call for ArgNames
    %   OzoneArgNames = astro.transmission.ozoneTransmission('GetArgNames', true);
    %
    %   % After functions are added, can also get ArgNames from model
    %   ArgNames1 = Model.Funs(1).ArgNames;     % Same as OzoneArgNames
    %   ArgNames2 = Model.Funs(2).ArgNames;     % Same as AerosolArgNames
    %
    % Usage Example - Information Getters:
    %   % Get function summary
    %   FunList = Model.getFunctionList();
    %   fprintf('Added %d functions\n', length(FunList));
    %
    %   % Get parameter information
    %   fprintf('Total parameters: %d\n', Model.getNumAllParam());
    %   fprintf('Fitted parameters: %d\n', Model.getNumFittedParam());
    %
    %   AllNames = Model.getAllParamNames();
    %   AllValues = Model.getAllParamValues();
    %   FittedNames = Model.getFittedParamNames();
    %   FittedInfo = Model.getFittedParamInfo();
    %
    % Usage Example - Dynamic Parameter Management:
    %   % Get current parameter structure
    %   AllParams = Model.getAllParamStruct();
    %
    %   % Modify parameters and fit flags for optimization
    %   AllParams.Values(2) = 350;      % Change ozone value
    %   AllParams.FitPar(1) = false;    % Fix zenith angle
    %   AllParams.FitPar(3) = true;     % Fit aerosol parameter
    %   Model = Model.setAllParamStruct(AllParams);
    %
    % Usage Example - Pre-calculation and Evaluation:
    %   % Define wavelength range
    %   Lambda = linspace(300, 1100, 401)';
    %
    %   % Pre-calculate functions with fixed parameters (after setting fit flags)
    %   Model.preCalc(Lambda);
    %
    %   % Evaluate with stored parameter values
    %   Transmission = Model.evaluate(Lambda);
    %
    %   % Evaluate with new parameter values (overrides stored values)
    %   NewAllValues = [45, 280, 0.08, 0.6];  % All parameters
    %   Transmission = Model.evaluate(Lambda, NewAllValues);
    %
    % % Usage Example - Optimization Integration:
    % %   % Get current parameter structure with bounds
    % %   CurrentParams = Model.getAllParamsStruct();
    % %  FittedIndices = find(CurrentParams.FitPar);
    % %  InitialGuess = CurrentParams.Values(FittedIndices);
    % %  LowerBounds = CurrentParams.Min(FittedIndices);
    % %  UpperBounds = CurrentParams.Max(FittedIndices);
    % %
    % %  % Define cost function for optimization
    % %  CostFunction = @(fittedVals) optimizationCost(Model, Lambda, ...
    % %                                  CurrentParams, FittedIndices, fittedVals, MeasuredData);
    % %
    % %  % Run optimization with bounds (minimizer adjusts only fitted parameters)
    % %  OptimalFitted = fmincon(CostFunction, InitialGuess, [], [], [], [], ...
    % %                         LowerBounds, UpperBounds);
    %
    % Methods:
    %   Constructor: CompositeFun() - Create composite function object
    %   addFun() - Add transmission function with parameters
    %   preCalc() - Pre-calculate functions with fixed parameters
    %   evaluate() - Evaluate composite function with AllParams vector
    %
    % Setters:
    %   setAllParamStruct() - Update AllParams values and fit flags dynamically
    %
    % Getters:
    %   getNumAllParam() - Total parameters (fitted + fixed)
    %   getAllParamNames() - Names of all parameters
    %   getAllParamValues() - Values of all parameters
    %   getNumFittedParam() - Count of fitted parameters only
    %   getFittedParamNames() - Names of fitted parameters only
    %   getFunctionList() - Summary of added functions
    %   getFittedParamInfo() - Comprehensive fitted parameter details
    %   getAllParamStruct() - Get AllParams structure with Values and FitPar
    %
    % See also: astro.transmission.ozoneTransmission, astro.transmission.aerosolTransmission,
    %           astro.transmission.rayleighTransmission, astro.transmission.loadAbsorptionInterpolants

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
        GlobalParamIndex = 1        % Next available global parameter index
        GlobalParamNames = {}       % List of global parameter names
    end


    methods % Constructor
        function Obj = CompositeFun()
            % Constructor for CompositeFun
            % Input  : None
            % Output : - CompositeFun object.
            % Example: Model = astro.transmission.CompositeFun();

            % Initialize properties
            Obj.FunOperator = '*';
            Obj.GlobalParamIndex = 1;
            Obj.GlobalParamNames = {};
            Obj.Funs = [];
        end
    end

    methods % setter/getters

        % All parameters (fitted + fixed)
        function NumParams = getNumAllParam(Obj)
            % Get total number of all global parameters (fitted + fixed)
            % Output : - NumParams - Number of all global parameters.
            NumParams = Obj.GlobalParamIndex - 1;
        end

        function ParamNames = getAllParamNames(Obj)
            % Get list of all global parameter names (fitted + fixed)
            % Output : - ParamNames - Cell array of all parameter names.
            ParamNames = Obj.GlobalParamNames;
        end

        function ParamValues = getAllParamValues(Obj)
            % Get current parameter values for all parameters (fitted + fixed)
            % Output : - ParamValues - Column vector of all parameter values.
            AllParams = getAllParamStruct(Obj);
            ParamValues = AllParams.Values;
        end

        % Fitted parameters only
        function NumFittedParams = getNumFittedParam(Obj)
            % Get total number of fitted parameters only
            % Output : - NumFittedParams - Number of parameters marked for fitting.
            if isempty(Obj.Funs)
                NumFittedParams = 0;
                return;
            end

            % Vectorized approach: sum all FitPar arrays at once
            NumFittedParams = sum(arrayfun(@(f) sum(f.FitPar), Obj.Funs));
        end

        function FittedNames = getFittedParamNames(Obj)
            % Get list of fitted parameter names only
            % Output : - FittedNames - Cell array of fitted parameter names.

            NumAllParams = Obj.GlobalParamIndex - 1;
            if NumAllParams == 0
                FittedNames = {};
                return;
            end

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

            % Extract fitted parameter names directly
            FittedNames = Obj.GlobalParamNames(IsFitted);
        end

        % Function information
        function FunList = getFunctionList(Obj)
            % Get summary of added functions
            % Output : - FunList - Structure array with Name, Desc, NumParams, NumFittedParams.
            if isempty(Obj.Funs)
                FunList = [];
                return;
            end

            FunList = struct('Name', {}, 'Desc', {}, 'NumParams', {}, 'NumFittedParams', {});
            for Ifun = 1:numel(Obj.Funs)
                FunList(Ifun).Name = Obj.Funs(Ifun).Name;
                FunList(Ifun).Desc = Obj.Funs(Ifun).Desc;
                FunList(Ifun).NumParams = length(Obj.Funs(Ifun).Par);
                FunList(Ifun).NumFittedParams = sum(Obj.Funs(Ifun).FitPar);
            end
        end

        function FittedInfo = getFittedParamInfo(Obj)
            % Get comprehensive information about fitted parameters
            % Output : - FittedInfo - Structure with TotalFitted, FittedNames, FunctionMapping.
            FittedInfo.TotalFitted = getNumFittedParam(Obj);
            FittedInfo.FittedNames = getFittedParamNames(Obj);

            % Map which global parameter indices each function uses for fitted params
            FittedInfo.FunctionMapping = {};
            for Ifun = 1:numel(Obj.Funs)
                FittedIndices = Obj.Funs(Ifun).ArgMapping(Obj.Funs(Ifun).FitPar);
                FittedInfo.FunctionMapping{Ifun} = FittedIndices(:)';  % Row vector
            end
        end

        function AllParamsStruct = getAllParamStruct(Obj)
            % Get complete parameter structure for optimization
            % Output : - AllParamsStruct - Structure with Names, Values, FitPar, Min, Max.
            %
            % Example usage:
            %   AllParams = Model.getAllParamStruct();
            %   % View bounds for optimization
            %   fprintf('Parameter bounds:\n');
            %   for i = 1:length(AllParams.Names)
            %       fprintf('  %s: [%.2f, %.2f]\n', AllParams.Names{i}, AllParams.Min(i), AllParams.Max(i));
            %   end
            %   % Modify parameter values and fit flags as needed
            %   AllParams.Values(2) = 350;  % Change parameter value
            %   AllParams.FitPar(3) = true; % Mark parameter for fitting
            %   % Update the model
            %   Model.setAllParamStruct(AllParams);

            AllParamsStruct = struct();

            % Initialize arrays
            NumAllParams = Obj.GlobalParamIndex - 1;
            AllParamsStruct.Names = Obj.GlobalParamNames;
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

        function Obj = setAllParamStruct(Obj, AllParamsStruct)
            % Update all parameter values, FitPar flags, and bounds if provided
            % Input  : - self.
            %          - AllParamsStruct - Structure with Values and FitPar fields.
            %                            Values: vector of parameter values
            %                            FitPar: logical vector of fit flags
            %                            Min: (optional) vector of lower bounds
            %                            Max: (optional) vector of upper bounds
            % Output : - Updated object.
            %
            % Example usage:
            %   AllParams = Model.getAllParamStruct();
            %   AllParams.Values(2) = 350;  % Change parameter value
            %   AllParams.FitPar(3) = true; % Mark parameter for fitting
            %   AllParams.Min(2) = 200;     % Set lower bound
            %   AllParams.Max(2) = 500;     % Set upper bound
            %   Model = Model.setAllParamStruct(AllParams);

            % Validate input structure
            if ~isstruct(AllParamsStruct) || ~isfield(AllParamsStruct, 'Values') || ~isfield(AllParamsStruct, 'FitPar')
                error('CompositeFun:setAllParamsStruct:InvalidInput', 'Input must be structure with Values and FitPar fields');
            end

            NumAllParams = Obj.GlobalParamIndex - 1;

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

        function ArgNames = generateArgNames(~, FunctionHandle)
            % Auto-generate ArgNames structure from function handle
            % Calls the transmission function with 'GetArgNames', true flag to obtain parameter information.
            % Input  : - self (not used, static-like method).
            %          - FunctionHandle - Function handle to transmission function that supports GetArgNames flag.
            % Output : - ArgNames - Structure array with Name, Description, Min, Max fields.
            %
            % Example usage:
            %   ArgNames = Model.generateArgNames(@astro.transmission.ozoneTransmission);
            %   ArgNames = Model.generateArgNames(@astro.transmission.aerosolTransmission);
            %   ArgNames = Model.generateArgNames(@astro.transmission.rayleighTransmission);

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
                error('CompositeFun:generateArgNames:Failed', ...
                      'Cannot generate ArgNames from function handle: %s', ME.message);
            end
        end

    end

    methods % utilities

        function Obj=addFun(Obj, Desc, Handle, ArgNames, varargin)
            % Add a function component to Funs
            % Input  : - self.
            %          - Desc - Description string (obligatory).
            %          - Handle - Function handle (obligatory).
            %          * ...,key,val,... Optional arguments:
            %          - ArgNames - Argument names structure array (optional) for ParamMatrix elements:
            %            For each ParamMatrix element: .Name - consecutive number in ParamMatrix,
            %                                        .Description - parameter name,
            %                                        .Min - lower bound,
            %                                        .Max - upper bound.
            %            If not submitted, ArgNames will be uploaded from the function. 
            %            'Par' - Parameter values for ParamMatrix (default from Handle).
            %                   Vector corresponding to one row of transmission function's ParamMatrix.
            %            'FitPar' - Logical vector for fitting ParamMatrix elements (default all false).
            %            'OptionalArgs' - Cell array for transmission function's optional arguments.
            % Output : - Updated object.
            % Example: Model.addFun('Ozone', @astro.transmission.ozoneTransmission, [], 'Par', [45, 300]); % Auto-generate ArgNames
            %          Model.addFun('Aerosol', @astro.transmission.aerosolTransmission, ArgInfo, 'Par', []);
            %          ArgNames = Model.generateArgNames(@astro.transmission.ozoneTransmission); % Explicit generation

            % Check obligatory inputs
            if nargin < 3
                error('CompositeFun:addFun:MissingInputs', 'Desc and Handle are obligatory');
            end

            % Auto-generate ArgNames if not provided
            if nargin < 4 || isempty(ArgNames)
                ArgNames = generateArgNames(Obj, Handle);
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
            Obj = argMapping(Obj);
        end

        function Obj=argMapping(Obj)
            % Map parameters of the last added function to global parameter list
            % Input  : - self.
            % Output : - Updated object with ArgMapping populated for last function.

            if isempty(Obj.Funs)
                return;
            end

            % Process the last added function
            CurrentFun = Obj.Funs(end);
            NumParams = length(CurrentFun.Par);

            for Ipar = 1:NumParams
                % Validate ArgNames
                if isempty(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:MissingArgNames', 'ArgNames is empty for function %d', CurrentFun.Name);
                end
                if Ipar > length(CurrentFun.ArgNames)
                    error('CompositeFun:argMapping:ArgNamesMismatch', 'ArgNames has %d elements but Par has %d elements for function %d', ...
                          length(CurrentFun.ArgNames), NumParams, CurrentFun.Name);
                end

                % Get parameter name (now stored in Description field)
                ParamName = CurrentFun.ArgNames(Ipar).Description;

                % Check if parameter exists in global list
                ExistingIndex = find(strcmp(Obj.GlobalParamNames, ParamName), 1);

                if isempty(ExistingIndex)
                    % New parameter, add to global list
                    Obj.GlobalParamNames{end+1} = ParamName;
                    GlobalIndex = Obj.GlobalParamIndex;
                    Obj.GlobalParamIndex = Obj.GlobalParamIndex + 1;
                else
                    % Parameter exists, use existing index
                    GlobalIndex = ExistingIndex;
                end

                % Store mapping for this function
                Obj.Funs(end).ArgMapping(Ipar) = GlobalIndex;
            end
        end

        function Obj=preCalc(Obj, X)
            % Pre-calculate function values for functions with all fixed parameters
            % Uses only arguments-based caching (not persistent cache)
            % Input  : - self.
            %          - X - Input values (e.g., wavelengths).
            % Output : - Updated object with PreCalc populated.

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
        function Y=evaluate(Obj, X, AllParams)
            % Evaluate the composite function
            % Input  : - self.
            %          - X - Input values (e.g., wavelengths), column vector.
            %          - AllParams - Full parameter matrix (optional).
            %                       If vector: single parameter set.
            %                       If matrix: each row is a parameter set.
            %                       If not provided, uses stored parameter values.
            % Output : - Y - Output values matrix (wavelengths × parameter_sets).

            if nargin < 3
                AllParams = [];
            end

            % Validate AllParams size if provided
            if ~isempty(AllParams)
                ExpectedSize = Obj.GlobalParamIndex - 1;
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

    end

end