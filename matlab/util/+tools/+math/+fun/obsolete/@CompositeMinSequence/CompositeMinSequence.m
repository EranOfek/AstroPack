classdef CompositeMinSequence < handle
    % CompositeMinSequence - Optimization sequence manager for transmission parameter fitting
    % This class manages multi-stage optimization sequences for photometric calibration,
    % coordinating parameter fitting across different stages with configurable minimizers.
    %
    % Example - Basic Usage:
    %   % Create sequence
    %   Seq = tools.math.fun.CompositeMinSequence();
    %
    %   % Add optimization stages (using global parameter indices from CompositeFun)
    %   Seq.addStage('Normalization', [1], 'nonlinear', ...
    %                'SigmaClipping', true, 'SigmaThreshold', 3.0, 'SigmaIterations', 3, ...
    %                'Description', 'Initial normalization with outlier removal');
    %
    %   Seq.addStage('NormAndCenter', [1, 6], 'nonlinear', ...
    %                'SigmaClipping', true, 'SigmaThreshold', 3.0, 'SigmaIterations', 3, ...
    %                'Description', 'Optimize normalization and QE center');
    %
    %   % Get sequence summary
    %   fprintf('Total stages: %d\n', Seq.numStages());
    %   Seq.printSummary();
    %
    % Example - Adapted Sequence (from transmissionFast):
    %   Seq = tools.math.fun.CompositeMinSequence();
    %
    %   % Stage 1: Normalization only
    %   Seq.addStage('NormOnly_Initial', [1], 'nonlinear', ...
    %                'SigmaClipping', true, 'SigmaThreshold', 3.0, 'SigmaIterations', 3);
    %
    %   % Stage 2: Norm + QE center
    %   Seq.addStage('NormAndCenter', [1, 6], 'nonlinear', ...
    %                'SigmaClipping', true, 'SigmaThreshold', 3.0, 'SigmaIterations', 3);
    %
    %   % Stage 3: Field correction (linear)
    %   Seq.addStage('FieldCorrection', [10, 11, 12, 13, 14], 'linear', ...
    %                'SigmaClipping', true, 'SigmaThreshold', 2.0, 'SigmaIterations', 3);
    %
    %   % Stage 4: Normalization refinement
    %   Seq.addStage('Normalization_Refined', [1], 'nonlinear', ...
    %                'SigmaClipping', false);
    %
    %   % Stage 5: Atmospheric parameters
    %   Seq.addStage('Atmospheric', [3, 4], 'nonlinear', ...
    %                'SigmaClipping', false);
    %
    % Properties:
    %   Stages - Array of optimization stage structures
    %
    % Methods:
    %   addStage() - Add optimization stage to sequence
    %   getStage() - Get specific stage by index
    %   numStages() - Get total number of stages
    %   printSummary() - Display sequence summary
    %   toStruct() - Convert to structure array (for serialization)
    %
    % Author: D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.

    properties
        Stages = struct('Name', {}, 'FreeParams', {}, 'Method', {}, ...
                       'SigmaClipping', {}, 'SigmaThreshold', {}, 'SigmaIterations', {}, ...
                       'Description', {})
                       % Name - Stage name (string)
                       % FreeParams - Global parameter indices to optimize (vector)
                       % Method - Optimization method: 'linear' or 'nonlinear' (string)
                       % SigmaClipping - Enable sigma clipping (logical)
                       % SigmaThreshold - Sigma threshold for outlier removal (double)
                       % SigmaIterations - Number of sigma clipping iterations (double)
                       % Description - Human-readable description (string)
    end

    methods % Constructor
        function Obj = CompositeMinSequence()
            % Constructor for CompositeMinSequence
            % Input  : None
            % Output : - CompositeMinSequence object
            % Author : D. Kovaleva (Nov 2025)
            % Example: Seq = tools.math.fun.CompositeMinSequence();

            % Initialize with empty Stages
            Obj.Stages = struct('Name', {}, 'FreeParams', {}, 'Method', {}, ...
                               'SigmaClipping', {}, 'SigmaThreshold', {}, ...
                               'SigmaIterations', {}, 'Description', {});
        end
    end

    methods % Stage management
        function addStage(Obj, Name, FreeParams, Method, Args)
            % Add optimization stage to sequence
            % Input  : - Obj - CompositeMinSequence object
            %          - Name - Stage name (string or char)
            %          - FreeParams - Global parameter indices to optimize (vector)
            %          - Method - Optimization method: 'linear' or 'nonlinear'
            %          * ...,key,val,...
            %            'SigmaClipping' - Enable sigma clipping. Default is false.
            %            'SigmaThreshold' - Sigma threshold for outlier removal. Default is 3.0.
            %            'SigmaIterations' - Number of sigma clipping iterations. Default is 3.
            %            'Description' - Human-readable description. Default is empty string.
            % Output : - None (modifies object in-place - handle class)
            % Author : D. Kovaleva (Nov 2025)
            % Example: Seq.addStage('Normalization', [1], 'nonlinear', ...
            %                       'SigmaClipping', true, 'SigmaThreshold', 3.0);

            arguments
                Obj
                Name
                FreeParams double
                Method {mustBeMember(Method, {'linear', 'nonlinear'})}
                Args.SigmaClipping logical = false
                Args.SigmaThreshold double = 3.0
                Args.SigmaIterations double = 3
                Args.Description string = ""
            end

            % Validate FreeParams
            if isempty(FreeParams)
                error('FreeParams cannot be empty');
            end
            if any(FreeParams <= 0) || any(mod(FreeParams, 1) ~= 0)
                error('FreeParams must be positive integers (global parameter indices)');
            end

            % Convert Name to string
            if ischar(Name)
                Name = string(Name);
            end

            % Create new stage structure
            NewStage = struct();
            NewStage.Name = Name;
            NewStage.FreeParams = FreeParams(:)';  % Ensure row vector
            NewStage.Method = string(Method);
            NewStage.SigmaClipping = Args.SigmaClipping;
            NewStage.SigmaThreshold = Args.SigmaThreshold;
            NewStage.SigmaIterations = Args.SigmaIterations;
            NewStage.Description = Args.Description;

            % Add to Stages array
            if isempty(Obj.Stages)
                Obj.Stages = NewStage;
            else
                Obj.Stages(end+1) = NewStage;
            end
        end

        function Stage = getStage(Obj, StageIdx)
            % Get specific stage by index
            % Input  : - Obj - CompositeMinSequence object
            %          - StageIdx - Stage index (1-based)
            % Output : - Stage - Stage structure
            % Author : D. Kovaleva (Nov 2025)

            if StageIdx < 1 || StageIdx > length(Obj.Stages)
                error('Invalid stage index: %d. Must be 1-%d', StageIdx, length(Obj.Stages));
            end

            Stage = Obj.Stages(StageIdx);
        end

        function N = numStages(Obj)
            % Get total number of stages in sequence
            % Input  : - Obj - CompositeMinSequence object
            % Output : - N - Number of stages
            % Author : D. Kovaleva (Nov 2025)

            N = length(Obj.Stages);
        end

        function printSummary(Obj)
            % Display sequence summary
            % Input  : - Obj - CompositeMinSequence object
            % Output : - None (prints to console)
            % Author : D. Kovaleva (Nov 2025)

            if isempty(Obj.Stages)
                fprintf('Empty optimization sequence (no stages defined)\n');
                return;
            end

            fprintf('=== OPTIMIZATION SEQUENCE SUMMARY ===\n');
            fprintf('Total stages: %d\n\n', length(Obj.Stages));

            for i = 1:length(Obj.Stages)
                Stage = Obj.Stages(i);
                fprintf('Stage %d: %s [%s]\n', i, Stage.Name, Stage.Method);
                fprintf('  Free parameters: [%s]\n', num2str(Stage.FreeParams));

                if Stage.SigmaClipping
                    fprintf('  Sigma clipping: ON (threshold=%.1f, iterations=%d)\n', ...
                            Stage.SigmaThreshold, Stage.SigmaIterations);
                else
                    fprintf('  Sigma clipping: OFF\n');
                end

                if strlength(Stage.Description) > 0
                    fprintf('  Description: %s\n', Stage.Description);
                end
                fprintf('\n');
            end

            fprintf('=====================================\n');
        end

        function StructArray = toStruct(Obj)
            % Convert sequence to structure array
            % Input  : - Obj - CompositeMinSequence object
            % Output : - StructArray - Structure array with all stages
            % Author : D. Kovaleva (Nov 2025)
            % Example: StageArray = Seq.toStruct();

            StructArray = Obj.Stages;
        end
    end

    methods (Static) % Factory methods
        function Obj = fromStruct(StructArray)
            % Create CompositeMinSequence from structure array
            % Input  : - StructArray - Structure array with stage definitions
            % Output : - Obj - CompositeMinSequence object
            % Author : D. Kovaleva (Nov 2025)
            % Example: Seq = tools.math.fun.CompositeMinSequence.fromStruct(StageArray);

            Obj = tools.math.fun.CompositeMinSequence();

            for i = 1:length(StructArray)
                Stage = StructArray(i);

                % Extract fields with defaults for backward compatibility
                if isfield(Stage, 'Name'), Name = Stage.Name; else, Name = sprintf('Stage_%d', i); end
                if isfield(Stage, 'FreeParams'), FreeParams = Stage.FreeParams; else, error('FreeParams required'); end
                if isfield(Stage, 'Method'), Method = Stage.Method; else, Method = 'nonlinear'; end
                if isfield(Stage, 'SigmaClipping'), SigmaClipping = Stage.SigmaClipping; else, SigmaClipping = false; end
                if isfield(Stage, 'SigmaThreshold'), SigmaThreshold = Stage.SigmaThreshold; else, SigmaThreshold = 3.0; end
                if isfield(Stage, 'SigmaIterations'), SigmaIterations = Stage.SigmaIterations; else, SigmaIterations = 3; end
                if isfield(Stage, 'Description'), Description = Stage.Description; else, Description = ""; end

                % Add stage
                Obj.addStage(Name, FreeParams, Method, ...
                            'SigmaClipping', SigmaClipping, ...
                            'SigmaThreshold', SigmaThreshold, ...
                            'SigmaIterations', SigmaIterations, ...
                            'Description', Description);
            end
        end

        function Obj = createAdaptedSequence()
            % Create the Adapted sequence from transmissionFast (5 stages)
            % This is the default recommended sequence for LAST photometric calibration
            % Output : - Obj - CompositeMinSequence object with 5-stage sequence
            % Author : D. Kovaleva (Nov 2025)
            % Example: Seq = tools.math.fun.CompositeMinSequence.createAdaptedSequence();
            %
            % NOTE: This assumes specific global parameter indices:
            %   [1] = Normalization
            %   [3] = Pwv_cm (water vapor)
            %   [4] = Tau_aod500 (aerosol optical depth)
            %   [6] = QE Center wavelength
            %   [10-14] = Field correction parameters (placeholder indices)

            Obj = tools.math.fun.CompositeMinSequence();

            % Stage 1: Normalization only
            Obj.addStage('NormOnly_Initial', [1], 'nonlinear', ...
                        'SigmaClipping', true, ...
                        'SigmaThreshold', 3.0, ...
                        'SigmaIterations', 3, ...
                        'Description', "Initial normalization with outlier removal");

            % Stage 2: Norm + QE center
            Obj.addStage('NormAndCenter', [1, 6], 'nonlinear', ...
                        'SigmaClipping', true, ...
                        'SigmaThreshold', 3.0, ...
                        'SigmaIterations', 3, ...
                        'Description', "Optimize normalization and QE center with outlier removal");

            % Stage 3: Field correction (linear solver)
            % NOTE: Field correction parameter indices are placeholders
            % Actual indices depend on how field correction is added to CompositeFun
            Obj.addStage('FieldCorrection_Python', [10, 11, 12, 13, 14], 'linear', ...
                        'SigmaClipping', true, ...
                        'SigmaThreshold', 2.0, ...
                        'SigmaIterations', 3, ...
                        'Description', "Python-like Chebyshev field corrections (LINEAR SOLVER)");

            % Stage 4: Normalization refinement
            Obj.addStage('Normalization_Refined', [1], 'nonlinear', ...
                        'SigmaClipping', false, ...
                        'Description', "Refine normalization after field corrections");

            % Stage 5: Atmospheric parameters
            Obj.addStage('Atmospheric', [3, 4], 'nonlinear', ...
                        'SigmaClipping', false, ...
                        'Description', "Optimize water vapor and aerosol parameters");

            % Print warning about parameter indices
            warning('CompositeMinSequence:createAdaptedSequence:ParameterIndices', ...
                    ['Adapted sequence uses assumed global parameter indices. ' ...
                     'Verify these match your CompositeFun configuration:\n' ...
                     '  [1] = Normalization\n' ...
                     '  [3] = Pwv_cm\n' ...
                     '  [4] = Tau_aod500\n' ...
                     '  [6] = QE Center\n' ...
                     '  [10-14] = Field correction (placeholder)']);
        end

        function Obj = createSimpleSequence()
            % Create a simple 2-stage sequence (Norm → Atmospheric)
            % Output : - Obj - CompositeMinSequence object with 2-stage sequence
            % Author : D. Kovaleva (Nov 2025)
            % Example: Seq = tools.math.fun.CompositeMinSequence.createSimpleSequence();

            Obj = tools.math.fun.CompositeMinSequence();

            % Stage 1: Normalization
            Obj.addStage('Normalization', [1], 'nonlinear', ...
                        'SigmaClipping', true, ...
                        'SigmaThreshold', 3.0, ...
                        'SigmaIterations', 3, ...
                        'Description', "Initial normalization with outlier removal");

            % Stage 2: Atmospheric parameters
            Obj.addStage('Atmospheric', [3, 4], 'nonlinear', ...
                        'SigmaClipping', false, ...
                        'Description', "Optimize water vapor and aerosol");

            warning('CompositeMinSequence:createSimpleSequence:ParameterIndices', ...
                    ['Simple sequence uses assumed global parameter indices:\n' ...
                     '  [1] = Normalization\n' ...
                     '  [3] = Pwv_cm\n' ...
                     '  [4] = Tau_aod500']);
        end
    end
end
