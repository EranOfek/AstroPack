%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.PlanData.m
% Author      : Chen Tishler
% Created     : 17/02/2025
% Updated     : 21/09/2025
% Description : Data class for plan data.
%==========================================================================

classdef PlanData < handle
    properties
        % The properties here are mapped to Postgres table columns
        pk = []                % Primary key
        id = ''                % Unique plan ID
        plan_kind = 'imaging'  % Always 'imaging' for Observation Planner
        plan_type = ''         % Plan type (LCS, etc.)
        ast_planner = ''       %
        title = ''             % Title of the plan
        status = ''            % Plan status
        created_by = ''        % User who created the plan
        create_time = []       % Timestamp for creation
        update_time = []       % Timestamp for last update
        start_time = []        % Start time of the plan
        end_time = []          % End time of the plan
        allow_edit = true      % Allow edit or false for read-only
        deleted = false        % Soft delete flag

        metadata = struct()    % Metadata, created by newMetadata() below
        history = struct()     % Array of struct, see addHistory() below
        targets = struct()     % Array of struct, created by uplanner.planTable2struct()
        planner = []           % MATLAB object (binary data) - instance of ultrasat.uplanner
    end


    methods (Static)
        function obj = fromStruct(data)
            % Create new class instance from struct
            obj = ultrasat.api.utils.ModelUtils.struct2class(data, 'ultrasat.api.models.PlanData');
        end


        function obj = fromJson(js)
            % Create new class instance from JSON text
            s = ultrasat.api.utils.JsonUtils.json2struct(js);
            obj = ultrasat.api.models.PlanData.fromStruct(s);
        end
    end


    methods
        function obj = PlanData()
            obj.create_time = ultrasat.api.utils.DateTimeUtils.nowUtc();
			obj.update_time = obj.create_time;
            obj.metadata = obj.newMetadata();
        end


        function metadata = newMetadata(obj)
            % Create new Metadata struct
            metadata = struct(...
                'SelfConsitencyStatus', obj.newStatusData(), ...
                'BuildStatus', obj.newStatusData(), ...
                'ValidationStatus', obj.newStatusData(), ...
                'SubmitStatus', obj.newStatusData(), ...
                'ValidationResponse', [] ...
            );
        end


        function data = toStruct(obj)
            % Converts the object back to a struct
            data = ultrasat.api.utils.ModelUtils.class2struct(obj);
        end


        function js = toJson(obj)
            % Converts the object back to a struct
            js = ultrasat.api.utils.ModelUtils.class2json(obj);
        end


        % MATLAB cannot have array with single struct item, the
        % only solution is to convert the array to cellarray
        %if numel(data.targets) == 1
        %    data.targets = {data.targets};
        %end


        function data = newStatusData(obj, Status)
            %
            if nargin < 2
                Status = [];
            end

            % Define the data for the model with all fields set to []
            data = struct(...
                'Status', Status, ...   % 'OK', 'Error', 'Warning'
                'StartTime', [], ...    % Operation start time (i.e. validation start time)
                'UpdateTime', [], ...   % Status update time
                'ShortStatus', [], ...  % Short status plain text
                'Text', [], ...         % Detailed status as plain text
                'Html', [] ...          % Detailed status as HTML
            );
        end


        function obj = setStatus(obj, fieldName, Status, Args)
            % Updates a status struct with new values
            % - Status (Required): New status ('OK', 'Error', etc.)
            % - Args.ShortStatus (Optional): Short description
            % - Args.Text (Optional): Detailed plain text message
            % - Args.Html (Optional): Detailed HTML message
            % - UpdateTime is always set to nowUtc()
            % - If StartTime is empty, it is set to UpdateTime

            arguments
                obj
                fieldName
                Status
                Args.ShortStatus = []
                Args.Text = []
                Args.Html = []
            end

            % Ensure the field exists in obj.metadata
            if ~isfield(obj.metadata, fieldName)
                error('Field "%s" does not exist in obj.metadata', fieldName);
            end

            % Directly modify obj.metadata.<fieldName>
            obj.metadata.(fieldName).Status = Status;

            % Assign optional fields if provided
            if ~isempty(Args.ShortStatus), obj.metadata.(fieldName).ShortStatus = Args.ShortStatus; end
            if ~isempty(Args.Text), obj.metadata.(fieldName).Text = Args.Text; end
            if ~isempty(Args.Html), obj.metadata.(fieldName).Html = Args.Html; end

            % Set UpdateTime to current UTC time
            obj.metadata.(fieldName).UpdateTime = ultrasat.api.utils.DateTimeUtils.nowUtc();

            % If StartTime is empty, set it to UpdateTime
            if isempty(obj.metadata.(fieldName).StartTime)
                obj.metadata.(fieldName).StartTime = obj.metadata.(fieldName).UpdateTime;
            end
        end


        function clearStatus(obj)
            % Clear all status fields
            obj.metadata.SelfConsitencyStatus = obj.newStatusData();
            obj.metadata.BuildStatus = obj.newStatusData();
            obj.metadata.ValidationStatus = obj.newStatusData();
            obj.metadata.SubmitStatus = obj.newStatusData();
        end


        function addHistory(obj, message)
            % Adds a new entry to the history with the current timestamp.
            newHistoryEntry = struct(...
                'timestamp', ultrasat.api.utils.DateTimeUtils.nowUtc(), ... % datestr(now, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                'message', message ...
            );

            if isempty(fieldnames(obj.history)) || (isstruct(obj.history) && isempty(obj.history))
                % Handle empty history struct array
                obj.history = newHistoryEntry;
            elseif isstruct(obj.history)
                obj.history(end+1) = newHistoryEntry;
            else
                error('Invalid history format.');
            end
        end

    end
end
