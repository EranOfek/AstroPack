%==========================================================================
% ULTRASAT 
%
% File:   PlanData.m
% Author: Chen Tishler
% Created: 17/02/2025
% Updated: 17/02/2025
%
%==========================================================================

classdef PlanData < handle
    properties
        pk = []                % Primary key
        id = ''                % Unique plan ID
        created_by = ''        % User who created the plan
        plan_info = struct()   % JSON structure for plan details
        targets = struct()     % JSON structure for targets
        planner = []           % MATLAB object (binary data) - instance of ultrasat.uplanner 
        create_time = []       % Timestamp for creation
        update_time = []       % Timestamp for last update
        status = ''            % Plan status
        metadata = struct()    % JSON structure for metadata
        history = struct()     % JSON structure for history
        deleted = false        % Soft delete flag
    end

    
    methods
        function obj = PlanData(data)
            % Constructor for PlanData
            if nargin > 0
                obj.fromStruct(data);
            end
        end


        function fromStruct(obj, data)
            % Sets the properties from a provided struct
            fields = fieldnames(data);
            for i = 1:numel(fields)
                if isprop(obj, fields{i})
                    obj.(fields{i}) = data.(fields{i});
                end
            end
        end


        function data = toStruct(obj)
            % Converts the object back to a struct
            data = struct(...
                'pk', obj.pk, ...
                'id', obj.id, ...
                'created_by', obj.created_by, ...
                'plan_info', obj.plan_info, ...
                'targets', obj.targets, ...
                'planner', obj.planner, ...
                'create_time', obj.create_time, ...
                'update_time', obj.update_time, ...
                'status', obj.status, ...
                'metadata', obj.metadata, ...
                'history', obj.history, ...
                'deleted', obj.deleted ...
            );
        end


        function addHistory(obj, message)
            % Adds a new entry to the history with the current timestamp.
            newHistoryEntry = struct(...
                'timestamp', datestr(now, 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
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


        function display(obj)
            % Display method to show the object contents
            disp(obj.toStruct());
        end
    end
end
