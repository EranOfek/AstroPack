classdef MissionClient < soc.api.ClientBase
    % VirtualTimeClient - Derived class for virtual time management.
    
    properties
        
    end
    
    methods
        function obj = MissionClient(apiUrl, apiKey, timeout)
            % Constructor 
            obj@soc.api.ClientBase(apiUrl, apiKey, timeout);

        end        
        
        function response = getApprovedTargets(obj, start_time, end_time)
            % Start the virtual time manager simulation.
            if nargin < 2
                factor = 1;
            end
            if nargin < 3
                base = [];  % Default to empty if not provided
            end
            params = soc.api.MissionModels.GetApprovedTargetsParams(start_time, end_time);
            response = obj.postRequest('/get_approved_targets/', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end

    end
end
