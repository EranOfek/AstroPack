classdef VirtualTimeClient < ultrasat.api.ClientBase
    % VirtualTimeClient - Derived class for virtual time management.
    
    properties
        ID = 'vtm1';  % Identifier for the Virtual Time Manager
    end
    
    methods
        function obj = VirtualTimeClient(apiUrl, apiKey, timeout, id)
            % Constructor for VirtualTimeClient
            obj@api.ClientBase(apiUrl, apiKey, timeout);
            if nargin > 3
                obj.ID = id;
            end
        end        
        
        function success = start(obj, factor, base)
            % Start the virtual time manager simulation.
            if nargin < 2
                factor = 1;
            end
            if nargin < 3
                base = [];  % Default to empty if not provided
            end
            params = ultrasat.api.VirtualTimeModels.StartParams(obj.ID, factor, base);
            response = obj.postRequest('/start/', params.Data);
            success = isfield(response, 'ok') && response.ok;
        end
        
        function success = pause(obj)
            % Pause the virtual time manager simulation.
            params = ultrasat.api.VirtualTimeModels.PauseParams(obj.ID);
            response = obj.postRequest('/pause/', params.Data);
            success = isfield(response, 'ok') && response.ok;
        end
        
        function state = getState(obj)
            % Get the current state as JSON text.
            params = struct('id', obj.ID);
            response = obj.postRequest('/state/', params);
            if isfield(response, 'state')
                state = response.state;
            else
                state = '';
            end
        end
    end
end
