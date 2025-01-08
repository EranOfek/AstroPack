
classdef VirtualTimeModels < api.ModelFactoryBase
    % VirtualTimeModels - Factory for creating VirtualTime model instances.
    
    methods (Static)
        function model = StartParams(id, factor, base)
            % StartParams - Creates an instance of ModelBase for StartParams.
            if nargin < 1 || isempty(id), id = 'vtm1'; end
            if nargin < 2 || isempty(factor), factor = 1; end
            %if nargin < 3 || isempty(base), base = posixtime(datetime('now')); end
            
            % Ensure 'base' is formatted as an ISO 8601 datetime string
            if isa(base, 'datetime')
                base = datestr(base, 'yyyy-mm-ddTHH:MM:SS.FFFZ'); % Convert datetime to ISO 8601
            end

            % Define the data for the model
            data = struct('id', id, 'factor', factor, 'base', base);
            
            % Create and return the model instance
            model = api.ModelBase(data);
        end
        
        function model = PauseParams(id)
            % PauseParams - Creates an instance of ModelBase for PauseParams.
            if nargin < 1 || isempty(id), id = 'vtm1'; end
            
            % Define the data for the model
            data = struct('id', id);
            
            % Create and return the model instance
            model = api.ModelBase(data);
        end
    end
end
