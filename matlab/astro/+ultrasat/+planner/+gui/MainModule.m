%==========================================================================
% ULTRASAT Planner
%
% File:   .m
% Author: Chen Tishler
% Created: 07/01/2025
% Updated: 07/01/2025
%
%==========================================================================

classdef MainModule < handle
    % This class serves as DataModule in Delphi.
    
    properties
        Modified = false;       % True after data is being modified
        UP                      % uplanner instance
        ApiClient               % MissionClient instance
        UserName                % Current user
        MainApp
        

    end
    

    methods
        function obj = MainModule()
            % Constructor
            disp('app.MainModule');
            obj.ApiClient = soc.api.MissionClient();
            obj.ApiClient.ApiUrl = 'http://localhost:8215';
        end

        function setModified(obj)
            obj.Modified = true;
        end

        function clearModified(obj)
            obj.Modified = true;
        end

        function msglog(obj, msg)
            fprintf('%s\n', msg);
        end
    end
end


