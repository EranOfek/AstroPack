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
        ApiClient               % MissionClient instance
        UserName                % Current user
        MainApp                 %
        
        PlanType                % HCS, LCS, AllSS, DDT, TOO 
        Planner                 % ultrasat.planner.uplanner
        Modified = false;       % True after data is being modified        
    end
    

    methods
        function obj = MainModule()
            % Constructor
            disp('app.MainModule');
            obj.ApiClient = ultrasat.api.MissionClient();
            obj.ApiClient.ApiUrl = 'http://localhost:8215';
        end


        function Result = login(obj, UserName, Password)
            obj.UserName = [];
            Result = obj.MainModule.ApiClient.login(UserName, Password);
            if Result
                obj.UserName = UserName;
            end
        end


        function Result = logout(obj)
            obj.UserName = [];
            Result = obj.MainModule.ApiClient.logout(obj.UserName);
            if Result
                obj.UserName = [];
            end
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


