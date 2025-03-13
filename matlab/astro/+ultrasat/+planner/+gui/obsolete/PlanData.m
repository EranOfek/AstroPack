%==========================================================================
% ULTRASAT Planner
%
% File:   +planner/+gui/PlanData.m
% Author:  Chen Tishler
% Created: 20/01/2025
% Updated: 20/01/2025
% Title:   
%==========================================================================

classdef PlanData < handle
    % This class serves as DataModule in Delphi.
    
    properties
        UserName                % Current user
        Pk                      % Primary key
        CreateTime              %
        UpdateTime              %
        Metadata                % 

        Planner                 % ultrasat.planner.uplanner, it contains all targets

        % Status
        StatusText              % Status text for display        
        CurrentStatus           % 'OK', 'Error', 'Warning'

        BuildStatus             %
        SelfConsistencyStatus   %
        ValidateStatus          %
        ValidateStatusText      %
        SubmitStatus            %

        %
        Modified = false;       % True after data is being modified        
        DebugPath               %
    end
    

    methods
        function obj = PlanData()
            % Constructor
            obj.DebugPath = 'C:/Temp/_planner';
        end

        % =================================================================

        function setModified(obj)
            obj.Modified = true;
        end

        function clearModified(obj)
            obj.Modified = true;
        end

        % =================================================================
        %
        % =================================================================        

        function msglog(obj, msg)
            % Log message to console % file @Todo
            timestamp = datestr(now, 'yyyy-mm-dd HH:MM:SS');
            msg = sprintf('%s - %s', timestamp, msg);
            fprintf('%s\n', msg);
        end

        function msgex(obj, s, ME)
            % Log exception with message
            obj.msglog(sprintf('Exception: %s - %s', s, ME.message));
        end      
       
    end
end
