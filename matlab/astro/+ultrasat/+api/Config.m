%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.Config.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Configuration class for the API.
%========================================================================== 

classdef Config < handle
    %LOGGABLE A base class that provides logging to the console and a file.
    %   Classes that inherit from Loggable gain access to the msglog method,
    %   which handles timestamping, formatting, and writing log entries to
    %   both the MATLAB command window and a central log file.

     methods (Access = public)
        function obj = Config()      
        end
     end
     

     methods (Static)

        function config = getApiConfig()
            % Get the API configuration from the global singleton configuration object
            % To be used with config/UltrasatPlanner.yaml
            config = Configuration.getSingleton().Data.UltrasatPlanner.PlannerApi;
        end

    end

end
