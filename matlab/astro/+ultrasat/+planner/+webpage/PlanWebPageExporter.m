classdef PlanWebPageExporter < ultrasat.planner.webpage.WebPageExporter
    % PlanWebPageExporter - Export structured HTML + images for observation plan
    %
    % This class extends WebPageExporter to provide plan-specific
    % functionality for generating observation plan web pages.
    
    methods
        function obj = PlanWebPageExporter(planId, outputBaseFolder, templatePath, debugMode)
            % Constructor for PlanWebPageExporter
            %
            % Parameters:
            %   planId - Numeric ID of the plan
            %   outputBaseFolder - Base folder for output files
            %   templatePath - Path to HTML template file
            %   debugMode - Optional, enable debug logging (default: false)
            arguments
                planId double
                outputBaseFolder char
                templatePath char
                debugMode logical = false
            end
            
            % Call parent constructor
            obj@ultrasat.planner.webpage.WebPageExporter('plan', planId, outputBaseFolder, templatePath, debugMode);
            
            if obj.DebugMode
                obj.debugLog('Initialized PlanWebPageExporter for plan %d', planId);
            end
        end
    end
end