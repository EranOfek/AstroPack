% ***************************************************************************
% Project     : ULTRASAT Observation Planner
% Filename    : PlanDataUtils.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 23/02/2026
% Description : Utility functions for PlanData and plan conversion operations
% ***************************************************************************

classdef PlanDataUtils
    methods (Static)

        function submitData = convertPlanTableToSubmitData(Plan)
            % Converts the uplanner.Plan table to a list of structs for submission.
            %
            % Parameters:
            %   Plan - MATLAB table containing observation plan data.
            %
            % Returns:
            %   submitData - Array of structs with only the required fields for submission.
            %
            % Notes:
            %   Extracts specific fields needed for the submission API and
            %   performs necessary data type conversions.
            submitData = [];
            for i = 1:height(Plan)
                rowStruct = struct(...
                    'coord_ra', Plan.RA(i), ...
                    'coord_dec', Plan.Dec(i), ...
                    'tiles', Plan.Tiles(i), ...
                    'exposure', seconds(Plan.ExpTime(i)), ...
                    'image_count', Plan.Nexposures(i), ...
                    'start_time', datestr(Plan.Tstart(i), 'yyyy-mm-ddTHH:MM:SS.FFFZ'), ...
                    'jd_start', Plan.JDstart(i), ...
                    'jd_end', Plan.JDend(i), ...
                    'total_duration', seconds(Plan.TotalDuration(i)), ...
                    'slew_time_before', seconds(Plan.SlewTimeBefore(i)) ...
                    );
                submitData = [submitData; rowStruct];
            end
        end


        function Plan = convertPlanTimesToUtc(Plan)
            % Converts start_time and end_time fields of each plan entry to UTC.
            %
            % Parameters:
            %   Plan - Array of structs containing plan data with time fields
            %
            % Returns:
            %   Plan - Same array with time fields converted to UTC format
            %
            % Notes:
            %   Used to ensure consistent time format for API communication
            for i = 1:numel(Plan)
                Plan(i).start_time = ultrasat.api.utils.DateTimeUtils.toUtc(Plan(i).start_time);
                Plan(i).end_time = ultrasat.api.utils.DateTimeUtils.toUtc(Plan(i).end_time);
            end
        end


        function syncFromPlanner(PlanData, planner)
            % Updates PlanData with data from uplanner, including targets list.
            %
            % Parameters:
            %   PlanData - ultrasat.api.models.PlanData instance (modified in place)
            %   planner  - uplanner instance
            %
            % Notes:
            %   Syncs plan_type, ast_planner, title, start/end times, targets,
            %   and status. Handles single-target cell array fix for MATLAB.
            if ~isempty(planner)
                PlanData.plan_type = planner.Type;
                PlanData.ast_planner = planner.AstPlanner;
                PlanData.title = planner.Title;
                PlanData.start_time = planner.StartTime;
                PlanData.end_time = planner.EndTime;
                PlanData.targets = planner.planTable2struct();
                PlanData.status = planner.Status;

                % Convert targets to cell array if it is a single struct, required for proper JSON serialization
                if numel(PlanData.targets) == 1
                    PlanData.targets = {PlanData.targets};
                end
            end
        end

    end
end
