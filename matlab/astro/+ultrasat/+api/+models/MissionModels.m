%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionModels.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Factory for creating Mission data models instances.
%==========================================================================

classdef MissionModels < ultrasat.api.utils.ModelFactoryBase
    % MissionModels - Factory for creating Mission data models instances.
    % This class contain builder functions that return instancs

    methods (Static)

        % -------------------------------------------------------------------

        function model = newImagingTarget()
            % ImagingTarget - Creates an instance of ModelBase with empty fields.

            % Define the data for the model with all fields set to []
            data = struct(...
                'pk', [], ...
                'target_id', [], ...
                'target_type', [], ...
				'target_subtype', [], ...
				'plan_id', [], ...
                'status', [], ...
                'update_time', [], ...
                'ra', [], ...
                'decl', [], ...
                'roll', [], ...
                'start_time', [], ...
                'end_time', [], ...
                'exposure', [], ...
                'image_count', [], ...
                'tiles', [], ...
                'total_seconds', [], ...
                'history', [], ...
                'metadata', [] ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end

        % -----------------------------------------------------------------

        function planStruct = newEmptyPlanStruct()
            % Creates a new empty struct for PlanData with all fields initialized.
			% @Todo - Seems that this not used ??? (reviewed 08/06/2025)
            planStruct = struct(...
                'title', '', ...            % Target name/title
                'ra', [], ...               % Right Ascension
                'decl', [], ...             % Declination
                'roll', [], ...             % Expected Roll
                'start_time', '', ...       % Start time in ISO format
                'end_time', '', ...         % End time in ISO format
                'exposure', [], ...         % Exposure time in seconds
                'image_count', [], ...      % Number of exposures
                'total_seconds', [], ...    % Total duration in seconds
                'tiles', '' ...             % Tiles as comma-separated string
            );
        end

    end
end
