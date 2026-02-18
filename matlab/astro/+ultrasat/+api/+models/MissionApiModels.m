%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.MissionApiModels.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Factory for creating Mission API calls models instances.
%==========================================================================

classdef MissionApiModels < ultrasat.api.utils.ModelFactoryBase
    % MissionApiModels - Factory for creating Mission API calls models instances.
    % This class

    properties (Constant)
        APPROVED_TARGETS = 'mission.sky_exposure_targets'       % Table for approved sky exposure targets
        RECEIVED_IMAGES  = 'mission.sky_exposure_images'        % Table for received sky exposure images
        PROCESSED_IMAGES = 'mission.sky_exposure_processed'     % Table for processed sky exposure images
    end


    methods (Static)

        function model = LoginParams(UserName, Password)
            % LoginParams - Creates an instance of ModelBase with empty fields.
            data = struct(...
                'user', UserName, ...
                'password', Password ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = GetKeyValueParams(Store, Key, Default)
            % GetKeyValue - Creates an instance of ModelBase with empty fields.
            data = struct(...
                'store', Store, ...
                'key', Key, ...
                'default', Default ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = SetKeyValueParams(Store, Key, Value)
            % GetKeyValue - Creates an instance of ModelBase with empty fields.
            data = struct(...
                'store', Store, ...
                'key', Key, ...
                'value', Value ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = LogoutParams(UserName)
            % LogoutParams - Creates an instance of ModelBase with empty fields.
            data = struct(...
                'user', UserName ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end

        % -------------------------------------------------------------------

        function model = GetApprovedTargetsParams(start_time, end_time)
            % GetApprovedTargetsParams - Creates an instance of ModelBase for approved targets query parameters.

            % Ensure datetime fields are converted to ISO 8601 format
            if nargin >= 1 && isa(start_time, 'datetime')
                start_time = datestr(start_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            end
            if nargin >= 2 && isa(end_time, 'datetime')
                end_time = datestr(end_time, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            end

            % Define the data for the model
            data = struct(...
                'start_time', start_time, ...
                'end_time', end_time ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = ValidateParams(Targets)
            % ValidateParams - Creates an instance of ModelBase for validate request parameters.
            data = struct(...
                'targets', Targets ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = SubmitParams(Plan)
            % SubmitParams - Creates an instance of ModelBase for submit request parameters.
            data = struct(...
                'plan', Plan ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        % -------------------------------------------------------------------

        % Select Parameters
        function model = ExposureParams(table_name, healpix_indices, start_timestamp, end_timestamp, select_all, debug_print)
            % GetSelectParams - Creates an instance of ModelBase for select parameters.
            %
            % Arguments:
            %   table_name: Name of the table to query
            %   healpix_indices: Optional list of HEALPix indices
            %   start_timestamp: Optional start timestamp
            %   end_timestamp: Optional end timestamp
            %   select_all: Boolean for selecting all rows
            %   debug_print: Debugging information

            if nargin < 6
                debug_print = ''; % Default to empty if not provided
            end
            if nargin < 5
                select_all = false; % Default to false if not provided
            end
            if nargin >= 3 && isa(start_timestamp, 'datetime')
                start_timestamp = datestr(start_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            end
            if nargin >= 4 && isa(end_timestamp, 'datetime')
                end_timestamp = datestr(end_timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ');
            end

            data = struct(...
                'table_name', table_name, ...
                'healpix_indices', healpix_indices, ...
                'start_timestamp', start_timestamp, ...
                'end_timestamp', end_timestamp, ...
                'select_all', select_all, ...
                'debug_print', debug_print ...
            );
            model = ultrasat.api.utils.ModelBase(data);
        end

        % -------------------------------------------------------------------

        function model = GetPlansListParams()
            % GetPlansListParams - Creates an instance of ModelBase for get plans list request parameters.
            data = struct(...
                'args', [] ... % Placeholder for additional arguments
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = LoadPlanParams()
            % LoadPlanParams - Creates an instance of ModelBase for load plan request parameters.
            data = struct(...
                'args', [] ... % Placeholder for additional arguments
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = SavePlanParams()
            % SavePlanParams - Creates an instance of ModelBase for save plan request parameters.
            data = struct(...
                'args', [] ... % Placeholder for additional arguments
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = DeletePlanParamsPlanId
            % DeletePlanParams - Creates an instance of ModelBase for delete plan request parameters.
            data = struct(...
                'plan_id', PlanId ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end


        function model = GetPlanStatusParams(PlanId)
            % GetPlanStatusParams - Creates an instance of ModelBase for get plan status request parameters.
            data = struct(...
                'plan_id', PlanId ...
            );

            % Create and return the model instance
            model = ultrasat.api.utils.ModelBase(data);
        end

    end
end
