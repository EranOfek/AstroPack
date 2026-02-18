%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.SkyExposureTrackerClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Client for interacting with the Sky Exposure Tracker API.
%==========================================================================

classdef SkyExposureTrackerClient < ultrasat.api.clients.ClientBase
    % Client for interacting with the Sky Exposure Tracker API
    % This class provides methods to interact with the Sky Exposure Tracker API
    % It is a subclass of ultrasat.api.ClientBase.
    %
    % Typical Usage:
    %   client = ultrasat.api.SkyExposureTrackerClient();
    %   response = client.initTable('mission.sky_exposure_debug_matlab', 1000, 5, 1:10);

    properties
        TableName = '';
    end

    methods
        function obj = SkyExposureTrackerClient(Args)
            arguments
                Args.SubUrl     = '/sky_exposure_tracker';
            end
            ArgsCell = namedargs2cell(Args);
            obj@ultrasat.api.clients.ClientBase(ArgsCell{:});  % Args);  % , 'SubUrl', '/mission');
        end


        function response = initTable(obj, table_name, healpix_rows, healpix_level, healpix_indices)
            % Initialize a Sky Exposure Tracker table.
            % Params:
            %   table_name: Name of the table to initialize
            %   healpix_rows: Number of rows in the table
            %   healpix_level: HEALPix level
            %   healpix_indices: Optional list of HEALPix indices
            params = ultrasat.api.future.SkyExposureTrackerModels.InitTableParams(table_name);  %, healpix_rows, healpix_level, healpix_indices);
            response = obj.postRequest('/init_table', params.Data);
            response.ok = isfield(response, 'success') && response.success;
        end


        function response = update(obj, table_name, healpix_indices, duration, timestamp)
            % Update rows in the Sky Exposure Tracker table.
            % Params:
            %   table_name: Name of the table to update
            %   healpix_indices: List of HEALPix indices to update
            %   duration: Duration to add
            %   timestamp: Timestamp to append
            params = ultrasat.api.future.SkyExposureTrackerModels.UpdateParams(table_name, healpix_indices, duration, timestamp);
            response = obj.postRequest('/update', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end


        function response = select(obj, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
            % Select rows from the Sky Exposure Tracker table.
            % Params:
            %   table_name: Name of the table to select from
            %   healpix_indices: Optional list of HEALPix indices
            %   start_timestamp: Optional start timestamp for filtering
            %   end_timestamp: Optional end timestamp for filtering
            %   select_all: Boolean indicating whether to use select_all
            if nargin < 6
                select_all = false;
            end
            params = ultrasat.api.future.SkyExposureTrackerModels.SelectParams(table_name, healpix_indices, start_timestamp, end_timestamp, select_all);
            response = obj.postRequest('/select', params.Data);
            response.ok = isfield(response, 'status') && strcmp(response.status, 'ok');
        end

    end
end
