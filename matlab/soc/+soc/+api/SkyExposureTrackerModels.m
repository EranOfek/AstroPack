classdef SkyExposureTrackerModels < soc.api.ModelFactoryBase
    % Factory for creating SkyExposureTracker models.

    properties (Constant)
        APPROVED_TARGETS = 'mission.sky_exposure_targets'       % Table for approved sky exposure targets
        RECEIVED_IMAGES  = 'mission.sky_exposure_images'        % Table for received sky exposure images
        PROCESSED_IMAGES = 'mission.sky_exposure_processed'     % Table for processed sky exposure images
    end


    methods (Static)

        function model = InitTableParams(table_name, Args)
            % GetInitTableParams - Creates an instance of ModelBase for table initialization parameters.
            %
            % Positional Parameters:
            %   table_name: Name of the table to initialize.
            %
            % Named Arguments:
            %   Args.healpix_rows: Number of rows in the table (default: []).
            %   Args.healpix_level: HEALPix level defining sky granularity (default: []).
            %   Args.healpix_indices: Optional list of HEALPix indices (default: []).
            %
            % Returns:
            %   A ModelBase instance containing the initialization parameters.
        
            % Define named arguments with defaults
            arguments
                table_name (1, :) char                 % Positional: Table name as a required parameter
                Args.healpix_rows = []                 % Default value is empty
                Args.healpix_level = []                % Default value is empty
                Args.healpix_indices = []              % Default value is empty
            end
        
            % Construct the data structure
            data = struct(...
                'table_name', table_name, ...
                'healpix_rows', Args.healpix_rows, ...
                'healpix_level', Args.healpix_level, ...
                'healpix_indices', Args.healpix_indices ...
            );
        
            % Create and return the model instance
            model = soc.api.ModelBase(data);
        end

        % -------------------------------------------------------------------
        % Update Parameters
        function model = UpdateParams(table_name, healpix_indices, duration, timestamp)
            % GetUpdateParams - Creates an instance of ModelBase for update parameters.
            %
            % Arguments:
            %   table_name: Name of the table to update
            %   healpix_indices: List of HEALPix indices to update
            %   duration: Duration of the exposure in seconds
            %   timestamp: Timestamp of the exposure
            
            if isa(timestamp, 'datetime')
                timestamp = datestr(timestamp, 'yyyy-mm-ddTHH:MM:SS.FFFZ'); % ISO 8601 format
            end
            
            data = struct(...
                'table_name', table_name, ...
                'healpix_indices', healpix_indices, ...
                'duration', duration, ...
                'timestamp', timestamp ...
            );
            model = soc.api.ModelBase(data);
        end

        % -------------------------------------------------------------------
        % Select Parameters
        function model = SelectParams(table_name, healpix_indices, start_timestamp, end_timestamp, select_all, debug_print)
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
            model = soc.api.ModelBase(data);
        end

    end
end
