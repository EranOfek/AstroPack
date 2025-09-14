%==========================================================================
% ULTRASAT 
%
% File:   ultrasat.MissionClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 16/02/2025
%
%==========================================================================

classdef SimpleFileClient < ultrasat.api.Loggable
    % Client for uploading and downloading files from a simple file server.
    %  This class provides methods to interact with a backend server for file
    %  operations like listing, reading, and writing files. It is a MATLAB
    %  conversion of the Delphi TMS WebCore CTWeb.Backend.SimpleFileClient.
    %
    %  To use:
    %    client = SimpleFileClient('http://localhost:5000', 'my_data/');
    %    files = client.listFiles('images/', '*.jpg');
    %    disp(files);

    properties
        % The base URL of the server (e.g., 'http://localhost:5000').
        BaseUrl char

        % An optional base path on the server to prepend to all file paths.
        % It's recommended to use a trailing slash if not empty (e.g., 'data/').
        BasePath char

        % Timeout for web requests in seconds. Default is 30.
        Timeout (1,1) double {mustBeNumeric, mustBePositive} = 30

        % Use synchronous (awaited) requests. In MATLAB, all web requests
        % are synchronous by default, so this property is for API
        % compatibility and does not change behavior.
        UseAwait (1,1) logical = true
    end


    methods (Access = public)
        function obj = SimpleFileClient(baseUrl, basePath)
            % Construct an instance of this class.
            %   obj = SimpleFileClient(baseUrl) creates a client with the specified base URL.
            %   obj = SimpleFileClient(baseUrl, basePath) also specifies a base path on the server.
            arguments
                baseUrl char
                basePath char = ''
            end

            % Initialize the logger
            obj.LogPrefix = 'SimpleFileClient';

            obj.BaseUrl = baseUrl;
            obj.BasePath = basePath;
        end


        function fileList = listFiles(obj, folderPath, masks)
            % List files in a folder on the server.
            %  fileList = obj.listFiles(folderPath) lists all files.
            %  fileList = obj.listFiles(folderPath, masks) lists files matching
            %   the specified masks (e.g., '*.txt,*.json').
            %   Returns a string array of file names or an empty string array on error.
            arguments
                obj
                folderPath char
                masks char = ''
            end

            endpoint = 'files/list';
            payload.path = obj.safePath(obj.BasePath + folderPath);
            if ~isempty(masks)
                payload.masks = masks;
            end

            try
                response = obj.performPostRequest(endpoint, payload);
                if isfield(response, 'files') && ~isempty(response.files)
                    % response.files will be a cell array, convert to string array
                    fileList = char(response.files);
                else
                    fileList = {};
                end
            catch ME
                obj.msglog(sprintf('Error listing files in %s: %s', folderPath, ME.message));
                fileList = {};
            end
        end


        function content = readFile(obj, filePath)
            % Read file content as text from the server.
            %   content = obj.readFile(filePath)
            %   Returns the file content as a string or an empty string on error.
            arguments
                obj
                filePath char
            end

            endpoint = 'files/read';
            payload.path = obj.safePath(obj.BasePath + filePath);

            try
                % For reading raw text, we expect a text response
                options = weboptions('Timeout', obj.Timeout, 'RequestMethod', 'post', ...
                                     'MediaType', 'application/json', 'ContentType', 'text');
                fullUrl = obj.getFullUrl(endpoint);
                jsonPayload = jsonencode(payload);
                content = char(webread(fullUrl, jsonPayload, options));
            catch ME
                obj.msglog(sprintf('Error reading file %s: %s', filePath, ME.message));
                content = '';
            end
        end


        function data = readJson(obj, filePath)
            % Read and parse a JSON file from the server.
            %   data = obj.readJson(filePath)
            %   Returns the parsed JSON as a MATLAB struct or an empty struct on error.
            arguments
                obj
                filePath char
            end

            content = obj.readFile(filePath);
            if ~isempty(content)
                try
                    data = jsondecode(content);
                catch ME
                    obj.msglog(sprintf('Error decoding JSON from file %s: %s', filePath, ME.message));
                    data = struct();
                end
            else
                data = struct();
            end
        end


        function success = writeFile(obj, filePath, data, append)
            % Write text to a file on the server.
            %   success = obj.writeFile(filePath, data) writes/overwrites a file.
            %   success = obj.writeFile(filePath, data, true) appends to the file.
            %   Returns true on success, false on error.
            arguments
                obj
                filePath char
                data char
                append logical = false
            end

            endpoint = 'files/write';
            payload.path = obj.safePath(obj.BasePath + filePath);
            payload.data = data;
            payload.append = append;

            try
                % A successful request with no error is considered success
                obj.performPostRequest(endpoint, payload);
                success = true;
            catch ME
                obj.msglog(sprintf('Error writing to file %s: %s', filePath, ME.message));
                success = false;
            end
        end


        function success = writeJson(obj, filePath, data)
            % Write a MATLAB struct/array to a JSON file on the server.
            %   success = obj.writeJson(filePath, data)
            %   The data is automatically pretty-printed.
            %   Returns true on success, false on error.
            arguments
                obj
                filePath char
                data
            end

            try
                % jsonencode in modern MATLAB versions pretty-prints by default.
                jsonStr = jsonencode(data);
                success = obj.writeFile(filePath, jsonStr);
            catch ME
                obj.msglog(sprintf('Error encoding data for file %s: %s', filePath, ME.message));
                success = false;
            end
        end


        function result = nextAvailableFile(obj, folderPath, mask, zeroPad, minIndex, maxIndex)
            % Get the next available file in a folder.
            %   result = obj.nextAvailableFile(folderPath, mask, zeroPad, minIndex, maxIndex)
            %   Finds the next sequential filename based on a numeric prefix.
            %   Returns a struct with details or an empty struct on error.
            arguments
                obj
                folderPath char
                mask char
                zeroPad (1,1) double {mustBeInteger, mustBeNonnegative}
                minIndex (1,1) double {mustBeInteger, mustBeNonnegative}
                maxIndex (1,1) double {mustBeInteger, mustBePositive}
            end

            endpoint = 'files/next_available_file';
            payload.path = obj.safePath(obj.BasePath + folderPath);
            payload.mask = mask;
            payload.zero_pad = zeroPad;
            payload.min_index = minIndex;
            payload.max_index = maxIndex;

            try
                result = obj.performPostRequest(endpoint, payload);
            catch ME
                obj.msglog(sprintf('Error getting next available file in %s: %s', folderPath, ME.message));
                result = struct();
            end
        end

    end


    methods (Access = private)
        function fullUrl = getFullUrl(obj, endpoint)
            % Construct the full URL for a given endpoint.
            baseUrl = obj.BaseUrl;
            if ~endsWith(baseUrl, '/')
                baseUrl = baseUrl + '/';
            end
            fullUrl = baseUrl + endpoint;
        end


        function response = performPostRequest(obj, endpoint, payload)
            % A helper function for making JSON POST requests.
            fullUrl = obj.getFullUrl(endpoint);
            options = weboptions('Timeout', obj.Timeout, 'RequestMethod', 'post', 'MediaType', 'application/json');
            jsonPayload = jsonencode(payload);
            % webread automatically decodes the JSON response into a MATLAB struct
            response = webread(fullUrl, jsonPayload, options);
        end
    end


    methods (Static, Access = private)
        function safe_path = safePath(path)
            % Convert Windows-style backslashes to URL-friendly forward slashes.
            safe_path = replace(path, '\', '/');
        end
    end
end
