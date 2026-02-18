%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : ultrasat.api.SimpleFileClient.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 21/09/2025
% Description : Client for uploading and downloading files from a simple file server.
%               Used by ApiSimProvider, see simple_file_server.py in Ultrasat repository.
%==========================================================================

classdef SimpleFileClient < ultrasat.api.core.Loggable
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


        function result = healthCheck(obj)
            % Check the health of the server.
            %   healthCheck = obj.healthCheck()
            %   Returns true on success, false on error.
            arguments
                obj
            end
            result = false;
            try
                response = obj.performPostRequest('api/files/health', {});
                if isfield(response, 'status') && strcmp(response.status, 'ok')
                    result = true;
                end
            catch ME
                obj.msglog(sprintf('Error checking health of server: %s', ME.message));
            end
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

            endpoint = 'api/files/list';
            payload.path = obj.safePath([obj.BasePath, folderPath]);
            if ~isempty(masks)
                payload.masks = masks;
            end

            try
                response = obj.performPostRequest(endpoint, payload);
                if isfield(response, 'files') && ~isempty(response.files)
                    % response.files will be a cell array, convert to string array
                    fileList = response.files;
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

            endpoint = 'api/files/read';
            payload.path = obj.safePath([obj.BasePath, filePath]);

            try
                % Use performPostRequest (it handles URL + JSON)
                resp = obj.performPostRequest(endpoint, payload);

                % Extract content from response (depending on FastAPI return type)
                if isstruct(resp) && isfield(resp, "data")
                    content = resp.data;   % assumes FastAPI returns {"data": "..."}
                else
                    content = char(resp);  % fallback if raw string
                end
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

            endpoint = 'api/files/write';
            payload.path = obj.safePath([obj.BasePath, filePath]);
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
                jsonStr = jsonencode(data, 'PrettyPrint', true);
                success = obj.writeFile(filePath, jsonStr);
            catch ME
                obj.msglog(sprintf('Error encoding data for file %s: %s', filePath, ME.message));
                success = false;
            end
        end


        function binaryData = readBinaryFile(obj, relativeFilePath)
            % Reads a binary file from the remote server by requesting it as Base64.
            % Returns the decoded file content as a uint8 row vector.
            arguments
                obj
                relativeFilePath (1,:) char
            end

            binaryData = uint8.empty(1,0); % Default empty response
            endpoint = 'api/files/read';

            % Create a payload telling the server we want the file as Base64
            payload.path = obj.safePath([obj.BasePath, relativeFilePath]);
            payload.encoding = 'base64';

            try
                % The server should return a JSON struct: {"data": "base64_string"}
                response = obj.performPostRequest(endpoint, payload);

                if isfield(response, 'data') && ~isempty(response.data)
                    % Use MATLAB's built-in Base64 decoder to convert the string
                    % back to raw bytes (uint8 array).
                    binaryData = matlab.net.base64decode(response.data);
                else
                    obj.msglog('Server response for binary file did not contain a "data" field.');
                end
            catch ME
                obj.msglog(sprintf('Error reading binary file "%s": %s', relativeFilePath, ME.message));
            end
        end


        function success = writeBinaryFile(obj, relativeFilePath, binaryData)
            % Writes binary data (uint8 array) to the remote server by sending it as Base64.
            arguments
                obj
                relativeFilePath (1,:) char
                binaryData (1,:) uint8 % Ensure data is a uint8 row vector
            end

            success = false; % Default to failure
            endpoint = 'api/files/write';

            try
                % Use MATLAB's built-in Base64 encoder to convert the raw bytes
                % into a string that can be safely sent in a JSON payload.
                base64String = matlab.net.base64encode(binaryData);

                % Create the payload, including the data and the encoding flag
                payload.path = obj.safePath([obj.BasePath, relativeFilePath]);
                payload.data = base64String;
                payload.encoding = 'base64';

                % Send the request. We only care about success, not the response body.
                obj.performPostRequest(endpoint, payload);
                success = true;
            catch ME
                obj.msglog(sprintf('Error writing binary file "%s": %s', relativeFilePath, ME.message));
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

            endpoint = 'api/files/next_available_file';
            payload.path = obj.safePath([obj.BasePath, folderPath]);
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


        function result = deleteFile(obj, filePath)
            % Delete a file from the server.
            %   result = obj.deleteFile(filePath)
            %   Returns true on success, false on error.
            arguments
                obj
                filePath char
            end

            % Delete a file from the server
            endpoint = 'api/files/delete';
            payload.path = obj.safePath([obj.BasePath, filePath]);
            try
                result = obj.performPostRequest(endpoint, payload);
                result = strcmp(result.status, 'ok');
            catch ME
                obj.msglog(sprintf('Error deleting file %s: %s', filePath, ME.message));
                result = false;
            end
        end

    end


    methods (Access = public)
        function fullUrl = getFullUrl(obj, endpoint)
            % Construct the full URL for a given endpoint.
            baseUrl = obj.BaseUrl;
            if ~endsWith(baseUrl, '/')
                baseUrl = [baseUrl, '/'];
            end
            fullUrl = [baseUrl, endpoint];
        end

        % =================================================================

        function response = performPostRequest(obj, endpoint, payload)
            % performPostRequest Send a JSON POST request with robust logging.
            %
            %   response = performPostRequest(obj, endpoint, payload)
            %
            %   - Encodes the payload to JSON and sends it to the specified endpoint.
            %   - Logs the request and response using obj.msglog without dumping large data.
            %   - Truncates long strings and summarizes structs, arrays, and other types.
            %   - Catches and logs all errors (including logging errors), never throws.
            %
            % Input:
            %   endpoint - relative URL to send the POST to
            %   payload  - struct, string, or data to JSON-encode
            %
            % Output:
            %   response - decoded response body (usually struct, char, or string)

            response = [];
            try
                fullUrl = obj.getFullUrl(endpoint);

                % --- Request preview (robust) ---
                try
                    reqPreview = obj.previewDataForLog(payload);
                catch innerME
                    reqPreview = sprintf('[request preview error: %s]', innerME.message);
                end
                obj.msglog(sprintf('performPostRequest: POST %s -> %s | Request preview: %s', ...
                    endpoint, fullUrl, reqPreview));

                % --- Build and send request ---
                jsonPayload = jsonencode(payload);
                headers = matlab.net.http.HeaderField('Content-Type', 'application/json');
                body = matlab.net.http.io.StringProvider(jsonPayload);
                req = matlab.net.http.RequestMessage('post', headers, body);

                resp = req.send(fullUrl);

                % --- Response preview (robust) ---
                try
                    respPreview = obj.previewDataForLog(resp.Body.Data);
                catch innerME
                    respPreview = sprintf('[response preview error: %s]', innerME.message);
                end

                obj.msglog(sprintf('performPostRequest: Status: %s', string(resp.StatusCode)));
                obj.msglog(sprintf('performPostRequest: Response preview: %s', respPreview));

                response = resp.Body.Data;

            catch ME
                obj.msglog(sprintf('performPostRequest: ERROR endpoint=%s | %s', endpoint, ME.message));
            end
        end


        function response = performPostRequest0(obj, endpoint, payload)
            % A helper function for making JSON POST requests.

            fullUrl = obj.getFullUrl(endpoint);
            contentTypeField = matlab.net.http.HeaderField('Content-Type', 'application/json');
            jsonPayload = jsonencode(payload);
            body = matlab.net.http.io.StringProvider(jsonPayload);

            req = matlab.net.http.RequestMessage('post', contentTypeField, body);

            % Send request
            resp = req.send(fullUrl);

            % Display response
            disp(resp.Body.Data);
            response = resp.Body.Data;
        end

    end


    methods (Access = private)
        function previewStr = previewDataForLog(obj, data)
            % previewDataForLog Create a short, safe preview string for logging.

            maxLen = 200; % max preview length

            try
                if ischar(data) || isstring(data)
                    strData = char(data);
                    if length(strData) > maxLen
                        previewStr = sprintf('%s ... [truncated %d chars]', ...
                            strData(1:maxLen), length(strData) - maxLen);
                    else
                        previewStr = strData;
                    end

                elseif isnumeric(data)
                    previewStr = sprintf('[numeric array %dx%d]', size(data,1), size(data,2));

                elseif isstruct(data)
                    fieldsList = strjoin(fieldnames(data), ', ');
                    previewStr = sprintf('[struct with fields: %s]', fieldsList);

                elseif iscell(data)
                    previewStr = sprintf('[cell array %dx%d]', size(data,1), size(data,2));

                elseif isempty(data)
                    previewStr = '[empty]';

                else
                    previewStr = sprintf('[%s]', class(data));
                end

            catch ME
                previewStr = sprintf('[preview error: %s]', ME.message);
            end
        end
    end


    methods (Static, Access = private)
        function safe_path = safePath(path)
            % Convert Windows-style backslashes to URL-friendly forward slashes.
            safe_path = replace(path, '\', '/');
        end
    end
end
