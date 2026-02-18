%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.ClientBase.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 06/10/2025
% Description : Base class for interacting with REST API services.
%==========================================================================

classdef ClientBase < ultrasat.api.core.Loggable
    % ClientBase - Base class for interacting with REST API services.
    % https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623
    % This class provides a standardized interface for communicating with
    % FastAPI backend services in the ULTRASAT project. It manages HTTP
    % requests, authentication, and API interactions.
    %
    % Key Features:
    % - Stores API configuration (BaseUrl, ApiKey, Timeout, etc.).
    % - Supports synchronous and asynchronous API requests.
    % - Handles file uploads with additional parameters.
    %
    % This class is intended to be extended by specific API clients that
    % interact with various FastAPI services.

    properties
        BaseUrl             % Base URL of the API
        ApiKey              % API Key for authentication
        Namespace           % Namespace for plans_manager API (optional)
        Timeout = 30;       % Timeout for HTTP requests (seconds)
        LogFileName
    end

    methods
        function obj = ClientBase(Args)
            % Constructor for ClientBase
            %
            % Initializes the API client with optional arguments, using
            % defaults from environment variables if not provided.
            %
            % :param Args.BaseUrl: Base URL of the API (default from ENV).
            % :param Args.SubUrl: Service-specific URL path (default empty).
            % :param Args.ApiKey: API key for authentication (default from ENV).
            % :param Args.Timeout: Timeout in seconds (default 30).
            % :return: An instance of ClientBase.

            arguments
                Args.BaseUrl
                Args.ApiKey
                Args.Namespace
                Args.Timeout
                Args.LogFileName
            end

            % Get default values from environment variables
            if isempty(Args.ApiKey)
                Args.ApiKey = getenv('SOC_API_KEY');
            end
            if isempty(Args.Timeout)
                Args.Timeout = getenv('SOC_API_TIMEOUT');
                if isempty(Args.Timeout)
                    Args.Timeout = 30;
                else
                    Args.Timeout = str2double(Args.Timeout);
                end
            end

            % Get default log file name
            if isempty(LogFileName)
                srcFile = mfilename('fullpath');  srcFolder = fileparts(srcFile);
                obj.LogFileName = fullfile(srcFolder, [mfilename, '.log']);
            else
                obj.LogFileName = LogFileName;
            end

            % Remove trailing slash from BaseUrl if it exists   
            if endsWith(Args.BaseUrl, '/')
                Args.BaseUrl = Args.BaseUrl(1:end-1);
            end

            % Assign properties
            obj.BaseUrl = Args.BaseUrl;
            obj.ApiKey = Args.ApiKey;
            obj.Namespace = Args.Namespace;
            obj.Timeout = Args.Timeout;
        end

        % -----------------------------------------------------------------

        function response = postRequest(obj, endpoint, params)
            % Sends a synchronous POST request to the API
            %
            % :param endpoint: API endpoint path (appended to BaseUrl).
            % :param params: Struct containing request parameters.
            % :return: Response data as a struct.

            import matlab.net.*
            import matlab.net.http.*

            if endpoint(1) ~= '/'
                endpoint = ['/', endpoint];
            end
            url = [obj.BaseUrl, endpoint];

            % Check if params is struct
            if ~isstruct(params)
                error('postRequest:InvalidParams', 'params must be a struct');
            end

            % Remove empty fields and convert to JSON
            cleanedData = ultrasat.api.utils.ModelBase.removeEmptyFields(params);
            jsonData = ultrasat.api.utils.ModelBase.struct2json(cleanedData);
            jsonData = jsondecode(jsonData);

            % Create HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json')
            ];
            if ~isempty(obj.ApiKey)
                headers = [headers, HeaderField('api-key', obj.ApiKey)];
            end
            if ~isempty(obj.Namespace)
                headers = [headers, HeaderField('namespace', char(obj.Namespace))];
            end

            % Create and send the HTTP request
            body = MessageBody(jsonData);
            request = RequestMessage('POST', headers, body);
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            try
                rawResponse = send(request, url, options);

                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    respJson = jsonencode(rawResponse.Body.Data);
                    response = ultrasat.api.utils.ModelBase.fromJson(respJson);  % rawResponse.Body.Data);
                else
                    error('HTTP Error: %s', char(rawResponse.StatusCode));
                end
            catch ME
                switch ME.identifier
                    case 'MATLAB:networklib:ConnectionFailed'
                        error('Connection failed. Check server URL or network.');
                    case 'MATLAB:webservices:Timeout'
                        error('Request timed out after %d seconds.', obj.Timeout);
                    case 'MATLAB:webservices:HTTPErrorStatusCode'
                        error('HTTP error: %s. Check API endpoint or parameters.', ME.message);
                    otherwise
                        rethrow(ME);
                end
            end
        end

        % -----------------------------------------------------------------

        function response = getRequest(obj, endpoint, includeAuth)
            % Sends a synchronous GET request to the API.
            %
            % :param endpoint: API endpoint path (appended to ApiUrl).
            % :param includeAuth: (optional) If true, send api-key and namespace headers; if false, omit them (e.g. for /health). Default true.
            % :return: Response data as a struct (e.g. from GET /health).
            arguments
                obj
                endpoint (1,1) string
                includeAuth (1,1) logical = true
            end
            import matlab.net.*
            import matlab.net.http.*

            endpoint = char(endpoint);
            if endpoint(1) ~= '/'
                endpoint = ['/', endpoint];
            end
            url = [obj.BaseUrl, endpoint];

            headers = [HeaderField('Content-Type', 'application/json')];
            if includeAuth
                if ~isempty(obj.ApiKey)
                    headers = [headers, HeaderField('api-key', obj.ApiKey)];
                end
                if ~isempty(obj.Namespace)
                    headers = [headers, HeaderField('namespace', char(obj.Namespace))];
                end
            end

            request = RequestMessage('GET', headers);
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            try
                rawResponse = send(request, url, options);
                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    if isempty(rawResponse.Body.Data)
                        response = struct();
                    else
                        respJson = jsonencode(rawResponse.Body.Data);
                        response = ultrasat.api.utils.ModelBase.fromJson(respJson);
                    end
                else
                    error('HTTP Error: %s', char(rawResponse.StatusCode));
                end
            catch ME
                switch ME.identifier
                    case 'MATLAB:networklib:ConnectionFailed'
                        error('Connection failed. Check server URL or network.');
                    case 'MATLAB:webservices:Timeout'
                        error('Request timed out after %d seconds.', obj.Timeout);
                    otherwise
                        rethrow(ME);
                end
            end
        end


        function result = healthCheck(obj)
            % Checks the health/status of the API server.
            %
            % Sends a GET request to /health without authentication or namespace headers.
            % Returns:
            %   result - True if the API server is healthy, false otherwise.
            %
            import matlab.net.*
            import matlab.net.http.*

            endpoint = '/health';
            url = [obj.BaseUrl, endpoint];

            headers = [HeaderField('Content-Type', 'application/json')]; % No api-key, no namespace

            request = RequestMessage('GET', headers);
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            try
                rawResponse = send(request, url, options);
                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    if isempty(rawResponse.Body.Data)
                        result = false;
                    else
                        respJson = jsonencode(rawResponse.Body.Data);
                        result = ultrasat.api.utils.ModelBase.fromJson(respJson);
                        result = result.ok;
                    end
                else
                    result = false;
                    error('HTTP Error: %s', char(rawResponse.StatusCode));
                end
            catch ME
                result = false;
                error('Health check failed: %s', ME.message);
            end
        end

        % -----------------------------------------------------------------

        function postRequestAsync(obj, endpoint, params, callback)
            % CURRENTLY UNUSED - Sends an asynchronous POST request to the API
            %
            % :param endpoint: API endpoint path (appended to BaseUrl).
            % :param params: Struct containing request parameters.
            % :param callback: Function handle for response handling.

            import matlab.net.*
            import matlab.net.http.*

            endpoint = char(endpoint);
            if endpoint(1) ~= '/'
                endpoint = ['/', endpoint];
            end
            url = [obj.BaseUrl, endpoint];

            if ~isstruct(params)
                error('postRequest:InvalidParams', 'params must be a struct');
            end

            % Remove empty fields and convert to JSON
            cleanedData = ultrasat.api.utils.ModelBase.removeEmptyFields(params);

            % Create HTTP headers (match postRequest)
            headers = [HeaderField('Content-Type', 'application/json')];
            if ~isempty(obj.ApiKey)
                headers = [headers, HeaderField('api-key', obj.ApiKey)];
            end
            if ~isempty(obj.Namespace)
                headers = [headers, HeaderField('namespace', char(obj.Namespace))];
            end

            body = MessageBody(cleanedData);
            request = RequestMessage('POST', headers, body);

            % Start an asynchronous request using a timer
            t = timer('ExecutionMode', 'singleShot', ...
                      'StartDelay', 0, ...
                      'TimerFcn', @(~,~) asyncSend(request, url, obj.Timeout, callback), ...
                      'StopFcn', @(~,~) delete(timerfind)); % Clean up timer
            start(t);
        end

        % -----------------------------------------------------------------

        function sendFilesWithParams(obj, url, filePaths, params)
            % CURRENTLY UNUSED - Sends a multipart/form-data request with files and parameters.
            %
            % :param url: Target API endpoint URL.
            % :param filePaths: Cell array of file paths to upload.
            % :param params: Struct of additional key-value parameters.

            import matlab.net.*
            import matlab.net.http.*
            import matlab.net.http.io.*

            % Create the MultipartFormProvider
            formProvider = MultipartFormProvider();

            % Attach each file in the array of file paths
            for i = 1:numel(filePaths)
                fieldName = sprintf('file%d', i);
                formProvider.addPart(FormProvider(fieldName, filePaths{i}));
            end

            % Add additional parameters if provided
            if nargin > 2 && ~isempty(params)
                fieldNames = fieldnames(params);
                for i = 1:numel(fieldNames)
                    formProvider.addPart(FormProvider(fieldNames{i}, params.(fieldNames{i})));
                end
            end

            % Create and send the HTTP request
            request = RequestMessage('POST', [], formProvider);
            response = request.send(url);

            % Display response
            obj.msglog(sprintf('Response Status Code: %s', response.StatusCode));
            obj.msglog(sprintf('Response Body: %s', response.Body.Data));
        end

    end

end
