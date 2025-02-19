%==========================================================================
% ULTRASAT 
%
% File:   api.ClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%
%==========================================================================

classdef ClientBase < handle
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
        BaseUrl         % Base URL of the API
        SubUrl          % Service-specific URL path
        ApiUrl          % Full API endpoint URL
        ApiKey          % API Key for authentication
        Timeout = 30;   % Timeout for HTTP requests (seconds)
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
                Args.BaseUrl    = getenv('SOC_API_BASE');       
                Args.SubUrl     = '';                          
                Args.ApiKey     = getenv('SOC_API_KEY');           
                Args.Timeout    = getenv('SOC_API_TIMEOUT');    
            end
        
            % Assign default timeout if environment variable is invalid
            if isempty(Args.Timeout)
                Args.Timeout = 30; 
            else
                Args.Timeout = str2double(Args.Timeout);
            end
            
            % Assign properties
            obj.BaseUrl = Args.BaseUrl;
            obj.SubUrl = Args.SubUrl;
            obj.ApiKey = Args.ApiKey;
            obj.Timeout = Args.Timeout;
            
            % Ensure SubUrl starts with `/` (but avoid `//`)
            if ~isempty(obj.SubUrl)
                if obj.SubUrl(1) ~= '/'
                    obj.SubUrl = ['/', obj.SubUrl];
                end
            end

            % Construct the full API URL
            obj.ApiUrl = [obj.BaseUrl, obj.SubUrl];

        end

        % -----------------------------------------------------------------

        function response = postRequest(obj, endpoint, params)
            % Sends a synchronous POST request to the API.
            %
            % :param endpoint: API endpoint path (appended to BaseUrl).
            % :param params: Struct containing request parameters.
            % :return: Response data as a struct.
            
            import matlab.net.*
            import matlab.net.http.*
            
            if endpoint(1) ~= '/'
                endpoint = ['/', endpoint];
            end
            url = [obj.ApiUrl, endpoint];

            % Check if params is an instance of ModelBase or derived class
            if isa(params, 'api.ModelBase')                
                params = params.Data;
            elseif ~isstruct(params)
                error('postRequest:InvalidParams', 'params must be a struct or an instance of api.ModelBase.');
            end

            % Remove empty fields and convert to JSON
            cleanedData = api.ModelBase.removeEmptyFields(params);
            jsonData = api.ModelBase.struct2json(cleanedData);
            jsonData = jsondecode(jsonData);
            

            % Create HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json') % Ensure correct header
            ];
            if ~isempty(obj.ApiKey)
                headers = [headers, HeaderField('x-api-key', obj.ApiKey)];
            end

            % Create and send the HTTP request
            body = MessageBody(jsonData);
            request = RequestMessage('POST', headers, body);
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            try
                rawResponse = send(request, url, options);
                
                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    respJson = jsonencode(rawResponse.Body.Data);
                    response = api.ModelBase.fromJson(respJson);  % rawResponse.Body.Data);
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

        function postRequestAsync(obj, endpoint, params, callback)
            % Sends an asynchronous POST request to the API.
            %
            % :param endpoint: API endpoint path (appended to BaseUrl).
            % :param params: Struct containing request parameters.
            % :param callback: Function handle for response handling.

            import matlab.net.*
            import matlab.net.http.*

            url = [obj.ApiUrl, endpoint];

            % Check if params is an instance of ModelBase or derived class
            if isa(params, 'api.ModelBase')                
                params = params.Data;
            elseif ~isstruct(params)
                error('postRequest:InvalidParams', 'params must be a struct or an instance of api.ModelBase.');
            end

            % Remove empty fields and convert to JSON
            cleanedData = soc.api.ModelBase.removeEmptyFields(params);
            %jsonData = soc.api.ModelBase.struct2json(cleanedData);

            % Create HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json'), ...
                HeaderField('x-api-key', obj.ApiKey)
            ];

            % Create the HTTP request
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
            % Sends a multipart/form-data request with files and parameters.
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
        
        % -----------------------------------------------------------------

        function msglog(obj, varargin)
            % Logs a formatted message to the console.
            %
            % :param varargin: Formatted message arguments.
            
            fprintf('Client: ');
            fprintf(varargin{:});
            fprintf('\n');
        end

    end

    % ---------------------------------------------------------------------

    methods (Static)

    end

end
