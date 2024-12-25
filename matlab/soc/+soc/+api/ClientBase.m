classdef ClientBase < handle
    % ClientBase - Base class for interacting with REST API services.
    % https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623
    
    properties
        BaseUrl         % Base URL of the API
        SubUrl          % Service-specific URL path
        ApiUrl          % Base URL of the API
        ApiKey          % API Key for authentication
        Timeout = 30;   % Timeout for HTTP requests (seconds)
    end
    

    methods
        function obj = ClientBase(Args)
            % Constructor for ClientBase using arguments block
        
            % Accept input arguments with defaults
            arguments          
                Args.BaseUrl    = getenv('SOC_API_BASE');       % Default BaseUrl from environment or empty if not set
                Args.SubUrl     = '';                           % Default SubUrl is an empty string
                Args.ApiKey     = getenv('SOC_API_KEY');        % Default ApiKey from environment or empty if not set           
                Args.Timeout    = getenv('SOC_API_TIMEOUT');    % Default Timeout from environment or 30 seconds
            end
        
            % Assign default timeout if environment variable is invalid
            if isempty(Args.Timeout)
                Args.Timeout = getenv('SOC_API_TIMEOUT');
            end
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
            
            % Construct the full API URL
            if ~isempty(obj.BaseUrl) && ~isempty(obj.SubUrl)
                obj.ApiUrl = [obj.BaseUrl, obj.SubUrl];
            else
                obj.ApiUrl = obj.BaseUrl; % Use BaseUrl alone if SubUrl is empty
            end
        end


        function response = postRequest(obj, endpoint, params)
            % Sends a POST request to the API
            import matlab.net.*
            import matlab.net.http.*
            
            url = [obj.ApiUrl, endpoint];

            % Converts the Data property to a JSON string, excluding empty fields
            cleanedData = soc.api.ModelBase.removeEmptyFields(params);
            jsonData = jsonencode(cleanedData);

            % Create the HTTP headers
            headers = [
                HeaderField('Content-Type', 'application/json'), ...
                HeaderField('x-api-key', obj.ApiKey)  % Add the x-api-key header
            ];

            % Create the HTTP request
            body = MessageBody(cleanedData);
            request = RequestMessage('POST', headers, body);
            
            options = HTTPOptions('ConnectTimeout', obj.Timeout);

            % Initialize response
            response = struct();

            try
                rawResponse = send(request, url, options);
                
                if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                    response = rawResponse.Body.Data;
                    %response = jsondecode(rawResponse.Body.Data);
                else
                    error('HTTP Error: %s', char(rawResponse.StatusCode));
                end
            catch ME
                % Handle exceptions
                switch ME.identifier
                    case 'MATLAB:networklib:ConnectionFailed'
                        error('Connection failed. Please check the server URL or your network connection.');
                    case 'MATLAB:webservices:Timeout'
                        error('Request timed out after %d seconds.', obj.Timeout);
                    case 'MATLAB:webservices:HTTPErrorStatusCode'
                        error('HTTP error: %s. Check your API endpoint or parameters.', ME.message);
                    otherwise
                        % Re-throw unexpected errors
                        rethrow(ME);
                end
            end
        end
    end
end


