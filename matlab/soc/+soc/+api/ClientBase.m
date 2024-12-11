classdef ClientBase < handle
    % ClientBase - Base class for interacting with REST API services.
    % https://chatgpt.com/c/6756dedd-4c2c-8012-adad-4772c6780623
    
    properties
        ApiUrl  % Base URL of the API
        ApiKey  % API Key for authentication
        Timeout = 30;  % Timeout for HTTP requests (seconds)
    end
    
    methods
        function obj = ClientBase(apiUrl, apiKey, timeout)
            % Constructor for ClientBase
            if nargin > 0
                obj.ApiUrl = apiUrl;
            end
            if nargin > 1
                obj.ApiKey = apiKey;
            end
            if nargin > 2
                obj.Timeout = timeout;
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
            rawResponse = send(request, url, options);
            
            if rawResponse.StatusCode == matlab.net.http.StatusCode.OK
                response = rawResponse.Body.Data;
                %response = jsondecode(rawResponse.Body.Data);
            else
                error('HTTP Error: %s', char(rawResponse.StatusCode));
            end
        end
    end
end


