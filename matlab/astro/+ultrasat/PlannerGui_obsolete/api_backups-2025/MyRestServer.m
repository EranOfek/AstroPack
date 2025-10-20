% Sample REST API Server side implementation in MATLAB
% Using class matlab.net.http.server.HTTPServer
%
% https://chatgpt.com/c/678d0093-e15c-8012-905d-82577b0379ac


classdef MyRestServer < matlab.net.http.server.HTTPServer
    methods
        function obj = MyRestServer(port)
            % Initialize HTTP server on the given port
            obj@matlab.net.http.server.HTTPServer(port);
        end
        
        function handleRequest(obj, req, resp)
            % Handle incoming HTTP requests
            import matlab.net.http.*
            
            if req.Method == RequestMethod.POST
                try
                    % Parse JSON payload from POST request
                    data = jsondecode(char(req.Body.Data));
                    
                    % Process the request data (Example: sum two numbers)
                    if isfield(data, 'a') && isfield(data, 'b')
                        result = data.a + data.b;
                        responseData = struct('status', 'success', 'sum', result);
                    else
                        responseData = struct('status', 'error', 'message', 'Missing parameters a and b');
                    end
                    
                    % Set the response content
                    resp.Body = jsonencode(responseData);
                    resp.ContentType = "application/json";
                    
                catch ME
                    % Handle errors
                    resp.StatusCode = matlab.net.http.StatusCode.BadRequest;
                    resp.Body = jsonencode(struct('status', 'error', 'message', ME.message));
                end
            else
                % If the request is not POST, return an error
                resp.StatusCode = matlab.net.http.StatusCode.MethodNotAllowed;
                resp.Body = jsonencode(struct('status', 'error', 'message', 'Only POST requests are allowed.'));
            end
        end
    end
end


function startSever()
    % Call this function to start the server
    
    port = 8080; % Choose any available port
    server = MyRESTServer(port); % Create server instance
    server.RequestFcn = @server.handleRequest; % Assign request handler
    start(server); % Start the server
    disp(['Server running on http://localhost:' num2str(port)]);
end
