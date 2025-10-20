% Sample REST API Client side implementation in MATLAB
% Using RequestMessage() function
%
% https://chatgpt.com/c/678d0093-e15c-8012-905d-82577b0379ac

function MyRestClient()
    import matlab.net.http.*
    
    % Define the request data (JSON)
    data = struct('a', 5, 'b', 3);
    jsonData = jsonencode(data);
    
    % Create HTTP request
    uri = 'http://localhost:8080';
    req = RequestMessage('post', [], jsonData);
    
    % Send request
    resp = req.send(uri);
    
    % Display response
    disp(resp.Body.Data);
end
