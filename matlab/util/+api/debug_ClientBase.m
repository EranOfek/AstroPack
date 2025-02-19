%==========================================================================
% ULTRASAT 
%
% File:   debug_ClientBase.m
% Author: Chen Tishler
% Created: 01/12/2024
% Updated: 11/02/2025
%==========================================================================
%
% Debugging function for api.ClientBase class.
% Run this script in MATLAB:
% >> debug_ClientBase
%
% This script tests:
% - POST requests to FastAPI (`/add`, `/multiply`)
% - JSON encoding/decoding of doubles & strings.
%
% Run Python script debug_ClientBase_server.py from this folder as backend.
%

function debug_ClientBase()
    % Initialize ClientBase with FastAPI server URL
    client = api.ClientBase('BaseUrl', 'http://127.0.0.1:8299');
    client.ApiKey = [];
    
    % Test addition endpoint
    debug_postRequest(client, '/add', 2.5, 2.5);
    
    % Test multiplication endpoint
    debug_postRequest(client, '/multiply', 2, 2);
end


function debug_postRequest(client, endpoint, a, b)
    % Tests the postRequest method for addition/multiplication.
    %
    % :param client: ClientBase instance.
    % :param endpoint: API endpoint ('/add' or '/multiply').
    % :param a: First number.
    % :param b: Second number.

    disp(['Testing ', endpoint, '...']);
    
    % Ensure 'a' and 'b' are explicitly doubles
    a = double(a);
    b = double(b);

    % Create request payload
    params = struct('a', a, 'b', b);
    
    % Send request
    response = client.postRequest(endpoint, params);
    
    % Display response
    disp('Response:');
    disp(response);
    
    % Validate response fields
    assert(isfield(response, 'result') && isa(response.result, 'double'), 'Error: result missing or incorrect type.');
    assert(isfield(response, 'status') && isa(response.status, 'char'), 'Error: status missing or incorrect type.');
    assert(isfield(response, 'message') && isa(response.message, 'char'), 'Error: message missing or incorrect type.');

    disp('[PASS] Test successful.');
end

