
function debug_ClientBase()
    %debug_constructor();
    %debug_postRequest();
    %debug_postRequestAsync();
    debug_serializeDeserialize();

    %debug_sendFiles();
end


function debug_constructor()
    disp('--- Testing Constructor ---');
    
    % Create an instance with default arguments
    client = ClientBase(struct());
    disp('Default Client:');
    disp(client);

    % Create an instance with custom arguments
    args = struct('BaseUrl', 'https://api.example.com', ...
                  'SubUrl', '/v1/resource', ...
                  'ApiKey', 'test-api-key', ...
                  'Timeout', 15);
    client = ClientBase(args);
    disp('Custom Client:');
    disp(client);
    
    disp('--- Constructor Test Completed ---');
end


function debug_postRequest()
    disp('--- Testing postRequest ---');
    
    client = ClientBase(struct('BaseUrl', 'https://jsonplaceholder.typicode.com', 'SubUrl', '/posts'));
    endpoint = '/1';
    params = struct('title', 'foo', 'body', 'bar', 'userId', 1);
    
    try
        response = client.postRequest(endpoint, params);
        disp('POST Request Response:');
        disp(response);
    catch ME
        disp('Error during postRequest:');
        disp(ME.message);
    end
    
    disp('--- postRequest Test Completed ---');
end


function debug_postRequestAsync()
    disp('--- Testing postRequestAsync ---');
    
    client = api.ClientBase(struct('BaseUrl', 'https://jsonplaceholder.typicode.com', 'SubUrl', '/posts'));
    endpoint = '/1';
    params = struct('title', 'foo', 'body', 'bar', 'userId', 1);
    
    callback = @(response) disp(['Async Response: ', jsonencode(response)]);
    
    try
        client.postRequestAsync(endpoint, params, callback);
        pause(5); % Allow time for async request to complete
    catch ME
        disp('Error during postRequestAsync:');
        disp(ME.message);
    end
    
    disp('--- postRequestAsync Test Completed ---');
end


function debug_serializeDeserialize()

    Client = api.ClientBase();

    % Example MATLAB object
    myStruct = struct('name', 'MATLAB', 'value', 42);
    
    % Serialize to Base64
    encodedData = Client.serializeToBase64(myStruct);
    disp('Serialized Base64 Data:');
    disp(encodedData);


    % Deserialize from Base64
    decodedStruct = Client.deserializeFromBase64(encodedData);
    disp('Deserialized MATLAB Object:');
    disp(decodedStruct);
    
end


function debug_sendFiles()

    url = 'https://example.com/upload';
    
    % Array of file paths
    filePaths = {
        'path/to/large_file1.bin', ...
        'path/to/large_file2.bin', ...
        'path/to/large_file3.bin'
    };
    
    % Parameters to include in the request
    params = struct('username', 'JohnDoe', 'description', 'Multi-file upload test');
    
    % Send the files and parameters
    sendFilesWithParams(url, filePaths, params);

end


