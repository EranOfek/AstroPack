function debug_SimpleFileLocal()
    % Main function to test all SimpleFileClient functionalities.
    % This script tests file writing, reading, listing, and other features
    % of the SimpleFileClient class against a running simple_file_server.

    fprintf('====================================================\n');
    fprintf('Starting SimpleFileLocal Debug Script\n');
    fprintf('Time: %s\n', datestr(now));
    fprintf('====================================================\n\n');

    % --- Configuration ---
    BASE_PATH = '';

    % --- Instantiate Client ---
    client = ultrasat.api.clients.SimpleFileLocal(BASE_PATH);

    fprintf('Using server base path: %s\n\n', client.BasePath);

    % --- Run Individual Debug Functions ---
    debugHealthCheck(client);
    debugWriteAndReadFile(client);
    debugWriteAndReadJson(client);
    debugWriteAndReadBinary(client);
    debugListFiles(client);
    debugNextAvailableFile(client);
    debugDeleteFile(client);

    fprintf('====================================================\n');
    fprintf('SimpleFileLocal Debug Script Finished\n');
    fprintf('====================================================\n');
end


function debugHealthCheck(client)
    % debugHealthCheck - Tests the health check of the server.
    fprintf('--- Testing HealthCheck ---\n');
    healthCheck = client.healthCheck();
    if healthCheck
        fprintf('  [SUCCESS] HealthCheck returned true.\n');
    else
        fprintf('  [FAIL] HealthCheck returned false.\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugWriteAndReadFile(client)
    % debugWriteAndReadFile - Tests basic text file writing and reading.
    fprintf('--- Testing WriteFile and ReadFile ---\n');

    % 1. Prepare test data
    filePath = 'test_file.txt';
    originalContent = sprintf('Hello from MATLAB!\nThis is a test file created on %s.', datetime('now'));

    % 2. Write the file to the server
    fprintf('Attempting to write to: %s\n', filePath);
    success = client.writeFile(filePath, originalContent);

    if ~success
        fprintf('  [FAIL] Failed to write file.\n\n');
        return;
    else
        fprintf('  [SUCCESS] writeFile returned true.\n');
    end

    % 3. Read the file back from the server
    fprintf('Attempting to read back file: %s\n', filePath);
    readContent = client.readFile(filePath);

    % 4. Verify the content
    if isempty(readContent)
        fprintf('  [FAIL] Failed to read file or file was empty.\n\n');
    elseif strcmp(originalContent, readContent)
        fprintf('  [SUCCESS] Read content matches original content.\n');
        disp('Original:');
        disp(originalContent);
        disp('Read:');
        disp(readContent);
    else
        fprintf('  [FAIL] Read content does NOT match original content.\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugWriteAndReadJson(client)
    % debugWriteAndReadJson - Tests JSON file writing and reading.
    fprintf('--- Testing WriteJson and ReadJson ---\n');

    % 1. Prepare a sample MATLAB struct
    filePath = 'test_data.json';
    originalStruct = struct();
    originalStruct.testName = 'MATLAB JSON Test';
    originalStruct.timestamp = datetime('now', 'Format', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''');
    originalStruct.values = [10, 20, 30; 40, 50, 60];
    originalStruct.metadata = struct('user', 'debug_script', 'version', 1.2);

    fprintf('Attempting to write a struct to: %s\n', filePath);
    disp('Original Struct:');
    disp(originalStruct);

    % 2. Write the struct as a JSON file to the server
    success = client.writeJson(filePath, originalStruct);

    if ~success
        fprintf('  [FAIL] Failed to write JSON file.\n\n');
        return;
    else
        fprintf('  [SUCCESS] writeJson returned true.\n');
    end

    % 3. Read the JSON file back and parse it
    fprintf('Attempting to read back JSON file: %s\n', filePath);
    readStruct = client.readJson(filePath);

    % 4. Verify the struct
    if isempty(fieldnames(readStruct))
        fprintf('  [FAIL] Failed to read or parse JSON file.\n\n');
    elseif isequal(originalStruct, readStruct)
        fprintf('  [SUCCESS] Read struct matches original struct.\n');
        disp('Read Struct:');
        disp(readStruct);
    else
        fprintf('  [FAIL] Read struct does NOT match original struct.\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugWriteAndReadBinary(client)
    % debugWriteAndReadBinary - Tests binary file writing and reading.
    fprintf('--- Testing WriteBinary and ReadBinary ---\n');

    % 1. Prepare test data
    filePath = 'test_binary.bin';
    originalData = randi([0, 255], 100, 1);

    % 2. Write the binary file to the server
    fprintf('Attempting to write binary file: %s\n', filePath);
    success = client.writeBinaryFile(filePath, originalData);

    if ~success
        fprintf('  [FAIL] Failed to write binary file.\n\n');
        return;
    else
        fprintf('  [SUCCESS] writeBinary returned true.\n');
    end

    % 3. Read the binary file back from the server

    fprintf('Attempting to read back binary file: %s\n', filePath);
    readData = client.readBinaryFile(filePath);

    % 4. Verify the data
    if isempty(readData)
        fprintf('  [FAIL] Failed to read binary file or file was empty.\n\n');
    elseif isequal(originalData, readData)
        fprintf('  [SUCCESS] Read data matches original data.\n');
        disp('Original Data:');
        disp(originalData);
        disp('Read Data:');
        disp(readData);
    else
        fprintf('  [FAIL] Read data does NOT match original data.\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugListFiles(client)
    % debugListFiles - Tests the listing of files in a directory.
    % This function relies on files created by previous debug steps.
    fprintf('--- Testing ListFiles ---\n');

    % 1. List all files in the base path
    fprintf('Listing all files in the base path ("%s")...\n', client.BasePath);
    allFiles = client.listFiles(''); % Empty string means the client's BasePath

    if isempty(allFiles)
        fprintf('  [WARN] No files found. This might be an error if previous tests did not run.\n');
    else
        fprintf('  [SUCCESS] Found %d files:\n', numel(allFiles));
        disp(allFiles);
    end

    % 2. List files with a specific mask
    mask = '*.json';
    fprintf('Listing files with mask: "%s"\n', mask);
    jsonFiles = client.listFiles('', mask);

    if isempty(jsonFiles)
        fprintf('  [WARN] No JSON files found.\n');
    else
        fprintf('  [SUCCESS] Found %d JSON files:\n', numel(jsonFiles));
        disp(jsonFiles);

        % Save response for inspection if needed
        save('c:\temp\sfc_list_response.mat', 'jsonFiles');
        fprintf('Saved JSON file list to c:\\temp\\sfc_list_response.mat\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugNextAvailableFile(client)
    % debugNextAvailableFile - Tests finding the next available filename.
    % Note: This requires the server to have files that match the pattern
    % to properly test the "next" index calculation. We will call it to
    % ensure the API endpoint works.

    fprintf('--- Testing NextAvailableFile ---\n');

    % 1. Define parameters for the request
    params = struct(...
        'folderPath', 'sequenced_data/', ...
        'mask', 'capture_*.fits', ...
        'zeroPad', 5, ...
        'minIndex', 1, ...
        'maxIndex', 99999 ...
    );

    fprintf('Asking server for next available file with this mask: "%s"\n', params.mask);
    disp(params);

    % 2. Call the function
    % We will combine the client's base path with the subfolder for this test.
    response = client.nextAvailableFile(params.folderPath, params.mask, params.zeroPad, params.minIndex, params.maxIndex);

    % 3. Display the result
    if isempty(fieldnames(response))
        fprintf('  [FAIL] Received an empty or invalid response from the server.\n');
    else
        fprintf('  [SUCCESS] Received a response from the server:\n');
        disp(response);

        % Save for inspection
        save('c:\temp\sfc_nextfile_response.mat', 'response');
        fprintf('Saved next file response to c:\\temp\\sfc_nextfile_response.mat\n');
    end
    fprintf('----------------------------------------\n\n');
end


function debugDeleteFile(client)
    % debugDeleteFile - Tests deleting a file.
    fprintf('--- Testing DeleteFile ---\n');

    % 1. Define parameters for the request
    filePath = 'test_file.txt';
    fprintf('Attempting to delete file: %s\n', filePath);
    success = client.deleteFile(filePath);

    if success
        fprintf('  [SUCCESS] File %s deleted successfully.\n', filePath);
    else
        fprintf('  [FAIL] Failed to delete file %s.\n', filePath);
    end
    fprintf('----------------------------------------\n\n');
end
