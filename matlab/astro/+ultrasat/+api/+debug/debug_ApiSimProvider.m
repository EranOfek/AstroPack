function debug_ApiSimProvider()
    % Main function to test the ApiSimProvider class.
    %   This script runs a full suite of tests against the ApiSimProvider in
    %   both its remote (HTTP server) and local (filesystem) modes.
    %
    %   Prerequisites:
    %   - For the remote test, the Python 'simple_file_server.py' must be running.

    fprintf('====================================================\n');
    fprintf('Starting ApiSimProvider Debug Script\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('====================================================\n\n');

    % 1. Test the LOCAL provider (using SimpleFileLocal)
    debug_TestLocal();

    % 2. Test the REMOTE provider (using SimpleFileClient)
    debug_TestRemote();

    fprintf('====================================================\n');
    fprintf('ApiSimProvider Debug Script Finished\n');
    fprintf('====================================================\n');
end


function debug_TestLocal()
    LOCAL_BASE_PATH = 'C:\temp\api_sim_local_tests'; % A temporary local directory

    fprintf('\n>>> Running test suite for LOCAL provider...\n');
    fprintf('    Target: %s\n', LOCAL_BASE_PATH);
    try
        localProvider = ultrasat.api.ApiSimProvider('', '');  %LOCAL_BASE_PATH);
        testProviderSuite(localProvider, 'Local');
    catch ME
        fprintf('\n  [FATAL ERROR] Could not run local tests.\n');
        fprintf('  Error details: %s\n\n', ME.message);
    end
end


function debug_TestRemote()
    % --- Configuration ---
    REMOTE_URL = 'http://localhost:8090';
    REMOTE_BASE_PATH = 'api_sim_remote_tests/'; % A dedicated folder on the server

    fprintf('>>> Running test suite for REMOTE provider...\n');
    fprintf('    Target: %s\n', REMOTE_URL);
    fprintf('    Base Path: %s\n', REMOTE_BASE_PATH);
    try
        remoteProvider = ultrasat.api.ApiSimProvider(REMOTE_URL, REMOTE_BASE_PATH);
        testProviderSuite(remoteProvider, 'Remote');
    catch ME
        fprintf('\n  [FATAL ERROR] Could not run remote tests.\n');
        fprintf('  Please ensure the Python server is running at %s\n', REMOTE_URL);
        fprintf('  Error details: %s\n\n', ME.message);
    end
end


function testProviderSuite(provider, providerType)
    % Runs a generic set of tests against any provider object.
    %   provider: An instance of ApiSimProvider.
    %   providerType: A string ('Remote' or 'Local') for logging purposes.

    % --- Test 1: Write and Read JSON ---
    fprintf('\n--- Testing WriteJsonFile and ReadJsonFile (%s) ---\n', providerType);
    testStruct.name = 'Test Data';
    testStruct.id = 12345;
    testStruct.timestamp = datestr(now, 30);
    testStruct.valid = true;
    testStruct.matrix = [1, 2, 3; 4, 5, 6];
    jsonFileName = 'test_config.json';


    % --- Test 1: Health Check ---
    fprintf('\n--- Testing HealthCheck (%s) ---\n', providerType);
    healthCheck = provider.healthCheck();
    if healthCheck
        fprintf('  [SUCCESS] HealthCheck returned true.\n');
    else
        fprintf('  [FAIL] HealthCheck returned false.\n');
    end
    fprintf('----------------------------------------\n\n');

    % --- Test 1: Write and Read JSON ---
    fprintf('Writing struct to %s...\n', jsonFileName);
    disp(testStruct);
    success = provider.WriteJsonFile(jsonFileName, testStruct);

    if ~success
        fprintf('  [FAIL] WriteJsonFile returned false.\n');
        return; % Stop this suite if writing fails
    else
        fprintf('  [SUCCESS] WriteJsonFile returned true.\n');
    end

    fprintf('Reading back %s...\n', jsonFileName);
    readStruct = provider.ReadJsonFile(jsonFileName);

    if isequal(testStruct, readStruct)
        fprintf('  [SUCCESS] Read struct matches original struct.\n');
    else
        fprintf('  [FAIL] Read struct does NOT match original struct.\n');
        disp('Original:');
        disp(testStruct);
        disp('Read:');
        disp(readStruct);
    end

    % --- Test 2: Write and Read Binary ---
    fprintf('\n--- Testing WriteBinary and ReadBinary (%s) ---\n', providerType);

    % 1. Cast the data to uint8 to match the binary file type.
    % 2. Make it a 1x100 row vector to match the shape of the read data.
    testData = uint8(randi([0, 255], 1, 100));
    %testData = randi([0, 255], 100, 1);

    binaryFileName = 'test_binary.bin';
    fprintf('Writing binary data to %s...\n', binaryFileName);
    success = provider.WriteBinaryFile(binaryFileName, testData);

    if ~success
        fprintf('  [FAIL] WriteBinary returned false.\n');
        return;
    else
        fprintf('  [SUCCESS] WriteBinary returned true.\n');
    end

    fprintf('Reading back %s...\n', binaryFileName);
    readData = provider.ReadBinaryFile(binaryFileName);

    if isequal(testData, readData)
        fprintf('  [SUCCESS] Read data matches original data.\n');
    else
        fprintf('  [FAIL] Read data does NOT match original data.\n');
        disp('Original:');
        disp(testData);
        disp('Read:');
        disp(readData);
    end

    % --- Test 3: WriteMatObject and LoadMatObject ---
    fprintf('\n--- Testing WriteMatObject and LoadMatObject (%s) ---\n', providerType);
    matFileName = 'test_object.mat';
    variableName = 'myVar';
    testObject = struct('a', 42, 'b', rand(3,1), 'msg', 'hello world');
    fprintf('Writing MAT object to %s (variable: %s)...\n', matFileName, variableName);
    success = provider.saveMatObject(matFileName, testObject, variableName);

    if ~success
        fprintf('  [FAIL] WriteMatObject returned false.\n');
        return;
    else
        fprintf('  [SUCCESS] WriteMatObject returned true.\n');
    end

    fprintf('Loading MAT object from %s (variable: %s)...\n', matFileName, variableName);
    [loadedObject, success] = provider.loadMatObject(matFileName, variableName);

    if ~success
        fprintf('  [FAIL] LoadMatObject returned false.\n');
        return;
    elseif isequal(testObject, loadedObject)
        fprintf('  [SUCCESS] Loaded object matches original object.\n');
    else
        fprintf('  [FAIL] Loaded object does NOT match original object.\n');
        disp('Original:');
        disp(testObject);
        disp('Loaded:');
        disp(loadedObject);
    end

    % --- Test 2: List Files ---
    fprintf('\n--- Testing ListFilesInFolder (%s) ---\n', providerType);
    mask = '*.json';
    fprintf('Listing files with mask %s...\n', mask);
    fileList = provider.ListFilesInFolder('', mask); % List in the base path

    if isempty(fileList)
        fprintf('  [FAIL] File list is empty.\n');
    elseif ismember(jsonFileName, fileList)
        fprintf('  [SUCCESS] Found %s in the file list.\n', jsonFileName);
        disp(fileList);
    else
        fprintf('  [FAIL] Did not find %s in the file list.\n', jsonFileName);
        disp(fileList);
    end

    % --- Test 3: Next Available File ---
    fprintf('\n--- Testing NextAvailableFile (%s) ---\n', providerType);
    seqFolder = 'sequence_test/';
    seqMask = '*.dat';

    % Setup: create a few dummy files to establish a sequence
    provider.WriteJsonFile(fullfile(seqFolder, '001.dat'), struct('seq', 1));
    provider.WriteJsonFile(fullfile(seqFolder, '002.dat'), struct('seq', 2));
    provider.WriteJsonFile(fullfile(seqFolder, '003.dat'), struct('seq', 3));

    fprintf('Searching for next available file in %s with mask %s\n', seqFolder, seqMask);

    nextFileResult = provider.NextAvailableFile(seqFolder, seqMask, 3, 1, 100);

    if isempty(fieldnames(nextFileResult))
        fprintf('  [FAIL] NextAvailableFile returned an empty result.\n');
    elseif isfield(nextFileResult, 'index') && nextFileResult.index >= 2
        fprintf('  [SUCCESS] Correctly identified next available index as %d.\n', nextFileResult.index);
        fprintf('  Next filename: %s\n', nextFileResult.filename);
        disp(nextFileResult);
    else
        fprintf('  [FAIL] Incorrectly identified next available index.\n');
        fprintf('  Expected index: 3\n');
        disp(nextFileResult);
    end

    % --- Test 4: Delete File ---
    fprintf('\n--- Testing DeleteFile (%s) ---\n', providerType);
    fprintf('Deleting file %s...\n', jsonFileName);
    success = provider.DeleteFile(jsonFileName);
    if success
        fprintf('  [SUCCESS] File %s deleted successfully.\n', jsonFileName);
    else
        fprintf('  [FAIL] Failed to delete file %s.\n', jsonFileName);
    end
end
