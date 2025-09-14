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
    
    % --- Configuration ---
    REMOTE_URL = 'http://localhost:5000';
    REMOTE_BASE_PATH = 'api_sim_remote_tests/'; % A dedicated folder on the server
    
    LOCAL_BASE_PATH = 'C:\temp\api_sim_local_tests'; % A temporary local directory
    
    % --- Test Suite Execution ---
    
    % 1. Test the REMOTE provider (using SimpleFileClient)
    fprintf('>>> Running test suite for REMOTE provider...\n');
    fprintf('    Target: %s\n', REMOTE_URL);
    fprintf('    Base Path: %s\n', REMOTE_BASE_PATH);
    try
        remoteProvider = ApiSimProvider(REMOTE_URL, REMOTE_BASE_PATH);
        testProviderSuite(remoteProvider, 'Remote');
    catch ME
        fprintf('\n  [FATAL ERROR] Could not run remote tests.\n');
        fprintf('  Please ensure the Python server is running at %s\n', REMOTE_URL);
        fprintf('  Error details: %s\n\n', ME.message);
    end
    
    % 2. Test the LOCAL provider (using SimpleFileLocal)
    fprintf('\n>>> Running test suite for LOCAL provider...\n');
    fprintf('    Target: %s\n', LOCAL_BASE_PATH);
    try
        localProvider = ApiSimProvider(LOCAL_BASE_PATH);
        testProviderSuite(localProvider, 'Local');
    catch ME
        fprintf('\n  [FATAL ERROR] Could not run local tests.\n');
        fprintf('  Error details: %s\n\n', ME.message);
    end

    fprintf('====================================================\n');
    fprintf('ApiSimProvider Debug Script Finished\n');
    fprintf('====================================================\n');
end


function testProviderSuite(provider, providerType)
    %TESTPROVIDERSUITE Runs a generic set of tests against any provider object.
    %   provider: An instance of ApiSimProvider.
    %   providerType: A string ('Remote' or 'Local') for logging purposes.
    
    % --- Test 1: Write and Read JSON ---
    fprintf('\n--- Testing WriteJsonFile and ReadJsonFile (%s) ---\n', providerType);
    testStruct.name = 'Test Data';
    testStruct.id = 12345;
    testStruct.timestamp = datestr(now, 'isodatetime');
    testStruct.valid = true;
    testStruct.matrix = [1, 2, 3; 4, 5, 6];
    jsonFileName = 'test_config.json';

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
    seqMask = 'data_*.dat';
    
    % Setup: create a few dummy files to establish a sequence
    provider.WriteJsonFile(fullfile(seqFolder, 'data_001.dat'), struct('seq', 1));
    provider.WriteJsonFile(fullfile(seqFolder, 'data_002.dat'), struct('seq', 2));
    
    fprintf('Searching for next available file in %s with mask %s\n', seqFolder, seqMask);
    
    nextFileResult = provider.NextAvailableFile(seqFolder, seqMask, 3, 1, 100);
    
    if isempty(fieldnames(nextFileResult))
        fprintf('  [FAIL] NextAvailableFile returned an empty result.\n');
    elseif isfield(nextFileResult, 'index') && nextFileResult.index == 3
        fprintf('  [SUCCESS] Correctly identified next available index as %d.\n', nextFileResult.index);
        fprintf('  Next filename: %s\n', nextFileResult.filename);
        disp(nextFileResult);
    else
        fprintf('  [FAIL] Incorrectly identified next available index.\n');
        fprintf('  Expected index: 3\n');
        disp(nextFileResult);
    end
end
