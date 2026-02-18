function debug_Loggable()

    % debug_Loggable - Tests the functionality of the Loggable class.
    fprintf('=== Testing Loggable Class Functionality ===\n\n');

    try
        % Test 1: Constructor and basic setup
        fprintf('--- Test 1: Constructor and Basic Setup ---\n');
        logger = ultrasat.api.core.Loggable();
        fprintf('  [SUCCESS] Loggable instance created.\n');
        fprintf('  Log file path: %s\n', logger.LogFilePath);
        fprintf('  Log prefix: %s\n', logger.LogPrefix);

        % Test 2: Basic msglog functionality
        fprintf('\n--- Test 2: Basic msglog Functionality ---\n');
        logger.msglog('This is a basic test message.');
        logger.msglog('Another message with no formatting.');
        fprintf('  [SUCCESS] Basic messages logged.\n');

        % Test 3: msglog with sprintf-style formatting
        fprintf('\n--- Test 3: msglog with Formatting ---\n');
        testValue = 42;
        testString = 'test_string';
        logger.msglog('Formatted message: value=%d, string=%s', testValue, testString);
        logger.msglog('Multiple values: %d, %s, %.2f', 100, 'hello', 3.14159);
        fprintf('  [SUCCESS] Formatted messages logged.\n');

        % Test 4: Test custom log prefix
        fprintf('\n--- Test 4: Custom Log Prefix ---\n');
        logger.LogPrefix = 'CustomTest';
        logger.msglog('This message should have a custom prefix.');
        logger.LogPrefix = 'Loggable'; % Reset to default
        logger.msglog('This message should have the default prefix again.');
        fprintf('  [SUCCESS] Custom prefix functionality tested.\n');

        % Test 5: Error handling in msglog
        fprintf('\n--- Test 5: Error Handling in msglog ---\n');
        logger.msglog('Testing error handling with invalid format: %d %s', 'not_a_number');
        logger.msglog('Testing with too few arguments: %d %s');
        logger.msglog('Testing with too many arguments: %d', 1, 2, 3);
        fprintf('  [SUCCESS] Error handling tested (check console for error messages).\n');

        % Test 6: Verify log file exists and contains messages
        fprintf('\n--- Test 6: Log File Verification ---\n');
        if ~isempty(logger.LogFilePath) && exist(logger.LogFilePath, 'file')
            fprintf('  Log file exists: %s\n', logger.LogFilePath);

            % Read and display last few lines of log file
            try
                fid = fopen(logger.LogFilePath, 'r');
                if fid ~= -1
                    % Read all lines
                    logContent = textscan(fid, '%s', 'Delimiter', '\n');
                    fclose(fid);

                    if ~isempty(logContent{1})
                        fprintf('  Last 3 log entries:\n');
                        logLines = logContent{1};
                        startIdx = max(1, length(logLines) - 2);
                        for i = startIdx:length(logLines)
                            fprintf('    %s\n', logLines{i});
                        end
                    end
                end
            catch ME
                fprintf('  [WARNING] Could not read log file: %s\n', ME.message);
            end
        else
            fprintf('  [WARNING] Log file does not exist or path is empty.\n');
        end

        % Test 7: Test resolveDefaultBasePath0 method
        fprintf('\n--- Test 7: Base Path Resolution ---\n');
        basePath = logger.resolveDefaultBasePath0();
        fprintf('  Resolved base path: %s\n', basePath);
        fprintf('  [SUCCESS] Base path resolution tested.\n');

        fprintf('\n=== All Tests Completed Successfully ===\n');

    catch ME
        fprintf('\n[ERROR] Test failed with error:\n');
        fprintf('  Message: %s\n', ME.message);
        fprintf('  Stack trace:\n');
        for i = 1:length(ME.stack)
            fprintf('    %s (line %d)\n', ME.stack(i).name, ME.stack(i).line);
        end
    end
end

