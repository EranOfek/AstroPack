function tests = TestLogFile
    % TestLogFile
    % Function-based unit tests for the LogFile class.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the LogFile class.
    %
    % Author: Chen Tishler (Apr 2021)
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testConstructor(testCase)
    % Test the LogFile constructor
    logDir = tempname; % Temporary directory for log files
    mkdir(logDir);
    
    % Create a LogFile instance
    logFile = LogFile(fullfile(logDir, 'test.log'));
    
    % Verify the file was created
    verifyEqual(testCase, logFile.FileName, fullfile(logDir, 'test.log'));
    
    % Verify the timestamp is not empty
    verifyNotEmpty(testCase, logFile.Timestamp, 'Timestamp should not be empty.');
    
    % Clean up
    delete(logFile);
    rmdir(logDir, 's');
end

function testWriteLogEntry(testCase)
    % Test writing a log entry
    logDir = tempname;
    mkdir(logDir);
    
    % Create a LogFile instance
    logFile = LogFile(fullfile(logDir, 'test.log'));
    
    % Write to the log
    logFile.write('This is a test log entry.');
    
    % Verify the file content
    fid = fopen(logFile.FileName, 'r');
    content = fread(fid, '*char')';
    fclose(fid);
    
    verifyContains(testCase, content, 'This is a test log entry.', 'Log entry was not written correctly.');
    
    % Clean up
    delete(logFile);
    rmdir(logDir, 's');
end

function testFileSizeSwitching(testCase)
    % Test the automatic file switching when the max file size is reached
    logDir = tempname;
    mkdir(logDir);
    
    % Create a LogFile instance with a very small max file size
    logFile = LogFile(fullfile(logDir, 'test.log'), 'MaxFileSize', 100);
    
    % Write entries until the file switches
    for i = 1:10
        logFile.write('This is a test log entry.');
    end
    
    % Check if the file was switched
    oldLogFile = fullfile(logDir, 'test.old');
    oldLogFiles = dir(fullfile(logDir, '*.old'));
    verifyTrue(testCase, ~isempty(oldLogFiles), 'The log file was not switched after exceeding max file size.');
    
    % Clean up
    delete(logFile);
    rmdir(logDir, 's');
end

function testSingleton(testCase)
    % Test the singleton pattern for the LogFile class
    logDir = tempname;
    mkdir(logDir);
    
    % Get a singleton LogFile instance
    logFile1 = LogFile.getSingleton('FileName', fullfile(logDir, 'singleton.log'));
    
    % Get another reference to the singleton
    logFile2 = LogFile.getSingleton();
    
    % Verify both references point to the same object
    verifyEqual(testCase, logFile1, logFile2, 'The singleton instances should be the same.');
    
    % Clean up
    delete(logFile1);
    rmdir(logDir, 's');
end

function testWrite2(testCase)
    % Test the write2 method (writing with a title)
    logDir = tempname;
    mkdir(logDir);
    
    % Create a LogFile instance
    logFile = LogFile(fullfile(logDir, 'test.log'));
    
    % Write to the log with a title
    logFile.write2('INFO', 'This is a titled log entry.');
    
    % Verify the file content
    fid = fopen(logFile.FileName, 'r');
    content = fread(fid, '*char')';
    fclose(fid);
    
    verifyContains(testCase, content, 'INFO > This is a titled log entry.', 'Titled log entry was not written correctly.');
    
    % Clean up
    delete(logFile);
    rmdir(logDir, 's');
end

function testDestructor(testCase)
    % Test the destructor for closing the file
    logDir = tempname;
    mkdir(logDir);
    
    % Create a LogFile instance
    logFile = LogFile(fullfile(logDir, 'test.log'));
    
    % Write to the log
    logFile.write('This is a test log entry.');
    
    % Delete the log file object, which should close the file
    delete(logFile);
    
    % Verify the file is closed
    verifyError(testCase, @() fprintf(logFile.Fid, 'This should fail'), 'MATLAB:badfid_mx', 'File handle should be invalid after destruction.');
    
    % Clean up
    rmdir(logDir, 's');
end

