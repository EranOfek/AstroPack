% https://chatgpt.com/c/a3cbb7bb-fb14-4d79-bfe8-dfd600a7a67b
% https://www.mathworks.com/help/matlab/matlab_prog/run-tests-for-various-workflows.html

function runTestsFromFolder() %folder, logFileName)
    % Function to run tests from a given top-level folder and log results
    % 
    % Inputs:
    %   - folder: The top-level folder containing the tests
    %   - logFileName: The name of the log file where test results will be written
    %
    % Example usage:
    %   runTestsFromFolder('C:\Ultrasat\AstroPack\tests', 'TestResults.log');
    
    folder = 'C:\Ultrasat\AstroPack\tests';
    logFileName = 'C:\Ultrasat\AstroPack\tests\log1.log';

    % Create a test suite from the specified folder
    suite = matlab.unittest.TestSuite.fromFolder(folder, 'IncludingSubfolders', true);

    % Create a test runner that provides detailed text output
    runner = matlab.unittest.TestRunner.withTextOutput('OutputDetail', matlab.unittest.Verbosity.Detailed);

    % Define the log file for writing test results
    %if nargin < 2 || isempty(logFileName)
    %    logFileName = 'TestResults.log';
    %end

    % Create a plugin to log results to the specified file
    %filePlugin = matlab.unittest.plugins.ToFile(logFileName);
    %runner.addPlugin(filePlugin);

    % Run the test suite
    results = runner.run(suite);

    % Save the complete results to a .mat file for future analysis
    resultFileName = fullfile(folder, 'TestResults.mat');
    save(resultFileName, 'results');

    % Log details of any failed tests in the log file
    failedTests = results([results.Failed]);
    if ~isempty(failedTests)
        % Open the log file in append mode to add failed test details
        fid = fopen(logFileName, 'a');
        fprintf(fid, '\n\nFailed Tests:\n');
        for i = 1:length(failedTests)
            fprintf(fid, 'Test Name: %s\n', failedTests(i).Name);
            fprintf(fid, 'Diagnostics:\n%s\n', ...
                join(string({failedTests(i).Details.DiagnosticRecord.Message}), '\n'));
        end
        fclose(fid);
    end

    % Display summary of results
    disp('Test results saved to:');
    disp(['Log file: ', logFileName]);
    disp(['MAT file: ', resultFileName]);
end
