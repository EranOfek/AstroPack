
import matlab.unittest.TestRunner;
import matlab.unittest.TestSuite;
import matlab.unittest.plugins.TestReportPlugin;

% Create a test suite from the 'tests' folder
suite = TestSuite.fromFolder('.') % , 'IncludingSubfolders', false);

% Create a test runner with text output
runner = TestRunner.withTextOutput('Verbosity', 3);

% Add a plugin to generate an HTML report
htmlFile = 'testReport.html';
runner.addPlugin(TestReportPlugin.producingHTML(htmlFile, 'IncludePassingDiagnostics', true));

% Run the tests
results = runner.run(suite);
disp(results);

