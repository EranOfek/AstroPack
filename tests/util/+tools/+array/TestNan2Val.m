function tests = TestNan2Val
    % TestNan2Val
    % Function-based unit tests for the nan2val function.
    % This function creates a test suite that includes the local test functions.
    %
    % The function tests the behavior of nan2val under different conditions,
    % such as replacing NaN values with default or specified values.
    %
    % Author: Chen Tishler

    % Create a test suite from local test functions
    tests = functiontests(localfunctions);
end

function testReplaceNaNsWithDefaultValue(testCase)
    % Test the replacement of NaN values with the default value (0)
    %
    % This test verifies that the nan2val function correctly replaces NaN
    % values in a matrix with the default value of 0 when no replacement value is specified.

    % Define a matrix with NaN values
    Mat = [1, NaN, 3; NaN, 5, NaN];
    
    % Call the nan2val function without specifying a replacement value
    result = tools.array.nan2val(Mat);
    
    % Define the expected result, where NaNs are replaced by the default value 0
    expectedResult = [1, 0, 3; 0, 5, 10];
    
    % Verify that the result matches the expected output
    verifyEqual(testCase, result, expectedResult, ...
        'Default NaN replacement failed.');
end

function testReplaceNaNsWithSpecifiedValue(testCase)
    % Test the replacement of NaN values with a specified value
    %
    % This test checks that the nan2val function correctly replaces NaN
    % values in a matrix with a specified value, rather than the default 0.

    % Define a matrix with NaN values
    Mat = [1, NaN, 3; NaN, 5, NaN];
    
    % Specify a replacement value for NaN
    Val = -1;
    
    % Call the nan2val function with the specified replacement value
    result = tools.array.nan2val(Mat, Val);
    
    % Define the expected result, where NaNs are replaced by the specified value -1
    expectedResult = [1, -1, 3; -1, 5, -1];
    
    % Verify that the result matches the expected output
    verifyEqual(testCase, result, expectedResult, ...
        'Specified NaN replacement failed.');
end
