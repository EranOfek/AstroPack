function tests = test_spherical_triangle_circum_circle
    % Unit tests for the spherical_triangle_circum_circle function in AstroPack
    %
    % Description:
    % This unit test verifies the correct operation of the `spherical_triangle_circum_circle`
    % function for all supported methods ('sides', 'angles', and 'vertex'). It includes tests for
    % basic cases, edge cases like degenerate triangles, and regression tests to ensure 
    % consistency over time.
    %
    % Author: Yarin Shani
    % Date: 2024-10-19
    
    tests = functiontests(localfunctions);
end

%% Setup and Teardown
function setupOnce(testCase)
    % Setup common test data used in multiple tests
    testCase.TestData.sides = [pi/3, pi/3, pi/3];   % Equilateral triangle with sides of pi/3
    testCase.TestData.angles = {[pi/2, pi/2, pi], [pi/3, pi/4, pi/2], [pi/2, pi/4, pi/2]}; % Updated angles
    testCase.TestData.vertices = [0, 0, pi/3, 0, pi/3, pi/3];  % Equilateral triangle at vertices

    % Load regression data (assumes the .mat file exists)
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_spherical_triangle_circum_circle_results.mat');
    if exist(dataFilePath, 'file')
        testCase.TestData.ExpectedResults = load(dataFilePath);
    else
        error('Regression data file does not exist. Run the regression data generation script first.');
    end
end

function teardownOnce(~)
    % Teardown function: nothing needed for now, but can be used for cleanup
end

%% Test Functions

% More tests required!


function testRegressionCase(testCase)
    % Regression test to verify consistency with stored results
    
    expectedResults = testCase.TestData.ExpectedResults.expectedResults;

    % Loop through each set of expected results
    for i = 1:numel(expectedResults)
        Method = expectedResults(i).Method;
        Inputs = expectedResults(i).Inputs;
        
        % Call the function using the stored inputs
        if strcmp(Method, 'sides')
            [a, b, c] = Inputs{:};
            R = celestial.coo.spherical_triangle_circum_circle('sides', a, b, c);
        elseif strcmp(Method, 'angles')
            [A, B, C] = Inputs{:};
            R = celestial.coo.spherical_triangle_circum_circle('angles', A, B, C);
        elseif strcmp(Method, 'vertex')
            [Long1, Lat1, Long2, Lat2, Long3, Lat3] = Inputs{:};
            R = celestial.coo.spherical_triangle_circum_circle('vertex', Long1, Lat1, Long2, Lat2, Long3, Lat3);
        end
        
        % Verify the result matches the stored result
        verifyEqual(testCase, R, expectedResults(i).R, 'RelTol', 1e-6, ...
            sprintf('Circumradius result for Method: %s has changed unexpectedly.', Method));
    end
end
