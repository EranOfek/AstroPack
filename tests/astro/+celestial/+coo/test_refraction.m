function tests = test_refraction
    % Unit tests for the refraction function, which estimates atmospheric
    % refraction in visible light based on altitude and meteorological data.
    %
    % Author: Yarin Shani
    % Date: 04 Nov
    %
    % This script tests the function across standard known values, verifies
    % default behaviors, and checks boundary conditions.
    
    tests = functiontests(localfunctions);
end

function setupOnce(testCase)
    % Load expected results from file only once for all tests
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_refraction_results.mat');
    testData = load(dataFilePath);
    testCase.TestData.expectedResults = testData.expectedResults;
end

%% Test Functions

function testHorizonRefraction(testCase)
    % Test refraction at 0 degrees altitude (horizon) under standard conditions.
    
    RAD = 180 / pi;
    Alt = 0 / RAD;  % Altitude = 0 degrees in radians
    MetoData = [15, 1013.25, 0.8];  % Standard temperature, pressure, humidity
    Formula = 'AA';
    expectedR = 0.566 / RAD;  % Expected refraction in radians at horizon
    
    % Run refraction function
    R = celestial.coo.refraction(Alt, MetoData, Formula);
    
    % Verify result is within a tolerance of expected value
    verifyEqual(testCase, R, expectedR, 'AbsTol', 1e-4);
end

function test10DegreeAltitude(testCase)
    % Test refraction at 10 degrees altitude under standard conditions.
    
    RAD = 180 / pi;
    Alt = 10 / RAD;  % Altitude = 10 degrees in radians
    MetoData = [15, 1013.25, 0.8];
    Formula = 'AA';
    expectedR = 5.3 / 60 / RAD;  % Expected refraction in radians at 10 degrees
    
    % Run refraction function
    R = celestial.coo.refraction(Alt, MetoData, Formula);
    
    % Verify result is within tolerance of expected value
    verifyEqual(testCase, R, expectedR, 'AbsTol', 1e-4);
end

function test45DegreeAltitude(testCase)
    % Test refraction at 45 degrees altitude under standard conditions.
    
    RAD = 180 / pi;
    Alt = 45 / RAD;  % Altitude = 45 degrees in radians
    MetoData = [15, 1013.25, 0.8];
    Formula = 'AA';
    expectedR = 1 / 60 / RAD;  % Expected refraction in radians at 45 degrees
    
    % Run refraction function
    R = celestial.coo.refraction(Alt, MetoData, Formula);
    
    % Verify result is within tolerance of expected value
    verifyEqual(testCase, R, expectedR, 'AbsTol', 1e-4);
end

function testZenithRefraction(testCase)
    % Test refraction at 90 degrees altitude (zenith), where refraction should be zero.
    
    RAD = 180 / pi;
    Alt = 90 / RAD;  % Altitude = 90 degrees in radians (zenith)
    MetoData = [15, 1013.25, 0.8];
    Formula = 'AA';
    expectedR = 0;  % Expected refraction at zenith is zero
    
    % Run refraction function
    R = celestial.coo.refraction(Alt, MetoData, Formula);
    
    % Verify result is within tolerance of expected value
    verifyEqual(testCase, R, expectedR, 'AbsTol', 1e-5);
end



function testNegativeAltitude(testCase)
    % Test refraction function for a slightly negative altitude.
    
    RAD = 180 / pi;
    Alt = -0.9 / RAD;  % Altitude = -0.5 degrees in radians
    MetoData = [15, 1013.25, 0.8];
    Formula = 'AA';
    expectedR = 0;  % Expected refraction for altitudes below -0.6 degrees
    
    % Run refraction function
    R = celestial.coo.refraction(Alt, MetoData, Formula);
    
    % Verify result is as expected (zero refraction)
    verifyEqual(testCase, R, expectedR, 'AbsTol', 1e-4);
end



function testRelativeResults(testCase)
    % Compare the function’s output to precomputed expected results
    
    % Define tolerance for comparison
    tolerance = 1e-4;
    
    % Retrieve expected results from setupOnce
    expectedResults = testCase.TestData.expectedResults;
    
    % Loop over each test case stored in expected_results.mat
    for i = 1:length(expectedResults)
        Alt = expectedResults(i).Alt;
        MetoData = expectedResults(i).MetoData;
        Formula = expectedResults(i).Formula;
        expectedR = expectedResults(i).ExpectedR;
        
        % Calculate refraction using the current function
        R = celestial.coo.refraction(Alt, MetoData, Formula);
        
        % Verify that the calculated result is within tolerance
        verifyEqual(testCase, R, expectedR, 'AbsTol', tolerance, ...
            sprintf('Test case %d failed: Alt = %f, Formula = %s', i, Alt * 180 / pi, Formula));
    end
end