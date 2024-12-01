function tests = test_coo2box
    % Unit tests for the celestial.coo.coo2box function, which calculates
    % the vertices of a box around specified RA/Dec coordinates with a
    % specified half-size.
    %
    % Author: Yarin Shani
    % Date: 13 Nov
    %
    % This script tests standard and edge cases, unit conversions,
    % and compares with expected relative results.
    
    tests = functiontests(localfunctions);
end

%% Test Setup Function

function setupOnce(testCase)
    % Load expected results from file only once for all tests
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_coo2box_results.mat');
    testData = load(dataFilePath);
    testCase.TestData.expectedResults = testData.expectedResults;
end

%% Test Functions

function testStandardInputRadians(testCase)
    % Test with standard input in radians
    
    % Define inputs
    RA = 1;  % RA in radians
    Dec = 0.5;  % Dec in radians
    HalfSize = [0.01, 0.01];  % Box half-size in radians
    OutUnits = 'rad';
    
    % Expected outputs
    expectedDec1 = (Dec - HalfSize(2)) ;
    expectedDec2 = (Dec + HalfSize(2)) ;
    MaxDec = max(abs(expectedDec1),abs(expectedDec2));
    expectedRA1 = (RA - HalfSize(1) / cos(MaxDec)) ;
    expectedRA2 = (RA + HalfSize(1) / cos(MaxDec));
    
    % Run coo2box function
    [RA1, RA2, Dec1, Dec2] = celestial.coo.coo2box(RA, Dec, HalfSize, OutUnits);
    
    % Verify outputs
    verifyEqual(testCase, RA1, expectedRA1, 'RelTol', 1e-6);
    verifyEqual(testCase, RA2, expectedRA2, 'RelTol', 1e-6);
    verifyEqual(testCase, Dec1, expectedDec1, 'RelTol', 1e-6);
    verifyEqual(testCase, Dec2, expectedDec2, 'RelTol', 1e-6);
end

function testStandardInputDegrees(testCase)
    % Test with standard input, output in degrees
    
    % Define inputs
    RA = pi / 4;  % 45 degrees in radians
    Dec = pi / 6;  % 30 degrees in radians
    HalfSize = [pi / 18, pi / 18];  % 10 degrees in radians
    OutUnits = 'deg';
    
    % Expected outputs in degrees
    
    expectedDec1 = (Dec - HalfSize(2)) * 180 / pi;
    expectedDec2 = (Dec + HalfSize(2)) * 180 / pi;
    MaxDec = max(abs(expectedDec1),abs(expectedDec2))*pi/180;
    expectedRA1 = (RA - HalfSize(1) / cos(MaxDec)) * 180 / pi;
    expectedRA2 = (RA + HalfSize(1) / cos(MaxDec)) * 180 / pi;
    
    % Run coo2box function
    [RA1, RA2, Dec1, Dec2] = celestial.coo.coo2box(RA, Dec, HalfSize, OutUnits);
    
    % Verify outputs
    verifyEqual(testCase, RA1, expectedRA1, 'AbsTol', 1e-6);
    verifyEqual(testCase, RA2, expectedRA2, 'AbsTol', 1e-6);
    verifyEqual(testCase, Dec1, expectedDec1, 'AbsTol', 1e-6);
    verifyEqual(testCase, Dec2, expectedDec2, 'AbsTol', 1e-6);
end

function testEdgeCaseAtPole(testCase)
    % Test with Dec near the pole (close to ±90 degrees)
    
    % Define inputs
    RA = pi / 3;  % 60 degrees in radians
    Dec = pi / 2 - 1e-4;  % Close to 90 degrees in radians
    HalfSize = [0.1, 0.1];  % Box half-size in radians
    OutUnits = 'rad';
    
    % Expected outputs
    expectedDec1 = (Dec - HalfSize(2)) ;
    expectedDec2 = (Dec + HalfSize(2)) ;
    MaxDec = max(abs(expectedDec1),abs(expectedDec2));
    expectedRA1 = (RA - HalfSize(1) / cos(MaxDec)) ;
    expectedRA2 = (RA + HalfSize(1) / cos(MaxDec));
    
    % Run coo2box function
    [RA1, RA2, Dec1, Dec2] = celestial.coo.coo2box(RA, Dec, HalfSize, OutUnits);
    
    % Verify outputs
    verifyEqual(testCase, RA1, expectedRA1, 'AbsTol', 1e-6);
    verifyEqual(testCase, RA2, expectedRA2, 'AbsTol', 1e-6);
    verifyEqual(testCase, Dec1, expectedDec1, 'AbsTol', 1e-6);
    verifyEqual(testCase, Dec2, expectedDec2, 'AbsTol', 1e-6);
end

function testRelativeResults(testCase)
    % Test function output relative to precomputed expected results
    
    % Define tolerance for comparison
    tolerance = 1e-6;
    
    % Retrieve expected results from setupOnce
    expectedResults = testCase.TestData.expectedResults;
    
    % Loop over each test case stored in expected_results_coo2box.mat
    for i = 1:length(expectedResults)
        RA = expectedResults(i).RA;
        Dec = expectedResults(i).Dec;
        HalfSize = expectedResults(i).HalfSize;
        OutUnits = expectedResults(i).OutUnits;
        
        expectedRA1 = expectedResults(i).RA1;
        expectedRA2 = expectedResults(i).RA2;
        expectedDec1 = expectedResults(i).Dec1;
        expectedDec2 = expectedResults(i).Dec2;
        
        % Run coo2box function
        [RA1, RA2, Dec1, Dec2] = celestial.coo.coo2box(RA, Dec, HalfSize, OutUnits);
        
        % Verify each output with expected results
        verifyEqual(testCase, RA1, expectedRA1, 'AbsTol', tolerance);
        verifyEqual(testCase, RA2, expectedRA2, 'AbsTol', tolerance);
        verifyEqual(testCase, Dec1, expectedDec1, 'AbsTol', tolerance);
        verifyEqual(testCase, Dec2, expectedDec2, 'AbsTol', tolerance);
    end
end
