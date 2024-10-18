function tests = test_sky_area_above_am
    % Unit tests for the sky_area_above_am function in AstroPack
    %
    % 
    %
    % Description:
    % This unit test verifies the correct operation of the `sky_area_above_am` 
    % function assuming the results are correct. The tests try to cover both typical and edge cases
    % under different inputs, including varying Julian Dates, latitudes, 
    % airmass thresholds, and visibility times. testRegressionCase, loads
    % saved results from 18-10-24 (in
    % ~/matlab/AstroPack/tests/relativeData/expected_<function
    % name>_results.mat)
    %
    % Inputs Tested:
    %   - JD (Julian Date)
    %   - Lat (Latitude) [radians]
    %   - AM (Airmass)
    %   - TimeVis (Visibility Time) [hours]
    %
    % Outputs Verified:
    %   - SkyArea [deg^2]
    %   - Length of Night (LON) [fraction of a day]
    %
    % The tests include checks for boundary conditions like polar latitudes,
    % very high or low airmasses, and various night lengths.
    %
    % Example: results = runtests('test_sky_area_above_am')
    % Author: Yarin Shani
    % Date: 2024-10-18

    tests = functiontests(localfunctions); % Creates a test suite using local test functions
end

%% Setup and Teardown
function setupOnce(testCase)
    % Setup common values once before all tests run
    testCase.TestData.DefaultJD = 2451545;          % Example Julian Date (J2000)
    testCase.TestData.DefaultLat = 33 * pi / 180;   % Latitude: 33 degrees in radians
    testCase.TestData.DefaultAM = 1.6;              % Default airmass threshold
    testCase.TestData.DefaultTimeVis = 1;           % Time visibility: 1 hour
end

function teardownOnce(~)
    % Teardown function: nothing needed for now, but can be used for cleanup
end

%% Test Functions

function testNominalCase(testCase)
    % Test nominal input values for sky_area_above_am function
    JD = testCase.TestData.DefaultJD;
    Lat = testCase.TestData.DefaultLat;
    AM = testCase.TestData.DefaultAM;
    TimeVis = testCase.TestData.DefaultTimeVis;
    
    % Call the function with typical values
    [SkyArea, LON] = celestial.coo.sky_area_above_am(JD, Lat, AM, TimeVis);
    
    % Verify that the output is within expected ranges
    verifyGreaterThanOrEqual(testCase, SkyArea, 0, 'Sky area should be non-negative');
    verifyGreaterThanOrEqual(testCase, LON, 0, 'Length of night should be non-negative');
    verifyLessThanOrEqual(testCase, LON, 1, 'Length of night should be <= 1 (a fraction of the day)');
end

function testEdgeCaseLatitude(testCase)
    % Test extreme latitudes (near the poles)
    JD = testCase.TestData.DefaultJD;
    AM = testCase.TestData.DefaultAM;
    TimeVis = testCase.TestData.DefaultTimeVis;
    
    % Max latitude avb (air mass ~ 1.08)
    Lat = pi /2.653280;  % 90 degrees (North Pole)
    [SkyArea, LON] = celestial.coo.sky_area_above_am(JD, Lat, AM, TimeVis);
    verifyGreaterThanOrEqual(testCase, SkyArea, 0, 'Sky area should be non-negative at the North Pole');
    
    % Test horizon
    Lat = 0;  % -90 degrees (South Pole)
    [SkyArea, LON] = celestial.coo.sky_area_above_am(JD, Lat, AM, TimeVis);
    verifyGreaterThanOrEqual(testCase, SkyArea, 0, 'Sky area should be non-negative at the South Pole');
end

function testEdgeCaseAirmass(testCase)
    % Test an edge case for high airmass values (near the horizon)
    JD = testCase.TestData.DefaultJD;
    Lat = testCase.TestData.DefaultLat;
    TimeVis = testCase.TestData.DefaultTimeVis;
    
    % Set an extreme airmass value, close to horizon
    AM = 3.0;  % Large airmass, close to the horizon
    [SkyArea, LON] = celestial.coo.sky_area_above_am(JD, Lat, AM, TimeVis);
    
    % Verify sky area is within reasonable limits
    verifyGreaterThanOrEqual(testCase, SkyArea, 0, 'Sky area should be non-negative for high airmass');
    verifyLessThanOrEqual(testCase, SkyArea, 41253, 'Sky area should be <= total sky area (41253 deg^2)');
end



function testMinimalInputs(testCase)
    % Test with minimal input (no arguments) to check default behavior
    [SkyArea, LON] = celestial.coo.sky_area_above_am();
    
    % Check that output is reasonable with default input
    verifyGreaterThanOrEqual(testCase, SkyArea, 0, 'Sky area should be non-negative with default input');
    verifyGreaterThanOrEqual(testCase, LON, 0, 'Length of night should be non-negative');
end


function testRegressionCase(testCase)
    % Regression test to verify the function's consistency with stored results
    %
    % This test compares the current output of the `sky_area_above_am` function
    % to the previously saved results stored in 'expected_sky_area_results.mat'
    % in the 'tests/data/' directory. This ensures that future changes to the
    % function don't alter its behavior unexpectedly.

    % Path to the 'expected_sky_area_results.mat' file (relative to the current working directory)
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_sky_area_above_am_results.mat');
    
    % Load the expected results
    loadedData = load(dataFilePath);
    expectedResults = loadedData.expectedResults;

    % Loop through each set of stored results and compare
    for i = 1:numel(expectedResults)
        % Extract stored inputs
        JD = expectedResults(i).JD;
        Lat = expectedResults(i).Lat;
        AM = expectedResults(i).AM;
        TimeVis = expectedResults(i).TimeVis;

        % Call the function with the same inputs
        [currentSkyArea, currentLON] = celestial.coo.sky_area_above_am(JD, Lat, AM, TimeVis);

        % Compare the current output to the stored expected results
        verifyEqual(testCase, currentSkyArea, expectedResults(i).SkyArea, 'relTol', 1e-6, ...
            sprintf('Sky area result for JD: %.0f, Lat: %.2f, AM: %.2f, TimeVis: %.2f has changed unexpectedly', JD, Lat, AM, TimeVis));
        verifyEqual(testCase, currentLON, expectedResults(i).LON, 'relTol', 1e-6, ...
            sprintf('Length of night result for JD: %.0f, Lat: %.2f, AM: %.2f, TimeVis: %.2f has changed unexpectedly', JD, Lat, AM, TimeVis));
    end
end
