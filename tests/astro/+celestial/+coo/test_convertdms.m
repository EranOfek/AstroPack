function tests = test_convertdms
    % Unit tests for the celestial.coo.convertdms function, which converts
    % between various representations of coordinates and time.
    %
    % Author: Yarin Shani
    % Date: 13 Nov
    %
    % This test suite validates the function’s capability to handle conversions
    % between radians, degrees, sexagesimal formats, fractions, and ensures
    % auto-detection works as expected.
    
    tests = functiontests(localfunctions);
end

%% Test Setup Function

function setupOnce(testCase)
    % Load expected results from file only once for all tests
    dataFilePath = fullfile('~', 'matlab', 'AstroPack', 'tests', 'relativeData', 'expected_convertdms_results.mat');
    testData = load(dataFilePath);
    testCase.TestData.expectedResults = testData.expectedResults;
end

%% Test Functions

function testRadiansToDegrees(testCase)
    % Test conversion from radians to degrees
    
    InData = pi / 4;  % 45 degrees in radians
    InType = 'r';
    OutType = 'd';
    
    expectedOut = 45;  % Expected output in degrees
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testDegreesToRadians(testCase)
    % Test conversion from degrees to radians
    
    InData = 180;  % 180 degrees
    InType = 'd';
    OutType = 'r';
    
    expectedOut = pi;  % Expected output in radians
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testHoursToRadians(testCase)
    % Test conversion from hours to radians
    
    InData = 1;  % 1 hour
    InType = 'h';
    OutType = 'r';
    
    expectedOut = pi / 12;  % Expected output in radians (15 degrees)
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testSexagesimalStringToRadians(testCase)
    % Test conversion from sexagesimal string to radians
    
    InData = '15:30:00';  % 15.5 hours in HH:MM:SS format
    InType = 'SH';
    OutType = 'r';
    
    expectedOut = (15.5 * 15) * pi / 180;  % Expected output in radians
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testSexagesimalStringDegrees(testCase)
    % Test conversion from sexagesimal degrees string to radians
    
    InData = '+30:15:00';  % 30.25 degrees in DD:MM:SS format
    InType = 'SD';
    OutType = 'r';
    
    expectedOut = 30.25 * pi / 180;  % Expected output in radians
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testFractionToRadians(testCase)
    % Test conversion from fractional value to radians
    
    InData = 0.5;  % 50% of the way through the circle
    InType = 'f';
    OutType = 'r';
    
    expectedOut = pi;  % Expected output in radians (half circle)
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end

function testAutoDetectHours(testCase)
    % Test automatic detection with 'gH' input type for hours
    
    InData = [10, 30, 0];  % 10 hours, 30 minutes
    InType = 'gH';
    OutType = 'r';
    
    expectedOut = (10.5 * 15) * pi / 180;  % Expected output in radians
    
    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output
    verifyEqual(testCase, OutData, expectedOut, 'RelTol', 1e-6);
end




function testSiderealToSolarTime(testCase)
    % Convert sidereal hour to radians and compare with a solar time scaling factor
    
    InData = 1;  % 1 sidereal hour
    InType = 'h';
    OutType = 'r';
    
    % Expected result
    expectedOut = (pi / 12) ;

    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);
    
    % Verify output with sidereal-to-solar scaling
    verifyEqual(testCase, OutData, expectedOut, 'AbsTol', 1e-6);
end



function testEquinoxConversion(testCase)
    % Test conversions for known equinox coordinates in degrees

    InData = [0; 23.5];  % RA at Vernal Equinox, Dec at Summer Solstice
    InType = 'd';
    OutType = 'r';

    % Expected values in radians
    expectedOut = [0; 23.5 * pi / 180];

    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);

    % Verify equinox conversion results
    verifyEqual(testCase, OutData, expectedOut, 'AbsTol', 1e-6);
end

function testNegativeDeclination(testCase)
    % Test conversion for negative Declination values

    InData = '-45:00:00';  % -45 degrees
    InType = 'SD';
    OutType = 'r';

    % Expected output in radians
    expectedOut = -pi / 4;

    % Run convertdms function
    OutData = celestial.coo.convertdms(InData, InType, OutType);

    % Verify correct negative conversion
    verifyEqual(testCase, OutData, expectedOut, 'AbsTol', 1e-6);
end


function testRelativeResults(testCase)
    % Test function output relative to precomputed expected results
    
    % Define tolerance for comparison
    tolerance = 1e-6;
    
    % Retrieve expected results from setupOnce
    expectedResults = testCase.TestData.expectedResults;
    
    % Loop over each test case stored in expected_results_convertdms.mat
    for i = 1:length(expectedResults)
        InData = expectedResults(i).InData;
        InType = expectedResults(i).InType;
        OutType = expectedResults(i).OutType;
        
        expectedOut = expectedResults(i).ExpectedOut;
        
        % Run convertdms function
        OutData = celestial.coo.convertdms(InData, InType, OutType);
        
        % Verify output matches expected results within tolerance
        verifyEqual(testCase, OutData, expectedOut, 'AbsTol', tolerance, ...
            sprintf('Relative test %d failed for InData: %s, InType: %s, OutType: %s', i, mat2str(InData), InType, OutType));
    end
end