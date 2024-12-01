function tests = test_radec2azalt
    % Unit tests for the radec2azalt function
    % This function converts RA/Dec to Az/Alt with additional airmass and parallactic angle calculations
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicConversion(testCase)
    % Test basic RA/Dec to Az/Alt conversion with known values
    
    JD = 2460618.90501378;  % 04-Nov-2024 00:00:00
    RA = 90.398750 ; Dec = 54.283722;  % Example RA/Dec in degrees
    GeoCoo = [30.0491 35.0281];  % Geodetic coordinates [Longitude, Latitude] in degrees
    
    % Call radec2azalt
    [Az, Alt, ~, ~] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected values (replace with values calculated by a reliable reference)
    expectedAz = 246.1;  % Expected azimuth (example value)
    expectedAlt = 35.2;  % Expected altitude (example value)
    
    % Verify Az and Alt are correct
    verifyEqual(testCase, Az, expectedAz, 'RelTol', 1e-6);
    verifyEqual(testCase, Alt, expectedAlt, 'RelTol', 1e-6);
end

function testAirmassCalculation(testCase)
    % Test the airmass calculation for different altitudes
    
    JD = 2451545;
    RA = 10; Dec = 20;
    GeoCoo = [35, 30];
    
    % Call radec2azalt
    [~, Alt, AM, ~] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected airmass using a reliable reference or formula
    expectedAM = celestial.coo.hardie(pi / 2 - Alt);
    
    % Verify airmass is calculated correctly
    verifyEqual(testCase, AM, expectedAM, 'RelTol', 1e-6);
end

function testParallacticAngle(testCase)
    % Test parallactic angle calculation for different values of HA and Dec
    
    JD = 2451545;
    RA = 50; Dec = 30;
    GeoCoo = [35, 45];
    
    % Call radec2azalt
    [~, ~, ~, PA] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected parallactic angle calculated using a reference method
    expectedPA = 15.0;  % Example expected parallactic angle
    
    % Verify PA is calculated correctly
    verifyEqual(testCase, PA, expectedPA, 'RelTol', 1e-6);
end

function testGeodeticCoordinatesError(testCase)
    % Test that the function throws an error if GeoCoo is not provided
    
    JD = 2451545;
    RA = 10; Dec = 20;
    
    % Verify error is thrown
    verifyError(testCase, @() celestial.coo.radec2azalt(JD, RA, Dec), 'MATLAB:expectedArgument');
end

function testInputOutputUnits(testCase)
    % Test the function's handling of different input and output units for RA, Dec, Az, and Alt
    
    JD = 2451545;
    RA = 0.5; Dec = 0.35;  % RA and Dec in radians
    GeoCoo = [0.6, 0.7];  % Geodetic coordinates in radians
    InUnits = 'rad';
    OutUnits = 'deg';
    
    % Call radec2azalt with different units
    [Az, Alt, ~, ~] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Expected Azimuth and Altitude in degrees (conversion from radians)
    expectedAz = rad2deg(Az);  % Conversion from radians
    expectedAlt = rad2deg(Alt);  % Conversion from radians
    
    % Verify output units are correctly handled
    verifyEqual(testCase, Az, expectedAz, 'RelTol', 1e-6);
    verifyEqual(testCase, Alt, expectedAlt, 'RelTol', 1e-6);
end

function testLargeDatasetPerformance(testCase)
    % Test performance and correctness with a large dataset of RA and Dec
    
    JD = repmat(2451545, [1, 1000]);  % Array of Julian dates
    RA = linspace(0, 360, 1000);  % Array of RAs in degrees
    Dec = linspace(-90, 90, 1000);  % Array of Decs in degrees
    GeoCoo = [35, 30];  % Geodetic coordinates in degrees
    
    % Call radec2azalt with large dataset
    [Az, Alt, AM, PA] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Verify output sizes
    verifySize(testCase, Az, size(JD));
    verifySize(testCase, Alt, size(JD));
    verifySize(testCase, AM, size(JD));
    verifySize(testCase, PA, size(JD));
end
