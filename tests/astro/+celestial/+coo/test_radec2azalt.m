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
    % ###### comparing this function code to 
    
    JD  = 2460618.90501378;  % 04-Nov-2024 00:00:00
    RA  = 90.398750; Dec = 54.283722;  % Example RA/Dec in degrees
    GeoCoo = [35.0281 30.0491];  % Geodetic coordinates [Longitude, Latitude] in degrees
    
    % Call radec2azalt
    [Az, Alt, ~, ~] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected values (replace with values calculated by a reliable reference)
    expectedAz = 335.38805833333333;  % Expected azimuth (example value)
    expectedAlt = 2.979130555555556;  % Expected altitude (example value)
    
    % Verify Az and Alt are correct ## Cannot go beyond 1e-4
    verifyEqual(testCase, Az, expectedAz, 'RelTol', 1e-4);
    verifyEqual(testCase, Alt, expectedAlt, 'RelTol', 1e-4);
end

function testAirmassCalculation(testCase)
    % Test the airmass calculation for different altitudes
    
    JD = 2451545;
    RA = 10; Dec = 20;
    GeoCoo = [35, 30];
    
    % Call radec2azalt
    [~, Alt, AM, ~] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected airmass using a reliable reference or formula
    expectedAM = celestial.coo.hardie(pi / 2 - Alt*pi/180);
    
    % Verify airmass is calculated correctly
    verifyEqual(testCase, AM, expectedAM, 'RelTol', 1e-6);
end

function testParallacticAngle(testCase)
    % Test parallactic angle calculation for different values of HA and Dec
    
    JD = 2451545;
    RA = 50; Dec = 30;
    GeoCoo = [35,30];
    
    % Call radec2azalt
    [~, ~, ~, PA] = celestial.coo.radec2azalt(JD, RA, Dec, 'GeoCoo', GeoCoo);
    
    % Expected parallactic angle calculated using a reference method
    expectedPA =  -1.0747;  %rad
    
    % Verify PA is calculated correctly
    verifyEqual(testCase, PA*pi/180, expectedPA, 'RelTol', 1e-4);
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
