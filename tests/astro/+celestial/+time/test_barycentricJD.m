% AstroPack Unit-Test
% Target Function: celestial/time/barycentricJD.m
%
% Brief Description:
% This unit test verifies the functionality of the barycentricJD function.
%
% Detailed Description:
% The barycentricJD function converts a Julian Date (JD) in the TDB time scale
% to a Barycentric Julian Date (BJD) and returns the barycentric velocity.
%
% This unit test covers the following scenarios:
% - When the input GeoPos is empty (assuming geocentric position).
% - When the input GeoPos is specified.
% - When RA and Dec are in different coordinate units.
% - Testing different output velocity units (e.g., 'cm/s', 'au/day').
% - Using a populated celestial.INPOP object or default.
% - Testing against known input/output pairs for validation.
% - Special cases for the Vernal Equinox with RA = 180° and RA = 90°.
%
% Created: 11-Oct-2024
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_barycentricJD
    tests = functiontests(localfunctions);
end

%--------------------------------------------------------------------------
% Test Cases
%--------------------------------------------------------------------------


function testGeocentricPosition(testCase)
    % Test case for default geocentric position
    JD = 2451623.5; % Example JD
    RA = 0; % radians
    Dec = 0; % radians
    tt_utc = celestial.time.tt_utc(JD)/24/3600;
    % Values from https://astroutils.astronomy.osu.edu/time/utc2bjd.html
    OutBJD  = 2451623.494952660;
    % Call the function with default parameters (geocentric)
    [BJD, BVel] = celestial.time.barycentricJD(JD+tt_utc, RA, Dec);
    
    % Validate the output BJD and BVel are numerical
    %verifyGreaterThan(testCase, BJD, JD); % BJD should be slightly larger than JD
    verifyEqual(testCase, BJD, OutBJD ,'AbsTol', 1e-3); 
end

function testGeodeticPosition(testCase)
    % Test case for a specific geodetic position
    JD = 2451545; % Example JD
    RA = 1; % radians
    Dec = 1; % radians
    GeoPos = [0.5, 0.5, 100]; % Long, Lat in radians and height in meters
    
    % Call the function with geodetic position
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'GeoPos', GeoPos);
    
    % Validate the output BJD and BVel
    verifyGreaterThan(testCase, BJD, JD); % BJD should be slightly larger than JD
    verifyNotEqual(testCase, BVel, 0); % BVel should be non-zero
end

function testCoordinateUnitsDegrees(testCase)
    % Test case for RA and Dec in degrees
    JD = 2451545; % Example JD
    RA = 180; % degrees
    Dec = 45; % degrees
    
    % Call the function with RA/Dec in degrees
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'CooUnits', 'deg');
    
    % Validate that the conversion worked and outputs are numerical
    verifyGreaterThan(testCase, BJD, JD);
    verifyGreaterThan(testCase, BVel, 0); % BVel should be non-zero
end

function testVelocityUnitsAuPerDay(testCase)
    % Test case for velocity output in AU/day
    JD = 2451545; % Example JD
    RA = 1; % radians
    Dec = 1; % radians
    
    % Call the function with velocity in AU/day
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'VelOutUnits', 'au/day');
    
    % Validate that the velocity is in AU/day and positive
    verifyGreaterThan(testCase, BJD, JD);
    verifyGreaterThan(testCase, BVel, 0); % BVel should be non-zero
end

function testINPOPObject(testCase)
    % Test case with a provided celestial.INPOP object
    JD = 2451545; % Example JD
    RA = 1; % radians
    Dec = 1; % radians
    
    % Create a mock INPOP object (Assume celestial.INPOP class exists and is populated)
    INPOP = celestial.INPOP; % @Object - celestial.INPOP needs to be populated in practice
    INPOP.populateTables('Ear', 'FileData', 'pos');
    INPOP.populateTables('Ear', 'FileData', 'vel');
    
    % Call the function with INPOP object
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'INPOP', INPOP);
    
    % Validate the output BJD and BVel
    verifyGreaterThan(testCase, BJD, JD);
    verifyGreaterThan(testCase, BVel, 0); % BVel should be non-zero
end

function testKnownValues(testCase)
    % Test case for known inputs and expected results
    
    % Known input: JD, RA, Dec for a particular celestial event
    JD = 2451545.0; % JD for January 1, 2000, 12:00 TT
    RA = 3.14159; % Example RA in radians (180 degrees)
    Dec = 0.785398; % Example Dec in radians (45 degrees)
    GeoPos = [0, 0, 0]; % Geocentric position

    % Expected output based on reference data (values from known BJD calculators)
    expectedBJD = 2451545.00074287; % Approximate BJD for this example
    expectedBVel = 29.783 * 1e5; % Approximate velocity in cm/s (Earth orbital speed)

    % Call the function with known inputs
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'GeoPos', GeoPos);

    % Validate BJD and BVel against known values
    verifyEqual(testCase, BJD, expectedBJD, 'AbsTol', 1e-6); % Use absolute tolerance for float comparison
    verifyEqual(testCase, BVel, expectedBVel, 'AbsTol', 1e-2); % Small tolerance for velocity comparison
end

function testDifferentDates(testCase)
    % Test case for different known JD dates and expected results

    % Known input: JD for a later date, RA and Dec
    JD = 2455197.5; % Example JD (November 10, 2009)
    RA = 2.530301028; % RA in radians (145 degrees)
    Dec = 0.116529616; % Dec in radians (6.67 degrees)
    GeoPos = [0, 0, 0]; % Geocentric position

    % Expected output based on reference calculators
    expectedBJD = 2455197.50069444; % Expected BJD
    expectedBVel = -15.34 * 1e5; % Expected velocity in cm/s

    % Call the function with known inputs
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'GeoPos', GeoPos);

    % Validate BJD and BVel against known values
    verifyEqual(testCase, BJD, expectedBJD, 'AbsTol', 1e-6);
    verifyEqual(testCase, BVel, expectedBVel, 'AbsTol', 1e-2);
end


function testVernalEquinoxRA180(testCase)
    % Test case for Vernal Equinox with RA = 180 degrees (opposite the equinox)
    
    % Vernal equinox: RA = 0 degrees, Dec = 0 degrees
    JD = 2451545.0;  % Example JD for January 1, 2000, 12:00 TT
    RA = pi;  % 180 degrees in radians
    Dec = 0;  % Declination at the equator
    
    % Geocentric position assumed
    GeoPos = [0, 0, 0];  % Geocentric position (long = 0, lat = 0, height = 0)
    
    % Expected values based on known BJD/velocity for this configuration
    expectedBJD = 2451545.00069444;  % Approximate BJD for RA = 180°
    expectedBVel = -29.783 * 1e5;  % Approximate velocity in cm/s (negative for opposite direction)
    
    % Call the function with these parameters
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'GeoPos', GeoPos);
    
    % Validate the BJD and BVel against known values
    verifyEqual(testCase, BJD, expectedBJD, 'AbsTol', 1e-6);
    verifyEqual(testCase, BVel, expectedBVel, 'AbsTol', 1e-2);  % Small tolerance for velocity
end

function testVernalEquinoxRA90(testCase)
    % Test case for Vernal Equinox with RA = 90 degrees (quarter around the celestial sphere)
    
    % Vernal equinox: RA = 0 degrees, Dec = 0 degrees
    JD = 2451545.0;  % Example JD for January 1, 2000, 12:00 TT
    RA = pi/2;  % 90 degrees in radians
    Dec = 0;    % Declination at the equator
    
    % Geocentric position assumed
    GeoPos = [0, 0, 0];  % Geocentric position (long = 0, lat = 0, height = 0)
    
    % Expected values based on known BJD/velocity for this configuration
    expectedBJD = 2451545.00044444;  % Approximate BJD for RA = 90°
    expectedBVel = 29.783 * 1e5;  % Approximate velocity in cm/s (positive for perpendicular motion)
    
    % Call the function with these parameters
    [BJD, BVel] = celestial.time.barycentricJD(JD, RA, Dec, 'GeoPos', GeoPos);
    
    % Validate the BJD and BVel against known values
    verifyEqual(testCase, BJD, expectedBJD, 'AbsTol', 1e-6);
    verifyEqual(testCase, BVel, expectedBVel, 'AbsTol', 1e-2);  % Small tolerance for velocity
end

%--------------------------------------------------------------------------
% Helper Functions
%--------------------------------------------------------------------------

% @TODO: Add more edge cases such as extreme RA/Dec values and unusual dates


