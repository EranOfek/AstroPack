function tests = test_sphere_offset
    % Unit tests for the sphere_offset function, which calculates the offset needed
    % to move between two points on the celestial sphere.
    %
    % Author: Yarin Shani
    %

    tests = functiontests(localfunctions);
end

%% Test Functions

function testZeroOffset(testCase)
    % Test if the function returns zero offsets when both points are identical.
    RA1 = pi / 4;  % RA in radians
    Dec1 = pi / 6; % Dec in radians
    RA2 = RA1;
    Dec2 = Dec1;
    Units = 'rad';
    OffsetType = 'rd';
    
    % Call sphere_offset function
    [OffsetLong, OffsetLat, Dist, PA] = celestial.coo.sphere_offset(RA1, Dec1, RA2, Dec2, Units, OffsetType);
    
    % Verify that offsets, distance, and position angle are zero
    verifyEqual(testCase, OffsetLong, 0, 'AbsTol', 1e-9, 'Offset in longitude should be zero when points are identical.');
    verifyEqual(testCase, OffsetLat, 0, 'AbsTol', 1e-9, 'Offset in latitude should be zero when points are identical.');
    verifyEqual(testCase, Dist, 0, 'AbsTol', 1e-9, 'Distance should be zero when points are identical.');
    verifyEqual(testCase, PA, nan, 'AbsTol', 1e-9, 'Position angle should be zero when points are identical.');
end

function testSmallOffsetRD(testCase)
    % Test if the function correctly calculates small offsets with 'rd' offset type.
    RA1 = 0;  % RA in radians
    Dec1 = 0; % Dec in radians
    RA2 = RA1 + 1e-3;
    Dec2 = Dec1 + 1e-3;
    Units = 'rad';
    OffsetType = 'rd';
    
    % Call sphere_offset function
    [OffsetLong, OffsetLat, Dist, PA] = celestial.coo.sphere_offset(RA1, Dec1, RA2, Dec2, Units, OffsetType);
    
    % Verify the calculated offsets
    verifyEqual(testCase, OffsetLong, 1e-3, 'relTol', 1e-6, 'Incorrect longitude offset for small movement.');
    verifyEqual(testCase, OffsetLat, 1e-3, 'relTol', 1e-9, 'Incorrect latitude offset for small movement.');
end

function testSmallOffsetDR(testCase)
    % Test if the function correctly calculates small offsets with 'dr' offset type.
    RA1 = 0;  % RA in radians
    Dec1 = 0; % Dec in radians
    RA2 = RA1 + 1e-3;
    Dec2 = Dec1 + 1e-3;
    Units = 'rad';
    OffsetType = 'dr';
    
    % Call sphere_offset function
    [OffsetLong, OffsetLat, Dist, PA] = celestial.coo.sphere_offset(RA1, Dec1, RA2, Dec2, Units, OffsetType);
    
    % Verify the calculated offsets
    verifyEqual(testCase, OffsetLat, 1e-3, 'AbsTol', 1e-9, 'Incorrect latitude offset for small movement.');
    verifyEqual(testCase, OffsetLong, 1e-3, 'AbsTol', 1e-9, 'Incorrect longitude offset for small movement.');
end

function testDifferentUnits(testCase)
    % Test if the function correctly handles different units (degrees vs radians).
    RA1 = 0;     % RA in degrees
    Dec1 = 0;    % Dec in degrees
    RA2 = 1;     % RA in degrees
    Dec2 = 1;    % Dec in degrees
    Units = 'deg';
    OffsetType = 'rd';
    
    % Call sphere_offset function
    [OffsetLong, OffsetLat, Dist, PA] = celestial.coo.sphere_offset(RA1, Dec1, RA2, Dec2, Units, OffsetType);
    
    % Verify the calculated offsets
    expectedOffsetLong = deg2rad(1);  % Offset in radians
    expectedOffsetLat = deg2rad(1);   % Offset in radians
    verifyEqual(testCase, OffsetLong, expectedOffsetLong, 'AbsTol', 1e-6, 'Incorrect longitude offset for degree input.');
    verifyEqual(testCase, OffsetLat, expectedOffsetLat, 'AbsTol', 1e-6, 'Incorrect latitude offset for degree input.');
end

function testPoleMovement(testCase)
    % Test if the function correctly handles movement involving the poles.
    RA1 = 0;      % RA at prime meridian
    Dec1 = pi / 2 - 1e-6; % Dec very close to the North Pole
    RA2 = pi;     % RA opposite side
    Dec2 = pi / 2; % Dec exactly at the North Pole
    Units = 'rad';
    OffsetType = 'rd';
    
    % Call sphere_offset function
    [OffsetLong, OffsetLat, Dist, PA] = celestial.coo.sphere_offset(RA1, Dec1, RA2, Dec2, Units, OffsetType);
    
    % Verify the offsets and distance
    verifyEqual(testCase, OffsetLat, 1e-6, 'AbsTol', 1e-9, 'Incorrect latitude offset near the pole.');
    verifyEqual(testCase, OffsetLong, pi, 'AbsTol', 1e-9, 'Incorrect longitude offset when moving across the pole.');
    verifyGreaterThanOrEqual(testCase, Dist, 0, 'Distance should be non-negative.');
end