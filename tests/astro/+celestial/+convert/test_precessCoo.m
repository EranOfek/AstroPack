% AstroPack Unit-Test
% Target Function: celestial/+convert/precessCoo.m
%
% Brief Description:
% This unit test verifies the functionality of the precessCoo function.
%
% Detailed Description:
% The precessCoo function precesses Right Ascension (RA) and Declination
% (Dec) coordinates from an input equinox to an output equinox, supporting 
% various formats and units. This test file checks that precession is 
% correctly applied for different input and output equinoxes, units, and 
% coordinate formats.
%
% This unit test covers the following scenarios:
% - Basic functionality with default parameters.
% - Handling of sexagesimal (string) and radian inputs.
% - Precessing to/from different input and output equinoxes.
% - Changing units between degrees and radians.
% - Edge case handling for invalid and empty inputs.
%
% Created: 30-Oct-2024
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_precessCoo
    tests = functiontests(localfunctions);
end

% Test default functionality with J2000 input and current output equinox
function testDefaultJ2000ToNow(testCase)
    InRA = 180; % Degrees
    InDec = -20; % Degrees
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec);
    
    % Verify outputs have correct dimensions and types
    validateattributes(OutRA, {'double'}, {'scalar'});
    validateattributes(OutDec, {'double'}, {'scalar'});
end

% Test input in sexagesimal format with conversion to degrees
function testSexagesimalInput(testCase)
    InRA = '12:00:00'; % 180 degrees
    InDec = '-20:00:00'; % -20 degrees
    JD = 2460673.82548842;
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec,'OutEquinox',JD);
    
    % Check that output matches expected degrees within tolerance
    testCase.assertEqual(OutRA, 180.319426022627, 'AbsTol', 1e-3);
    testCase.assertEqual(OutDec, -20.1391114870079, 'AbsTol', 1e-3);
end

% Test precession to a specified output equinox
function testToSpecificOutEquinox(testCase)
    InRA = 180; % Degrees
    InDec = -20; % Degrees
    
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec, [], 'OutEquinox',2462500);
    
    % Validate that output is numeric and scalar
    validateattributes(OutRA, {'double'}, {'scalar'});
    validateattributes(OutDec, {'double'}, {'scalar'});
end

% Test with radian input and output units
function testRadianInputOutput(testCase)
    InRA = pi; % Radians (180 degrees)
    InDec = -pi/9; % Radians (-20 degrees)
    Args  = {'InUnits', 'rad','OutUnits', 'rad'};
    
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec, [], Args{:});
    
    % Check output in radians
    testCase.assertGreaterThan(abs(OutRA), 0);
    testCase.assertGreaterThan(abs(OutDec), 0);
end

% Test precession with input as cosine direction vectors
function testCosineDirectionInput(testCase)
    % Define cosine direction vector for 180 deg RA, -20 deg Dec
    CD1 = cosd(180) * cosd(-20);
    CD2 = sind(180) * cosd(-20);
    CD3 = sind(-20);
    [OutRA, OutDec] = celestial.convert.precessCoo(CD1, CD2, CD3);
    
    % Verify outputs have correct dimensions and types
    validateattributes(OutRA, {'double'}, {'scalar'});
    validateattributes(OutDec, {'double'}, {'scalar'});
end


% Test empty input arrays for RA and Dec
function testEmptyInputRAandDec(testCase)
    InRA = [];
    InDec = [];
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec);
    
    % Expect empty output arrays
    testCase.assertEmpty(OutRA);
    testCase.assertEmpty(OutDec);
end

% Test known conversion from J2000 to JD of January 1, 2050
function testKnownJ2000To2050Conversion(testCase)
    InRA = 180; % Degrees
    InDec = -20; % Degrees
    % 2462500; % JD for January 1, 2050
    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec, [], 'OutEquinox',2462500);
    
    % Expected approximate values for RA and Dec in 2050
    expectedRA = 180.389018091026; % Replace with exact known values
    expectedDec = -20.1688659111515; % Replace with exact known values
    testCase.assertEqual(OutRA, expectedRA, 'AbsTol', 1e-3);
    testCase.assertEqual(OutDec, expectedDec, 'AbsTol',  1e-3);
end