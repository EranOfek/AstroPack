% AstroPack Unit-Test
% Target Function: celestial/convert/refractedCoo.m
%
% Brief Description:
% This unit test verifies the functionality of the refractedCoo function,
% which applies atmospheric refraction to Right Ascension (RA) and Declination (Dec)
% coordinates, adjusting them based on observing parameters such as position,
% temperature, pressure, and wavelength.
%
% Detailed Description:
% The refractedCoo function computes the corrected RA and Dec coordinates, 
% along with unrefracted altitude, refraction angle, parallactic angle, 
% and shifts in RA and Dec due to atmospheric refraction.
%
% This unit test covers the following scenarios:
% - Default settings with known RA/Dec values.
% - Different wavelength, temperature, and pressure settings.
% - Verification of outputs for boundary and edge cases (e.g., altitude < 0).
% - Error handling for invalid input types.
% - Correct conversions for output units.
%
% Created: 2025-01-06
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_refractedCoo
    tests = functiontests(localfunctions);
end

% Test with Default Parameters and Known Coordinates
function testDefaultParameters(testCase)
    % Define input RA and Dec in degrees
    InRA = 0;        % RA at 0 degrees
    InDec = 80;      % Dec at 80 degrees
    JD = 2460673.82548842;  

    % Run function with default parameters
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec, 'JD', JD);

    % Verify altitude and refraction are within expected ranges
    verifyEqual(testCase, Alt, 26.2350423817904,'AbsTol',1e-3); % Adjusted minimum
    verifyEqual(testCase, Refraction, 0.0323799332452728,'AbsTol',1e-4);
end

% Test with Different Wavelengths and Atmospheric Conditions
function testDifferentAtmosphericConditions(testCase)
    % Define input RA/Dec in degrees
    InRA = 180;
    InDec = -45;
    JD = 2460673.82548842;
    % Define varied atmospheric conditions
    Args = {'Wave', 6000, 'Temp', 10, 'Pressure', 780, 'Pw', 5};

    % Run function with varied conditions
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec, 'JD',JD,Args{:});

    % Check refraction outputs for expected behavior
    verifyEqual(testCase, Refraction, -0.110251,'AbsTol',1e-4);
    verifyEqual(testCase, Alt, -8.541607,'AbsTol',1e-3);
    %verifyGreaterThan(testCase, Alt, -90);
    %verifyLessThanOrEqual(testCase, Alt, 90);
end

% Test for Out-of-Bounds Altitude (Altitude < 0)
function testNegativeAltitude(testCase)
    % RA and Dec values near the horizon
    InRA = 180;   
    InDec = -90;
    JD = 2460673.82548842;

    % Run function and check outputs
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec);

    % Verify altitude and refraction angle
    verifyLessThan(testCase, Alt, 0);
end

% Test for Output Unit Conversion
function testOutputUnitConversion(testCase)
    % Define input RA/Dec in degrees
    InRA = 123.5;
    InDec = 45.3;

    % Set 'OutUnits' to 'rad' for conversion verification
    Args = {'OutUnits', 'rad'};

    % Run function
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec, Args{:});

    % Verify that the outputs are in radians
    verifyLessThan(testCase, OutRA, 2 * pi);
    verifyLessThan(testCase, OutDec, pi / 2);
    verifyLessThan(testCase, Alt, pi / 2);
end

% Test Invalid Input Type for RA/Dec
function testInvalidInputType(testCase)
    % Provide an invalid RA input type (string that is not a name)
    InRA = 'invalid_input';
    InDec = 45.3;

    % Verify that an error is thrown for invalid RA input
    verifyError(testCase, @() celestial.convert.refractedCoo(InRA, InDec), ...
        'MATLAB:textio:dataread:TroubleReading'); % Adjusted to actual error ID
end

% Test Empty Inputs for RA and Dec (e.g., object name input)
function testEmptyInputs(testCase)
    % Provide empty RA/Dec to test for object name resolution handling
    InRA = 'Sirius';  % Valid object name, assumed to resolve coordinates
    InDec = [];

    % Run function to verify it handles name-based resolution
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec);

    % Verify outputs have valid sizes and values
    verifyNotEmpty(testCase, OutRA);
    verifyNotEmpty(testCase, OutDec);
    verifyGreaterThanOrEqual(testCase, Alt, -90);
    verifyLessThanOrEqual(testCase, Alt, 90);
end

% Test Boundary JD Values for Refraction Adjustment
function testBoundaryJDValues(testCase)
    % RA and Dec at mid-latitude position
    InRA = 30;
    InDec = 60;

    % Boundary JD values (historical and far future)
    Args = {'JD', 1000000};  % Very early date

    % Run function with early date
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec, Args{:});

    % Check reasonable output range for boundary conditions
    verifyGreaterThanOrEqual(testCase, Alt, -90);
    verifyLessThanOrEqual(testCase, Alt, 90);

    Args = {'JD', 4000000};  % Far future date
    [OutRA, OutDec, Alt, Refraction, ParAng, DelAlpha, DelDelta] = ...
        celestial.convert.refractedCoo(InRA, InDec, Args{:});

    verifyGreaterThanOrEqual(testCase, Alt, -90);
    verifyLessThanOrEqual(testCase, Alt, 90);
end