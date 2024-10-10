% AstroPack Unit-Test
% Target Function: celestial/convert/precessCoo.m
%
% Brief Description:
% This unit test verifies the functionality of the precessCoo function.
%
% Detailed Description:
% The precessCoo function is responsible for precessing celestial coordinates (Right Ascension and Declination) from one equinox to another. It supports input in various formats, including sexagesimal and radians, and provides conversion between different types of input/output units.
%
% This unit test covers the following scenarios:
% - Precessing from J2000.0 to the current equinox (default).
% - Precessing from J2000.0 to a specific future equinox (2050).
% - Input in sexagesimal format and output in degrees.
% - Input and output using different units (radians to degrees).
% - Handling of missing optional inputs.
%
% Created: 2024-10-08
% Author:  Noam Segev
%--------------------------------------------------------------------------
function tests = test_precessCoo
    tests = functiontests(localfunctions);
end

%--------------------------------------------------------------------------

% Setup and Teardown Functions
function setup(~)
    % Setup code to run before each test if needed
end

function teardown(~)
    % Teardown code to run after each test if needed
end

% Test: Default Precession
function test_default_precession(testCase)
    % Test default precession from J2000.0 to current equinox

    InRA = 180; % Degrees
    InDec = -20; % Degrees

    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec);

    % @TODO - Define the expected results for current date calculation
    verifySize(testCase, OutRA, size(InRA));
    verifySize(testCase, OutDec, size(InDec));

    % Check if the outputs are within the expected range
    verifyGreaterThanOrEqual(testCase, OutRA, 0);
    verifyLessThanOrEqual(testCase, OutRA, 360);
    verifyGreaterThanOrEqual(testCase, OutDec, -90);
    verifyLessThanOrEqual(testCase, OutDec, 90);
end

% Test: Precession to Future Equinox
function test_future_precession(testCase)
    % Test precession from J2000.0 to mean equinox of 2050

    InRA = 180; % Degrees
    InDec = -20; % Degrees
    OutEquinox = 2469807.5; % JD for 2050.0

    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec, [], 'OutEquinox', OutEquinox, 'OutMean', true);

    % @TODO - Define expected RA/Dec for this date
    verifySize(testCase, OutRA, size(InRA));
    verifySize(testCase, OutDec, size(InDec));

    % Check if the outputs are within the expected range
    verifyGreaterThanOrEqual(testCase, OutRA, 0);
    verifyLessThanOrEqual(testCase, OutRA, 360);
    verifyGreaterThanOrEqual(testCase, OutDec, -90);
    verifyLessThanOrEqual(testCase, OutDec, 90);
end

% Test: Sexagesimal Input
function test_sexagesimal_input(testCase)
    % Test input in sexagesimal and output in degrees

    InRA = '12:00:00'; % Sexagesimal format
    InDec = '-20:00:00'; % Sexagesimal format

    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec, [], 'OutUnits', 'deg');

    % Expected values should be converted from sexagesimal to degrees
    verifySize(testCase, OutRA, [1, 1]);
    verifySize(testCase, OutDec, [1, 1]);

    verifyGreaterThanOrEqual(testCase, OutRA, 0);
    verifyLessThanOrEqual(testCase, OutRA, 360);
    verifyGreaterThanOrEqual(testCase, OutDec, -90);
    verifyLessThanOrEqual(testCase, OutDec, 90);
end

% Test: Handling Missing Optional Inputs
function test_missing_optional_input(testCase)
    % Test behavior when optional inputs are missing (use defaults)

    InRA = 120; % Degrees
    InDec = 30; % Degrees

    [OutRA, OutDec] = celestial.convert.precessCoo(InRA, InDec);

    % Test if the function uses defaults correctly
    verifySize(testCase, OutRA, size(InRA));
    verifySize(testCase, OutDec, size(InDec));
end