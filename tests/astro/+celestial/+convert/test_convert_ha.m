function tests = test_convert_ha
% TestStruct2KeyVal
% Function-based unit tests for the struct2keyval function.
% This function returns a test suite containing multiple test functions,
% each of which tests a different aspect of the struct2keyval function.
%
% Author: Noam Segev

tests = functiontests(localfunctions);
end

% Test Functions

function testDefaultConversion(testCase)
% Test: Default conversion from H.A. to R.A.
% @TODO - Modify In and JD values to realistic test case data
In = 15;            % Input in degrees
JD = 2451545;       % Julian Date

% Run the function and assert the expected output
Out = celestial.convert.convert_ha(In, JD);

% @TODO - Verify with expected output
expectedOut = -59.5429301;  % @TODO - Replace with correct expected value
testCase.verifyEqual(Out, expectedOut, 'AbsTol', 1e-6);
end

function testCustomUnits(testCase)
% Test: Conversion with custom input and output units
In = 15;                  % Input H.A. in degrees
JD = 2451545;             % Julian Date
Args.InUnits = 'deg';     % @Object - Args structure: Input in degrees
Args.OutUnits = 'rad';    % Output in radians

% Run the function
Out = celestial.convert.convert_ha(In, JD, 'OutUnits',Args.OutUnits);

% Expected result calculated manually or from reference
expectedOut = -1.03922017742367;        % @TODO - Replace with actual expected value

% Validate the result (with some tolerance for floating-point precision)
testCase.verifyEqual(Out, expectedOut, 'AbsTol', 1e-6);
end

function testLongitudeConversion(testCase)
% Test: Conversion with different longitude settings
In = 15;                % Input H.A. in degrees
JD = 2451545;           % Julian Date
Args.Long = 45;         % Observer's longitude in degrees
Args.LongUnits = 'deg'; % Input longitude units in degrees

% Run the function
Out = celestial.convert.convert_ha(In, JD,'LongUnits',Args.LongUnits);

% @TODO - Verify with expected output
expectedOut = -59.5429301512128;       % @TODO - Replace with actual expected value
testCase.verifyEqual(Out, expectedOut, 'AbsTol', 1e-6);
end

function testOutRangePi(testCase)
% Test: Output range in [-pi, pi]
In = 10;               % Input H.A. in degrees
JD = 2451545;           % Julian Date
Args.OutRange = 'pi';   % Output range between -pi to pi

% Run the function
Out = celestial.convert.convert_ha(In, JD, 'OutRange',Args.OutRange);

% Expected output manually calculated for this test case
expectedOut = -54.5429301512128;      % @TODO - Replace with correct expected value

% Validate the result
testCase.verifyEqual(Out, expectedOut, 'AbsTol', 1e-6);
end

function testOutRange2Pi(testCase)
% Test: Output range in [0, 2*pi]
In = 10;               % Input H.A. in degrees
JD = 2451545;           % Julian Date
Args.OutRange = '2pi';  % Output range between 0 and 2*pi

% Run the function
Out = celestial.convert.convert_ha(In, JD, 'OutRange',Args.OutRange);

% Expected output manually calculated
expectedOut = 305.457069848787;      % @TODO - Replace with correct expected value

% Validate the result
testCase.verifyEqual(Out, expectedOut, 'AbsTol', 1e-6);
end


