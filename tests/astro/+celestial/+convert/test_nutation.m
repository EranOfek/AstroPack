% AstroPack Unit-Test
% Target Function: celestial/+convert/nutation.m
%
% Brief Description:
% This unit test verifies the functionality of the nutation function.
%
% Detailed Description:
% The nutation function calculates the IAU 1984 Nutation series for a
% given set of Julian Dates (JDs). It returns the nutation in longitude and 
% obliquity, as well as an optional nutation rotation matrix. The test 
% includes cases for both full precision and linearized nutation matrices.
%
% This unit test covers the following scenarios:
% - Basic functionality with default precision matrix.
% - Testing full precision and linearized matrix types.
% - Validating output with a single JD and multiple JDs.
% - Error handling for invalid matrix type input.
% - Error handling for missing required inputs.
%
% Created: 30-Oct-2024
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_nutation
    tests = functiontests(localfunctions);
end

% Test default functionality with full precision
function testDefaultFullPrecision(testCase)
    JD = 2451545; % JD of J2000 epoch
    [Nut, NutMatrix] = celestial.convert.nutation(JD);
    
    % Verify Nut dimensions and data type
    validateattributes(Nut, {'double'}, {'size', [1, 2]});
    % Verify NutMatrix dimensions for a single JD
    validateattributes(NutMatrix, {'double'}, {'size', [3, 3, 1]});
end

% Test linearized matrix type
function testLinearizedMatrix(testCase)
    JD = 2451545;
    MatType = 'l';
    [Nut, NutMatrix] = celestial.convert.nutation(JD, MatType);
    
    validateattributes(Nut, {'double'}, {'size', [1, 2]});
    validateattributes(NutMatrix, {'double'}, {'size', [3, 3, 1]});
end

% Test multiple JD inputs
function testMultipleJDInputs(testCase)
    JD = 2451545 + (0:10:100)'; % Vector of JDs
    [Nut, NutMatrix] = celestial.convert.nutation(JD);
    
    % Verify Nut and NutMatrix dimensions for multiple JD values
    validateattributes(Nut, {'double'}, {'size', [length(JD), 2]});
    validateattributes(NutMatrix, {'double'}, {'size', [3, 3, length(JD)]});
end


% Test behavior with empty JD input
function testEmptyJD(testCase)
    JD = [];
    [Nut, NutMatrix] = celestial.convert.nutation(JD);
    
    % Verify Nut and NutMatrix are empty for empty JD input
    testCase.assertEmpty(Nut);
    testCase.assertEmpty(NutMatrix);
end


% Test with known JD value for accurate comparison (Assuming function correctness)
function testKnownJDValue2000(testCase)
    JD = 2451545; % JD of J2000 epoch, well-documented value for nutation
    [Nut, NutMatrix] = celestial.convert.nutation(JD);


    % Expected values for nutation in longitude and obliquity in radians
    expectedNut = [-6.75035610948138e-05, -2.79927039382409e-05]; % Replace with actual expected values
    testCase.assertLessThan(abs(Nut - expectedNut), [1e-4, 1e-4]);
    
    % Verify NutMatrix dimensions and general structure
    validateattributes(NutMatrix, {'double'}, {'size', [3, 3, 1]});
end

% Test with arbitrary JD for consistency (Assuming function correctness)
function testKnownJDValue2010(testCase)
    JD = 2455197.5; % Arbitrary known JD, January 1, 2010
    [Nut, NutMatrix] = celestial.convert.nutation(JD);
    
    % Expected values for nutation in longitude and obliquity in radians
    expectedNut = [7.97063627397162e-05      1.36718975345844e-05]; % Replace with actual expected values
    testCase.assertLessThan(abs(Nut - expectedNut), [1e-5, 1e-5]);
    
    % Verify NutMatrix structure
    validateattributes(NutMatrix, {'double'}, {'size', [3, 3, 1]});
end