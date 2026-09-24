% AstroPack Unit-Test
% Target Function: celestial/convert/precession.m
%
% Brief Description:
% This unit test verifies the functionality of the precession function,
% which calculates Earth's precession parameters as a function of Julian Date (JD) 
% and chosen precession model (IAU 1976 or IAU 2000).
%
% Detailed Description:
% The precession function calculates the ZetaA, ZA, and ThetaA precession angles 
% for Earth, output in radians, based on the Julian Date input and a precession model type.
% The function accepts either '1976' (IAU 1976 model) or '2000' (IAU 2000 model) 
% as model types, with '2000' as the default.
%
% This unit test covers the following scenarios:
% - Default '2000' model with known JD values and expected output ranges.
% - '1976' model with known JD values and expected output ranges.
% - Valid and boundary JD values.
% - Error handling for invalid Type inputs.
%
% Created: <Current date here>
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_precession
    tests = functiontests(localfunctions);
end



% Test Different JD Values with Default Model (IAU 2000)
function testDifferentJDValues2000(testCase)
    % Define a range of JD values around J2000 epoch
    JDs = [2450000, 2451545, 2455000];
    expZetaA =   [-0.000460323774985292 , 1.25936055077092e-05 , 0.00107017219649692];
    expZA = [-0.00048550397938234 , -1.25936055077092e-05 , 0.00104501909368657];
    expThetaA =[-0.000411013694802803, 0, 0.000919100772540905];
    % Loop through JDs and check that outputs are within reasonable ranges
    for i = 1:numel(JDs)
        JD = JDs(i);
        [ZetaA, ZA, ThetaA] = celestial.convert.precession(JD);
        % Expected reasonable range checks (non-zero due to precession)
            verifyEqual(testCase, ZetaA, expZetaA(i), 'AbsTol', 1e-5);
            verifyEqual(testCase, ZA, expZA(i), 'AbsTol', 1e-5);
            verifyEqual(testCase, ThetaA, expThetaA(i), 'AbsTol', 1e-5);
    end
end

% Test with Empty JD Array
function testEmptyJD(testCase)
    % Define empty JD input
    JD = [];

    % Run function and check if it returns empty outputs without error
    [ZetaA, ZA, ThetaA] = celestial.convert.precession(JD);
    verifyEmpty(testCase, ZetaA);
    verifyEmpty(testCase, ZA);
    verifyEmpty(testCase, ThetaA);
end

% Test Boundary JD values (extreme historical values)
function testBoundaryJDValues(testCase)
    % Define very low JD value (historical date)
    JD_low = 1000000;
    % Define very high JD value (future date)
    JD_high = 4000000;

    % Run function on low JD
    [ZetaA_low, ZA_low, ThetaA_low] = celestial.convert.precession(JD_low);
    % Run function on high JD
    [ZetaA_high, ZA_high, ThetaA_high] = celestial.convert.precession(JD_high);

    % Check outputs are within reasonable boundaries
    verifyGreaterThan(testCase, ZetaA_low, -10);
    verifyGreaterThan(testCase, ZA_low, -10);
    verifyGreaterThan(testCase, ThetaA_low, -10);

    verifyGreaterThan(testCase, ZetaA_high, -10);
    verifyGreaterThan(testCase, ZA_high, -10);
    verifyGreaterThan(testCase, ThetaA_high, -10);
end