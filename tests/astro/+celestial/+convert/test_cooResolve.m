
% AstroPack Function Test
% Target Function: +celestial/+convert/ cooResolve.m
%
% Brief Description:
% This function test verifies the functionality of the cooResolve function, which resolves object names
% into RA/Dec or converts RA/Dec between different units.
%
% Created: 2024-10-08
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_cooResolve
    tests = functiontests(localfunctions);
end

% Setup function (optional, for initializing any required data)
%function setup(testCase)
    % @TODO - Add any setup code, such as loading necessary data or configurations.
%end

% Test converting RA/Dec from degrees to radians
function testConvertDegreesToRadians(testCase)
    InRA = 180;  % Input RA in degrees
    InDec = 45;  % Input Dec in degrees
    Args = {'InUnits', 'deg', 'OutUnits', 'rad'};  % Convert from degrees to radians

    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});

    % Verify the output is in radians
    verifyEqual(testCase, OutRA, pi, 'AbsTol', 1e-10);
    verifyEqual(testCase, OutDec, pi/4, 'AbsTol', 1e-10);
end

% Test resolving an object name using Simbad
function testResolveObjectName(testCase)
    ObjectName = 'M31';  % Andromeda Galaxy
    Args = {'Server', @VO.name.server_simbad};  % Use Simbad to resolve object name

    [OutRA, OutDec] =celestial.convert.cooResolve(ObjectName, Args{:});

    verifyEqual(testCase, OutRA, 10.6847, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutDec, 41.269, 'AbsTol', 1e-3);
end

function testConvertSexagesimalToDegrees(testCase)
    InRA = '00h 42m 44.3s';  % Sexagesimal RA
    InDec = '+41d 16'' 9"';  % Sexagesimal Dec
    Args = {'InUnits', 'sex', 'OutUnits', 'deg'};  % Convert from sexagesimal to degrees

    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args);

    % Verify the output is in decimal degrees
    verifyEqual(testCase, OutRA, 10.6847, 'AbsTol', 1e-4);
    verifyEqual(testCase, OutDec, 41.269, 'AbsTol', 1e-4);
end


% Optional teardown function
function teardown(testCase)
    % @TODO - Add any teardown code, such as closing connections or cleaning up data.
end
