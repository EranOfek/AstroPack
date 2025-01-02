
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
    verifyEqual(testCase, OutRA, pi, 'AbsTol', 1e-8);
    verifyEqual(testCase, OutDec, pi/4, 'AbsTol', 1e-8);
end

function testConvertRadiansToDegrees(testCase)
    InRA = pi;       % radians
    InDec = pi/4;    % radians
    Args = {'InUnits', 'rad', 'OutUnits', 'deg'};
    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});
    % Expected output in degrees
    expOutRA = 180;        % pi radians = 180 degrees
    expOutDec = 45;        % pi/4 radians = 45 degrees
    verifyEqual(testCase, OutRA, expOutRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expOutDec, 'AbsTol', 1e-6);
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
    InRA = '01:00:00.00';  % Sexagesimal RA
    InDec = '+30:16:90';  % Sexagesimal Dec
    Args = {'InUnits', 'sex', 'OutUnits', 'deg'};  % Convert from sexagesimal to degrees

    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});

    % Verify the output is in decimal degrees
    
    verifyEqual(testCase, OutRA, 15, 'AbsTol', 1e-4);
    verifyEqual(testCase, OutDec, 30.2916666666667, 'AbsTol', 1e-4);
end

function testObjectNameResolution(testCase)
    InRA = 'm31';      % Example object name
    InDec = [];        % Leave Dec empty to indicate name resolution
    % Mock server function for testing purpose
    mockServer = @(name, unit) deal(10, -10);  % Replace with real resolver in actual tests
    Args = {'Server', mockServer, 'OutUnits', 'deg'};
    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});
    % Expected mock coordinates
    expOutRA = 10;
    expOutDec = -10;
    verifyEqual(testCase, OutRA, expOutRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expOutDec, 'AbsTol', 1e-6);
end

% Edge Case - Empty inputs
function testEmptyInputs(testCase)
    InRA = [];
    InDec = [];
    Args = {'InUnits', 'deg', 'OutUnits', 'deg'};
    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});
    verifyTrue(testCase, isnan(OutRA));
    verifyTrue(testCase, isnan(OutDec));
end


% Invalid object name with name server
function testInvalidObjectName(testCase)
    InRA = 'unknown_object';
    InDec = [];
    % Mock server that returns NaN for unknown objects
    mockServer = @(name, unit) deal(NaN, NaN);
    Args = {'Server', mockServer, 'OutUnits', 'deg'};
    [OutRA, OutDec] = celestial.convert.cooResolve(InRA, InDec, Args{:});
    verifyTrue(testCase, isnan(OutRA));
    verifyTrue(testCase, isnan(OutDec));
end



% Optional teardown function
function teardown(testCase)
    % @TODO - Add any teardown code, such as closing connections or cleaning up data.
end
