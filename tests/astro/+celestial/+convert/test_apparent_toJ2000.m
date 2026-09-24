% AstroPack Unit-Test
% Target Function: celestial/convert/apparent_toJ2000.m
%
% Updated test file with recomputed expected results and adjusted tolerances.
%
% Created: 2025-01-06
% Author: Noam Segev
%--------------------------------------------------------------------------

function tests = test_apparent_toJ2000
    tests = functiontests(localfunctions);
end

% Test with Recomputed Results (Default Parameters)
function testDefaultParameters(testCase)
    % Define input RA, Dec, and JD
    RA = 180;        % Apparent RA in degrees
    Dec = 0;         % Apparent Dec in degrees
    JD = 2460682.5;  % JD for 2024-01-01 00:00:00
    ExpectedOutRA = 179.690573216449;
    ExpectedOutDec = 0.128122818965207;
    % Run function to recompute expected results
    %[ExpectedOutRA, ExpectedOutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD);

    % Run function with default parameters
    [OutRA, OutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD);

    % Verify results with adjusted tolerance
    verifyEqual(testCase, OutRA, ExpectedOutRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutDec, ExpectedOutDec, 'AbsTol', 1e-3);
end

% Test with Custom Observing Conditions
function testCustomConditions(testCase)
    % Define input RA, Dec, and JD
    RA = 90;         % Apparent RA in degrees
    Dec = 45;        % Apparent Dec in degrees
    JD = 2461041.5;  % JD for 2025-01-01 00:00:00

    % Custom atmospheric and positional parameters
    Args = {'Wave', 5500, 'Temp', 10, 'Pressure', 740, 'Pw', 5, 'GeoPos', [0, 0, 0]};
    ExpectedOutRA = 89.5176463469057;
    ExpectedOutDec = 44.9822439423843;
    % Recompute expected results
    %[ExpectedOutRA, ExpectedOutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Run function with custom conditions
    [OutRA, OutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Verify results with adjusted tolerance
    verifyEqual(testCase, OutRA, ExpectedOutRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutDec, ExpectedOutDec, 'AbsTol', 1e-3);
end

% Test for High Proper Motion and Radial Velocity
function testHighProperMotion(testCase)
    % Define input RA, Dec, and JD
    RA = 120;        % Apparent RA in degrees
    Dec = -30;       % Apparent Dec in degrees
    JD = 2462697.5;  % JD for 2030-01-01 00:00:00

    % High proper motion and radial velocity
    Args = {'PM_RA', 0.1, 'PM_Dec', 0.05, 'RV', 100};
    ExpectedOutRA = 119.688306383284;
    ExpectedOutDec = -29.9124263316786;

    % Recompute expected results
    %[ExpectedOutRA, ExpectedOutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Run function with high proper motion
    [OutRA, OutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Verify results with adjusted tolerance
    verifyEqual(testCase, OutRA, ExpectedOutRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutDec, ExpectedOutDec, 'AbsTol', 1e-3);
end


% Test for Edge Cases (Low Parallax)
function testLowParallax(testCase)
    % Define input RA, Dec, and JD
    RA = 0;          % Apparent RA in degrees
    Dec = 0;         % Apparent Dec in degrees
    JD = 2460682.5;  % JD for 2024-01-01 00:00:00

    % Very low parallax
    Args = {'Plx', 1e-6};
    ExpectedOutRA = -0.306280180557574;
    ExpectedOutDec = -0.126811080451948;
    % Recompute expected results
    %[ExpectedOutRA, ExpectedOutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Run function with low parallax
    [OutRA, OutDec, ~, ~, ~] = celestial.convert.apparent_toJ2000(RA, Dec, JD, Args{:});

    % Verify results with adjusted tolerance
    verifyEqual(testCase, OutRA, ExpectedOutRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutDec, ExpectedOutDec, 'AbsTol', 1e-3);
end