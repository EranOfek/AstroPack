% AstroPack Unit-Test
% Target Function: celestial.convert.j2000_toApparent.m
%
% Brief Description:
% This unit test verifies the functionality of the `j2000_toApparent` function.
%
% Detailed Description:
% The `j2000_toApparent` function calculates the apparent position of a star based on its
% J2000.0 right ascension and declination, and additional factors such as proper motion, parallax,
% aberration of light, precession, nutation, and refraction. Given Julian Date (JD) and optional 
% arguments, it returns the star's apparent coordinates (RA, Dec), altitude, refraction angle, 
% and auxiliary parameters.
%
% This unit test covers the following scenarios:
% - Default settings for basic RA/Dec conversion to apparent coordinates.
% - Conversion with specified input units (degrees or radians).
% - Handling of proper motion, parallax, and radial velocity.
% - Applying atmospheric refraction.
% - Edge cases with extreme values or missing values for inputs.
%
% Created: 2024-10-30
% Author:  Noam Segev
%--------------------------------------------------------------------------

function tests = test_j2000_toApparent
    tests = functiontests(localfunctions);
end

% Test default functionality with basic RA/Dec input
function testBasicConversion(testCase)
    RA = 180;       % Right Ascension in degrees
    Dec = 0;        % Declination in degrees
    JD = celestial.time.julday([1, 1, 2024]);  % Julian Date for 2024-01-01

    % Expected output is approximate since actual apparent coordinates 
    % depend on complex calculations
    [OutRA, OutDec, Alt, Refraction, Aux] = celestial.convert.j2000_toApparent(RA, Dec, JD);
    
    % Verify outputs are numeric and within expected ranges
    verifyGreaterThanOrEqual(testCase, OutRA, 0);
    verifyLessThanOrEqual(testCase, OutRA, 360);
    verifyGreaterThanOrEqual(testCase, OutDec, -90);
    verifyLessThanOrEqual(testCase, OutDec, 90);
    verifyTrue(testCase, isnumeric(Alt));
    verifyTrue(testCase, isnumeric(Refraction));
    verifyTrue(testCase, isstruct(Aux));
end

% Test conversion with specified input and output units (radians)
function testUnitsConversion(testCase)
    RA = pi;       % Right Ascension in radians (180 degrees)
    Dec = pi/4;    % Declination in radians (45 degrees)
    JD = celestial.time.julday([1, 1, 2024]);

    Args = {'InUnits', 'rad', 'OutUnits', 'rad'};
    [OutRA, OutDec, ~, ~] = celestial.convert.j2000_toApparent(RA, Dec, JD, Args{:});

    % Verify outputs are in radians and within expected ranges
    verifyGreaterThanOrEqual(testCase, OutRA, 0);
    verifyLessThanOrEqual(testCase, OutRA, 2*pi);
    verifyGreaterThanOrEqual(testCase, OutDec, -pi/2);
    verifyLessThanOrEqual(testCase, OutDec, pi/2);
end

% Test proper motion, parallax, and radial velocity effects
function testMotionParallaxRV(testCase)
    RA = 180;
    Dec = 0;
    JD = celestial.time.julday([1, 1, 2024]);
    
    % Custom Args for motion and distance effects
    Args = {'PM_RA', 10, 'PM_Dec', -5, 'Plx', 0.5, 'RV', 20};
    [OutRA, OutDec, ~, ~, Aux] = celestial.convert.j2000_toApparent(RA, Dec, JD,Args{:});

    % Verify Aux structure contains expected fields
    verifyTrue(testCase, isfield(Aux, 'RA_J2000'));
    verifyTrue(testCase, isfield(Aux, 'Dec_J2000'));
    verifyTrue(testCase, isnumeric(OutRA));
    verifyTrue(testCase, isnumeric(OutDec));
end

% Test application of atmospheric refraction
function testRefractionApplication(testCase)
    RA = 150;
    Dec = 20;
    JD = celestial.time.julday([1, 1, 2024]);
    
    Args = {'ApplyRefraction', true, 'Temp', 20, 'Pressure', 760, 'Pw', 8};
    [~, ~, ~, Refraction] = celestial.convert.j2000_toApparent(RA, Dec, JD, Args{:});

    % Check that refraction angle is a valid numeric value
    verifyTrue(testCase, isnumeric(Refraction));
    verifyTrue(testCase, ~isnan(Refraction));
    verifyGreaterThanOrEqual(testCase, Refraction, 0);
end

% Test edge case: extremely high declination
function testHighDeclination(testCase)
    RA = 0;               % RA at celestial pole
    Dec = 89.999;         % Near pole in Dec
    JD = celestial.time.julday([1, 1, 2024]);

    [~, OutDec, ~, ~] = celestial.convert.j2000_toApparent(RA, Dec, JD);
    
    % Check that output Declination remains close to high input value
    verifyGreaterThanOrEqual(testCase, OutDec, 80);
    verifyLessThanOrEqual(testCase, OutDec, 90);
end






% Test with specific inputs to validate outputs as baseline
function testBaselineExample1(testCase)
    RA = 180;       
    Dec = 0;        
    JD = celestial.time.julday([1, 1, 2024]);
    % We assume that the function is good 30.10.24
    TestDec = -0.120273116581701;
    TestRA = 180.290973922606;
    [OutRA, OutDec, Alt, Refraction] = celestial.convert.j2000_toApparent(RA, Dec, JD);
    
    % Assuming current output values are baseline correct:
    verifyEqual(testCase, OutRA,TestRA, 'AbsTol', 1e-5);
    verifyEqual(testCase, OutDec,TestDec, 'AbsTol', 1e-5);
    verifyTrue(testCase, Alt >= 0);  % Altitude should be >=0 when RA=180, Dec=0
    verifyTrue(testCase, Refraction >= 0);
end


function testBaselineNoAberrationRefraction(testCase)
    RA = 150;      
    Dec = -30;     
    JD = celestial.time.julday([12, 6, 2024]);
    
    Args= {'ApplyAberration', false, 'ApplyRefraction', false};
    [OutRA, OutDec, Alt, Refraction] = celestial.convert.j2000_toApparent(RA, Dec, JD, Args{:});
    
    % Expected baseline values without aberration and refraction:
    % We assume that the function is good 30.10.24

    verifyEqual(testCase, round(OutRA, 5), 150.271989345698, 'AbsTol', 1e-5);
    verifyEqual(testCase, round(OutDec, 5), -30.1165021234075, 'AbsTol', 1e-5);
    verifyEqual(testCase, Alt, NaN);  % No refraction implies NaN for Alt
    verifyEqual(testCase, Refraction, NaN);
end