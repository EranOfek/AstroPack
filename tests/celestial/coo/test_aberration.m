function tests = test_aberration
    % Unit tests for the aberration function, which calculates the
    % aberration of celestial coordinates due to the observer's motion.
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicConversion(testCase)
    % Test if the function correctly handles RA/Dec to cosine direction 
    % conversions and back to RA/Dec after applying aberration.
    
    inputRA = celestial.coo.convertdms([4 0 0.0], 'H', 'r');  % RA = 60°
    inputDec = celestial.coo.convertdms([1 30 0 0], 'D', 'R'); % Dec = 30°
    U1 = [inputRA, inputDec];
    V = [30e3, 0, 0]*(24*60*60/(1.496e11));  % Earth's velocity (30 km/s along x-axis) should go to au/day in case of two nargins == 2
    
    % Expected: after applying aberration, the position should shift
    result = celestial.coo.aberration(U1, V);
    
    % Verify output is within valid bounds for RA and Dec
    verifyGreaterThanOrEqual(testCase, result(1), 0);
    verifyLessThanOrEqual(testCase, result(1), 2*pi);
    verifyGreaterThanOrEqual(testCase, result(2), -pi/2);
    verifyLessThanOrEqual(testCase, result(2), pi/2);
end

function testZeroVelocity(testCase)
    % Test if the function returns the same position when observer velocity is zero.
    
    inputRA = celestial.coo.convertdms([4 0 0], 'H', 'r');  % RA = 60°
    inputDec = celestial.coo.convertdms([1 30 0 0], 'D', 'R'); % Dec = 30°
    U1 = [inputRA, inputDec];
    V = [0, 0, 0];    % Zero velocity
    
    % Call aberration function
    result = celestial.coo.aberration(U1, V);
    
    % Verify result is the same as the input (no aberration)
    verifyEqual(testCase, result, U1, 'relTol', 1e-6);
end

function testRandomCoordinates(testCase)
    % Test the aberration function with randomized celestial coordinates 
    % and observer velocities.
    
    for i = 1:100
        inputCoo = [rand(1, 1) * 2*pi rand(1, 1) * 2*pi-pi] ;  % Random RA/Dec between 0 and 360°
        obsVel = rand(1, 3) * 3e6 * convert.units('cm/s','au/day');    % Random velocities up to 30 km/s into au/day
        
        % Call the aberration function
        resultCoo = celestial.coo.aberration(inputCoo, obsVel);
        
        % Verify that the output coordinates are valid
        verifyGreaterThanOrEqual(testCase, resultCoo(1), 0);
        verifyLessThanOrEqual(testCase, resultCoo(1), 2*pi);
        verifyGreaterThanOrEqual(testCase, resultCoo(2), -pi);
        verifyLessThanOrEqual(testCase, resultCoo(2), pi);
    end
end

function testHighVelocity(testCase)
    % Test the function when observer velocity is close to the speed of light.
    
    inputRA = celestial.coo.convertdms([45 0 0], 'H', 'r');  % RA = 45°
    inputDec = celestial.coo.convertdms([1 0 0 0], 'D', 'R');  % Dec = 0°
    U1 = [inputRA, inputDec];
    V = [0.9 * constant.c, 0, 0]*convert.units('cm/s','au/day');  % Velocity close to the speed of light
    
    % Call aberration function
    result = celestial.coo.aberration(U1, V);
    
    % Verify result (numerical stability should be maintained)
    verifyNotEmpty(testCase, result);
    verifyGreaterThanOrEqual(testCase, result(1), 0);
    verifyLessThanOrEqual(testCase, result(1), 2*pi);
    verifyGreaterThanOrEqual(testCase, result(2), -pi);
    verifyLessThanOrEqual(testCase, result(2), pi);
end

function testVelocityFromJD(testCase)
    % Test if the function computes aberation provided Julian Date.
    
    U1 = [1.2, 0.5];  % Random RA/Dec in radians
    JD = celestial.time.julday([13.19, 11, 2028]);  % Input as Julian Date
    
    % Run the aberration function using JD as velocity input
    result = celestial.coo.aberration(U1, JD);
    
    % Verify output is within valid bounds for RA and Dec
    verifyGreaterThanOrEqual(testCase, result(1), 0);
    verifyLessThanOrEqual(testCase, result(1), 2*pi);
    verifyGreaterThanOrEqual(testCase, result(2), -pi/2);
    verifyLessThanOrEqual(testCase, result(2), pi/2);
end

function testUnitConversion(testCase)
    % Test if the function correctly handles unit conversions (SI, CGS, AGD).
    
    U1 = [1.5, 0.3];  % Random RA/Dec in radians
    Vcgs = [30e5, 40e5, 50e5];  % Random velocity [cm/s]
    Vagd = [30e5, 40e5, 50e5].*convert.units('cm/s','au/day');

    % Run with different units
    
    resultCGS = celestial.coo.aberration(U1, Vcgs, 'cgs');
    resultAGD = celestial.coo.aberration(U1, Vagd, 'agd');
    
    % Verify that results are the same
    verifyEqual(testCase, resultCGS, resultAGD, 'relTol', 1e-6);
end

function testCosinedVsRadResults(testCase)
    % Test if the function properly handles mismatched matrix sizes for U1 and V.
    RAD = 180/pi;
    U1 = [cos(30/RAD)*cos(60/RAD), cos(30/RAD)*sin(60/RAD) ,sin(30/RAD)];      % Cosined poisiotn
    V = [30e5 0 0].*convert.units('cm/s','au/day');      % Single velocity row (should be replicated)
    
    % Call aberration function
    resultCosined = celestial.coo.aberration(U1, V);
    
    % Verify output is the same as outtype = 'rad'
    inputRA = celestial.coo.convertdms([4 0 0.0], 'H', 'r');  % RA = 60°
    inputDec = celestial.coo.convertdms([1 30 0 0], 'D', 'R'); % Dec = 30°
    U1 = [inputRA, inputDec];
    V = [30e3, 0, 0]*(24*60*60/(1.496e11));  % Earth's velocity (30 km/s along x-axis) should go to au/day in case of two nargins == 2
    
    % Expected: after applying aberration, the position should shift
    resultRad = celestial.coo.aberration(U1, V);
    resultRad = celestial.coo.cosined(resultRad);



    verifyEqual(testCase, resultCosined, resultRad, 'relTol', 1e-6);
end




function testDifferentEpochs(testCase)
    % Test aberration for the same star at different Julian Dates (epochs)
    
    U1 = [1.5, 0.5];  % RA/Dec in radians
    
    JD1 = celestial.time.julday([1 1 2027]);  % Julian Date for Jan 1, 2023
    JD2 = celestial.time.julday([1 7 2028]);  % Julian Date for July 1, 2023
    
    result1 = celestial.coo.aberration(U1, JD1);
    result2 = celestial.coo.aberration(U1, JD2);
    
    % Verify that the results differ due to the different epochs
    verifyNotEqual(testCase, result1, result2)%
end


function testVelocityAlignedWithStar(testCase)
    % Test aberration when the observer's velocity is aligned with the star's motion
    
    inputRA = celestial.coo.convertdms([0 0 0], 'H', 'r');  % RA = 0°
    inputDec = celestial.coo.convertdms([1 0 0 0], 'D', 'R'); % Dec = 0° (aligned with x-axis)
    U1 = [inputRA, inputDec];
    V = [30e3, 0, 0] * (24 * 60 * 60 / 1.496e11);  % Observer's velocity aligned with the star's direction
    
    result = celestial.coo.aberration(U1, V);
    
    % Verify result remains within valid bounds
    verifyGreaterThanOrEqual(testCase, result(1), 0);
    verifyLessThanOrEqual(testCase, result(1), 2 * pi);
    verifyGreaterThanOrEqual(testCase, result(2), -pi/2);
    verifyLessThanOrEqual(testCase, result(2), pi/2);


end


function testPolePosition(testCase)
    % Test aberration for a star near the celestial pole (Dec = ±90°)
    
    inputRA = celestial.coo.convertdms([0 0 0], 'H', 'r');  % Arbitrary RA
    inputDec = celestial.coo.convertdms([1 90 0 0], 'D', 'R');  % Dec = 90°
    U1 = [inputRA, inputDec];
    V = [30e3, 0, 0] * (24 * 60 * 60 / 1.496e11);  % Convert km/s to AU/day
    
    result = celestial.coo.aberration(U1, V);
    
    % Verify Dec remains within valid limits, RA can vary
    verifyGreaterThanOrEqual(testCase, result(2), -pi/2);
    verifyLessThanOrEqual(testCase, result(2), pi/2);
    verifyGreaterThanOrEqual(testCase, result(2), -pi/2);
    verifyLessThanOrEqual(testCase, result(2), pi/2);
end