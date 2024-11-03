function tests = test_proper_motion
    % Unit tests for the proper_motion function, which applies proper motion
    % adjustments to a catalog of celestial sources over a given time span.
    %
    % Author: Yarin Shani
    % Date: 31  October 2024


    % TBD : Add the list of 1000 highest proper motion stars coorfinates
    % between J1950 to J2016 to have a relative test for future versions.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicPropagation(testCase)
    % Test if the function correctly propagates RA and Dec for a given time interval
    
    % Define inputs: initial epoch (J2000) and final epoch (example 2020)
    epoch_initial_RA  = 2433282.5; % J1950 in Julian days
    epoch_initial_Dec = 2433282.5;
    epoch_final       = 2457206.375; % 2015.5 in Julian days

    % Initial RA and Dec in radians   349.72896716, 5.40511585
    RA  = deg2rad(349.720132346704); 
    Dec = deg2rad(5.407205659444788); 
    
    % Proper motion in mas/yr
    PM_RA  = 483.41659019; 
    PM_Dec = -114.86339718;
    Plx    =  29.00319440995681;

    % Call proper_motion function
    [RA_final, Dec_final] = celestial.coo.proper_motion(epoch_final, epoch_initial_RA, epoch_initial_Dec, RA, Dec, PM_RA, PM_Dec);

    % Expected outputs are slightly modified based on PM and delta_T
    expectedRA  = 349.72896715675495;
    expectedDec = 5.405115847648341; 
    
    % Verify outputs are within expected bounds
    verifyEqual(testCase, rad2deg(RA_final),expectedRA, 'RelTol',1e-7);
 
    verifyEqual(testCase, rad2deg(Dec_final),expectedDec, 'RelTol',1e-7);
end

function testDefaultParallaxAndRadialVelocity(testCase)
    % Test if the function uses default parallax and radial velocity when they are omitted
    
    % Define inputs
    epoch_initial = 2451545.0; % J2000
    epoch_final = 2458849.5; % 2020
    RA = deg2rad(253.246163495076);
    Dec = deg2rad(1.83797226553749);
    PM_RA = -5.208;
    PM_Dec = -22.923;
    
    % Call proper_motion without parallax and radial velocity
    [RA_final, Dec_final] = celestial.coo.proper_motion(epoch_final, epoch_initial, epoch_initial, RA, Dec, PM_RA, PM_Dec);
    
    % Verify output
    verifyGreaterThanOrEqual(testCase, RA_final, 0);
    verifyLessThanOrEqual(testCase, RA_final, 2*pi);
    verifyGreaterThanOrEqual(testCase, Dec_final, -pi/2);
    verifyLessThanOrEqual(testCase, Dec_final, pi/2);
end

function testZeroProperMotion(testCase)
    % Test if the function returns the initial coordinates when proper motion is zero
    
    % Define inputs with zero proper motion
    epoch_initial = 2451545.0; % J2000
    epoch_final = 2458849.5; % 2020
    RA = deg2rad(253.246163495076);
    Dec = deg2rad(1.83797226553749);
    PM_RA = 0;
    PM_Dec = 0;
    
    % Call proper_motion function
    [RA_final, Dec_final] = celestial.coo.proper_motion(epoch_final, epoch_initial, epoch_initial, RA, Dec, PM_RA, PM_Dec);
    
    % Verify output matches input (no proper motion applied)
    verifyEqual(testCase, RA_final, RA, 'AbsTol', 1e-6);
    verifyEqual(testCase, Dec_final, Dec, 'AbsTol', 1e-6);
end

function testHighProperMotion(testCase)
    % Test with very high proper motion values to ensure stability
    
    epoch_initial = 2451545.0; % J2000
    epoch_final = 2458849.5; % 2020
    RA = deg2rad(253.246163495076);
    Dec = deg2rad(1.83797226553749);
    
    % Set exaggerated proper motion values
    PM_RA = 10000; % mas/yr
    PM_Dec = 10000; % mas/yr
    
    % Call proper_motion function
    [RA_final, Dec_final] = celestial.coo.proper_motion(epoch_final, epoch_initial, epoch_initial, RA, Dec, PM_RA, PM_Dec);
    
    % Verify output is within valid RA/Dec bounds
    verifyGreaterThanOrEqual(testCase, RA_final, 0);
    verifyLessThanOrEqual(testCase, RA_final, 2*pi);
    verifyGreaterThanOrEqual(testCase, Dec_final, -pi/2);
    verifyLessThanOrEqual(testCase, Dec_final, pi/2);
end

function testDifferentInitialEpochs(testCase)
    % Test if the function handles different initial epochs for RA and Dec
    
    % Inputs with different initial epochs for RA and Dec
    epoch_initial_RA = 2451545.0; % J2000 for RA
    epoch_initial_Dec = 2455000.0; % 2009 for Dec
    epoch_final = 2458849.5; % 2020
    RA = deg2rad(253.246163495076);
    Dec = deg2rad(1.83797226553749);
    PM_RA = -5.208;
    PM_Dec = -22.923;
    
    % Call proper_motion
    [RA_final, Dec_final] = celestial.coo.proper_motion(epoch_final, epoch_initial_RA, epoch_initial_Dec, RA, Dec, PM_RA, PM_Dec);
    
    % Verify outputs
    verifyGreaterThanOrEqual(testCase, RA_final, 0);
    verifyLessThanOrEqual(testCase, RA_final, 2*pi);
    verifyGreaterThanOrEqual(testCase, Dec_final, -pi/2);
    verifyLessThanOrEqual(testCase, Dec_final, pi/2);
end


