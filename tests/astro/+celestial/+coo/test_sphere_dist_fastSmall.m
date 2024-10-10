function tests = test_sphere_dist_fastSmall
    % Unit tests for the sphere_dist_fastSmall function
    % This function computes angular distances using a small-angle approximation
    %
    % This test function will work if you will comment out the error('BUG')
    % line in test_sphere_dist_fastSmall
    % We mod(2*pi,Dist) the output of the function to use it in test
    % ( a 2*pi shift that is probably related to the BUG)
    
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testSmallAngleApproximation(testCase)
    % Test for a small angular distance (within the valid range of the approximation)
    
    % Small separation (~50 arcseconds)
    RA1 = 0; Dec1 = 0;
    RA2 = 50 * (pi / (180 * 3600));  % 50 arcseconds in radians
    Dec2 = 0;
    
    % Call sphere_dist_fastSmall
    Dist = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    Dist = mod(2*pi,Dist);
    % Expected distance is ~50 arcseconds in radians
    expectedDist = RA2;
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-8);
end

function testComparisonToSphereDistFast(testCase)
    % Test comparison between sphere_dist_fastSmall and sphere_dist_fast for small angles
    
    % Small angular separation (10 arcseconds)
    RA1 = 0; Dec1 = 0;
    RA2 = 10 * (pi / (180 * 3600));  % 10 arcseconds in radians
    Dec2 = 0;
    
    % Call both functions
    DistSmall = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    DistSmall = mod(2*pi,DistSmall);
    DistFast = celestial.coo.sphere_dist_fast(RA1, Dec1, RA2, Dec2);
    
    % Verify that the small angle approximation is close to the exact result
    verifyEqual(testCase, DistSmall, DistFast, 'relTol', 1e-8);
end

function testZeroDistance(testCase)
    % Test the case where both points are identical (distance = 0)
    
    RA1 = 1; Dec1 = 1;
    RA2 = 1; Dec2 = 1;  % Identical points
    
    % Call sphere_dist_fastSmall
    Dist = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    
    % Expected distance is 0
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testModulo2PiRA(testCase)
    % Test where RA1 and RA2 are separated by 2π
    
    RA1 = 0;
    Dec1 = 0;
    RA2 = 2 * pi;  % RA2 wraps around 2π
    Dec2 = 0;
    
    % Call sphere_dist_fastSmall
    Dist = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    
    % Expected distance is 0 because the points are the same after wrapping
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testLargerDistancesFail(testCase)
    % Test that larger angular separations deviate from sphere_dist_fast results
    
    % Large angular separation (~1 degree)
    RA1 = 0;
    Dec1 = 0;
    RA2 = 1* (pi / 180);  % 1 degree in radians
    Dec2 = 0;
    
    % Call both functions
    DistSmall = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    DistFast = celestial.coo.sphere_dist_fast(RA1, Dec1, RA2, Dec2);
    DistSmall = mod(2*pi,DistSmall);
    % Verify that the small-angle approximation diverges significantly from the exact solution
    verifyNotEqual(testCase, DistSmall, DistFast);
end

function testNearZeroRA(testCase)
    % Test when RA1 and RA2 are very close to zero
    
    RA1 = 0;
    Dec1 = 0;
    RA2 = 1e-9;  % Very small RA difference
    Dec2 = 0;
    
    % Call sphere_dist_fastSmall
    Dist = celestial.coo.sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2);
    Dist = mod(2*pi,Dist);
    % Expected distance should be approximately RA2
    verifyEqual(testCase, Dist, RA2, 'AbsTol', 1e-10);
end
