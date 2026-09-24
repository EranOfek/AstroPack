function tests = test_sphere_dist_fastThresh
    % Unit tests for the sphere_dist_fastThresh function
    % This function calculates angular distances with a declination threshold
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicFunctionality(testCase)
    % Test for two points within the declination threshold
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.001;  % Second point with small angular separation
    Thresh = 0.01;  % Threshold larger than Dec difference
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected distance is the angular distance (since Dec difference is below the threshold)
    expectedDist = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

function testDeclinationAboveThreshold(testCase)
    % Test when declination difference exceeds the threshold
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.02;  % Second point with large angular separation
    Thresh = 0.01;  % Threshold smaller than Dec difference
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected result is Inf because Dec difference exceeds threshold
    verifyEqual(testCase, Dist, Inf, 'relTol', 1e-10);
end

function testEdgeCaseThreshold(testCase)
    % Test the edge case where Dec difference equals the threshold + 1e-16
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.01;  % Second point with angular separation
    Thresh = 0.01+1e-16;  % Threshold exactly equal to Dec difference
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected result: Still compute the angular distance since it's within the threshold
    expectedDist = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

function testSinglePointInput(testCase)
    % Test with scalar inputs for RA1, Dec1, RA2, Dec2 (single point)
    
    RA1 = 1; Dec1 = 1;
    RA2 = 2; Dec2 = 1.09;
    Thresh = 0.1;
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected distance is the angular distance since it's within the threshold
    expectedDist = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
end

function testMultiplePointsVector(testCase)
    % Test with multiple vector inputs for RA1, Dec1, RA2, Dec2
    
    RA1 = [0; 0]; Dec1 = [0; 0];  % Two points
    RA2 = [pi/4; pi/4]; Dec2 = [0.005; 0.02];  % Two comparison points
    Thresh = 0.01;  % Threshold
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected result: First distance is within threshold, second should be Inf
    expectedDist1 = acos(sin(Dec1(1)).*sin(Dec2(1)) + cos(Dec1(1)).*cos(Dec2(1)).*cos(RA1(1)-RA2(1)));
    verifyEqual(testCase, Dist(1), expectedDist1, 'relTol', 1e-10);
    verifyEqual(testCase, Dist(2), Inf, 'relTol', 1e-10);
end

function testComparisonWithSphereDistFast(testCase)
    % Compare results with sphere_dist_fast when within threshold
    
    RA1 = 0; Dec1 = 0;
    RA2 = pi/4; Dec2 = 0.005;
    Thresh = 0.01;  % Within threshold
    
    % Call both functions
    DistThresh = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    DistFast = celestial.coo.sphere_dist_fast(RA1, Dec1, RA2, Dec2);
    
    % Verify that both results match
    verifyEqual(testCase, DistThresh, DistFast, 'relTol', 1e-10);
end

function testPerformanceLargeInputs(testCase)
    % Test with large input arrays to assess performance and functionality
    
    N = 1e6;  % One million points
    RA1 = rand(N, 1) * 2 * pi;
    Dec1 = rand(N, 1) * pi - pi/2;  % Dec from -pi/2 to pi/2
    RA2 = rand(N, 1) * 2 * pi;
    Dec2 = rand(N, 1) * pi - pi/2;
    Thresh = 0.01;  % Threshold
    
    % Call sphere_dist_fastThresh
    Dist = celestial.coo.sphere_dist_fastThresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Verify size of output is correct
    verifySize(testCase, Dist, [N, 1]);
end
