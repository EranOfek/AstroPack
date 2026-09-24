function tests = test_sphere_dist_fast_thresh
    % Unit tests for the sphere_dist_fast_thresh function
    % This function calculates angular distances and position angles with a threshold
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicFunctionality(testCase)
    % Test for a basic calculation within the threshold
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.001;  % Second point with small angular separation
    Thresh = pi/2;  % Threshold larger than the distance
    
    % Call sphere_dist_fast_thresh
    [Dist, PA] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected distance and PA
    expectedDist = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));
    dRA = RA1 - RA2;
    SinPA = sin(dRA) .* cos(Dec2) ./ sin(expectedDist);
    CosPA = (sin(Dec2).*cos(Dec1) - cos(Dec2).*sin(Dec1).*cos(dRA)) ./ sin(expectedDist);
    expectedPA = atan2(SinPA, CosPA);
    
    % Verify distance and position angle
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
    verifyEqual(testCase, PA-2*pi, expectedPA, 'relTol', 1e-10);
end

function testThresholdExceeded(testCase)
    % Test PA when the distance exceeds the threshold
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.05;  % Second point with larger separation
    Thresh = 0.01;  % Threshold smaller than the distance
    
    % Call sphere_dist_fast_thresh
    [~, PA] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected result: distances exceeding the threshold should result in NaN

    verifyTrue(testCase, isnan(PA));
end

function testEdgeCaseThreshold(testCase)
    % Test when the distance is exactly equal to the threshold
    
    RA1 = 0; Dec1 = 0;  % First point
    RA2 = pi/4; Dec2 = 0.02;  % Second point with exact threshold separation
    Thresh = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));  % Exact threshold
    
    % Call sphere_dist_fast_thresh
    [Dist, PA] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Verify that distance and position angle are still calculated
    verifyEqual(testCase, Dist, Thresh, 'relTol', 1e-10);
    verifyNotEmpty(testCase, PA);
end

function testMultiplePointsVector(testCase)
    % Test with multiple vector inputs
    
    RA1 = [0; 0]; Dec1 = [0; 0];  % Two points
    RA2 = [pi/4; pi/4]; Dec2 = [0.005; 0.02];  % Two comparison points
    Thresh = 0.01;  % Threshold
    
    % Call sphere_dist_fast_thresh
    [Dist, PA] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected results: First distance within threshold, second PA should be NaN
    expectedDist1 = acos(sin(Dec1(1)).*sin(Dec2(1)) + cos(Dec1(1)).*cos(Dec2(1)).*cos(RA1(1)-RA2(1)));
    verifyEqual(testCase, Dist(1), expectedDist1, 'relTol', 1e-10);
    verifyTrue(testCase, isnan(PA(2)));
end

function testComparisonWithSphereDist(testCase)
    % Compare with sphere_dist for distances within threshold
    
    RA1 = 0; Dec1 = 0;
    RA2 =0; Dec2 = 0.005;
    Thresh = 0.01;  % Within threshold
    
    % Call both functions
    [DistThresh, PAThresh] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    [DistExact, PAExact] = celestial.coo.sphere_dist(RA1, Dec1, RA2, Dec2);
    
    % Verify that both results match
    verifyEqual(testCase, DistThresh, DistExact, 'relTol', 1e-10);
    verifyEqual(testCase, PAThresh, PAExact, 'relTol', 1e-10);
end

function testLargeInputPerformance(testCase)
    % Test performance with large input vectors
    
    N = 1e6;  % One million points
    RA1 = rand(N, 1) * 2 * pi;
    Dec1 = rand(N, 1) * pi - pi/2;  % Dec from -pi/2 to pi/2
    RA2 = rand(N, 1) * 2 * pi;
    Dec2 = rand(N, 1) * pi - pi/2;
    Thresh = 0.01;  % Threshold
    
    % Call sphere_dist_fast_thresh
    [Dist, PA] = celestial.coo.sphere_dist_fast_thresh(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Verify the size of the output is correct
    verifySize(testCase, Dist, [N, 1]);
    verifySize(testCase, PA, [N, 1]);
end
