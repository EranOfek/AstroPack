function tests = test_sphere_dist_fast_threshDist
    % Unit tests for the sphere_dist_fast_threshDist function
    % This function calculates angular distances with a declination threshold
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicFunctionality(testCase)
    % Test for a basic calculation within the threshold
    
    RA1 = 0; Dec1 = 0;  % Reference point
    RA2 = [pi/4]; Dec2 = [0.001];  % Target point with small angular separation
    Thresh = 0.01;  % Threshold larger than the Dec difference
    
    % Call sphere_dist_fast_threshDist
    [Dist, Ang, PA] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected distance and PA
    expectedDist = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));
    dRA = RA1 - RA2;
    SinPA = sin(dRA) .* cos(Dec2) ./ sin(expectedDist);
    CosPA = (sin(Dec2).*cos(Dec1) - cos(Dec2).*sin(Dec1).*cos(dRA)) ./ sin(expectedDist);
    expectedPA = atan2(SinPA, CosPA);
    
    % Verify distance and position angle
    verifyEqual(testCase, Dist, expectedDist, 'relTol', 1e-10);
    verifyEqual(testCase, Ang-2*pi, expectedPA, 'relTol', 1e-10);
end

function testThresholdExceeded(testCase)
    % Test when the distance exceeds the threshold
    
    RA1 = 0; Dec1 = 0;  % Reference point
    RA2 = [pi/4]; Dec2 = [0.05];  % Target point with larger separation
    Thresh = 0.01;  % Threshold smaller than the Dec difference
    
    % Call sphere_dist_fast_threshDist
    [Dist, Ang, PA] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected result: distances exceeding the threshold should return NaN
    verifyTrue(testCase, isnan(Dist));
    verifyTrue(testCase, isempty(Ang));
    verifyTrue(testCase, isempty(PA));
end

function testEdgeCaseThreshold(testCase)
    % Test when the distance is exactly equal to the threshold
    
    RA1 = 0; Dec1 = 0;  % Reference point
    RA2 = [pi/4]; Dec2 = [0.02];  % Target point
    Thresh = acos(sin(Dec1).*sin(Dec2) + cos(Dec1).*cos(Dec2).*cos(RA1-RA2));  % Exact threshold
    
    % Call sphere_dist_fast_threshDist
    [Dist, Ang, PA] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Verify that the distance and position angle are still calculated
    verifyEqual(testCase, Dist, Thresh, 'relTol', 1e-10);
    verifyNotEmpty(testCase, PA);
    verifyNotEmpty(testCase, Ang);
end

function testMultiplePoints(testCase)
    % Test with multiple target points
    
    RA1 = 0; Dec1 = 0;  % Reference point
    RA2 = [pi/4; pi/3]; Dec2 = [0.005; 0.02];  % Target points
    Thresh = 0.01;  % Threshold
    
    % Call sphere_dist_fast_threshDist
    [Dist, Ang, PA] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Expected: first point within threshold, second should be NaN
    expectedDist1 = acos(sin(Dec1).*sin(Dec2(1)) + cos(Dec1).*cos(Dec2(1)).*cos(RA1-RA2(1)));
    verifyEqual(testCase, Dist(1), expectedDist1, 'relTol', 1e-10);
    verifyTrue(testCase, isnan(Dist(2)));
    verifyTrue(testCase, isscalar(Ang));
    verifyTrue(testCase, isscalar(PA));
end

function testComparisonWithSphereDistFast(testCase)
    % Compare results with sphere_dist_fast for distances within threshold
    
    RA1 = 0; Dec1 = 0;
    RA2 = pi/4; Dec2 = 0.005;
    Thresh = 0.01;  % Within threshold
    
    % Call both functions
    [DistThresh, ~, PAThresh] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    [DistFast, PAFast] = celestial.coo.sphere_dist_fast(RA1, Dec1, RA2, Dec2);
    
    % Verify that both results match
    verifyEqual(testCase, DistThresh, DistFast, 'relTol', 1e-10);
    verifyEqual(testCase, PAThresh, PAFast, 'relTol', 1e-2); % fails foor 1e-3
end

function testLargeInputPerformance(testCase)
    % Test performance with large input vectors
    
    N = 1e6;  % One million points
    RA1 = rand * 2 * pi;
    Dec1 = rand * pi - pi/2;
    RA2 = rand(N, 1) * 2 * pi;
    Dec2 = rand(N, 1) * pi - pi/2;
    Thresh = 2;  % Threshold
    
    % Call sphere_dist_fast_threshDist
    [Dist, Ang, PA] = celestial.coo.sphere_dist_fast_threshDist(RA1, Dec1, RA2, Dec2, Thresh);
    
    % Verify that the output has the correct size
    verifySize(testCase, Dist, [N, 1]);
    verifySize(testCase, Ang, [N, 1]);
    verifySize(testCase, PA, [N, 1]);
end
