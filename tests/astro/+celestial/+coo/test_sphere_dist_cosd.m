function tests = test_sphere_dist_cosd
    % Unit tests for the sphere_dist_cosd function in celestial.coo
    %
    % The function calculates the angular distance between two sets of
    % cosine direction vectors (3D vectors).
    %
    % Author: Yarin Shani

    tests = functiontests(localfunctions);
end

%% Test Functions

function testZeroDistance(testCase)
    % Test when the two cosine direction vectors are identical (distance = 0)
    
    CD1 = [1, 0, 0];  % First vector
    CD2 = [1, 0, 0];  % Identical vector
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is 0
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testOrthogonalVectors(testCase)
    % Test for vectors that are orthogonal (90 degrees apart)
    
    CD1 = [1, 0, 0];  % First vector along x-axis
    CD2 = [0, 1, 0];  % Second vector along y-axis (orthogonal)
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is pi/2 (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end

function testOppositeVectors(testCase)
    % Test for vectors that are in opposite directions (180 degrees apart)
    
    CD1 = [1, 0, 0];  % First vector
    CD2 = [-1, 0, 0]; % Opposite vector
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is pi (180 degrees)
    verifyEqual(testCase, Dist, pi, 'relTol', 1e-10);
end

function testDifferentMagnitudes(testCase)
    % Test for vectors of different magnitudes (should still work correctly)
    
    CD1 = [2, 0, 0];  % First vector (larger magnitude)
    CD2 = [1, 0, 0];  % Second vector
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is 0 (since they point in the same direction)
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-10);
end

function testMultiplePairsOfVectors(testCase)
    % Test multiple pairs of vectors (should return a vector of distances)
    
    CD1 = [1, 0, 0; 0, 1, 0];  % First set of vectors
    CD2 = [1, 0, 0; 0, 1, 0];  % Identical set of vectors
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is [0; 0] (since both pairs are identical)
    verifyEqual(testCase, Dist, [0; 0], 'relTol', 1e-10);
end

function testNearlyIdenticalVectors(testCase)
    % Test for vectors that are nearly identical (very small angular separation)
    
    CD1 = [1, 0, 0];       % First vector
    CD2 = [1, 1e-6, 0];    % Very small angular separation
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected small distance (approximately 1e-6 radians)
    verifyEqual(testCase, Dist, 1e-06, 'AbsTol', 1e-10);
end



function testNonNormalizedVectors(testCase)
    % Test for non-normalized vectors (should still calculate correct distances)
    
    CD1 = [2, 0, 0];    % Non-normalized vector
    CD2 = [0, 2, 0];    % Another non-normalized vector (orthogonal)
    
    % Call sphere_dist_cosd
    Dist = celestial.coo.sphere_dist_cosd(CD1, CD2);
    
    % Expected distance is pi/2 (90 degrees)
    verifyEqual(testCase, Dist, pi/2, 'relTol', 1e-10);
end
