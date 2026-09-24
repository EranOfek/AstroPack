function tests = test_sphere_dist_thresh
    % Unit tests for the sphere_dist_thresh function, which checks whether points are within
    % a specified angular distance from a reference point.
    %
    % Author: Yarin Shani
    % 

    tests = functiontests(localfunctions);
end

%% Test Functions

function testCircularShapeWithinThreshold(testCase)
    % Test if points are correctly flagged within a circular threshold.
    Long = rand(100, 1) * 2 * pi;  % Random longitudes between 0 and 2*pi
    Lat = rand(100, 1) * pi - pi / 2;  % Random latitudes between -pi/2 and pi/2
    LongRef = pi / 4;  % Reference longitude
    LatRef = pi / 6;  % Reference latitude
    MaxDist = 0.2;  % Distance threshold in radians
    Shape = 'circ';
    
    % Call sphere_dist_thresh function
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long, Lat, LongRef, LatRef, MaxDist, Shape);
    
    % Verify that distances flagged are below threshold
    verifyEqual(testCase, Flag, Dist <= MaxDist, 'Failed to flag points correctly within circular threshold.');
    % Verify distance values are positive
    verifyGreaterThanOrEqual(testCase, Dist, 0);
    % Verify position angles are within the valid range
    verifyGreaterThanOrEqual(testCase, PA, 0);
    verifyLessThanOrEqual(testCase, PA, 2 * pi);
end

function testBoxShapeWithinThreshold(testCase)
    % Test if points are correctly flagged within a box threshold.
    Long = rand(100, 1) * 2 * pi;  % Random longitudes between 0 and 2*pi
    Lat = rand(100, 1) * pi - pi / 2;  % Random latitudes between -pi/2 and pi/2
    LongRef = pi / 3;  % Reference longitude
    LatRef = pi / 6;  % Reference latitude
    MaxDist = 0.1;  % Distance threshold in radians
    Shape = 'box';
    
    % Call sphere_dist_thresh function
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long, Lat, LongRef, LatRef, MaxDist, Shape);
    
    % Verify the flag output is logical
    verifyClass(testCase, Flag, 'logical');
    % Verify distances are computed if requested
    verifyGreaterThanOrEqual(testCase, Dist, 0);
    % Verify position angles are within the valid range if requested
    verifyGreaterThanOrEqual(testCase, PA, 0);
    verifyLessThanOrEqual(testCase, PA, 2 * pi);
end

function testDefaultShapeIsCircular(testCase)
    % Test if the default shape is correctly set to 'circ'.
    Long = rand(10, 1) * 2 * pi;
    Lat = rand(10, 1) * pi - pi / 2;
    LongRef = pi / 4;
    LatRef = pi / 6;
    MaxDist = 0.2;
    
    % Call sphere_dist_thresh without specifying the shape
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long, Lat, LongRef, LatRef, MaxDist);
    
    % Verify that distances flagged are below threshold (default shape should be 'circ')
    verifyEqual(testCase, Flag, Dist <= MaxDist, 'Failed to flag points correctly with default circular threshold.');
end


function testZeroMaxDist(testCase)
    % Test if the function handles zero distance threshold.
    Long = rand(10, 1) * 2 * pi;
    Lat = rand(10, 1) * pi - pi / 2;
    LongRef = pi / 4;
    LatRef = pi / 6;
    MaxDist = 0;
    
    % Call sphere_dist_thresh with zero MaxDist
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long, Lat, LongRef, LatRef, MaxDist);
    
    % Verify that only the reference point is flagged
    verifyTrue(testCase, all(~Flag | (Dist == 0)), 'Failed to correctly handle zero distance threshold.');
end

function testSinglePointInput(testCase)
    % Test if the function handles single point input correctly.
    Long = pi / 4;
    Lat = pi / 6;
    LongRef = pi / 4;
    LatRef = pi / 6;
    MaxDist = 0.1;
    Shape = 'circ';
    
    % Call sphere_dist_thresh with single point input
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long, Lat, LongRef, LatRef, MaxDist, Shape);
    
    % Verify the output flag is true since the point is the same as the reference
    verifyTrue(testCase, Flag, 'Failed to flag the point correctly when it matches the reference point.');
    % Verify the distance is zero
    verifyEqual(testCase, Dist, 0, 'relTol', 1e-9); % Adjusted tolerance value for better alignment with function precision requirements
end

%% Helper Functions
function verifyPositionAngleRange(testCase, PA)
    % Consolidated verification for position angle range
    verifyGreaterThanOrEqual(testCase, PA, 0);
    verifyLessThanOrEqual(testCase, PA, 2 * pi);
end

function testLatitudeBoundaries(testCase)
    % Test points near the boundaries of valid latitude values.
    Long = [0, pi];
    Lat = [-pi/2, pi/2]; % Boundary latitude values
    LongRef = 0;
    LatRef = 0;
    MaxDist = pi / 4;
    Shape = 'circ';

    % Call sphere_dist_thresh
    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long(1), Lat(1), LongRef, LatRef, MaxDist,Shape);

    % Verify the output flags are logical values
    verifyClass(testCase, Flag, 'logical');
    % Verify distances are valid
    verifyGreaterThanOrEqual(testCase, Dist, 0);
    % Verify position angles are within valid range
    verifyPositionAngleRange(testCase, PA);

    [Flag, Dist, PA] = celestial.coo.sphere_dist_thresh(Long(2), Lat(2), LongRef, LatRef, MaxDist,Shape);

    % Verify the output flags are logical values
    verifyClass(testCase, Flag, 'logical');
    % Verify distances are valid
    verifyGreaterThanOrEqual(testCase, Dist, 0);
    % Verify position angles are within valid range
    verifyPositionAngleRange(testCase, PA);


end