function tests = test_cosined
    % Unit tests for the cosined function, which converts between
    % longitude/latitude and cosine directions and vice versa.
    %
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testLonLatToCosineDir(testCase)
    % Test conversion from [longitude, latitude] to cosine directions
    
    LonLat = [0, 0; pi/2, pi/4];  % Test input (radians): [lon, lat]
    
    % Expected cosine directions [X, Y, Z]
    expectedRes = [1, 0, 0; cos(pi/2)*cos(pi/4), sin(pi/2)*cos(pi/4), sin(pi/4)];
    
    % Call cosined function
    result = celestial.coo.cosined(LonLat);
    
    % Verify that the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end

function testCosineDirToLonLat(testCase)
    % Test conversion from cosine directions [X, Y, Z] to [longitude, latitude]
    
    CosineDir = [1, 0, 0; cos(pi/2)*cos(pi/4), sin(pi/2)*cos(pi/4), sin(pi/4)];
    
    % Expected [longitude, latitude]
    expectedRes = [0, 0; pi/2, pi/4];  % Expected results [lon, lat]
    
    % Call cosined function
    result = celestial.coo.cosined(CosineDir);
    
    % Verify that the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end

function testPolePosition(testCase)
    % Test conversion for positions at the poles (latitude = ±90 degrees)
    
    LonLat = [0, pi/2; 0, -pi/2];  % [longitude, latitude] in radians at the poles
    
    % Expected cosine directions
    expectedRes = [0, 0, 1; 0, 0, -1];
    
    % Call cosined function
    result = celestial.coo.cosined(LonLat);
    
    % Verify the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end

function testEquatorPosition(testCase)
    % Test conversion for positions on the equator (latitude = 0 degrees)
    
    LonLat = [0, 0; pi/2, 0];  % [longitude, latitude] in radians at the equator
    
    % Expected cosine directions
    expectedRes = [1, 0, 0; 0, 1, 0];
    
    % Call cosined function
    result = celestial.coo.cosined(LonLat);
    
    % Verify the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end


function testBackAndForthConversion(testCase)
    % Test conversion back and forth between [longitude, latitude] and cosine direction
    
    LonLat = [pi/3, pi/6];  % Example longitude and latitude in radians
    
    % Convert to cosine directions
    CosineDir = celestial.coo.cosined(LonLat);
    
    % Convert back to [longitude, latitude]
    LonLatBack = celestial.coo.cosined(CosineDir);
    
    % Verify that the final result matches the initial input (with a small tolerance)
    verifyEqual(testCase, LonLatBack, LonLat, 'AbsTol', 1e-10);
end


function testSymmetryLonBeyond2Pi(testCase)
    % Test behavior for longitude values greater than 2*pi (should wrap around)
    
    LonLat = [0, pi/2; 3*pi, pi/4];  % 3*pi longitude wraps around to pi
    
    % Expected cosine directions
    expectedRes = [0, 0, 1; cos(pi)*cos(pi/4), sin(pi)*cos(pi/4), sin(pi/4)];
    
    result = celestial.coo.cosined(LonLat);
    
    % Verify that the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end


function testNegativeLongitude(testCase)
    % Test conversion for negative longitude values
    
    LonLat = [-pi/2, 0; -pi, pi/3];  % Negative longitudes
    
    % Expected cosine directions
    expectedRes = [0, -1, 0; -cos(pi/3), 0, sin(pi/3)];
    
    result = celestial.coo.cosined(LonLat);
    
    % Verify that the result matches expected output
    verifyEqual(testCase, result, expectedRes, 'AbsTol', 1e-10);
end

function testRandomizedBackAndForthConversion(testCase)
    % Test random longitudes/latitudes and check the back-and-forth conversion
    
    rng(42);  % Set seed for reproducibility
    
    % Generate 100 random longitude/latitude values (lon [0, 2*pi], lat [-pi/2, pi/2])
    LonLat = [rand(100, 1) * 2*pi, rand(100, 1) * pi - pi/2];
    
    % Convert to cosine directions
    CosineDir = celestial.coo.cosined(LonLat);
    
    % Convert back to longitude/latitude
    LonLatBack = celestial.coo.cosined(CosineDir);
    
    % Normalize the longitudes to the range [0, 2*pi) before comparison
    LonLat(:, 1) = mod(LonLat(:, 1), 2*pi);  % Normalize original longitudes
    LonLatBack(:, 1) = mod(LonLatBack(:, 1), 2*pi);  % Normalize back-converted longitudes
    
    % Verify that the final result matches the initial input (with a small tolerance)
    verifyEqual(testCase, LonLatBack, LonLat, 'AbsTol', 1e-10);
end