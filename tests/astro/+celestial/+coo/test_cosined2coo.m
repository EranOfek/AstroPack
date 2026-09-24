function tests = test_cosined2coo
    % Unit tests for the cosined2coo function, which converts from
    % cosine directions to longitude and latitude.
    %
    % cosined.m does the same.
    % Author: Yarin Shani
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicConversion(testCase)
    % Test basic conversion from cosine directions to longitude and latitude
    
    % Cosine directions for specific points
    CD1 = [1, 0, 0];
    CD2 = [0, 1, 0];
    CD3 = [0, 0, 1];
    
    % Expected longitude and latitude
    expectedLon = [0, pi/2, 0];
    expectedLat = [0, 0, pi/2];  % Latitude = pi/2 when Z direction is 1
    
    % Call cosined2coo
    [Long, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Verify that the results match the expected output
    verifyEqual(testCase, Long, expectedLon, 'AbsTol', 1e-10);
    verifyEqual(testCase, Lat, expectedLat, 'AbsTol', 1e-10);
end

function testNegativeLongitude(testCase)
    % Test conversion with negative longitude values
    
    % Cosine directions for longitude < 0 (in X-Y plane)
    CD1 = [cos(-pi/2)];
    CD2 = [sin(-pi/2)];
    CD3 = [0];  % Equatorial plane
    
    % Expected longitude is pi/2 when normalized to [0, 2*pi]
    expectedLon = [3*pi/2];
    expectedLat = [0];  % Still on equator
    
    % Call cosined2coo
    [Long, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Verify results
    verifyEqual(testCase, Long, expectedLon, 'AbsTol', 1e-10);
    verifyEqual(testCase, Lat, expectedLat, 'AbsTol', 1e-10);
end

function testNormalizationOfLongitude(testCase)
    % Test that longitude values are properly normalized to [0, 2*pi]
    
    % Cosine directions for longitude = -pi, should wrap to pi
    CD1 = [-1];
    CD2 = [0];
    CD3 = [0];
    
    % Expected longitude = pi after normalization
    expectedLon = pi;
    expectedLat = 0;
    
    % Call cosined2coo
    [Long, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Verify longitude is normalized
    verifyEqual(testCase, Long, expectedLon, 'AbsTol', 1e-10);
    verifyEqual(testCase, Lat, expectedLat, 'AbsTol', 1e-10);
end

function testLatitudeAtPoles(testCase)
    % Test latitude values for cosine directions at the poles
    
    % North pole
    CD1 = [0];
    CD2 = [0];
    CD3 = [1];  % Z direction positive
    
    % Expected latitude is pi/2
    expectedLat = pi/2;
    expectedLon = 0;  % Longitude doesn't matter at the poles
    
    % Call cosined2coo
    [Long, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Verify that the latitude is pi/2 at the North pole
    verifyEqual(testCase, Lat, expectedLat, 'AbsTol', 1e-10);
    verifyEqual(testCase, Long, expectedLon, 'AbsTol', 1e-10);
    
    % South pole
    CD3 = [-1];  % Z direction negative
    expectedLat = -pi/2;
    
    [Long, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Verify that the latitude is -pi/2 at the South pole
    verifyEqual(testCase, Lat, expectedLat, 'AbsTol', 1e-10);
    verifyEqual(testCase, Long, expectedLon, 'AbsTol', 1e-10);
end

function testBackAndForthConversion(testCase)
    % Test back and forth conversion between cosine directions and lon/lat
    
    % Generate random cosine directions
    rng(42);  % Set seed for reproducibility
    LonLat = [rand(100, 1) * 2*pi, rand(100, 1) * pi - pi/2];
    
    % Convert to cosine directions using cosined.m
    CosineDir = celestial.coo.cosined(LonLat);
    
    % Split into CD1, CD2, CD3
    CD1 = CosineDir(:, 1);
    CD2 = CosineDir(:, 2);
    CD3 = CosineDir(:, 3);
    
    % Convert back to longitude and latitude using cosined2coo
    [LongBack, LatBack] = celestial.coo.cosined2coo(CD1, CD2, CD3);
    
    % Normalize the longitudes for comparison
    LonLat(:, 1) = mod(LonLat(:, 1), 2*pi);  % Normalize original longitudes
    LongBack = mod(LongBack, 2*pi);          % Normalize back-converted longitudes
    
    % Verify that the final result matches the initial input (with a small tolerance)
    verifyEqual(testCase, [LongBack, LatBack], LonLat, 'AbsTol', 1e-10);
end

