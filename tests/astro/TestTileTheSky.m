function tests = TestTileTheSky
    % TestTileTheSky
    % Function-based unit tests for the tile_the_sky function.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the tile_the_sky function.
    %
    % Author: Yarin Shani
    % Date: 2024-09-07
    %
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testBasicTiling(testCase)
    % Test basic tiling with N_RA = 360, N_Dec = 180
    N_RA = 360;
    N_Dec = 180;
    
    % Run the function
    [TileList, TileArea] = celestial.grid.tile_the_sky(N_RA, N_Dec);
    
    % Expected number of tiles
    expectedNumTiles = sum(N_RA * cos([-90:1:90]*pi/180)); % This should roughly be the number of tiles
    
    % Verify the number of tiles is correct
    verifyEqual(testCase, height(TileList), expectedNumTiles,'AbsTol',1000, 'The number of tiles is incorrect.');
    
    % Verify that all tile areas are positive
    verifyGreaterThan(testCase, min(TileArea), 0, 'All tile areas should be positive.');
end

function testTotalTileArea(testCase)
    % Test if the total area of all tiles is approximately equal to the area of the celestial sphere
    N_RA = 360;
    N_Dec = 180;
    
    % Run the function
    [~, TileArea] = celestial.grid.tile_the_sky(N_RA, N_Dec);
    
    % The total area of the celestial sphere should be 4*pi steradians
    expectedTotalArea = 4 * pi;
    totalTileArea = sum(TileArea);
    
    % Verify that the total tile area is approximately 4*pi
    verifyEqual(testCase, totalTileArea, expectedTotalArea, 'AbsTol', 1e-3, ...
        'The total area of the tiles is not equal to the area of the celestial sphere.');
end

function testSingleTile(testCase)
    % Test tiling with a single tile (N_RA = 1, N_Dec = 1)
    N_RA = 1;
    N_Dec = 1;
    
    % Run the function
    [TileList, TileArea] = celestial.grid.tile_the_sky(N_RA, N_Dec);
    
    % Verify the function returns one tile
    verifySize(testCase, TileList, [1, 6], 'The function should return one tile.');
    
    % Verify that the tile covers the whole celestial sphere
    expectedArea = 4 * pi; % The total area of the celestial sphere
    verifyEqual(testCase, TileArea, expectedArea, 'AbsTol', 1e-3, ...
        'The area of the single tile should be equal to the area of the celestial sphere.');
end

function testInvalidInput(testCase)
    % Should we test for invalid input ???
    % where N_RA = 0 or N_Dec = 0
    % Expect an error due to invalid number of tiles
    
    % Test for N_RA = 0
    verifyError(testCase, @() celestial.grid.tile_the_sky(0, 180), 'MATLAB:expectedPositiveScalar', ...
        'The function should throw an error when N_RA is 0.');
    
    % Test for N_Dec = 0
    verifyError(testCase, @() celestial.grid.tile_the_sky(360, 0), 'MATLAB:expectedPositiveScalar', ...
        'The function should throw an error when N_Dec is 0.');
end


