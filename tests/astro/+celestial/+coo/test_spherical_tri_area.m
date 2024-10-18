function tests = test_spherical_tri_area
    % Unit tests for the spherical_tri_area function in AstroPack
    %
    % Description:
    % This unit test verifies the correct operation of the `spherical_tri_area`
    % function, ensuring accurate calculation of spherical triangle areas.
    % Includes regression testing for consistency across versions.
    %
    % Author: Yarin Shani
    % Date: 2024-10-18
    
    tests = functiontests(localfunctions);
end

%% Setup and Teardown
function setupOnce(testCase)
    % Setup common values once before all tests run
    testCase.TestData.Coo1 = [0, 0];               % Example coordinates
    testCase.TestData.Coo2 = [pi/2, 0];            % Example coordinates
    testCase.TestData.Coo3 = [pi/4, pi/2];         % Example coordinates

    % Load expected results for regression testing
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_spherical_tri_area_results.mat');;
    testCase.TestData.ExpectedResults = load(dataFilePath);
end

function teardownOnce(~)
    % Teardown function: nothing needed for now, but can be used for cleanup
end

%% Test Functions

function testNominalCase(testCase)
    % Test nominal input values for spherical_tri_area function
    Coo1 = testCase.TestData.Coo1;
    Coo2 = testCase.TestData.Coo2;
    Coo3 = testCase.TestData.Coo3;
    
    % Call the function
    Area = celestial.coo.spherical_tri_area(Coo1, Coo2, Coo3);
    
    % Verify the result using known values
    % Exact known result (for simplicity, calculate manually if possible)
    expectedArea = pi/2;  % Example known result for this triangle configuration (update as needed)
    
    % Compare with the known result using relative tolerance
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 1e-6, ...
        'The computed area does not match the expected value.');
end

function testEdgeCaseCollinearPoints(testCase)
    % Test collinear points (where the area should be exactly zero)
    Coo1 = [0, 0];
    Coo2 = [pi, 0];
    Coo3 = [pi/2, 0];  % Points on the equator
    
    % Call the function
    Area = celestial.coo.spherical_tri_area(Coo1, Coo2, Coo3);
    
    % The area should be exactly zero for collinear points
    verifyEqual(testCase, Area, 0, ...
        'Area should be exactly zero for collinear points on the equator.');
end

function testRegressionCase(testCase)
    % Regression test to verify the function's consistency with stored results
    %
    % This test compares the current output of the `spherical_tri_area` function
    % to the previously saved results stored in 'expected_spherical_tri_area_results.mat'.
    
    % Loop through each set of stored results and compare
    expectedResults = testCase.TestData.ExpectedResults.expectedResults;
    
    for i = 1:numel(expectedResults)
        % Extract stored inputs
        Coo1 = expectedResults(i).Coo1;
        Coo2 = expectedResults(i).Coo2;
        Coo3 = expectedResults(i).Coo3;
        
        % Call the function with the same inputs
        currentArea = celestial.coo.spherical_tri_area(Coo1, Coo2, Coo3);

        % Compare the current output to the stored expected results using relTol
        verifyEqual(testCase, currentArea, expectedResults(i).Area, 'RelTol', 1e-6, ...
            sprintf('Area result for Coo1: [%f, %f], Coo2: [%f, %f], Coo3: [%f, %f] has changed unexpectedly', ...
            Coo1(1), Coo1(2), Coo2(1), Coo2(2), Coo3(1), Coo3(2)));
    end
end

function testSmallTriangle(testCase)
    % Test for small triangles where points are very close to each other
    Coo1 = [0, 0];
    Coo2 = [1e-6, 0];   % Small difference in longitude
    Coo3 = [0, 1e-6];   % Small difference in latitude
    
    % Call the function
    Area = celestial.coo.spherical_tri_area(Coo1, Coo2, Coo3);
    
    % The area should be very close to 0
    verifyEqual(testCase, Area, 0, 'AbsTol', 1e-12, ...
        'The area for a small triangle should approach 0.');
end



function testQuadratureArea(testCase)
    % Test for large triangles spanning nearly half the sphere
    % Test fails for (0,0) ,(pi/2,pi/2),(0,pi); A = pi
    Coo1 = [0, 0];          % First point on the equator
    Coo2 = [499*pi/100, pi/2];         % Opposite side of the equator
    Coo3 = [ 998*pi/1000,0];       % North pole
    
    % Call the function
    Area = celestial.coo.spherical_tri_area(Coo1, Coo2, Coo3);
    
    % The area should be approximately half of the sphere's surface area
    expectedArea =  998*pi/1000;   
    verifyEqual(testCase, Area, expectedArea, 'RelTol', 1e-6, ...
        'The area of a large triangle covering a hemisphere should be close to 2*pi.');
end
 