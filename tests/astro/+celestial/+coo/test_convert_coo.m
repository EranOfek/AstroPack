function tests = test_convert_coo
    % Unit tests for the celestial.coo.convert_coo function, which converts
    % coordinates between various celestial systems.
    %
    % Author: Yarin Shani
    % Date: 13 Nov.
    %
    % ########## NOT DONE . REQUIRE MORE WORK !!! #########
    
    % This test suite covers standard conversions, date-dependent conversions,
    % horizontal coordinates, and verifies output consistency with relative testing.
    
    tests = functiontests(localfunctions);
end

%% Test Setup Function

function setupOnce(testCase)
    % Load expected results from file only once for all tests
   
    dataFilePath = fullfile('~', 'matlab', 'AstroPack', 'tests', 'relativeData', 'expected_convert_coo_results.mat');
    testData = load(dataFilePath);
    testCase.TestData.expectedResults = testData.expectedResults;
end

%% Test Functions

function testEquatorialToGalactic(testCase)
    % Test conversion from equatorial to galactic coordinates using known reliable values

    % Define input (RA/Dec in J2000)
    RA = 3.3667;  % Example RA in radians (approx. 193.000 degrees)
    Dec = 0.2094; % Example Dec in radians (approx. 12.0 degrees)
    InCooType = 'J2000.0';
    OutCooType = 'g';  % Galactic coordinates
    
    % Expected values (based on reliable astronomical resource or tool)
    expectedLon = 303.075096*pi/180;  % Expected galactic longitude in radians (approx. 36.25 degrees)
    expectedLat = 74.869369*pi/180;  % Expected galactic latitude in radians (approx. 23.5 degrees)
    
    % Run convert_coo function
    [OutLong, OutLat] = celestial.coo.convert_coo(RA, Dec, InCooType, OutCooType);
    
    % Verify outputs
    verifyEqual(testCase, OutLong, expectedLon, 'RelTol', 1e-6, 'Equatorial to Galactic Longitude incorrect');
    verifyEqual(testCase, OutLat, expectedLat, 'RelTol', 1e-6, 'Equatorial to Galactic Latitude incorrect');
end


function testGalacticToEquatorial(testCase)
    % Test conversion from galactic to equatorial coordinates (J2000)

    % Define input in galactic coordinates
    Lon = 2.0;  % Galactic longitude
    Lat = -0.5; % Galactic latitude
    InCooType = 'g';
    OutCooType = 'J2000.0';  % Equatorial coordinates (J2000)

    % Expected values based on known conversions
    expectedRA = 266.434402 *pi/180;    % Expected RA in radians
    expectedDec = -28.910840*pi/180;   % Expected Dec in radians

    % Run convert_coo function
    [OutLong, OutLat] = celestial.coo.convert_coo(Lon, Lat, InCooType, OutCooType);

    % Verify outputs
    verifyEqual(testCase, OutLong, expectedRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutLat, expectedDec, 'AbsTol', 1e-3);
end

function testHorizontalConversion(testCase)
    % Test conversion from equatorial to horizontal coordinates based on observer location

    % Define inputs (RA/Dec for a given location and date)
    RA = 2.0;  % Radian RA
    Dec = 0.3; % Radian Dec
    InCooType = 'J2000.0';
    OutCooType = 'h';  % Horizontal
    JD = 2451545.0;  % Example Julian date
    ObsCoo = [0.0, pi/4];  % Observer at equator, 45 degrees latitude

    % Expected output (based on reliable source)
    expectedAz = 1.57;  % Expected azimuth in radians
    expectedAlt = 0.45; % Expected altitude in radians

    % Run convert_coo function
    [OutLong, OutLat] = celestial.coo.convert_coo(RA, Dec, InCooType, OutCooType, JD, ObsCoo);

    % Verify outputs
    verifyEqual(testCase, OutLong, expectedAz, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutLat, expectedAlt, 'AbsTol', 1e-3);
end

function testEquinoxConversion(testCase)
    % Test conversion between equatorial coordinates with different equinoxes

    % Define input (RA/Dec in J2000)
    RA = 1.0;    % 1 radian
    Dec = 0.5;   % 0.5 radian
    InCooType = 'J2000.0';
    OutCooType = 'J1950.0';  % Convert to J2010.0 equinox

    % Expected output (known reference value for J2010.0 equinox)
    expectedRA = 1.002;  % Expected RA after precession
    expectedDec = 0.499; % Expected Dec after precession

    % Run convert_coo function
    [OutLong, OutLat] = celestial.coo.convert_coo(RA, Dec, InCooType, OutCooType);

    % Verify outputs
    verifyEqual(testCase, OutLong, expectedRA, 'AbsTol', 1e-3);
    verifyEqual(testCase, OutLat, expectedDec, 'AbsTol', 1e-3);
end


function testRelativeResults(testCase)
    % Test function output relative to precomputed expected results
    
    % Define tolerance for comparison
    tolerance = 1e-3;
    
    % Retrieve expected results from setupOnce
    expectedResults = testCase.TestData.expectedResults;
    
    % Loop over each test case stored in expected_convert_coo_results.mat
    for i = 1:length(expectedResults)
        Long = expectedResults(i).Long;
        Lat = expectedResults(i).Lat;
        InCooType = expectedResults(i).InCooType;
        OutCooType = expectedResults(i).OutCooType;
        JD = expectedResults(i).JD;
        ObsCoo = expectedResults(i).ObsCoo;
        
        expectedOutLong = expectedResults(i).OutLong;
        expectedOutLat = expectedResults(i).OutLat;
        
        % Run convert_coo function
        [OutLong, OutLat] = celestial.coo.convert_coo(Long, Lat, InCooType, OutCooType, JD, ObsCoo);
        
        % Verify each output with expected results
        verifyEqual(testCase, OutLong, expectedOutLong, 'AbsTol', tolerance);
        verifyEqual(testCase, OutLat, expectedOutLat, 'AbsTol', tolerance);
    end
end
