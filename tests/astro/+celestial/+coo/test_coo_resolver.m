function tests = test_coo_resolver
    % Unit tests for the celestial.coo.coo_resolver function, which converts
    % coordinates or resolves object names to RA/Dec in J2000 equatorial coordinates.
    %
    % Author: Yarin Shani
    % Date: 13 Nov
    %
    % This script validates conversion for direct coordinates and object names,
    % in both degrees and radians, and uses stored expected results for verification.
    
    tests = functiontests(localfunctions);
end

%% Test Setup Function

function setupOnce(testCase)
    % Load expected results from file only once for all tests
    dataFilePath = fullfile('~/','matlab','AstroPack','tests', 'relativeData', 'expected_coo_resolver_results.mat');
    testData = load(dataFilePath);
    testCase.TestData.expectedResults = testData.expectedResults;
end

%% Test Functions

function testBasicDegreesInput(testCase)
    % Test function with basic input in degrees for RA/Dec

    % Input in degrees
    RA = 1;        % 1 degree
    Dec = 1;       % 1 degree
    InSys = 'j2000';
    InUnits = 'deg';
    OutUnits = 'rad';
    
    % Expected output in radians
    expectedRA = RA * pi / 180;
    expectedDec = Dec * pi / 180;
    
    % Run coo_resolver function
    [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testBasicRadiansInput(testCase)
    % Test function with basic input in radians for RA/Dec

    % Input in radians
    RA = pi / 6;   % 30 degrees in radians
    Dec = pi / 6;  % 30 degrees in radians
    InSys = 'j2000';
    InUnits = 'rad';
    OutUnits = 'deg';
    
    % Expected output in degrees
    expectedRA = 30;
    expectedDec = 30;
    
    % Run coo_resolver function
    [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testSexagesimalInput(testCase)
    % Test function with RA/Dec in sexagesimal format (HH:MM:SS)
    
    % Input in sexagesimal format
    RA = '15:00:00';    % 15 hours in RA
    Dec = '-30:00:00';   % -30 degrees in Dec
    InSys = 'j2000';
    InUnits = 'deg';
    OutUnits = 'rad';
    
    % Expected output in radians
    expectedRA = 15 * 15 * pi / 180;  % Conversion needed based on function
    expectedDec = -30 * pi / 180;
    
    % Run coo_resolver function
    [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testObjectNameResolution(testCase)
    % Test function with object name, checking resolution using a name server
    Name = 'Deneb';  % Known star
    Dec = [];      % Empty to trigger name resolution
    InSys = 'j2000';
    InUnits = 'deg';
    OutUnits = 'deg';
    
    % Expected RA/Dec in degrees (need accurate reference values)
    expectedRA = 310.357980;  % Example value for Deneb (actual value may vary)
    expectedDec = 45.280339;
    
    % Run coo_resolver function with name server resolution
    [OutRA, OutDec] = celestial.coo.coo_resolver(Name,'NameServer',@VO.name.server_simbad, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);  % Looser tolerance for name resolution
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testCoordinateSystemConversion(testCase)
    % Test function with galactic coordinates input, expecting J2000 output

    % Input in galactic coordinates (example values)
    RA = '17:45:40.04';
    Dec = '-29:00:28.1';
    InSys = 'j2000';
    InUnits = 'deg';
    OutUnits = 'rad';
    
    % Expected J2000 output in radians (specific reference values required)
    expectedRA = 266.41683 * pi / 180;  % Example expected value
    expectedDec = -29.00781 * pi / 180;
    
    % Run coo_resolver function
    [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testOutputUnitConversion(testCase)
    % Test function to ensure output unit flexibility (degrees to radians)

    % Input in J2000 equatorial, degrees
    RA = 10;       % 10 degrees RA
    Dec = -10;     % -10 degrees Dec
    InSys = 'j2000';
    InUnits = 'deg';
    OutUnits = 'rad';
    
    % Expected output in radians
    expectedRA = 10 * pi / 180;
    expectedDec = -10 * pi / 180;
    
    % Run coo_resolver function
    [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, 'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
    
    % Verify outputs match expected results
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-6);
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-6);
end

function testRelativeResults(testCase)
    % Test celestial.coo.coo_resolver with various input configurations using expected results

    % Define tolerance for comparison in radians
    tolerance = 1e-6;
    
    % Retrieve expected results from setupOnce
    expectedResults = testCase.TestData.expectedResults;
    
    % Loop over each test case stored in expected_results_coo_resolver.mat
    for i = 1:length(expectedResults)
        RA = expectedResults(i).RA;
        Dec = expectedResults(i).Dec;
        InSys = expectedResults(i).InSys;
        InUnits = expectedResults(i).InUnits;
        OutUnits = expectedResults(i).OutUnits;
        expectedRA = expectedResults(i).ExpectedRA;
        expectedDec = expectedResults(i).ExpectedDec;
        
        % Run coo_resolver function with current test case inputs
        [OutRA, OutDec] = celestial.coo.coo_resolver(RA, Dec, ...
            'InSys', InSys, 'InUnits', InUnits, 'OutUnits', OutUnits);
        
        % Verify that the calculated RA and Dec are within tolerance of expected results
        verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', tolerance, ...
            sprintf('Test case %d failed for RA. Input: RA = %s, Dec = %s, InSys = %s, InUnits = %s, OutUnits = %s', ...
            i, mat2str(RA), mat2str(Dec), InSys, InUnits, OutUnits));
        verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', tolerance, ...
            sprintf('Test case %d failed for Dec. Input: RA = %s, Dec = %s, InSys = %s, InUnits = %s, OutUnits = %s', ...
            i, mat2str(RA), mat2str(Dec), InSys, InUnits, OutUnits));
    end
end
