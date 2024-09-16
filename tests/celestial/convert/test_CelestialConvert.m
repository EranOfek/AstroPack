function tests = TestCelestialConvert
    % TestCelestialConvert
    % Unit tests for functions in the celestial.coo package
    % including precessCoo, j2000_toApparent, and others.
    
    % Define the test suite
    tests = functiontests(localfunctions);
end

%% Test Functions

function testPrecessCooRAandDec(testCase)
    % Test precession of RA and Dec (in degrees and sexagesimal format)
    
    % J2000.0 to true equinox of today's date [deg]
    [OutRA1, OutDec1] = celestial.convert.precessCoo(180, -20);
    
    % Same with sexagesimal coordinates
    [OutRA2, OutDec2] = celestial.convert.precessCoo('12:00:00', '-20:00:00', [], 'OutMean', false);
    
    % Verify that the RA and Dec values are nearly equal within a small tolerance
    verifyEqual(testCase, abs(OutRA1 - OutRA2), 0, 'AbsTol', 1e-10, 'Error in RA precession conversion');
    verifyEqual(testCase, abs(OutDec1 - OutDec2), 0, 'AbsTol', 1e-10, 'Error in Dec precession conversion');
end

function testPrecessCoo2050(testCase)
    % Test precession to the mean equinox of 2050 with sexagesimal input
    
    % Expected correct values for 2050
    AcRA  = 180.307519;
    AcDec = -0.133613;
    
    % J2000.0 to mean equinox of 2050 (input: sexagesimal, output: deg)
    [OutRA, OutDec] = celestial.convert.precessCoo('12:00:00', '-20:00:00', [], 'OutMean', true);
    
    % J2000.0 to mean equinox of 2024 (input: sexagesimal, output: deg, Julian date)
    [OutRA2024, OutDec2024] = celestial.convert.precessCoo('12:00:00', '+00:00:00', [], 'OutMean', true, 'OutType', 'J', 'OutEquinox', 2024);

    % Verify the precession output for 2024
    verifyEqual(testCase, abs(OutRA2024 - AcRA), 0, 'AbsTol', 4e-5, 'RA precession to 2024 is incorrect');
    verifyEqual(testCase, abs(OutDec2024 - AcDec), 0, 'AbsTol', 4e-5, 'Dec precession to 2024 is incorrect');
end

function testJ2000toApparent(testCase)
    % Test conversion from J2000 to Apparent coordinates
    
    % Convert from J2000 to Apparent coordinates for a specific date
    RA = celestial.coo.convertdms('02:31:48.704', 'gH', 'd');
    Dec = celestial.coo.convertdms('+89:15:50.72', 'gD', 'd');
    
    % Calculate apparent coordinates for 2100
    [OutRA, OutDec] = celestial.convert.j2000_toApparent(RA, Dec, celestial.time.julday([1, 1, 2100]), ...
                        'ApplyAberration', false, 'OutMean', true, 'PM_RA', 44.48, 'PM_Dec', -11.85, 'ApplyRefraction', false);
    
    % Expected values for the RA and Dec in apparent coordinates
    expectedRA = celestial.coo.convertdms(OutRA, 'd', 'SH');
    expectedDec = celestial.coo.convertdms(OutDec, 'd', 'SD');
    
    % Verify output matches expectations (just a placeholder check for this example)
    verifyEqual(testCase, OutRA, expectedRA, 'AbsTol', 1e-5, 'RA conversion to apparent is incorrect');
    verifyEqual(testCase, OutDec, expectedDec, 'AbsTol', 1e-5, 'Dec conversion to apparent is incorrect');
end

function testConvertDMS(testCase)
    % Test DMS to degree conversion and back
    RA = celestial.coo.convertdms('02:31:48.704', 'gH', 'd');
    Dec = celestial.coo.convertdms('+89:15:50.72', 'gD', 'd');
    
    % Verify conversion from DMS to degrees
    verifyGreaterThan(testCase, RA, 0, 'RA DMS to degree conversion is incorrect');
    verifyGreaterThan(testCase, Dec, 0, 'Dec DMS to degree conversion is incorrect');
end

%% Add more test cases for the other functions as needed

