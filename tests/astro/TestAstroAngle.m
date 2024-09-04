function tests = TestAstroAngleFunctionBased
    % TestAstroAngleFunctionBased
    % Function-based unit tests for the AstroAngle class.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the AstroAngle class.
    %
    % Author: Moshe Ufnik
    % Date: 17/06/2024
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testConstructorWithDegrees(testCase)
    % Test the constructor with angle data in degrees
    Data = rand(10, 1) * 360;  % Random angles in degrees
    Units = 'deg';

    % Create an AstroAngle object
    A = AstroAngle(Data, Units);

    % Verify the properties
    verifyEqual(testCase, A.Angle, Data, ...
        'Constructor did not correctly store the angle data.');
    verifyEqual(testCase, A.Units, Units, ...
        'Constructor did not correctly store the units.');
end

function testConstructorWithSexagesimalString(testCase)
    % Test the constructor with sexagesimal strings
    Data = {'10:15:20.1', '-01:00:01.1'};  % Sexagesimal strings
    Units = 'd';  % Degrees

    % Create an AstroAngle object
    A = AstroAngle(Data, Units);

    % Verify the angle is correctly converted to radians
    expectedAngle = celestial.coo.convertdms(Data, 'SD', 'r');
    verifyEqual(testCase, A.Angle, expectedAngle, 'AbsTol', 1e-10, ...
        'Constructor did not correctly convert sexagesimal strings to radians.');
    verifyEqual(testCase, A.Units, 'rad', ...
        'Constructor did not correctly set units to radians.');
end

function testConversionToDegrees(testCase)
    % Test converting an AstroAngle object to degrees
    Data = pi / 2;  % 90 degrees in radians
    A = AstroAngle(Data, 'rad');

    % Convert to degrees
    A.convert('deg');

    % Verify the conversion
    verifyEqual(testCase, A.Angle, 90, 'AbsTol', 1e-10, ...
        'Conversion to degrees did not produce the expected result.');
    verifyEqual(testCase, A.Units, 'deg', ...
        'Units were not correctly updated to degrees.');
end

function testConversionToArray(testCase)
    % Test converting an AstroAngle object to an array of a different unit
    Data = [pi / 2, pi];  % 90 and 180 degrees in radians
    A = AstroAngle(Data, 'rad');

    % Convert to array in degrees
    Result = A.convert2array('deg');

    % Expected result
    expectedArray = [90, 180];

    % Verify the result
    verifyEqual(testCase, Result, expectedArray, 'AbsTol', 1e-10, ...
        'Conversion to array in degrees did not produce the expected result.');
end

function testTpi(testCase)
    % Test converting angles to the range of 0 to 2*pi
    Data = [0, 2 * pi, 4 * pi];  % Angles in radians
    A = AstroAngle(Data, 'rad');

    % Convert to range 0 to 2*pi
    A.tpi();

    % Expected result
    expectedAngles = [0, 0, 0];

    % Verify the result
    verifyEqual(testCase, A.Angle, expectedAngles, 'AbsTol', 1e-10, ...
        'Tpi conversion did not produce the expected result.');
end

function testMpi(testCase)
    % Test converting angles to the range of -pi to pi
    Data = [-3 * pi / 2, pi, 3 * pi / 2];  % Angles in radians
    A = AstroAngle(Data, 'rad');

    % Convert to range -pi to pi
    A.mpi();

    % Expected result
    expectedAngles = [-pi / 2, pi, -pi / 2];

    % Verify the result
    verifyEqual(testCase, A.Angle, expectedAngles, 'AbsTol', 1e-10, ...
        'Mpi conversion did not produce the expected result.');
end

function testSinFunction(testCase)
    % Test the sin function of an AstroAngle object
    Data = [0, pi / 2, pi];  % Angles in radians
    A = AstroAngle(Data, 'rad');

    % Calculate the sine of the angles
    Result = A.sin();

    % Expected result
    expectedSine = [0, 1, 0];

    % Verify the result
    verifyEqual(testCase, Result, expectedSine, 'AbsTol', 1e-10, ...
        'Sin function did not produce the expected result.');
end

function testCosFunction(testCase)
    % Test the cos function of an AstroAngle object
    Data = [0, pi / 2, pi];  % Angles in radians
    A = AstroAngle(Data, 'rad');

    % Calculate the cosine of the angles
    Result = A.cos();

    % Expected result
    expectedCosine = [1, 0, -1];

    % Verify the result
    verifyEqual(testCase, Result, expectedCosine, 'AbsTol', 1e-10, ...
        'Cos function did not produce the expected result.');
end

function testTanFunction(testCase)
    % Test the tan function of an AstroAngle object
    Data = [0, pi / 4, pi / 2];  % Angles in radians
    A = AstroAngle(Data, 'rad');

    % Calculate the tangent of the angles
    Result = A.tan();

    % Expected result
    expectedTangent = [0, 1, Inf];

    % Verify the result
    verifyEqual(testCase, Result, expectedTangent, 'AbsTol', 1e-10, ...
        'Tan function did not produce the expected result.');
end
