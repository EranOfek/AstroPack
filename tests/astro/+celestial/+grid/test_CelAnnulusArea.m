function tests = TestCelAnnulusArea
    % TestCelAnnulusArea
    % Function-based unit tests for the cel_annulus_area function.
    % This function returns a test suite containing multiple test functions,
    % each of which tests a different aspect of the cel_annulus_area function.
    %
    % Author: Yarin Shani
    % Date: 2024-09-07
    %
    
    tests = functiontests(localfunctions);
end

%% Test Functions

function testFullSphereSteradians(testCase)
    % Test area of the entire celestial sphere in steradians
    RadIn = 0;
    RadOut = pi;
    expectedArea = 4 * pi;  % Full celestial sphere in steradians
    
    % Run the function
    resultArea = celestial.coo.cel_annulus_area(RadIn, RadOut, 'sr');
    
    % Verify the result
    verifyEqual(testCase, resultArea, expectedArea, 'Full celestial sphere area in steradians failed.');
end

function testFullSphereDegrees(testCase)
    % Test area of the entire celestial sphere in square degrees
    RadIn = 0;
    RadOut = pi;
    RAD = 180 / pi;
    expectedArea = 4 * pi * RAD^2;  % Full celestial sphere in square degrees
    
    % Run the function
    resultArea = celestial.coo.cel_annulus_area(RadIn, RadOut, 'deg');
    
    % Verify the result
    verifyEqual(testCase, resultArea, expectedArea, 'Full celestial sphere area in degrees failed.');
end

function testFullSphereArcminutes(testCase)
    % Test area of the entire celestial sphere in square arcminutes
    RadIn = 0;
    RadOut = pi;
    RAD = 180 / pi;
    DEG_ARCMIN = 60;
    expectedArea = 4 * pi * RAD^2 * DEG_ARCMIN^2;  % Full celestial sphere in square arcminutes
    
    % Run the function
    resultArea = celestial.coo.cel_annulus_area(RadIn, RadOut, 'am');
    
    % Verify the result
    verifyEqual(testCase, resultArea, expectedArea, 'Full celestial sphere area in arcminutes failed.');
end

function testFullSphereArcseconds(testCase)
    % Test area of the entire celestial sphere in square arcseconds
    RadIn = 0;
    RadOut = pi;
    RAD = 180 / pi;
    DEG_ARCSEC = 3600;
    expectedArea = 4 * pi * RAD^2 * DEG_ARCSEC^2;  % Full celestial sphere in square arcseconds
    
    % Run the function
    resultArea = celestial.coo.cel_annulus_area(RadIn, RadOut, 'as');
    
    % Verify the result
    verifyEqual(testCase, resultArea, expectedArea, 'Full celestial sphere area in arcseconds failed.');
end

function testNonZeroInnerRadius(testCase)
    % Test annulus area with a non-zero inner radius
    RadIn = pi/6;
    RadOut = pi/3;
    
    % Compute expected area in steradians
    CapOut = 2 * pi * (1 - cos(RadOut));
    CapIn = 2 * pi * (1 - cos(RadIn));
    expectedArea = CapOut - CapIn;
    
    % Run the function
    resultArea = celestial.coo.cel_annulus_area(RadIn, RadOut, 'sr');
    
    % Verify the result
    verifyEqual(testCase, resultArea, expectedArea, 'Non-zero inner radius test failed.');
end

function testInvalidNumberOfArguments(testCase)
       % Test for invalid number of input arguments (too few arguments)
    verifyError(testCase, @() celestial.coo.cel_annulus_area(1),'celestial:coo:cel_annulus_area:illegalNumberOfInputs', ...
        'Error handling for too few arguments failed.');
    
    % Test for invalid number of input arguments (too many arguments)
    verifyError(testCase, @() celestial.coo.cel_annulus_area(1, 2, 'sr', 'extra'), 'MATLAB:TooManyInputs', ...
        'Error handling for too many arguments failed.');
end
