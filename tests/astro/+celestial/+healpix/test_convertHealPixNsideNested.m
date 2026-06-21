function tests = test_convertHealPixNsideNested
    % Unit tests for convertHealPixNsideNested.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testDowngradeMapping(testCase)
    % Lower NSide maps pix via floor(pix / Ratio^2).
    NSide = 16;
    NewNSide = 4;
    Pix = [0, 100, 500, 1234];
    NewPix = celestial.healpix.convertHealPixNsideNested(NSide, Pix, NewNSide);
    Ratio = (NSide / NewNSide)^2;
    Expected = floor(Pix / Ratio);
    testCase.verifyEqual(NewPix, Expected);
end

function testDocExample(testCase)
    % Doc example: downgrade pixel 1234 from NSide 16 to 4.
    NewPix = celestial.healpix.convertHealPixNsideNested(16, 1234, 4);
    testCase.verifyEqual(NewPix, floor(1234 / 16));
end

function testFullIdInputOutput(testCase)
    % Empty NSide means PixID is FullID; output preserves FullID convention.
    FullId = 4 * 16^2 + 1234;
    NewFullId = celestial.healpix.convertHealPixNsideNested([], FullId, 4);
    [NewNSide, NewLocal] = celestial.healpix.uniqueId2pix([], NewFullId);
    testCase.verifyEqual(NewNSide, 4);
    testCase.verifyEqual(NewLocal, floor(1234 / 16));
end

function testNewNSideGreaterThanOldErrors(testCase)
    % Upsampling is ambiguous and must be rejected.
    testCase.verifyError( ...
        @() celestial.healpix.convertHealPixNsideNested(4, 0, 16), ...
        'MATLAB:error');
end

function testNonPowerOfTwoNewNSideErrors(testCase)
    % NewNSide must be a positive power of two.
    testCase.verifyError( ...
        @() celestial.healpix.convertHealPixNsideNested(16, 0, 6), ...
        'MATLAB:error');
end
