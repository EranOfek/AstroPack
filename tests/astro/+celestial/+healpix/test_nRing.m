function tests = test_nRing
    % Unit tests for celestial.healpix.nRing.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testFormulaScalar(testCase)
    % Number of latitude rings is 4*Nside+1 (including both poles).
    NSide = 16;
    testCase.verifyEqual(celestial.healpix.nRing(NSide), 4 * NSide + 1);
end

function testFormulaVector(testCase)
    % nRing supports vectorized NSide input.
    NSide = [1, 2, 4, 8, 16];
    Expected = 4 * NSide + 1;
    testCase.verifyEqual(celestial.healpix.nRing(NSide), Expected);
end

function testMinimumNSide(testCase)
    % At Nside=1 there are 5 rings (4*1+1).
    testCase.verifyEqual(celestial.healpix.nRing(1), 5);
end
