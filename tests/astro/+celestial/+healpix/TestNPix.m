classdef TestNPix < matlab.unittest.TestCase
    % TestNPix  Unit tests for celestial.healpix.nPix.

    methods (Test)

        function testScalarFormula(testCase)
            % HEALPix total pixel count is 12 * Nside^2 (indices 0 .. Npix-1).
            NSide = 16;
            testCase.verifyEqual(celestial.healpix.nPix(NSide), 12 * NSide^2);
        end

        function testVectorizedInput(testCase)
            % nPix accepts a vector of Nside values and returns matching counts.
            NSide = [1, 2, 4, 8, 16, 32];
            Expected = 12 * NSide.^2;
            testCase.verifyEqual(celestial.healpix.nPix(NSide), Expected);
        end

        function testMinimumResolution(testCase)
            % At Nside=1 the sky is partitioned into 12 base faces.
            testCase.verifyEqual(celestial.healpix.nPix(1), 12);
        end

    end
end
