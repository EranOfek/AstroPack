classdef TestConvertHealPix2HighNsideNested < matlab.unittest.TestCase
    % TestConvertHealPix2HighNsideNested  Unit tests for convertHealPix2highNsideNested.

    methods (Test)

        function testDocExample(testCase)
            % Doc example: pixel 0 at NSide 2^8 spans [0, 65535] at NSide 2^16.
            [Low, High] = celestial.healpix.convertHealPix2highNsideNested(2^8, 0, 2^16);
            testCase.verifyEqual(Low, 0);
            testCase.verifyEqual(High, 65535);
        end

        function testChildRangeWidth(testCase)
            % Child range width equals (NewNSide/NSide)^2.
            NSide = 8;
            NewNSide = 32;
            Pix = 10;
            [Low, High] = celestial.healpix.convertHealPix2highNsideNested(NSide, Pix, NewNSide);
            Factor = (NewNSide / NSide)^2;
            testCase.verifyEqual(Low, Pix * Factor);
            testCase.verifyEqual(High, Low + Factor - 1);
        end

        function testFullIdPath(testCase)
            % FullID input returns FullID child range at higher NSide.
            FullId = 4 * 8^2 + 5;
            [Low, High] = celestial.healpix.convertHealPix2highNsideNested([], FullId, 2^16);
            testCase.verifyGreaterThanOrEqual(Low, 4 * 2^16^2);
            testCase.verifyGreaterThanOrEqual(High, Low);
        end

        function testNewNSideLessThanOldErrors(testCase)
            % This function only upgrades resolution.
            testCase.verifyError( ...
                @() celestial.healpix.convertHealPix2highNsideNested(16, 0, 8), ...
                'MATLAB:error');
        end

        function testNonPowerOfTwoNewNSideErrors(testCase)
            % NewNSide must be a positive power of two.
            testCase.verifyError( ...
                @() celestial.healpix.convertHealPix2highNsideNested(8, 0, 12), ...
                'MATLAB:error');
        end

    end
end
