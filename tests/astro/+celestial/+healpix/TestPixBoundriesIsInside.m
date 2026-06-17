classdef TestPixBoundriesIsInside < matlab.unittest.TestCase
    % TestPixBoundriesIsInside  Pins current error-by-design behavior.
    %
    % pixBoundries and isInside are known broken (pixBoundries always errors).
    % These tests document existing behavior; they are NOT desired behavior.

    methods (Test)

        function testPixBoundriesAlwaysErrors(testCase)
            % pixBoundries immediately errors with 'Likely not correct'.
            NSide = 16;
            Pix = 197;
            testCase.verifyError( ...
                @() celestial.healpix.pixBoundries(NSide, Pix, 'nested'), ...
                'MATLAB:error');
        end

        function testIsInsideErrorsViaPixBoundries(testCase)
            % isInside depends on pixBoundries and therefore errors today.
            NSide = 16;
            Pix = 197;
            Lon = 1.0;
            Lat = 0.5;
            testCase.verifyError( ...
                @() celestial.healpix.isInside(NSide, Pix, Lon, Lat, 'Type', 'nested'), ...
                'MATLAB:error');
        end

    end
end
