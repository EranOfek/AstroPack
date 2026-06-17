classdef HealpixTestHelper
    % HealpixTestHelper  Shared utilities for celestial.healpix unit tests.
    %
    % Provides mex-availability checks and assume-skip helpers so tests
    % degrade gracefully when MEX binaries or external libraries are missing.

    methods (Static)

        function Available = mexAvailable(MexName)
            % mexAvailable  True when a celestial.healpix.mex binary exists.
            MexFcn = ['celestial.healpix.mex.' MexName];
            Available = (exist(MexFcn, 'file') == 3);
        end

        function assumeMex(testCase, MexName)
            % assumeMex  Skip the current test when the named MEX is unavailable.
            testCase.assumeTrue( ...
                HealpixTestHelper.mexAvailable(MexName), ...
                sprintf('Skipping: MEX "%s" is not compiled.', MexName));
        end

        function assumeCoreAngPixMex(testCase)
            % assumeCoreAngPixMex  Skip when self-contained ang2pix/pix2ang MEX missing.
            HealpixTestHelper.assumeMex(testCase, 'ang2pix_nested');
            HealpixTestHelper.assumeMex(testCase, 'pix2ang_nested');
        end

        function assumeMappingToolbox(testCase)
            % assumeMappingToolbox  Skip when Mapping Toolbox reckon is unavailable.
            testCase.assumeTrue( ...
                exist('reckon', 'file') == 2, ...
                'Skipping: Mapping Toolbox function reckon is not available.');
        end

        function assumeFunctionExists(testCase, FunctionName)
            % assumeFunctionExists  Skip when a MATLAB function file is missing.
            testCase.assumeTrue( ...
                exist(FunctionName, 'file') == 2, ...
                sprintf('Skipping: function "%s" is not available.', FunctionName));
        end

        function NSide = defaultTestNSide()
            % defaultTestNSide  Small power-of-two NSide used across tests.
            NSide = 8;
        end

    end
end
