function run_HealpixUnitTests()
    % Delegate to the canonical healpix test runner in tests/astro/.
    RunScript = fullfile(fileparts(fileparts(mfilename('fullpath'))), 'runHealpixUnitTests.m');
    run(RunScript);
end
