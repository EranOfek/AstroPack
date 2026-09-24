% runHealpixUnitTests  Compile core MEX and run healpix unit test suite.
%
% Usage:
%   matlab -batch "run('C:\Ultrasat\AstroPack\tests\astro\runHealpixUnitTests.m')"

AstroPackRoot = fileparts(fileparts(fileparts(mfilename('fullpath'))));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'astro')));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'base')));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'util')));
addpath(genpath(fullfile(AstroPackRoot, 'matlab', 'external')));

MexDir = fullfile(AstroPackRoot, 'matlab', 'astro', '+celestial', '+healpix', '+mex');
CoreMex = {'ang2pix_nested.cpp', 'ang2pix_ring.cpp', ...
    'pix2ang_nested.cpp', 'pix2ang_ring.cpp'};

fprintf('=== HEALPix MEX compilation ===\n');
OldDir = cd(MexDir);
try
    for I = 1:numel(CoreMex)
        Src = CoreMex{I};
        if exist(Src, 'file') ~= 2
            fprintf('SKIP compile: source missing %s\n', Src);
            continue;
        end
        fprintf('Compiling %s ...\n', Src);
        try
            mex('-O', Src);
            fprintf('OK: %s\n', Src);
        catch ME
            fprintf('FAIL: %s - %s\n', Src, ME.message);
        end
    end
finally
    cd(OldDir);
end

TestFolder = fullfile(AstroPackRoot, 'tests', 'astro', '+celestial', '+healpix');
addpath(TestFolder);

fprintf('\n=== Running HEALPix unit tests ===\n');
Suite = matlab.unittest.TestSuite.fromFolder(TestFolder, 'IncludingSubfolders', false);
Runner = matlab.unittest.TestRunner.withTextOutput( ...
    'OutputDetail', matlab.unittest.Verbosity.Detailed);
Results = Runner.run(Suite);

fprintf('\n=== Summary ===\n');
fprintf('Total:   %d\n', numel(Results));
fprintf('Passed:  %d\n', sum([Results.Passed]));
fprintf('Failed:  %d\n', sum([Results.Failed]));
fprintf('Incomplete: %d\n', sum([Results.Incomplete]));
Skipped = sum(strcmp({Results.Status}, 'Skipped'));
fprintf('Skipped: %d\n', Skipped);

if any([Results.Failed])
    fprintf('\nFailed tests:\n');
    Failed = Results([Results.Failed]);
    for I = 1:numel(Failed)
        fprintf('  %s\n', Failed(I).Name);
    end
end

if Skipped > 0
    fprintf('\nSkipped tests:\n');
    SkippedResults = Results(strcmp({Results.Status}, 'Skipped'));
    for I = 1:numel(SkippedResults)
        fprintf('  %s\n', SkippedResults(I).Name);
    end
end

if any([Results.Failed])
    error('HEALPix unit tests failed.');
end
