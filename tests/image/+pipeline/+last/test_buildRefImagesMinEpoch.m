function tests = test_buildRefImagesMinEpoch
    % Unit tests for the MinEpoch guard of pipeline.last.reference.buildRefImages (issue #1404).
    %
    % A mock DB (a struct whose 'query' field returns a fixed table of crops) stands in
    % for ClickHouse, and the fields are ad hoc sky points, so no image is read and
    % no catalog is fetched: every case below stops at the epoch count of the DB.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testUnknownActionIsRejected(testCase)
    testCase.verifyError(@() build(mockDB(3), 'MinEpochAction','none'), 'buildRefImages:MinEpochAction');
end

function testErrorActionThrows(testCase)
    testCase.verifyError(@() build(mockDB(3), 'MinEpochAction','error'), 'buildRefImages:TooFewEpochs');
end

function testSkipActionSkipsFewEpochs(testCase)
    % with 'skip', a field with 3 epochs in the DB is not built
    [Result, Info] = build(mockDB(3), 'MinEpochAction','skip');
    testCase.verifyTrue(isemptyImage(Result), 'A skipped field must give an empty AstroImage.');
    testCase.verifyEqual(Info.RefMode, "skipped");
    testCase.verifyEqual(Info.Nepoch, 3);
end

function testBestActionSkipsFieldWithoutEpochs(testCase)
    % with the default 'best', a field with no epoch at all is still skipped
    [Result, Info] = build(mockDB(0));
    testCase.verifyTrue(isemptyImage(Result), 'A field with no epoch must give an empty AstroImage.');
    testCase.verifyEqual(Info.RefMode, "skipped");
    testCase.verifyEqual(Info.Nepoch, 0);
end

function testInfoHasOneElementPerField(testCase)
    % every field has its own Info element, also when the fields are skipped
    [Result, Info] = build(mockDB(2), 'MinEpochAction','skip', 'RA',[210 211], 'Dec',[54 54], 'RefName',["a","b"]);
    testCase.verifySize(Result, [2 1]);
    testCase.verifySize(Info, [1 2]);
    testCase.verifyEqual([Info.RefMode], ["skipped","skipped"]);
    testCase.verifyEqual([Info.Nepoch], [2 2]);
    testCase.verifyEqual([Info.CounterBadWCS], [0 0]);
end

%% Helpers

function [Result, Info] = build(DB, varargin)
    % one ad hoc field unless RA/Dec are given; nothing is written
    Args = [{'DB',DB, 'RA',210.8, 'Dec',54.3, 'Write2Disk',false, 'Verbose',0}, varargin];
    [Result, Info] = pipeline.last.reference.buildRefImages([], Args{:});
end

function DB = mockDB(Nepoch)
    % a DB whose query returns 2 crops for each of Nepoch epochs of one telescope
    N = 2 * Nepoch;
    T = table(ones(N,1), ones(N,1), 2460000 + repelem((1:Nepoch).', 2), 2.5 * ones(N,1), ...
              'VariableNames', {'mountnum','camnum','jd_start','fwhm'});
    DB = struct('query', @(Q) T);
end

function Flag = isemptyImage(AI)
    Flag = all(arrayfun(@(A) isempty(A.Image), AI(:)));
end
