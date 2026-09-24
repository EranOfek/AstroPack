function tests = test_renameCat
    % Unit tests for catsHTM.renameCat.
    %
    % Builds a tiny synthetic catsHTM catalog in a temp dir, renames it,
    % and verifies the renamed catalog is queryable (and DryRun is a no-op).
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    % Build a small synthetic catsHTM catalog 'RENSRC' in a temp dir.
    RAD = 180./pi;
    Tmp = tempname;
    mkdir(Tmp);
    rng(17);
    N   = 200;
    RA  = (45 + 0.2.*(rand(N,1)-0.5))./RAD;   % [rad]
    Dec = ( 0 + 0.2.*(rand(N,1)-0.5))./RAD;   % [rad]
    Val = rand(N,1);
    Mat = [RA, Dec, Val];

    Old = pwd;
    cd(Tmp);
    VO.prep.build_htm_catalog(Mat, 'CatName','RENSRC', 'HTM_Level',4, ...
        'ColCell',{'RA','Dec','Val'}, 'ColUnits',{'rad','rad',''});
    cd(Old);
    addpath(Tmp);

    testCase.TestData.Dir = Tmp;
    testCase.TestData.RA  = 45./RAD;   % [rad]
    testCase.TestData.Dec = 0;         % [rad]
end

function teardown(testCase)
    Tmp = testCase.TestData.Dir;
    if any(strcmp(strsplit(path, pathsep), Tmp))
        rmpath(Tmp);
    end
    if isfolder(Tmp)
        rmdir(Tmp, 's');
    end
end

%% Test Functions

function testRenameThenConeSearch(testCase)
    % renameCat renames data/index/colcell and the new catalog is queryable.
    TD = testCase.TestData;
    R  = catsHTM.renameCat('RENSRC', 'RENDST', TD.Dir);

    verifyGreaterThan(testCase, R.DataFiles, 0, 'No data files renamed.');
    verifyTrue(testCase, R.Index,   'Index not rewritten.');
    verifyTrue(testCase, R.ColCell, 'ColCell not renamed.');

    % old-name files gone, new-name files present
    verifyEmpty(testCase, dir(fullfile(TD.Dir, 'RENSRC_htm*')), ...
        'Old-name files still present.');
    verifyNotEmpty(testCase, dir(fullfile(TD.Dir, 'RENDST_htm_*.hdf5')), ...
        'New data files missing.');
    verifyTrue(testCase, isfile(fullfile(TD.Dir, 'RENDST_htm.hdf5')), ...
        'New index missing.');

    % renamed catalog is queryable and returns sources with the right columns
    [Cat, CC] = catsHTM.cone_search('RENDST', TD.RA, TD.Dec, 7200);  % 2 deg cone
    verifyGreaterThan(testCase, size(Cat, 1), 0, 'cone_search returned no rows.');
    verifyEqual(testCase, CC(:).', {'RA', 'Dec', 'Val'}, 'ColCell mismatch after rename.');
end

function testDryRunLeavesFilesUnchanged(testCase)
    % DryRun reports actions but renames nothing.
    TD = testCase.TestData;
    Before = sort({dir(fullfile(TD.Dir, 'RENSRC_htm*')).name});
    R = catsHTM.renameCat('RENSRC', 'RENDST', TD.Dir, 'DryRun', true);
    After  = sort({dir(fullfile(TD.Dir, 'RENSRC_htm*')).name});

    verifyEqual(testCase, After, Before, 'DryRun modified the source files.');
    verifyEmpty(testCase, dir(fullfile(TD.Dir, 'RENDST_htm*')), ...
        'DryRun created new-name files.');
    verifyGreaterThan(testCase, R.DataFiles, 0, 'DryRun should still count files.');
end
