function tests = test_add_remove_source
    % Function-based unit tests for catsHTM.add_source / remove_source.
    % Builds a tiny synthetic catsHTM catalog under a temp BaseDir and
    % exercises add/remove against it. No data on /euclid is touched.
    tests = functiontests(localfunctions);
end


% ------------------------------------------------------------------------
% Setup / teardown
% ------------------------------------------------------------------------

function setupOnce(testCase)
    Tmp = tempname;
    mkdir(Tmp);
    testCase.TestData.BaseDir   = Tmp;
    testCase.TestData.CatName   = 'TESTCAT';
    testCase.TestData.CatRelDir = '/TESTCAT/';
    testCase.TestData.Level     = 4;
    testCase.TestData.SortCol   = 2;
    testCase.TestData.StepRows  = 30;
    testCase.TestData.ColCell   = {'RA','Dec','Mag','MagErr'};
    testCase.TestData.ColUnits  = {'rad','rad','mag','mag'};

    buildSyntheticCatalog(testCase.TestData);
end

function teardownOnce(testCase)
    if isfield(testCase.TestData,'BaseDir') && isfolder(testCase.TestData.BaseDir)
        rmdir(testCase.TestData.BaseDir, 's');
    end
    OutRoot = getOutRoot(testCase);
    if isfolder(OutRoot)
        rmdir(OutRoot, 's');
    end
end


% ------------------------------------------------------------------------
% add_source tests
% ------------------------------------------------------------------------

function testAddToPopulatedCellMatrix(testCase)
    OutDir = freshOutDir(testCase, 'add_pop_mat');
    TD = testCase.TestData;

    NewMat = [1.0,  0.5,  17.5, 0.05;
              1.01, 0.51, 18.0, 0.06];

    R = catsHTM.add_source(TD.CatName, NewMat, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesAdded, 2);
    verifyGreaterThanOrEqual(testCase, R.CellsTouched, 1);
    verifyTrue(testCase, any(contains(R.ModifiedFiles, 'TESTCAT_htm.hdf5')));
    verifyAddedSourcesPresent(testCase, R, NewMat);
end

function testAddViaAstroCatalog(testCase)
    OutDir = freshOutDir(testCase, 'add_astrocat');
    TD = testCase.TestData;

    AC = AstroCatalog;
    AC.Catalog  = [2.0, 0.3, 19.0, 0.10];
    AC.ColNames = {'RA','Dec','Mag','MagErr'};
    AC.ColUnits = {'rad','rad','mag','mag'};

    R = catsHTM.add_source(TD.CatName, AC, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesAdded, 1);
    verifyAddedSourcesPresent(testCase, R, AC.Catalog);
end

function testAddProjectsByColumnName(testCase)
    % AstroCatalog with columns out of order and one missing -> NaN
    OutDir = freshOutDir(testCase, 'add_project');
    TD = testCase.TestData;

    AC = AstroCatalog;
    AC.Catalog  = [16.0, 0.4, 1.5];     % Mag, Dec, RA -- shuffled, no MagErr
    AC.ColNames = {'Mag','Dec','RA'};
    AC.ColUnits = {'mag','rad','rad'};

    R = catsHTM.add_source(TD.CatName, AC, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesAdded, 1);

    Cell = readBackAddedRow(R, TD, 1.5, 0.4);
    verifyEqual(testCase, Cell(1), 1.5,  'AbsTol', 1e-12);  % RA
    verifyEqual(testCase, Cell(2), 0.4,  'AbsTol', 1e-12);  % Dec
    verifyEqual(testCase, Cell(3), 16.0, 'AbsTol', 1e-12);  % Mag
    verifyTrue(testCase, isnan(Cell(4)));                    % MagErr -> NaN
end

function testAddDryRunWritesNothing(testCase)
    OutDir = freshOutDir(testCase, 'add_dry');
    TD = testCase.TestData;

    NewMat = [1.0, 0.5, 17.5, 0.05];
    R = catsHTM.add_source(TD.CatName, NewMat, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'DryRun', true, 'Verbose', false);

    verifyEqual(testCase, R.SourcesAdded, 1);
    % No files written
    Listing = dir(fullfile(OutDir, TD.CatRelDir, '*.hdf5'));
    verifyEmpty(testCase, Listing);
end

function testAddDuplicateError(testCase)
    OutDir = freshOutDir(testCase, 'add_dup');
    TD = testCase.TestData;

    Existing = readSeedRow(TD);
    % Same RA/Dec as a seeded source should trigger duplicate error
    NewMat = Existing(1, :);

    verifyError(testCase, ...
        @() catsHTM.add_source(TD.CatName, NewMat, OutDir, ...
            'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
            'DuplicateRadius', 1, 'OnDuplicate', 'error', ...
            'Verbose', false), ...
        'catsHTM:add_source:Duplicate');
end

function testAddDuplicateSkip(testCase)
    OutDir = freshOutDir(testCase, 'add_dup_skip');
    TD = testCase.TestData;

    Existing = readSeedRow(TD);
    NewMat = [Existing(1, :);                              % duplicate
              1.4, 0.6, 18.5, 0.08];                       % unique

    R = catsHTM.add_source(TD.CatName, NewMat, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'DuplicateRadius', 1, 'OnDuplicate', 'skip', ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesAdded,   1);
    verifyEqual(testCase, R.SourcesSkipped, 1);
end


% ------------------------------------------------------------------------
% remove_source tests
% ------------------------------------------------------------------------

function testRemoveExistingSource(testCase)
    OutDir = freshOutDir(testCase, 'rm_exist');
    TD = testCase.TestData;

    Existing = readSeedRow(TD);
    Target = Existing(1, :);

    R = catsHTM.remove_source(TD.CatName, Target(1), Target(2), OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'SearchRadius', 1, 'RadiusUnits', 'arcsec', ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesRemoved, 1);
    verifyFalse(testCase, R.NotFound(1));
    verifyTrue(testCase, any(contains(R.ModifiedFiles, 'TESTCAT_htm.hdf5')));
end

function testRemoveNoMatchWarn(testCase)
    OutDir = freshOutDir(testCase, 'rm_nomatch');
    TD = testCase.TestData;

    % Position far from any seeded source. Use lastwarn (rather than
    % verifyWarning) so we can also inspect the result struct.
    lastwarn('', '');
    OldState = warning('on', 'catsHTM:remove_source:NoMatch');
    Cleanup  = onCleanup(@() warning(OldState));

    R = catsHTM.remove_source(TD.CatName, 0.01, -0.99, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'SearchRadius', 1, 'RadiusUnits', 'arcsec', ...
        'OnNoMatch', 'warn', 'Verbose', false);
    [~, MsgId] = lastwarn;

    verifyEqual(testCase, R.SourcesRemoved, 0);
    verifyTrue(testCase,  R.NotFound(1));
    verifyEqual(testCase, MsgId, 'catsHTM:remove_source:NoMatch');
end

function testRemoveLastSourceClearsCell(testCase)
    % Remove every source seeded into a cell -> dataset deleted, Nsrc=0
    OutDir = freshOutDir(testCase, 'rm_clear');
    TD = testCase.TestData;

    Seeded = readSeedRow(TD);  % returns the full seed for the populated cell
    R = catsHTM.remove_source(TD.CatName, Seeded(:,1), Seeded(:,2), OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'SearchRadius', 1, 'RadiusUnits', 'arcsec', ...
        'OnMultiMatch', 'all', ...
        'Verbose', false);

    verifyEqual(testCase, R.SourcesRemoved, size(Seeded,1));

    % Index file must report Nsrc=0 for the affected cell. The HTM index
    % is stored as single in HDF5; cast for class-agnostic comparison.
    DstIndex = fullfile(OutDir, TD.CatRelDir, sprintf('%s_htm.hdf5', TD.CatName));
    DataHTM = HDF5.load(DstIndex, sprintf('%s_HTM', TD.CatName));
    SeedCellID = TD.SeedCellID;
    verifyEqual(testCase, double(DataHTM(SeedCellID, 13)), 0);
end


% ------------------------------------------------------------------------
% insertColumn / removeColumn tests
% ------------------------------------------------------------------------

function testInsertColumnAtEnd(testCase)
    % Insert JD_Added with FillValue=0 at the end and verify
    OutDir = freshOutDir(testCase, 'insert_end');
    TD = testCase.TestData;

    R = catsHTM.insertColumn(TD.CatName, 'JD_Added', 'day', OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'FillValue', 0, 'Verbose', false);

    verifyEqual(testCase, R.NewColCell, [TD.ColCell, {'JD_Added'}]);
    verifyEqual(testCase, R.NewSortCol, TD.SortCol);   % SortCol unchanged
    verifyGreaterThanOrEqual(testCase, R.CellsTouched, 1);

    % Inspect rewritten seed cell
    [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
        TD.CatName, TD.SeedCellID, 100);
    Cat = HDF5.load(fullfile(OutDir, TD.CatRelDir, DataFileName), ['/' DataSetName]);
    Seeded = readSeedRow(TD);
    verifyEqual(testCase, size(Cat,2), size(Seeded,2) + 1);
    verifyEqual(testCase, Cat(:, end), zeros(size(Seeded,1), 1));
    verifyEqual(testCase, Cat(:, 1:end-1), sortrows(Seeded, TD.SortCol), 'AbsTol', 0);

    % Loaded ColCell .mat must reflect new layout
    S = load(fullfile(OutDir, TD.CatRelDir, sprintf('%s_htmColCell.mat', TD.CatName)));
    verifyEqual(testCase, S.ColCell, [TD.ColCell, {'JD_Added'}]);
    verifyEqual(testCase, S.ColUnits, [TD.ColUnits, {'day'}]);
end

function testInsertColumnDuplicateName(testCase)
    OutDir = freshOutDir(testCase, 'insert_dup');
    TD = testCase.TestData;

    verifyError(testCase, ...
        @() catsHTM.insertColumn(TD.CatName, 'Mag', 'mag', OutDir, ...
            'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
            'Verbose', false), ...
        'catsHTM:insertColumn:DuplicateName');
end

function testInsertColumnFunctionFill(testCase)
    % FillValue function: derive new column from existing data
    OutDir = freshOutDir(testCase, 'insert_fun');
    TD = testCase.TestData;

    R = catsHTM.insertColumn(TD.CatName, 'TwoMag', 'mag', OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'FillValue', @(M) 2 .* M(:,3), 'Verbose', false);   % 2 * Mag

    [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
        TD.CatName, TD.SeedCellID, 100);
    Cat = HDF5.load(fullfile(OutDir, TD.CatRelDir, DataFileName), ['/' DataSetName]);

    verifyEqual(testCase, size(Cat,2), numel(R.NewColCell));
    verifyEqual(testCase, Cat(:, end), 2 .* Cat(:, 3), 'AbsTol', 1e-12);
end

function testRemoveColumnByName(testCase)
    OutDir = freshOutDir(testCase, 'rm_col');
    TD = testCase.TestData;

    R = catsHTM.removeColumn(TD.CatName, 'MagErr', OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'Verbose', false);

    verifyEqual(testCase, R.NewColCell, {'RA','Dec','Mag'});
    verifyEqual(testCase, R.NewSortCol, TD.SortCol);   % unchanged (Pos > SortCol)
    verifyEqual(testCase, R.RemovedAt, 4);

    [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
        TD.CatName, TD.SeedCellID, 100);
    Cat = HDF5.load(fullfile(OutDir, TD.CatRelDir, DataFileName), ['/' DataSetName]);
    Seeded = readSeedRow(TD);
    verifyEqual(testCase, size(Cat,2), size(Seeded,2) - 1);
    verifyEqual(testCase, Cat, sortrows(Seeded(:, [1 2 3]), TD.SortCol), 'AbsTol', 0);

    S = load(fullfile(OutDir, TD.CatRelDir, sprintf('%s_htmColCell.mat', TD.CatName)));
    verifyEqual(testCase, S.ColCell, {'RA','Dec','Mag'});
    verifyEqual(testCase, S.ColUnits, {'rad','rad','mag'});
end

function testRemoveColumnRefusesRA(testCase)
    OutDir = freshOutDir(testCase, 'rm_ra');
    TD = testCase.TestData;
    verifyError(testCase, ...
        @() catsHTM.removeColumn(TD.CatName, 'RA', OutDir, ...
            'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
            'Verbose', false), ...
        'catsHTM:removeColumn:CoordColumn');
end

function testRemoveColumnRefusesSortCol(testCase)
    OutDir = freshOutDir(testCase, 'rm_sort');
    TD = testCase.TestData;
    verifyError(testCase, ...
        @() catsHTM.removeColumn(TD.CatName, 'Dec', OutDir, ...
            'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
            'Verbose', false), ...
        'catsHTM:removeColumn:CoordColumn');
end

function testRemoveColumnNotFound(testCase)
    OutDir = freshOutDir(testCase, 'rm_missing');
    TD = testCase.TestData;
    verifyError(testCase, ...
        @() catsHTM.removeColumn(TD.CatName, 'DoesNotExist', OutDir, ...
            'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
            'Verbose', false), ...
        'catsHTM:removeColumn:NotFound');
end


% ------------------------------------------------------------------------
% Helpers
% ------------------------------------------------------------------------

function buildSyntheticCatalog(TD)
    % Build minimum-viable catsHTM catalog: ColCell file, index file with
    % HTM tree at TD.Level, and one populated leaf cell.
    CatDir = fullfile(TD.BaseDir, TD.CatRelDir);
    if ~isfolder(CatDir), mkdir(CatDir); end

    PrevDir = pwd;
    cleanup = onCleanup(@() cd(PrevDir));
    cd(CatDir);

    % Build HTM and write index file
    [HTM, LevelH] = celestial.htm.htm_build(TD.Level);
    Nhtm = numel(HTM);

    % Pick first leaf cell at the target level for seeding
    LeafLevel = LevelH(end);
    SeedCellID = LeafLevel.ptr(1);
    assignin('caller', 'unused', 0);  % keep MATLAB editor calm
    TD.SeedCellID = SeedCellID;       %#ok<NASGU>  -- set for caller below

    % Two seed sources whose RA/Dec lies inside the chosen leaf cell.
    Coo = HTM(SeedCellID).coo;       % vertices in radians
    MeanRA  = mean(Coo(:,1));
    MeanDec = mean(Coo(:,2));
    SeedMat = [MeanRA,         MeanDec,         15.0, 0.04;
               MeanRA + 1e-5,  MeanDec + 1e-5,  16.0, 0.05];

    % ColCell .mat (named exactly how catsHTM.load_colcell expects)
    ColCell  = TD.ColCell;  %#ok<NASGU>
    ColUnits = TD.ColUnits; %#ok<NASGU>
    save(fullfile(CatDir, sprintf('%s_htmColCell.mat', TD.CatName)), ...
        'ColCell','ColUnits');

    % Save the seed cell using catsHTM.save_cat
    [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
        TD.CatName, SeedCellID, 100);
    SortCol  = TD.SortCol;
    StepRows = TD.StepRows;
    catsHTM.save_cat(fullfile(CatDir, DataFileName), ...
        DataSetName, SeedMat, SortCol, StepRows);

    % Write the index file with Nsrc populated for the seed cell
    Nsrc = [SeedCellID, size(SeedMat, 1)];
    [IndexFileName, IndexVarName] = catsHTM.get_index_filename(TD.CatName);
    catsHTM.save_htm_ind(HTM, fullfile(CatDir, IndexFileName), ...
        IndexVarName, {}, Nsrc);

    % Stash seed details into a sidecar file the helpers below can read,
    % since this build function operates on a copy of TD.
    SeedInfo = struct('SeedCellID', SeedCellID, 'SeedMat', SeedMat); %#ok<NASGU>
    save(fullfile(CatDir, '_seedinfo.mat'), 'SeedInfo');
end

function SeedMat = readSeedRow(TD)
    S = load(fullfile(TD.BaseDir, TD.CatRelDir, '_seedinfo.mat'));
    SeedMat = S.SeedInfo.SeedMat;
end

function setSeedCellIDIntoTD(testCase)
    TD = testCase.TestData;
    S = load(fullfile(TD.BaseDir, TD.CatRelDir, '_seedinfo.mat'));
    testCase.TestData.SeedCellID = S.SeedInfo.SeedCellID;
end

function out = freshOutDir(testCase, Tag)
    if ~isfield(testCase.TestData, 'SeedCellID')
        setSeedCellIDIntoTD(testCase);
    end
    out = fullfile(getOutRoot(testCase), Tag);
    if isfolder(out)
        rmdir(out, 's');
    end
    mkdir(out);
end

function out = getOutRoot(testCase)
    if isfield(testCase.TestData, 'BaseDir') && ...
            ~isempty(testCase.TestData.BaseDir)
        out = [testCase.TestData.BaseDir '__out'];
    else
        out = tempname;
    end
end

function verifyAddedSourcesPresent(testCase, R, NewMat)
    % Read the rewritten cell from R.OutDir and confirm the new RA/Dec
    % values can be matched back.
    TD = testCase.TestData;
    HTM = celestial.htm.htm_build(TD.Level);
    for Irow = 1:size(NewMat, 1)
        CellID = celestial.htm.htm_search_point(HTM, NewMat(Irow, [1 2]));
        [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
            TD.CatName, CellID, 100);
        FilePath = fullfile(R.OutDir, TD.CatRelDir, DataFileName);
        verifyTrue(testCase, isfile(FilePath), ...
            sprintf('Expected file written: %s', FilePath));
        Cat = HDF5.load(FilePath, ['/' DataSetName]);
        D = celestial.coo.sphere_dist_fast( ...
            NewMat(Irow,1), NewMat(Irow,2), Cat(:,1), Cat(:,2));
        verifyTrue(testCase, any(D < 1e-9), ...
            sprintf('Row %d not found in rewritten cell %d', Irow, CellID));
    end
end

function Cell = readBackAddedRow(R, TD, RA, Dec)
    HTM = celestial.htm.htm_build(TD.Level);
    CellID = celestial.htm.htm_search_point(HTM, [RA, Dec]);
    [DataFileName, DataSetName] = catsHTM.get_file_var_from_htmid( ...
        TD.CatName, CellID, 100);
    FilePath = fullfile(R.OutDir, TD.CatRelDir, DataFileName);
    Cat = HDF5.load(FilePath, ['/' DataSetName]);
    D = celestial.coo.sphere_dist_fast(RA, Dec, Cat(:,1), Cat(:,2));
    [~, Imin] = min(D);
    Cell = Cat(Imin, :);
end
