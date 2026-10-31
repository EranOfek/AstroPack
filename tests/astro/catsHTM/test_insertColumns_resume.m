function tests = test_insertColumns_resume
    % Unit tests for the resume mode ('SkipExisting') of catsHTM.insertColumns.
    %
    % Builds a synthetic catsHTM catalog with several populated cells in one
    % data file under a temp BaseDir, and checks that a resumed run redoes a
    % file left incomplete (missing cells, a leftover .part file, a failed
    % fill) and skips only complete files. No data on /euclid is touched.

    tests = functiontests(localfunctions);
end

%% Fixture

function setupOnce(testCase)
    Tmp = tempname;
    mkdir(Tmp);
    TD = struct('BaseDir', Tmp, 'OutRoot', [Tmp '__out'], 'CatName', 'RESUMECAT', ...
                'CatRelDir', '/RESUMECAT/', 'Level', 4, 'SortCol', 2, 'StepRows', 30, ...
                'ColCell', {{'RA','Dec','Mag'}}, 'ColUnits', {{'rad','rad','mag'}});
    TD.Cells = buildCatalog(TD);
    testCase.TestData = TD;
end

function teardownOnce(testCase)
    TD = testCase.TestData;
    if isfolder(TD.BaseDir)
        rmdir(TD.BaseDir, 's');
    end
    if isfolder(TD.OutRoot)
        rmdir(TD.OutRoot, 's');
    end
end

%% Test Functions

function testCompleteFileIsSkipped(testCase)
    % A second resumed run skips the complete file and leaves it unchanged.

    TD = testCase.TestData;
    OutDir = freshOutDir(TD, 'complete');
    insert(TD, OutDir, false);
    File = dataFile(TD, OutDir);
    Before = readCells(File, TD.Cells.Names);

    R = insert(TD, OutDir, true);

    testCase.verifyEqual(R.CellsTouched, 0, 'A complete file must be skipped.');
    testCase.verifyEqual(readCells(File, TD.Cells.Names), Before, 'A skipped file must not change.');
end

function testFileWithMissingCellsIsRedone(testCase)
    % A file holding only some of its cells (at the new width) is rewritten in full.

    TD = testCase.TestData;
    OutDir = freshOutDir(TD, 'missing');
    insert(TD, OutDir, false);
    File = dataFile(TD, OutDir);
    % keep only the first cell, as a run that crashed after one cell left it
    Cat = HDF5.load(File, ['/' TD.Cells.Names{1}]);
    delete(File);
    catsHTM.save_cat(File, TD.Cells.Names{1}, Cat, TD.SortCol, TD.StepRows);

    R = insert(TD, OutDir, true);

    testCase.verifyEqual(R.CellsTouched, numel(TD.Cells.Names), 'All cells of the incomplete file must be redone.');
    Cells = readCells(File, TD.Cells.Names);
    testCase.verifyEqual(cellfun(@(C) size(C,1), Cells), TD.Cells.Nrows, 'Every cell must have its source rows.');
    testCase.verifyTrue(all(cellfun(@(C) size(C,2), Cells) == numel(TD.ColCell) + 1), 'Every cell must have the new column.');
end

function testLeftoverPartFileIsReplaced(testCase)
    % A .part file left by a crash is discarded, and the run leaves no .part file.

    TD = testCase.TestData;
    OutDir = freshOutDir(TD, 'part');
    File = dataFile(TD, OutDir);
    if ~isfolder(fileparts(File))
        mkdir(fileparts(File));
    end
    catsHTM.save_cat([File '.part'], TD.Cells.Names{1}, zeros(3, numel(TD.ColCell) + 1), TD.SortCol, TD.StepRows);

    insert(TD, OutDir, true);

    testCase.verifyFalse(isfile([File '.part']), 'No .part file may remain.');
    Cells = readCells(File, TD.Cells.Names);
    testCase.verifyEqual(cellfun(@(C) size(C,1), Cells), TD.Cells.Nrows, 'Every cell must have its source rows.');
end

function testFailedFillLeavesNoDataFile(testCase)
    % A fill that fails on the second cell leaves no data file, so a resumed run redoes it.

    TD = testCase.TestData;
    OutDir = freshOutDir(TD, 'fail');
    File = dataFile(TD, OutDir);
    FailMag = TD.Cells.FirstMag(2);
    Fill = @(M) failOn(M, FailMag);

    testCase.verifyError(@() catsHTM.insertColumns(TD.CatName, {'New'}, {''}, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, 'FillValue', Fill, ...
        'SkipExisting', true), 'test:insertColumns:fill', 'The failing fill must raise its error.');
    testCase.verifyFalse(isfile(File), 'A file whose cells were not all written must not exist.');

    R = insert(TD, OutDir, true);
    testCase.verifyEqual(R.CellsTouched, numel(TD.Cells.Names), 'The resumed run must write every cell.');
end

%% Helpers

function Cells = buildCatalog(TD)
    % Catalog with three populated leaf cells that share one data file.
    CatDir = fullfile(TD.BaseDir, TD.CatRelDir);
    mkdir(CatDir);
    [HTM, LevelH] = celestial.htm.htm_build(TD.Level);
    Leaf = LevelH(end).ptr;
    FileOf = floor(Leaf ./ 100);
    Ids = Leaf(FileOf == FileOf(1));
    Ids = Ids(1:3);

    ColCell  = TD.ColCell;  %#ok<NASGU>
    ColUnits = TD.ColUnits; %#ok<NASGU>
    save(fullfile(CatDir, sprintf('%s_htmColCell.mat', TD.CatName)), 'ColCell', 'ColUnits');

    Nrows = [2 3 4];
    Names = cell(1, numel(Ids));
    FirstMag = zeros(1, numel(Ids));
    Nsrc = zeros(numel(Ids), 2);
    for I = 1:numel(Ids)
        Coo = HTM(Ids(I)).coo;
        RA  = mean(Coo(:,1)) + 1e-5 * (0:Nrows(I)-1).';
        Dec = mean(Coo(:,2)) + 1e-5 * (0:Nrows(I)-1).';
        Mag = 10 * I + (1:Nrows(I)).';
        [DataFile, Names{I}] = catsHTM.get_file_var_from_htmid(TD.CatName, Ids(I), 100);
        catsHTM.save_cat(fullfile(CatDir, DataFile), Names{I}, [RA Dec Mag], TD.SortCol, TD.StepRows);
        FirstMag(I) = Mag(1);
        Nsrc(I,:) = [Ids(I), Nrows(I)];
    end
    [IndexFile, IndexVar] = catsHTM.get_index_filename(TD.CatName);
    catsHTM.save_htm_ind(HTM, fullfile(CatDir, IndexFile), IndexVar, {}, Nsrc);
    Cells = struct('Ids', Ids, 'Names', {Names}, 'Nrows', Nrows, 'FirstMag', FirstMag, 'DataFile', DataFile);
end

function R = insert(TD, OutDir, Skip)
    R = catsHTM.insertColumns(TD.CatName, {'New'}, {''}, OutDir, ...
        'BaseDir', TD.BaseDir, 'CatRelDir', TD.CatRelDir, ...
        'FillValue', @(M) 2 .* M(:,3), 'SkipExisting', Skip);
end

function File = dataFile(TD, OutDir)
    File = fullfile(OutDir, TD.CatRelDir, TD.Cells.DataFile);
end

function Cells = readCells(File, Names)
    Cells = cellfun(@(N) HDF5.load(File, ['/' N]), Names, 'UniformOutput', false);
end

function Out = freshOutDir(TD, Tag)
    Out = fullfile(TD.OutRoot, Tag);
    if isfolder(Out)
        rmdir(Out, 's');
    end
    mkdir(Out);
end

function V = failOn(M, Mag)
    if any(M(:,3) == Mag)
        error('test:insertColumns:fill', 'Fill failure on purpose.');
    end
    V = 2 .* M(:,3);
end
