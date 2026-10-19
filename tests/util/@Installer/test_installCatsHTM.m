function tests = test_installCatsHTM
    % Unit tests for Installer/installCatsHTM and Installer.catsHTMList.
    %
    % Downloads the small ROSATfsc catalog (24 files, ~26 MB) into a temporary
    % directory and checks the result table, resume (skip valid files) and
    % repair (re-download missing or corrupted files). Network tests are
    % skipped when the catsHTM server is not reachable; the NFS test is
    % skipped when /euclid is not mounted.
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    testCase.TestData.Dir = tempname;
    testCase.TestData.Cat = 'ROSATfsc';
end

function teardown(testCase)
    if isfolder(testCase.TestData.Dir)
        rmdir(testCase.TestData.Dir, 's');
    end
end

%% Test Functions

function testCatsHTMList(testCase)
    % The list holds the active catalogs; 'All' adds inactive ones.
    T = Installer.catsHTMList;
    testCase.verifyTrue(any(T.Name=="GAIADR3"), 'GAIADR3 missing from the list.');
    testCase.verifyTrue(all(T.Active), 'Default list should hold active catalogs only.');
    TA = Installer.catsHTMList('All',true);
    testCase.verifyGreaterThan(height(TA), height(T), 'All=true should add inactive catalogs.');
end

function testUnknownCatErrors(testCase)
    % An unknown catalog name throws a clear error before any download.
    I = Installer;
    testCase.verifyError(@() I.installCatsHTM({'ROSATfsc','NoSuchCat'}, 'OutDir',testCase.TestData.Dir), ...
        'Installer:installCatsHTM:UnknownCat', 'Unknown catalog should error.');
    testCase.verifyFalse(isfolder(testCase.TestData.Dir), 'Nothing should be written for a bad request.');
end

function testDryRun(testCase)
    % DryRun reports the files to get and writes nothing.
    assumeServer(testCase);
    I = Installer;
    R = I.installCatsHTM(lower(testCase.TestData.Cat), 'OutDir',testCase.TestData.Dir, 'DryRun',true);
    testCase.verifyEqual(height(R), 1, 'One row per catalog.');
    testCase.verifyEqual(R.Name, string(testCase.TestData.Cat), 'Name should be the registry name.');
    testCase.verifyGreaterThan(R.Nfiles, 0, 'Checksum list should not be empty.');
    testCase.verifyEqual(R.Nskipped, 0, 'Nothing is present yet.');
    testCase.verifyFalse(isfolder(testCase.TestData.Dir), 'DryRun should not create directories.');
end

function testDownloadResumeRepair(testCase)
    % Full download, then a resume that skips everything, then a repair of
    % one deleted and one corrupted file.
    assumeServer(testCase);
    I = Installer;
    Args = {'OutDir',testCase.TestData.Dir};
    Cat  = testCase.TestData.Cat;

    R1 = I.installCatsHTM(Cat, Args{:});
    testCase.verifyEqual(R1.Nfailed, 0, 'Download should succeed.');
    testCase.verifyEqual(R1.Ngot, R1.Nfiles, 'All files should be downloaded.');
    testCase.verifyEqual(R1.Message, "ok", 'Message should be ok.');
    CatDir = fullfile(testCase.TestData.Dir, Cat);
    testCase.verifyTrue(isfile(fullfile(CatDir, sprintf('%s_htm.hdf5', Cat))), 'Index file missing.');
    testCase.verifyTrue(isfile(fullfile(CatDir, sprintf('list.euler.checksum._%s_', Cat))), 'Checksum list copy missing.');

    R2 = I.installCatsHTM(Cat, Args{:});
    testCase.verifyEqual(R2.Nskipped, R1.Nfiles, 'Resume should skip all valid files.');
    testCase.verifyEqual(R2.Ngot, 0, 'Resume should download nothing.');

    Files = dir(fullfile(CatDir, '*_htm_*.hdf5'));
    delete(fullfile(CatDir, Files(1).name));
    FID = fopen(fullfile(CatDir, Files(2).name), 'r+');
    fwrite(FID, uint8([1 2 3 4]));
    fclose(FID);
    R3 = I.installCatsHTM(Cat, Args{:});
    testCase.verifyEqual(R3.Ngot, 2, 'The deleted and the corrupted file should be downloaded again.');
    testCase.verifyEqual(R3.Nfailed, 0, 'Repair should succeed.');
end

function testEuclidCopy(testCase)
    % Server='euclid' copies from the NFS mount with the same checks.
    testCase.assumeTrue(isfolder('/euclid/catsHTM') || isfolder('/mnt/euclid/catsHTM'), '/euclid not mounted.');
    I = Installer;
    R = I.installCatsHTM(testCase.TestData.Cat, 'OutDir',testCase.TestData.Dir, 'Server','euclid');
    testCase.verifyEqual(R.Nfailed, 0, 'NFS copy should succeed.');
    testCase.verifyEqual(R.Ngot, R.Nfiles, 'All files should be copied.');
end

%% Helpers

function assumeServer(testCase)
    % skip network tests when the catsHTM server cannot be reached
    I = Installer;
    URL = sprintf('%s/list.euler.checksum._%s_', I.Config.Data.Installer.Default.CatsHTMURL, testCase.TestData.Cat);
    try
        webread(URL, weboptions('ContentType','text', 'Timeout',20));
        Ok = true;
    catch
        Ok = false;
    end
    testCase.assumeTrue(Ok, 'catsHTM server not reachable.');
end
