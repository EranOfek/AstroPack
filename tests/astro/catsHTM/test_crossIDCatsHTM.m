function tests = test_crossIDCatsHTM
    % Unit tests for VO.prep.crossIDCatsHTM.
    %
    % Builds three tiny synthetic catsHTM catalogs in a temp dir:
    %   XIDREF - anchor (10 sources)
    %   XIDA   - 8 of the 10 anchor sources + 2 extra "orphan" sources
    %   XIDB   - a doubled source (tests the Nmatch>1 column)
    % and checks the cross-id table: cross-id indices, orphan appending,
    % per-catalog Nmatch, and the file output.
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    RAD = 180./pi;
    Tmp = tempname;
    mkdir(Tmp);

    % 10 anchor sources on a small patch around (45,0) deg
    Base = [45 + (0:9).'.*0.01, zeros(10,1)];        % [deg deg]
    Ref  = Base ./ RAD;                              % [rad]

    % XIDA: first 8 anchor sources (tiny 0.2" offsets) + 2 orphans
    Off  = 0.2/3600/RAD;                             % 0.2 arcsec in rad
    A    = Base(1:8,:)./RAD + Off;
    Orph = [45.50 0.30; 45.60 0.30] ./ RAD;          % far from anchor
    A    = [A; Orph];

    % XIDB: two sources 1" apart on top of anchor source #1 (Nmatch==2)
    Sep  = 1.0/3600/RAD;
    B    = [Base(1,:)./RAD; Base(1,:)./RAD + [Sep 0]];

    Old = pwd;
    cd(Tmp);
    VO.prep.build_htm_catalog(Ref, 'CatName','XIDREF', 'HTM_Level',4, ...
        'ColCell',{'RA','Dec'}, 'ColUnits',{'rad','rad'});
    VO.prep.build_htm_catalog(A,   'CatName','XIDA',   'HTM_Level',4, ...
        'ColCell',{'RA','Dec'}, 'ColUnits',{'rad','rad'});
    VO.prep.build_htm_catalog(B,   'CatName','XIDB',   'HTM_Level',4, ...
        'ColCell',{'RA','Dec'}, 'ColUnits',{'rad','rad'});
    cd(Old);
    addpath(Tmp);

    testCase.TestData.Dir = Tmp;
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

function testCrossIdAndOrphans(testCase)
    % Anchor + XIDA: 8 matches, 2 orphans appended -> 12 global sources.
    [T, Cats, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDA','XIDB'}, ...
        'MatchRadius',2, 'Verbose',false);

    % 10 anchor + 2 XIDA orphans = 12 rows
    verifyEqual(testCase, height(T), 12, 'Wrong global source count.');
    verifyEqual(testCase, S.Nref, 10);
    verifyEqual(testCase, S.Nglobal, 12);

    % first 8 anchor rows matched in XIDA, rows 9-10 did not
    IndA = T.Ind_XIDA;
    verifyEqual(testCase, sum(~isnan(IndA(1:8))), 8, 'Missing XIDA matches.');
    verifyTrue(testCase, all(isnan(IndA(9:10))), 'Rows 9-10 should be unmatched in XIDA.');

    % the two appended orphans originate from XIDA and self-index in XIDA
    verifyEqual(testCase, T.OriginCat(11:12), {'XIDA';'XIDA'});
    verifyTrue(testCase, all(~isnan(IndA(11:12))), 'Orphan rows must index into XIDA.');

    % anchor self-column is 1:Nref then NaN on appended rows
    verifyEqual(testCase, T.Ind_XIDREF(1:10), (1:10).');
    verifyTrue(testCase, all(isnan(T.Ind_XIDREF(11:12))));

    % Cats holds the (native-order) catalogs the indices point into
    verifyTrue(testCase, isfield(Cats,'XIDA') && isfield(Cats,'XIDREF'));
end

function testNativeOrderRoundTrip(testCase)
    % Pin the identifier contract: Ind_<Cat> indexes Cats.<Cat> in NATIVE
    % cone_search order, so (a) a fresh cone_search reproduces the same rows,
    % (b) an orphan's index points to its own source, and (c) a matched
    % index points to a source within the matching radius.
    RAD = 180./pi;
    [T, Cats, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'MatchRadius',2, 'Verbose',false);

    [sRA, sDec] = getLonLat(Cats.XIDA, 'deg');

    % (a) stored Cats.XIDA is in native cone_search order (bit-for-bit)
    Fresh = catsHTM.cone_search('XIDA', S.Field.RA./RAD, S.Field.Dec./RAD, ...
        S.Field.Radius, 'RadiusUnits', S.Field.RadiusUnits, 'OutType','astrocatalog');
    [fRA, fDec] = getLonLat(Fresh, 'deg');
    verifyEqual(testCase, [sRA, sDec], [fRA, fDec], 'AbsTol',1e-9, ...
        'Cats.XIDA is not in native cone_search order.');

    % (b) orphan rows: master coords ARE the indexed XIDA source coords
    IndA = T.Ind_XIDA;
    Orph = find(strcmp(T.OriginCat, 'XIDA'));
    verifyEqual(testCase, [sRA(IndA(Orph)), sDec(IndA(Orph))], ...
        [T.RA(Orph), T.Dec(Orph)], 'AbsTol',1e-9, ...
        'Orphan Ind_XIDA does not point to its own source.');

    % (c) matched rows: the indexed XIDA source is within MatchRadius (2")
    Mat  = find(~isnan(IndA) & ~strcmp(T.OriginCat, 'XIDA'));
    Drad = celestial.coo.sphere_dist_fast(T.RA(Mat)./RAD, T.Dec(Mat)./RAD, ...
        sRA(IndA(Mat))./RAD, sDec(IndA(Mat))./RAD);
    verifyTrue(testCase, all(Drad.*RAD.*3600 <= 2 + 1e-6), ...
        'Matched Ind_XIDA points beyond the matching radius.');
end

function testNmatchColumn(testCase)
    % XIDB has two sources within 2" of anchor #1 -> Nmatch_XIDB(1)==2.
    T = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDB'}, ...
        'MatchRadius',2, 'Verbose',false);
    verifyEqual(testCase, T.Nmatch_XIDB(1), 2, 'Nmatch should count both XIDB sources.');
end

function testPerCatRadius(testCase)
    % A tight per-catalog radius (0.1") drops the 0.2"-offset XIDA matches.
    T = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, ...
        'MatchRadius',2, 'RadiusPerCat',{'XIDA',0.1}, 'Verbose',false);
    verifyTrue(testCase, all(isnan(T.Ind_XIDA(1:10))), ...
        'Tight radius should reject the 0.2" XIDA matches.');
end

function testFileOutput(testCase)
    % OutFile writes a .mat (with Cats) and a .csv (flat table).
    Stem = fullfile(testCase.TestData.Dir, 'xid_out');
    VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'Verbose',false, ...
        'OutFile',Stem);
    verifyTrue(testCase, isfile([Stem '.mat']), 'MAT output missing.');
    verifyTrue(testCase, isfile([Stem '.csv']), 'CSV output missing.');
    Loaded = load([Stem '.mat']);
    verifyTrue(testCase, isfield(Loaded,'XidTable') && isfield(Loaded,'Cats'), ...
        'MAT output should preserve XidTable and Cats.');
end

function testCatsToDisk(testCase)
    % CatsToDisk streams each catalog to its own .mat; Cats holds PATHS and
    % the indices still resolve against the loaded (native-order) catalog.
    CatsDir = fullfile(testCase.TestData.Dir, 'streamed');
    [T, Cats, ~] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'MatchRadius',2, ...
        'CatsToDisk',true, 'CatsDir',CatsDir, 'Verbose',false);

    % Cats fields are file paths, not AstroCatalogs
    verifyTrue(testCase, ischar(Cats.XIDA) && isfile(Cats.XIDA), ...
        'Cats.XIDA should be a path to an existing file.');
    verifyTrue(testCase, ischar(Cats.XIDREF) && isfile(Cats.XIDREF), ...
        'Cats.XIDREF should be a path to an existing file.');

    % loaded catalog resolves a matched index to a nearby source (<2")
    RAD = 180./pi;
    L   = load(Cats.XIDA);
    verifyTrue(testCase, isa(L.Cat,'AstroCatalog'), 'Loaded Cat not an AstroCatalog.');
    [sRA, sDec] = getLonLat(L.Cat, 'deg');
    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA) & ~strcmp(T.OriginCat,'XIDA'));
    Drad = celestial.coo.sphere_dist_fast(T.RA(Mat)./RAD, T.Dec(Mat)./RAD, ...
        sRA(IndA(Mat))./RAD, sDec(IndA(Mat))./RAD);
    verifyTrue(testCase, all(Drad.*RAD.*3600 <= 2 + 1e-6), ...
        'Index into on-disk catalog points beyond the matching radius.');
end

function testTableToDisk(testCase)
    % TableToDisk streams the table to a v7.3 .mat; the first output is the
    % path, and lazily-read columns reproduce the in-memory table exactly.
    Common = {45.*pi./180, 0, 10800,'RefCat','XIDREF', ...
              'CatList',{'XIDA','XIDB'},'MatchRadius',2,'Verbose',false};
    Ref = VO.prep.crossIDCatsHTM(Common{:});

    TableFile = fullfile(testCase.TestData.Dir, 'streamed_table.mat');
    Path = VO.prep.crossIDCatsHTM(Common{:}, 'TableToDisk',true, 'TableFile',TableFile);

    % first output is the file path, and the file exists
    verifyEqual(testCase, Path, TableFile, 'XidTable output should be the file path.');
    verifyTrue(testCase, isfile(TableFile), 'Streamed table file missing.');

    % lazy column access reproduces the in-memory columns (isequaln for NaNs)
    Mf = matfile(TableFile);
    verifyTrue(testCase, isequaln(Mf.Ind_XIDA,    Ref.Ind_XIDA),    'Ind_XIDA mismatch.');
    verifyTrue(testCase, isequaln(Mf.Nmatch_XIDB, Ref.Nmatch_XIDB), 'Nmatch_XIDB mismatch.');
    verifyTrue(testCase, isequaln(Mf.RA,          Ref.RA),          'RA mismatch.');
    verifyTrue(testCase, isequaln(Mf.GlobalID,    Ref.GlobalID),    'GlobalID mismatch.');

    % Summary and Cats travel with the streamed file
    L = load(TableFile, 'Summary', 'Cats');
    verifyEqual(testCase, L.Summary.Nglobal, height(Ref), 'Summary.Nglobal mismatch.');
    verifyTrue(testCase, isfield(L.Cats,'XIDA') && isfield(L.Cats,'XIDREF'), ...
        'Cats not stored in the streamed file.');
end
