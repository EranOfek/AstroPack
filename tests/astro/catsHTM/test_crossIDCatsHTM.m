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
    [T, Cats, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
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

    % PerCat: Nmatched is catalog-source-centric and Nmatched+Norphan=Ncone
    P = S.PerCat;
    verifyTrue(testCase, all(P.Nmatched <= P.Ncone), 'Nmatched must be <= Ncone.');
    verifyEqual(testCase, P.Nmatched + P.Norphan, P.Ncone, ...
        'Nmatched + Norphan must equal Ncone.');
    % XIDA: 8 of its 10 cone sources match the anchor, 2 are orphans
    Ia = strcmp(P.Catalog,'XIDA');
    verifyEqual(testCase, P.Ncone(Ia),    10);
    verifyEqual(testCase, P.Nmatched(Ia),  8);
    verifyEqual(testCase, P.Norphan(Ia),   2);
end

function testNativeOrderRoundTrip(testCase)
    % Pin the identifier contract: Ind_<Cat> indexes Cats.<Cat> in NATIVE
    % cone_search order, so (a) a fresh cone_search reproduces the same rows,
    % (b) an orphan's index points to its own source, and (c) a matched
    % index points to a source within the matching radius.
    RAD = 180./pi;
    [T, Cats, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
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
    T = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDB'}, ...
        'MatchRadius',2, 'Verbose',false);
    verifyEqual(testCase, T.Nmatch_XIDB(1), 2, 'Nmatch should count both XIDB sources.');
end

function testPerCatRadius(testCase)
    % A tight per-catalog radius (0.1") drops the 0.2"-offset XIDA matches.
    T = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, ...
        'MatchRadius',2, 'RadiusPerCat',{'XIDA',0.1}, 'Verbose',false);
    verifyTrue(testCase, all(isnan(T.Ind_XIDA(1:10))), ...
        'Tight radius should reject the 0.2" XIDA matches.');
end

function testFileOutput(testCase)
    % OutFile writes a .mat (with Cats) and a .csv (flat table).
    Stem = fullfile(testCase.TestData.Dir, 'xid_out');
    VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
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
    [T, Cats, ~] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
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
    Common = {45.*pi./180, 0, 10800,'OutType','table','RefCat','XIDREF', ...
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

function testGatherCrossID(testCase)
    % gatherCrossID materializes catalog data per T row: matched rows carry
    % the indexed source's raw column values, unmatched rows are NaN.
    [T, Cats] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    D = VO.prep.gatherCrossID(T, Cats, 'Verbose',false);

    verifyTrue(testCase, all(ismember({'GlobalID','RA','Dec','OriginCat'}, ...
        D.Properties.VariableNames)), 'Missing global columns.');
    verifyTrue(testCase, all(ismember({'XIDREF_RA','XIDREF_Dec','XIDA_RA','XIDA_Dec'}, ...
        D.Properties.VariableNames)), 'Missing prefixed catalog columns.');

    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    % gathered value = raw catalog value at the index (col 1 = RA, native rad)
    verifyEqual(testCase, D.XIDA_RA(Mat), Cats.XIDA.Catalog(IndA(Mat),1), ...
        'AbsTol',1e-12, 'Gathered XIDA_RA mismatch vs indexed catalog value.');
    Un = find(isnan(IndA));
    verifyTrue(testCase, all(isnan(D.XIDA_RA(Un))), 'Unmatched rows should be NaN.');

    % file output
    Stem = fullfile(testCase.TestData.Dir, 'gather_out');
    [~, Written] = VO.prep.gatherCrossID(T, Cats, 'Verbose',false, ...
        'OutFile',Stem, 'OutFormat',{'mat','csv'});
    verifyTrue(testCase, isfile([Stem '.mat']) && isfile([Stem '.csv']), ...
        'gatherCrossID file output missing.');
    verifyEqual(testCase, numel(Written), 2, 'Should report 2 written files.');
end

function testGatherFromDiskCats(testCase)
    % gatherCrossID resolves Cats given as file paths (CatsToDisk form).
    CatsDir = fullfile(testCase.TestData.Dir, 'gstream');
    [T, Cats] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2, ...
        'CatsToDisk',true,'CatsDir',CatsDir,'Verbose',false);
    verifyTrue(testCase, ischar(Cats.XIDA), 'Cats.XIDA should be a path here.');

    D = VO.prep.gatherCrossID(T, Cats, 'CatList',{'XIDA'}, 'Verbose',false);
    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    Loaded = load(Cats.XIDA);
    verifyEqual(testCase, D.XIDA_RA(Mat), Loaded.Cat.Catalog(IndA(Mat),1), ...
        'AbsTol',1e-12, 'Gathered value from on-disk catalog mismatch.');
end

function testOutputTypes(testCase)
    % Default output is a MATLAB table (with OriginCat). OutType='astrocatalog'
    % gives a numeric AstroCatalog (OriginCat -> Summary.OriginCat). The .mat
    % stores the same type as the return; the .csv always keeps OriginCat.
    [Tb, Cats, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyTrue(testCase, istable(Tb), 'Default output should be a table.');
    verifyTrue(testCase, ismember('OriginCat', Tb.Properties.VariableNames), ...
        'Default table should include OriginCat.');

    Ac = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyClass(testCase, Ac, 'AstroCatalog', 'OutType=astrocatalog should return AstroCatalog.');
    verifyFalse(testCase, ismember('OriginCat', Ac.ColNames), ...
        'AstroCatalog must not carry the text OriginCat column.');
    verifyEqual(testCase, numel(S.OriginCat), height(Tb), ...
        'Summary.OriginCat must be row-aligned.');
    verifyTrue(testCase, isequaln(getCol(Ac,'Ind_XIDA'), Tb.Ind_XIDA), 'Ind_XIDA differs.');

    % gatherCrossID works on both forms
    D1 = VO.prep.gatherCrossID(Tb, Cats, 'Verbose',false);
    D2 = VO.prep.gatherCrossID(Ac, Cats, 'Verbose',false);
    verifyTrue(testCase, ismember('XIDA_RA', D1.Properties.VariableNames), 'gather(table) failed.');
    verifyTrue(testCase, ismember('XIDA_RA', D2.Properties.VariableNames), 'gather(AstroCatalog) failed.');

    % OutFile .mat stores the SAME type as the return
    StemT = fullfile(testCase.TestData.Dir, 'xid_tbl');
    VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false, 'OutFile',StemT);
    Lt = load([StemT '.mat']);
    verifyTrue(testCase, istable(Lt.XidTable), 'Default .mat XidTable should be a table.');

    StemA = fullfile(testCase.TestData.Dir, 'xid_ac');
    VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false, ...
        'OutFile',StemA, 'OutFileFormat',{'mat','csv'});
    La = load([StemA '.mat']);
    verifyClass(testCase, La.XidTable, 'AstroCatalog', ...
        'astrocatalog .mat XidTable should be an AstroCatalog.');
    Cd = readtable([StemA '.csv']);
    verifyTrue(testCase, ismember('OriginCat', Cd.Properties.VariableNames), ...
        'CSV should include the OriginCat column.');
end

function testKeepExtraMatches(testCase)
    % XIDB has two sources within 2" of anchor #1: one row, nearest data in
    % Ind_XIDB, actual count in Nmatch_XIDB, and the additional match in the
    % IndExtra_XIDB cell column.
    [Tb, ~, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'Verbose',false);

    verifyEqual(testCase, Tb.Nmatch_XIDB(1), 2, 'Row 1 should have 2 XIDB matches.');
    Extra1 = Tb.IndExtra_XIDB{1};
    verifyEqual(testCase, numel(Extra1), 1, 'Row 1 should have one additional match.');
    verifyFalse(testCase, any(isnan(Extra1)), 'Additional match index should not be NaN.');
    verifyNotEqual(testCase, Extra1, Tb.Ind_XIDB(1), 'Extra index must differ from nearest.');
    % nearest + extra together are the two XIDB sources (rows 1 and 2)
    verifyEqual(testCase, sort([Tb.Ind_XIDB(1); Extra1(:)]), [1;2], ...
        'Nearest+extra should be the two XIDB sources.');

    % rows with <=1 match carry NaN in IndExtra
    verifyTrue(testCase, all(cellfun(@(c) isscalar(c) && isnan(c), Tb.IndExtra_XIDB(2:end))), ...
        'Rows with <=1 match must have NaN extra.');

    % Summary.ExtraMatches mirrors the IndExtra column
    verifyTrue(testCase, isequaln(S.ExtraMatches.XIDB, Tb.IndExtra_XIDB), ...
        'Summary.ExtraMatches must match the IndExtra column.');
end

function testKeepExtraAstroCatAndErrors(testCase)
    % OutType='astrocatalog': extras live only in Summary.ExtraMatches, not as
    % an AstroCatalog column; and KeepExtraMatches + TableToDisk errors.
    [Ac, ~, S] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'Verbose',false);
    verifyClass(testCase, Ac, 'AstroCatalog');
    verifyFalse(testCase, any(startsWith(Ac.ColNames,'IndExtra_')), ...
        'AstroCatalog must not carry IndExtra cell columns.');
    verifyTrue(testCase, isfield(S,'ExtraMatches') && isfield(S.ExtraMatches,'XIDB'), ...
        'Extras must be in Summary for AstroCatalog output.');

    verifyError(testCase, @() VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF','CatList',{'XIDB'},'KeepExtraMatches',true, ...
        'TableToDisk',true,'TableFile',fullfile(testCase.TestData.Dir,'x.mat'), ...
        'Verbose',false), 'crossIDCatsHTM:extraStream', ...
        'KeepExtraMatches + TableToDisk should error.');
end

function testSourcePointer(testCase)
    % catsHTM.sourcePointer returns a stable (cellID, rowInCell) per source.
    RAD = 180./pi;
    [C, CC] = catsHTM.cone_search('XIDA', 45./RAD, 0, 10800);  % all XIDA sources
    ColRA  = find(strcmp(CC,'RA'),1);
    ColDec = find(strcmp(CC,'Dec'),1);
    Ra  = C(:,ColRA);
    Dec = C(:,ColDec);

    [Cid, Row, D] = catsHTM.sourcePointer('XIDA', Ra, Dec);
    verifyFalse(testCase, any(isnan(Cid)), 'Every source should resolve to a cell id.');
    verifyFalse(testCase, any(isnan(Row)), 'Every source should resolve to a row.');
    verifyTrue(testCase, all(D < 0.1), 'Self-match distance should be ~0 arcsec.');
    % the pointer is unique per source
    verifyEqual(testCase, size(unique([Cid, Row],'rows'),1), numel(Ra), ...
        'Pointers must be unique per source.');
end

function testAddPointer(testCase)
    % Default AddPointer adds numeric CellID_/RowInCell_ columns that equal a
    % fresh catsHTM.sourcePointer call on the matched sources.
    [T, Cats, ~] = VO.prep.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyTrue(testCase, all(ismember({'CellID_XIDA','RowInCell_XIDA', ...
        'CellID_XIDREF','RowInCell_XIDREF'}, T.Properties.VariableNames)), ...
        'AddPointer columns missing.');

    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    verifyFalse(testCase, any(isnan(T.CellID_XIDA(Mat))), 'Matched rows must have a cell id.');

    [sRA, sDec] = getLonLat(Cats.XIDA, 'rad');
    [Cid, Row] = catsHTM.sourcePointer('XIDA', sRA(IndA(Mat)), sDec(IndA(Mat)));
    verifyEqual(testCase, T.CellID_XIDA(Mat),    Cid, 'CellID mismatch vs sourcePointer.');
    verifyEqual(testCase, T.RowInCell_XIDA(Mat), Row, 'RowInCell mismatch vs sourcePointer.');
end
