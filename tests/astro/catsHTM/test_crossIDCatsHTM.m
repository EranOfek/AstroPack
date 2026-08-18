function tests = test_crossIDCatsHTM
    % Unit tests for catsHTM.crossIDCatsHTM.
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

    % Synthetic catalogs on a COMPACT INTERIOR patch, deliberately off the HTM
    % cell boundaries. build_htm_catalog double-writes sources that sit on the
    % Dec=0 equator (a N/S cell boundary) and DROPS sources that land exactly on
    % a cell edge - which a regular grid at a round RA/Dec does. A small jittered
    % cluster (fixed seed) at an off-boundary centre keeps all 10 sources inside
    % a single cell, so each catalog holds exactly the sources we insert. The
    % field queries below still use (45,0): a 3 deg cone from there contains this
    % ~1 deg-away cluster. NOTE: the cluster does not preserve input order, so
    % tests assert match COUNTS / set membership, not fixed within-anchor rows.
    rng(7);
    CRA = 45.357;  CDec = 0.894;                     % [deg] interior centre
    Base = [CRA + 0.015.*(rand(10,1)-0.5), ...       % 10 anchor sources in a
            CDec + 0.015.*(rand(10,1)-0.5)];         % ~54" box, min sep ~9"
    Ref  = Base ./ RAD;                              % [rad]

    % XIDA: first 8 anchor sources (tiny 0.2" offsets) + 2 orphans
    Off  = 0.2/3600/RAD;                             % 0.2 arcsec in rad
    A    = Base(1:8,:)./RAD + Off;
    Orph = [CRA+0.020, CDec; CRA+0.022, CDec] ./ RAD;  % far (~72") from anchor, same cell
    A    = [A; Orph];

    % XIDB: two sources 1" apart on top of anchor source #1 (Nmatch==2)
    Sep  = 1.0/3600/RAD;
    B    = [Base(1,:)./RAD; Base(1,:)./RAD + [Sep./cosd(CDec), 0]];

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

    % build_htm_catalog reuses the catalog NAMES across tests, and catsHTM
    % caches each HTM index as <Cat>_HTM in the base workspace
    % (HDF5.load_check, default WS 'base'). Drop any stale cache so cone_search
    % reads THIS fixture's freshly-written index rather than a previous build's
    % cells (otherwise it looks for htm_<id> files this build never wrote).
    evalin('base', 'clear XIDREF_HTM XIDA_HTM XIDB_HTM');

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
    [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA','XIDB'}, ...
        'MatchRadius',2, 'Verbose',false);

    % 10 anchor + 2 XIDA orphans = 12 rows
    verifyEqual(testCase, height(T), 12, 'Wrong global source count.');
    verifyEqual(testCase, S.Nref, 10);
    verifyEqual(testCase, S.Nglobal, 12);

    % first 8 anchor rows matched in XIDA, rows 9-10 did not
    % anchor rows are 1:Nref (orphans are appended after); the cluster does not
    % preserve input order, so assert 8 of the 10 anchors matched, 2 did not.
    IndA = T.Ind_XIDA;
    verifyEqual(testCase, sum(~isnan(IndA(1:10))), 8, 'Missing XIDA matches.');
    verifyEqual(testCase, sum(isnan(IndA(1:10))), 2, 'Two anchors should be unmatched in XIDA.');

    % the two appended orphans originate from XIDA and self-index in XIDA
    verifyEqual(testCase, T.OriginCat(11:12), {'XIDA';'XIDA'});
    verifyTrue(testCase, all(~isnan(IndA(11:12))), 'Orphan rows must index into XIDA.');

    % anchor self-column is 1:Nref then NaN on appended rows
    verifyEqual(testCase, T.Ind_XIDREF(1:10), (1:10).');
    verifyTrue(testCase, all(isnan(T.Ind_XIDREF(11:12))));

    % Cats_cone holds the (native-order) catalogs the indices point into
    verifyTrue(testCase, isfield(Cats_cone,'XIDA') && isfield(Cats_cone,'XIDREF'));

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
    % Pin the identifier contract: Ind_<Cat> indexes Cats_cone.<Cat> in NATIVE
    % cone_search order, so (a) a fresh cone_search reproduces the same rows,
    % (b) an orphan's index points to its own source, and (c) a matched
    % index points to a source within the matching radius.
    RAD = 180./pi;
    [T, Cats_cone, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'MatchRadius',2, 'Verbose',false);

    [sRA, sDec] = getLonLat(Cats_cone.XIDA, 'deg');

    % (a) stored Cats_cone.XIDA is in native cone_search order (bit-for-bit)
    Fresh = catsHTM.cone_search('XIDA', S.Field.RA./RAD, S.Field.Dec./RAD, ...
        S.Field.Radius, 'RadiusUnits', S.Field.RadiusUnits, 'OutType','astrocatalog');
    [fRA, fDec] = getLonLat(Fresh, 'deg');
    verifyEqual(testCase, [sRA, sDec], [fRA, fDec], 'AbsTol',1e-9, ...
        'Cats_cone.XIDA is not in native cone_search order.');

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
    T = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDB'}, ...
        'MatchRadius',2, 'Verbose',false);
    verifyEqual(testCase, max(T.Nmatch_XIDB), 2, 'Nmatch should count both XIDB sources.');
end

function testPerCatRadius(testCase)
    % A tight per-catalog radius (0.1") drops the 0.2"-offset XIDA matches.
    T = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, ...
        'MatchRadius',2, 'RadiusPerCat',{'XIDA',0.1}, 'Verbose',false);
    verifyTrue(testCase, all(isnan(T.Ind_XIDA(1:10))), ...
        'Tight radius should reject the 0.2" XIDA matches.');
end

function testFileOutput(testCase)
    % OutFile writes a .mat (with Cats_cone) and a .csv (flat table).
    Stem = fullfile(testCase.TestData.Dir, 'xid_out');
    catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'Verbose',false, ...
        'OutFile',Stem);
    verifyTrue(testCase, isfile([Stem '.mat']), 'MAT output missing.');
    verifyTrue(testCase, isfile([Stem '.csv']), 'CSV output missing.');
    Loaded = load([Stem '.mat']);
    verifyTrue(testCase, isfield(Loaded,'XidTable') && isfield(Loaded,'Cats_cone'), ...
        'MAT output should preserve XidTable and Cats_cone.');
end

function testSignatureJsonOutput(testCase)
    % OutFileFormat 'json' writes a lean <OutFile>_signature.json sidecar with
    % Summary.Signature; it round-trips and validates via checkCatalogSignature.
    Stem = fullfile(testCase.TestData.Dir, 'xid_sig');
    [~, ~, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'MatchRadius',2, 'Verbose',false, ...
        'OutFile',Stem, 'OutFileFormat',{'json'});
    JsonFile = [Stem '_signature.json'];
    verifyTrue(testCase, isfile(JsonFile), 'Signature JSON sidecar missing.');
    % json-only: no .mat/.csv written
    verifyFalse(testCase, isfile([Stem '.mat']), 'json-only should not write .mat.');

    Loaded = jsondecode(fileread(JsonFile));
    verifyTrue(testCase, isfield(Loaded,'XIDA') && isfield(Loaded,'XIDREF'), ...
        'JSON should carry the per-catalog signatures.');
    verifyEqual(testCase, Loaded.XIDA.LayoutHash, S.Signature.XIDA.LayoutHash, ...
        'JSON LayoutHash must match the in-memory signature.');

    % a JSON-restored signature validates identically to a .mat one
    [ok, rep] = catsHTM.checkCatalogSignature('XIDA', Loaded.XIDA, 'Warn',false);
    verifyTrue(testCase, ok, 'JSON-restored signature should validate.');
    verifyEqual(testCase, rep.Status, 'valid', 'JSON-restored signature should be valid.');
end

function testCatsToDisk(testCase)
    % CatsToDisk streams each catalog to its own .mat; Cats_cone holds PATHS and
    % the indices still resolve against the loaded (native-order) catalog.
    CatsDir = fullfile(testCase.TestData.Dir, 'streamed');
    [T, Cats_cone, ~] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF', 'CatList',{'XIDA'}, 'MatchRadius',2, ...
        'CatsToDisk',true, 'CatsDir',CatsDir, 'Verbose',false);

    % Cats_cone fields are file paths, not AstroCatalogs
    verifyTrue(testCase, ischar(Cats_cone.XIDA) && isfile(Cats_cone.XIDA), ...
        'Cats_cone.XIDA should be a path to an existing file.');
    verifyTrue(testCase, ischar(Cats_cone.XIDREF) && isfile(Cats_cone.XIDREF), ...
        'Cats_cone.XIDREF should be a path to an existing file.');

    % loaded catalog resolves a matched index to a nearby source (<2")
    RAD = 180./pi;
    L   = load(Cats_cone.XIDA);
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
    % KeepExtraMatches (now default true) is unsupported with TableToDisk, so
    % turn it off explicitly for the streaming comparison.
    Common = {45.*pi./180, 0, 10800,'OutType','table','RefCat','XIDREF', ...
              'CatList',{'XIDA','XIDB'},'MatchRadius',2, ...
              'KeepExtraMatches',false,'Verbose',false};
    Ref = catsHTM.crossIDCatsHTM(Common{:});

    TableFile = fullfile(testCase.TestData.Dir, 'streamed_table.mat');
    Path = catsHTM.crossIDCatsHTM(Common{:}, 'TableToDisk',true, 'TableFile',TableFile);

    % first output is the file path, and the file exists
    verifyEqual(testCase, Path, TableFile, 'XidTable output should be the file path.');
    verifyTrue(testCase, isfile(TableFile), 'Streamed table file missing.');

    % lazy column access reproduces the in-memory columns (isequaln for NaNs)
    Mf = matfile(TableFile);
    verifyTrue(testCase, isequaln(Mf.Ind_XIDA,    Ref.Ind_XIDA),    'Ind_XIDA mismatch.');
    verifyTrue(testCase, isequaln(Mf.Nmatch_XIDB, Ref.Nmatch_XIDB), 'Nmatch_XIDB mismatch.');
    verifyTrue(testCase, isequaln(Mf.RA,          Ref.RA),          'RA mismatch.');
    verifyTrue(testCase, isequaln(Mf.MasterID,    Ref.MasterID),    'MasterID mismatch.');

    % Summary and Cats_cone travel with the streamed file
    L = load(TableFile, 'Summary', 'Cats_cone');
    verifyEqual(testCase, L.Summary.Nglobal, height(Ref), 'Summary.Nglobal mismatch.');
    verifyTrue(testCase, isfield(L.Cats_cone,'XIDA') && isfield(L.Cats_cone,'XIDREF'), ...
        'Cats_cone not stored in the streamed file.');
end

function testGatherCrossID(testCase)
    % gatherCrossIDData materializes catalog data per T row: matched rows carry
    % the indexed source's raw column values, unmatched rows are NaN.
    [T, Cats_cone] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    % Columns={} = all columns (default is now 'auto' = prime mag / Dec fallback)
    D = catsHTM.gatherCrossIDData(T, Cats_cone, 'Columns',{}, 'Verbose',false);

    verifyTrue(testCase, all(ismember({'MasterID','RA','Dec','OriginCat'}, ...
        D.Properties.VariableNames)), 'Missing global columns.');
    verifyTrue(testCase, all(ismember({'XIDREF_RA','XIDREF_Dec','XIDA_RA','XIDA_Dec'}, ...
        D.Properties.VariableNames)), 'Missing prefixed catalog columns.');

    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    % gathered value = raw catalog value at the index (col 1 = RA, native rad)
    verifyEqual(testCase, D.XIDA_RA(Mat), Cats_cone.XIDA.Catalog(IndA(Mat),1), ...
        'AbsTol',1e-12, 'Gathered XIDA_RA mismatch vs indexed catalog value.');
    Un = find(isnan(IndA));
    verifyTrue(testCase, all(isnan(D.XIDA_RA(Un))), 'Unmatched rows should be NaN.');

    % file output
    Stem = fullfile(testCase.TestData.Dir, 'gather_out');
    [~, Written] = catsHTM.gatherCrossIDData(T, Cats_cone, 'Verbose',false, ...
        'OutFile',Stem, 'OutFormat',{'mat','csv'});
    verifyTrue(testCase, isfile([Stem '.mat']) && isfile([Stem '.csv']), ...
        'gatherCrossIDData file output missing.');
    verifyEqual(testCase, numel(Written), 2, 'Should report 2 written files.');
end

function testGatherFromDiskCats(testCase)
    % gatherCrossIDData resolves Cats_cone given as file paths (CatsToDisk form).
    CatsDir = fullfile(testCase.TestData.Dir, 'gstream');
    [T, Cats_cone] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2, ...
        'CatsToDisk',true,'CatsDir',CatsDir,'Verbose',false);
    verifyTrue(testCase, ischar(Cats_cone.XIDA), 'Cats_cone.XIDA should be a path here.');

    D = catsHTM.gatherCrossIDData(T, Cats_cone, 'CatList',{'XIDA'}, 'Columns',{}, 'Verbose',false);
    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    Loaded = load(Cats_cone.XIDA);
    verifyEqual(testCase, D.XIDA_RA(Mat), Loaded.Cat.Catalog(IndA(Mat),1), ...
        'AbsTol',1e-12, 'Gathered value from on-disk catalog mismatch.');
end

function testGatherPointerSource(testCase)
    % Source='pointer' fetches from catsHTM via CellID_/RowInCell_, needs no
    % Cats_cone, and matches what the 'cats' snapshot path returns.
    [T, Cats_cone] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);

    % pointer mode: Cats_cone omitted entirely (auto -> pointer when Cats_cone empty)
    Dp = catsHTM.gatherCrossIDData(T, [], 'Columns',{'RA','Dec'}, 'Verbose',false);
    verifyTrue(testCase, all(ismember({'XIDA_RA','XIDA_Dec','XIDREF_RA','XIDREF_Dec'}, ...
        Dp.Properties.VariableNames)), 'Pointer-mode columns missing.');

    % same values as the snapshot ('cats') path on the matched rows
    Dc   = catsHTM.gatherCrossIDData(T, Cats_cone, 'Columns',{'RA','Dec'}, 'Verbose',false);
    Mat  = find(~isnan(T.Ind_XIDA));
    verifyEqual(testCase, Dp.XIDA_RA(Mat), Dc.XIDA_RA(Mat), 'AbsTol',1e-12, ...
        'Pointer vs cats RA mismatch.');
    % unmatched rows (NaN pointer) come back as FillValue
    Un = find(isnan(T.Ind_XIDA));
    verifyTrue(testCase, all(isnan(Dp.XIDA_RA(Un))), 'Unmatched pointer rows must be NaN.');

    % explicit Source='cats' with no Cats_cone is an error
    verifyError(testCase, @() catsHTM.gatherCrossIDData(T, [], 'Source','cats','Verbose',false), ...
        'gatherCrossIDData:noCats', 'Source=cats without Cats_cone should error.');
end

function testOutputTypes(testCase)
    % Default output is a MATLAB table (with OriginCat). OutType='astrocatalog'
    % gives a numeric AstroCatalog (OriginCat -> Summary.OriginCat). The .mat
    % stores the same type as the return; the .csv always keeps OriginCat.
    [Tb, Cats_cone, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyTrue(testCase, istable(Tb), 'Default output should be a table.');
    verifyTrue(testCase, ismember('OriginCat', Tb.Properties.VariableNames), ...
        'Default table should include OriginCat.');

    Ac = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyClass(testCase, Ac, 'AstroCatalog', 'OutType=astrocatalog should return AstroCatalog.');
    verifyFalse(testCase, ismember('OriginCat', Ac.ColNames), ...
        'AstroCatalog must not carry the text OriginCat column.');
    verifyEqual(testCase, numel(S.OriginCat), height(Tb), ...
        'Summary.OriginCat must be row-aligned.');
    verifyTrue(testCase, isequaln(getCol(Ac,'Ind_XIDA'), Tb.Ind_XIDA), 'Ind_XIDA differs.');

    % gatherCrossIDData works on both forms
    D1 = catsHTM.gatherCrossIDData(Tb, Cats_cone, 'Columns',{}, 'Verbose',false);
    D2 = catsHTM.gatherCrossIDData(Ac, Cats_cone, 'Columns',{}, 'Verbose',false);
    verifyTrue(testCase, ismember('XIDA_RA', D1.Properties.VariableNames), 'gather(table) failed.');
    verifyTrue(testCase, ismember('XIDA_RA', D2.Properties.VariableNames), 'gather(AstroCatalog) failed.');

    % OutFile .mat stores the SAME type as the return
    StemT = fullfile(testCase.TestData.Dir, 'xid_tbl');
    catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false, 'OutFile',StemT);
    Lt = load([StemT '.mat']);
    verifyTrue(testCase, istable(Lt.XidTable), 'Default .mat XidTable should be a table.');

    StemA = fullfile(testCase.TestData.Dir, 'xid_ac');
    catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
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
    % XIDB has two sources within 2" of anchor #1: the nearest is in Ind_XIDB;
    % the extra goes into the compact Summary.ExtraMatches TABLE (one row).
    [Tb, ~, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'Verbose',false);

    R = find(Tb.Nmatch_XIDB == 2);
    verifyEqual(testCase, numel(R), 1, 'Exactly one master row should have 2 XIDB matches.');
    % XidTable is fully flat now - no ragged IndExtra_ cell columns
    verifyFalse(testCase, any(startsWith(Tb.Properties.VariableNames,'IndExtra_')), ...
        'XidTable should not have IndExtra_ cell columns anymore.');

    EM = S.ExtraMatches;
    verifyTrue(testCase, istable(EM), 'Summary.ExtraMatches must be a table.');
    verifyEqual(testCase, EM.Properties.VariableNames, ...
        {'MasterID','Catalog','Ind','CellID','RowInCell','Dist_arcsec'}, ...
        'ExtraMatches columns wrong.');

    % exactly one extra match: multi-match master row, catalog XIDB
    verifyEqual(testCase, height(EM), 1, 'Expected exactly one extra-match row.');
    verifyEqual(testCase, EM.MasterID, R, 'Extra row MasterID should be the multi-match row.');
    verifyEqual(testCase, EM.Catalog{1}, 'XIDB', 'Extra row catalog should be XIDB.');
    verifyNotEqual(testCase, EM.Ind, Tb.Ind_XIDB(R), 'Extra Ind must differ from the nearest.');
    % nearest + extra together are the two XIDB sources (indices 1 and 2)
    verifyEqual(testCase, sort([Tb.Ind_XIDB(R); EM.Ind]), [1;2], ...
        'Nearest + extra should be the two XIDB sources.');

    % the stored pointer is set and dereferences to one source; distance sane
    verifyFalse(testCase, isnan(EM.CellID) || isnan(EM.RowInCell), 'Extra pointer should be set.');
    D = catsHTM.gatherByPointer('XIDB', EM.CellID, EM.RowInCell, 'Columns',{'RA','Dec'});
    verifyEqual(testCase, size(D,1), 1, 'gatherByPointer should return one row.');
    verifyTrue(testCase, EM.Dist_arcsec > 0 && EM.Dist_arcsec < 2, 'Extra distance out of range.');
end

function testKeepExtraAstroCatAndErrors(testCase)
    % OutType='astrocatalog': XidTable is numeric-only, extras still in the
    % Summary.ExtraMatches table; and KeepExtraMatches + TableToDisk errors.
    [Ac, ~, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','astrocatalog', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'Verbose',false);
    verifyClass(testCase, Ac, 'AstroCatalog');
    verifyFalse(testCase, any(startsWith(Ac.ColNames,'IndExtra_')), ...
        'AstroCatalog must not carry IndExtra cell columns.');
    verifyTrue(testCase, isfield(S,'ExtraMatches') && istable(S.ExtraMatches), ...
        'Extras must be in the Summary.ExtraMatches table for AstroCatalog output.');
    verifyEqual(testCase, height(S.ExtraMatches), 1, 'One extra-match row expected.');

    verifyError(testCase, @() catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, ...
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
    [T, Cats_cone, ~] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyTrue(testCase, all(ismember({'CellID_XIDA','RowInCell_XIDA', ...
        'CellID_XIDREF','RowInCell_XIDREF'}, T.Properties.VariableNames)), ...
        'AddPointer columns missing.');

    IndA = T.Ind_XIDA;
    Mat  = find(~isnan(IndA));
    verifyFalse(testCase, any(isnan(T.CellID_XIDA(Mat))), 'Matched rows must have a cell id.');

    [sRA, sDec] = getLonLat(Cats_cone.XIDA, 'rad');
    [Cid, Row] = catsHTM.sourcePointer('XIDA', sRA(IndA(Mat)), sDec(IndA(Mat)));
    verifyEqual(testCase, T.CellID_XIDA(Mat),    Cid, 'CellID mismatch vs sourcePointer.');
    verifyEqual(testCase, T.RowInCell_XIDA(Mat), Row, 'RowInCell mismatch vs sourcePointer.');
end

function testGetNsrcMetaLocationIndependent(testCase)
    % getNsrcMeta resolves the catalog dir via which(), so it works from any
    % cwd (the catalogs are only on the path, not in the current directory).
    Nsrc = catsHTM.getNsrcMeta('XIDA');
    verifyEqual(testCase, sum(Nsrc(:,2)), 10, 'XIDA should hold 10 sources.');
    % explicit CatDir override yields the same per-cell counts
    Nsrc2 = catsHTM.getNsrcMeta('XIDA', 'CatDir', testCase.TestData.Dir);
    verifyEqual(testCase, sortrows(Nsrc2,1), sortrows(Nsrc,1), 'CatDir override mismatch.');
end

function testCatRowID(testCase)
    % sourcePointer's 4th output and catsHTM.catRowID collapse the
    % (CellID,RowInCell) pair into a contiguous, unique scalar id.
    RAD = 180./pi;
    [C, CC] = catsHTM.cone_search('XIDA', 45./RAD, 0, 10800);  % all XIDA sources
    ColRA  = find(strcmp(CC,'RA'),1);
    ColDec = find(strcmp(CC,'Dec'),1);
    Ra  = C(:,ColRA);
    Dec = C(:,ColDec);

    [Cid, Row, ~, Gid] = catsHTM.sourcePointer('XIDA', Ra, Dec);
    Ntot = sum(catsHTM.getNsrcMeta('XIDA'), 1);       % [sumCellID sumNsrc]
    Ntot = Ntot(2);

    % every source is addressed exactly once -> ids are a 1..N permutation
    verifyEqual(testCase, sort(Gid), (1:Ntot).', 'CatRowID must be a 1..N permutation.');
    % the 4th output equals a direct catRowID call on the pointer pair
    Gid2 = catsHTM.catRowID('XIDA', Cid, Row);
    verifyEqual(testCase, Gid2, Gid, 'sourcePointer CatRowID differs from catRowID.');
    % NaN pointers propagate to NaN ids
    GidNan = catsHTM.catRowID('XIDA', [Cid(1); NaN], [Row(1); 5]);
    verifyTrue(testCase, isnan(GidNan(2)), 'NaN cell id must give NaN CatRowID.');
end

function testAddCatRowID(testCase)
    % AddCatRowID adds a CatRowID_<Cat> scalar that inverts back to the
    % CellID_/RowInCell_ pair via catsHTM.catRowID2Pointer.
    [T, ~, ~] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2, ...
        'AddCatRowID',true,'Verbose',false);
    verifyTrue(testCase, all(ismember({'CatRowID_XIDA','CatRowID_XIDREF'}, ...
        T.Properties.VariableNames)), 'CatRowID_<Cat> columns missing.');

    Mat = find(~isnan(T.Ind_XIDA));
    Gid = T.CatRowID_XIDA(Mat);
    verifyFalse(testCase, any(isnan(Gid)), 'Matched rows must have a scalar id.');
    % round-trips back to the stored pointer pair
    [Cid, Row] = catsHTM.catRowID2Pointer('XIDA', Gid);
    verifyEqual(testCase, Cid, T.CellID_XIDA(Mat),    'CatRowID does not invert to CellID.');
    verifyEqual(testCase, Row, T.RowInCell_XIDA(Mat), 'CatRowID does not invert to RowInCell.');

    % AddCatRowID is silently skipped when AddPointer is off
    [T2, ~, ~] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2, ...
        'AddPointer',false,'AddCatRowID',true,'Verbose',false);
    verifyFalse(testCase, any(startsWith(T2.Properties.VariableNames, 'CatRowID_')), ...
        'CatRowID_<Cat> must be absent without AddPointer.');
end

function testExtraMatchesPointer(testCase)
    % The Summary.ExtraMatches pointer equals a fresh sourcePointer on that native
    % XIDB source; with AddPointer off, Ind/Dist stay but the pointer is NaN.
    [~, Cats_cone, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'AddPointer',true,'Verbose',false);
    EM = S.ExtraMatches;
    verifyEqual(testCase, height(EM), 1, 'Expected one extra-match row.');

    % pointer matches sourcePointer on the extra source's coordinates
    [xRA, xDec] = getLonLat(Cats_cone.XIDB, 'rad');
    [Cid, Row]  = catsHTM.sourcePointer('XIDB', xRA(EM.Ind), xDec(EM.Ind));
    verifyEqual(testCase, EM.CellID,    Cid, 'Extra CellID mismatch vs sourcePointer.');
    verifyEqual(testCase, EM.RowInCell, Row, 'Extra RowInCell mismatch vs sourcePointer.');

    % AddPointer off -> Ind and Dist_arcsec present, pointer columns NaN
    [~, ~, S2] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDB'},'MatchRadius',2, ...
        'KeepExtraMatches',true,'AddPointer',false,'Verbose',false);
    EM2 = S2.ExtraMatches;
    verifyEqual(testCase, height(EM2), 1, 'Expected one extra-match row (no pointer).');
    verifyFalse(testCase, isnan(EM2.Ind), 'Ind should be present without AddPointer.');
    verifyTrue(testCase, isnan(EM2.CellID) && isnan(EM2.RowInCell), ...
        'Pointer columns should be NaN when AddPointer is off.');
    verifyTrue(testCase, EM2.Dist_arcsec > 0, 'Dist should be present without AddPointer.');
end

function testCatRowID2Pointer(testCase)
    % catRowID2Pointer is the exact inverse of catRowID (round-trip).
    RAD = 180./pi;
    [C, CC] = catsHTM.cone_search('XIDA', 45./RAD, 0, 10800);
    ColRA  = find(strcmp(CC,'RA'),1);
    ColDec = find(strcmp(CC,'Dec'),1);
    [Cid, Row] = catsHTM.sourcePointer('XIDA', C(:,ColRA), C(:,ColDec));

    Gid          = catsHTM.catRowID('XIDA', Cid, Row);
    [Cid2, Row2] = catsHTM.catRowID2Pointer('XIDA', Gid);
    verifyEqual(testCase, Cid2, Cid, 'CellID round-trip failed.');
    verifyEqual(testCase, Row2, Row, 'RowInCell round-trip failed.');

    % invalid ids (NaN / out of range) map to NaN pointers
    [CidBad, RowBad] = catsHTM.catRowID2Pointer('XIDA', [NaN; 0; 1e9]);
    verifyTrue(testCase, all(isnan(CidBad)), 'Invalid ids must give NaN CellID.');
    verifyTrue(testCase, all(isnan(RowBad)), 'Invalid ids must give NaN RowInCell.');
end

function testGatherByPointer(testCase)
    % gatherByPointer reads the real rows addressed by (CellID,RowInCell),
    % matching the cone_search values, without a query or in-memory catalog.
    RAD = 180./pi;
    [C, CC] = catsHTM.cone_search('XIDA', 45./RAD, 0, 10800);
    ColRA  = find(strcmp(CC,'RA'),1);
    ColDec = find(strcmp(CC,'Dec'),1);
    Ra  = C(:,ColRA);
    Dec = C(:,ColDec);
    [Cid, Row] = catsHTM.sourcePointer('XIDA', Ra, Dec);

    % selected columns come back in request order and match the source values
    [D, Cols] = catsHTM.gatherByPointer('XIDA', Cid, Row, 'Columns',{'Dec','RA'});
    verifyEqual(testCase, Cols, {'Dec','RA'}, 'Column order not preserved.');
    verifyEqual(testCase, D(:,2), Ra,  'AbsTol',1e-12, 'Gathered RA mismatch.');
    verifyEqual(testCase, D(:,1), Dec, 'AbsTol',1e-12, 'Gathered Dec mismatch.');

    % all-columns default + NaN pointer -> FillValue row
    DAll = catsHTM.gatherByPointer('XIDA', [Cid(1); NaN], [Row(1); NaN]);
    verifyEqual(testCase, size(DAll,2), numel(CC), 'Default should return all columns.');
    verifyTrue(testCase, all(isnan(DAll(2,:))), 'NaN pointer row must be FillValue.');

    % unknown column errors
    verifyError(testCase, @() catsHTM.gatherByPointer('XIDA', Cid, Row, 'Columns',{'NoSuchCol'}), ...
        'catsHTM:gatherByPointer:badColumn', 'Unknown column should error.');
end

function testCatalogSignature(testCase)
    % catalogSignature returns a deterministic version fingerprint whose Nsrc
    % and Ncell agree with getNsrcMeta, computed from the index alone.
    Sig = catsHTM.catalogSignature('XIDA');
    verifyTrue(testCase, all(isfield(Sig, {'Name','LayoutHash','ColHash', ...
        'ChecksumHash','Version','Nsrc','Ncell','StampedAt'})), ...
        'Signature struct is missing fields.');
    verifyEqual(testCase, Sig.Name, 'XIDA', 'Signature Name wrong.');

    Nsrc = catsHTM.getNsrcMeta('XIDA');
    verifyEqual(testCase, Sig.Nsrc, sum(Nsrc(:,2)), 'Signature Nsrc != getNsrcMeta.');
    verifyEqual(testCase, Sig.Ncell, sum(Nsrc(:,2) > 0), 'Signature Ncell wrong.');

    % deterministic: same catalog -> identical hashes (StampedAt aside)
    Sig2 = catsHTM.catalogSignature('XIDA', 'CatDir', testCase.TestData.Dir);
    verifyEqual(testCase, Sig2.LayoutHash, Sig.LayoutHash, 'LayoutHash not deterministic.');
    verifyEqual(testCase, Sig2.ColHash,    Sig.ColHash,    'ColHash not deterministic.');

    % a catalog with a DIFFERENT layout (XIDB has 2 sources vs XIDA's 10) gets
    % a different signature. (Two catalogs with identical layout+columns share a
    % layout signature by design - it fingerprints the build, not the identity.)
    SigB = catsHTM.catalogSignature('XIDB');
    verifyTrue(testCase, ~strcmp(SigB.LayoutHash, Sig.LayoutHash), ...
        'Different-layout catalogs should have different LayoutHash.');
end

function testCheckCatalogSignatureClassifies(testCase)
    % checkCatalogSignature classifies changes: valid / columns-changed /
    % stale-layout, driven only by which stored hash we perturb.
    Sig = catsHTM.catalogSignature('XIDA');

    % identical -> valid, Ok true
    [Ok, Rep] = catsHTM.checkCatalogSignature('XIDA', Sig, 'Warn', false);
    verifyTrue(testCase, Ok, 'Unchanged catalog should validate.');
    verifyEqual(testCase, Rep.Status, 'valid', 'Status should be valid.');

    % only ColHash differs -> columns-changed, row pointers still Ok
    ColChanged = Sig; ColChanged.ColHash = 'deadbeefdeadbeefdeadbeefdeadbeef';
    [Ok2, Rep2] = catsHTM.checkCatalogSignature('XIDA', ColChanged, 'Warn', false);
    verifyTrue(testCase, Ok2, 'Column-only change must keep row pointers valid.');
    verifyEqual(testCase, Rep2.Status, 'columns-changed', 'Should be columns-changed.');

    % LayoutHash differs -> stale-layout, Ok false
    LayoutChanged = Sig; LayoutChanged.LayoutHash = '00000000000000000000000000000000';
    [Ok3, Rep3] = catsHTM.checkCatalogSignature('XIDA', LayoutChanged, 'Warn', false);
    verifyFalse(testCase, Ok3, 'Layout change must invalidate pointers.');
    verifyEqual(testCase, Rep3.Status, 'stale-layout', 'Should be stale-layout.');
end

function testStampSignatureInSummary(testCase)
    % crossIDCatsHTM stamps a per-catalog signature into Summary.Signature that
    % matches a fresh catalogSignature call.
    [~, ~, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);
    verifyTrue(testCase, isfield(S, 'Signature') && isfield(S.Signature, 'XIDA'), ...
        'Summary.Signature.XIDA missing.');
    verifyEqual(testCase, S.Signature.XIDA.LayoutHash, ...
        catsHTM.catalogSignature('XIDA').LayoutHash, 'Stamped LayoutHash mismatch.');

    % opting out drops the field
    [~, ~, S2] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2, ...
        'StampSignature',false,'Verbose',false);
    verifyFalse(testCase, isfield(S2, 'Signature'), 'StampSignature=false should omit it.');
end

function testGatherByPointerValidatesSignature(testCase)
    % gatherByPointer honours a stored Signature: a stale row layout errors,
    % the correct signature is a no-op that returns the same data.
    RAD = 180./pi;
    [C, CC] = catsHTM.cone_search('XIDA', 45./RAD, 0, 10800);
    Ra  = C(:,strcmp(CC,'RA'));
    Dec = C(:,strcmp(CC,'Dec'));
    [Cid, Row] = catsHTM.sourcePointer('XIDA', Ra, Dec);
    Sig = catsHTM.catalogSignature('XIDA');

    % correct signature -> identical result to no-signature gather
    D0 = catsHTM.gatherByPointer('XIDA', Cid, Row, 'Columns',{'RA'});
    D1 = catsHTM.gatherByPointer('XIDA', Cid, Row, 'Columns',{'RA'}, 'Signature',Sig);
    verifyEqual(testCase, D1, D0, 'AbsTol',1e-12, 'Valid signature changed the data.');

    % stale layout -> refuse to dereference
    Bad = Sig; Bad.LayoutHash = '00000000000000000000000000000000';
    verifyError(testCase, @() catsHTM.gatherByPointer('XIDA', Cid, Row, ...
        'Columns',{'RA'}, 'Signature',Bad), ...
        'catsHTM:gatherByPointer:staleSignature', 'Stale layout should error.');

    % ValidateSig=false bypasses the check even with a bad signature
    Dbypass = catsHTM.gatherByPointer('XIDA', Cid, Row, 'Columns',{'RA'}, ...
        'Signature',Bad, 'ValidateSig',false);
    verifyEqual(testCase, Dbypass, D0, 'AbsTol',1e-12, 'ValidateSig=false should bypass.');
end

function testGatherCrossIDDataValidatesSignature(testCase)
    % gatherCrossIDData(Source='pointer') passes Summary.Signature through and
    % refuses a stale catalog; the correct signature is transparent.
    [T, ~, S] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA'},'MatchRadius',2,'Verbose',false);

    % correct Summary.Signature -> gathers normally
    D = catsHTM.gatherCrossIDData(T, [], 'Columns',{'RA'}, ...
        'Signature',S.Signature, 'Verbose',false);
    verifyTrue(testCase, ismember('XIDA_RA', D.Properties.VariableNames), ...
        'Validated pointer gather failed.');

    % corrupt XIDA's layout hash -> stale-layout error from the gather
    BadSig = S.Signature;
    BadSig.XIDA.LayoutHash = '00000000000000000000000000000000';
    verifyError(testCase, @() catsHTM.gatherCrossIDData(T, [], 'Columns',{'RA'}, ...
        'Signature',BadSig, 'Verbose',false), ...
        'catsHTM:gatherByPointer:staleSignature', 'Stale signature should error.');
end

function testGatherAutoColumnsAndSkip(testCase)
    % Default Columns='auto' pulls ONE column per catalog (prime mag, or the Dec
    % fallback for the RA/Dec-only fixtures); {} pulls all; SkipCats drops a
    % catalog from Data without touching T. Verified in both cats and pointer mode.
    [T, Cats_cone] = catsHTM.crossIDCatsHTM(45.*pi./180, 0, 10800, 'OutType','table', ...
        'RefCat','XIDREF','CatList',{'XIDA','XIDB'},'MatchRadius',2,'Verbose',false);

    % auto (default): one column per catalog -> Dec fallback (no mag in fixture)
    Da = catsHTM.gatherCrossIDData(T, Cats_cone, 'Verbose',false);
    verifyTrue(testCase, ismember('XIDA_Dec', Da.Properties.VariableNames), ...
        'auto should pull the Dec fallback for a mag-less catalog.');
    verifyFalse(testCase, ismember('XIDA_RA', Da.Properties.VariableNames), ...
        'auto should pull only one column per catalog.');

    % auto in pointer mode (reads the colcell straight from catsHTM)
    Dp = catsHTM.gatherCrossIDData(T, [], 'Source','pointer', 'Verbose',false);
    verifyTrue(testCase, ismember('XIDA_Dec', Dp.Properties.VariableNames), ...
        'auto pointer-mode should pull the Dec fallback.');
    verifyFalse(testCase, ismember('XIDA_RA', Dp.Properties.VariableNames), ...
        'auto pointer-mode: one column per catalog.');

    % Columns={} : all columns (RA and Dec both present)
    Df = catsHTM.gatherCrossIDData(T, Cats_cone, 'Columns',{}, 'Verbose',false);
    verifyTrue(testCase, all(ismember({'XIDA_RA','XIDA_Dec'}, Df.Properties.VariableNames)), ...
        'Columns={} should pull all columns.');

    % SkipCats drops XIDA from Data but leaves it in T
    Ds = catsHTM.gatherCrossIDData(T, Cats_cone, 'Columns',{}, 'SkipCats',{'XIDA'}, 'Verbose',false);
    verifyFalse(testCase, any(startsWith(Ds.Properties.VariableNames,'XIDA_')), ...
        'SkipCats should exclude XIDA columns.');
    verifyTrue(testCase, any(startsWith(Ds.Properties.VariableNames,'XIDB_')), ...
        'SkipCats should keep other catalogs.');
    verifyTrue(testCase, ismember('Ind_XIDA', T.Properties.VariableNames), ...
        'SkipCats must not modify T.');
end

function testAutoPicksPrimeMag(testCase)
    % When a catalog HAS magnitudes, 'auto' picks the prime band (G over R), and
    % AutoNMag=2 returns both. Uses a one-source catalog with Mag_G/Mag_R.
    RAD = 180./pi;
    CRA = 45.357; CDec = 0.894;
    Src = [CRA./RAD, CDec./RAD, 18.0, 17.0];       % RA Dec Mag_G Mag_R
    Old = pwd; cd(testCase.TestData.Dir);           % built in the (path'd) fixture dir
    VO.prep.build_htm_catalog(Src, 'CatName','XMAG', 'HTM_Level',4, ...
        'ColCell',{'RA','Dec','Mag_G','Mag_R'}, 'ColUnits',{'rad','rad','mag','mag'});
    cd(Old);
    evalin('base','clear XMAG_HTM');                % avoid a stale cached index

    [Cid, Row]  = catsHTM.sourcePointer('XMAG', CRA./RAD, CDec./RAD);
    [~, Cols]   = catsHTM.gatherByPointer('XMAG', Cid, Row, 'Columns','auto');
    verifyEqual(testCase, Cols, {'Mag_G'}, 'auto should pick the G band over R.');

    [~, Cols2]  = catsHTM.gatherByPointer('XMAG', Cid, Row, 'Columns','auto', 'AutoNMag',2);
    verifyEqual(testCase, sort(Cols2), {'Mag_G','Mag_R'}, 'AutoNMag=2 should pick both mags.');
end

function testMarkShared(testCase)
    % MarkShared adds NcoreShare_<Cat>: when ONE Cat source is the nearest match
    % to TWO core sources (a blend), both rows get NcoreShare_<Cat>=2. Duplicates
    % are marked, never dropped; the column is absent when MarkShared is off.
    RAD = 180./pi;
    CRA = 45.357; CDec = 0.894;
    Sep = 1.0/3600/RAD;                                  % 1 arcsec (rad)
    % two core sources 1" apart, and one Cat source at their midpoint (0.5" from
    % each) so a 2" match assigns that single Cat source to BOTH core sources
    Ref = [CRA./RAD,                    CDec./RAD; ...
           CRA./RAD + Sep./cosd(CDec),  CDec./RAD];
    Cat = [CRA./RAD + 0.5.*Sep./cosd(CDec), CDec./RAD];
    Old = pwd; cd(testCase.TestData.Dir);
    VO.prep.build_htm_catalog(Ref, 'CatName','SREF','HTM_Level',4, ...
        'ColCell',{'RA','Dec'}, 'ColUnits',{'rad','rad'});
    VO.prep.build_htm_catalog(Cat, 'CatName','SCAT','HTM_Level',4, ...
        'ColCell',{'RA','Dec'}, 'ColUnits',{'rad','rad'});
    cd(Old);
    evalin('base','clear SREF_HTM SCAT_HTM');

    T = catsHTM.crossIDCatsHTM(CRA./RAD, CDec./RAD, 10800, 'OutType','table', ...
        'RefCat','SREF', 'CatList',{'SCAT'}, 'MatchRadius',2, ...
        'MarkShared',true, 'Verbose',false);
    verifyTrue(testCase, ismember('NcoreShare_SCAT', T.Properties.VariableNames), ...
        'NcoreShare_SCAT column missing.');
    Matched = ~isnan(T.Ind_SCAT);
    verifyEqual(testCase, sum(Matched), 2, 'Both core sources should match the one SCAT source.');
    verifyEqual(testCase, T.NcoreShare_SCAT(Matched), [2;2], ...
        'A Cat source shared by two core sources should have NcoreShare=2.');

    % default off -> no NcoreShare_ columns
    T2 = catsHTM.crossIDCatsHTM(CRA./RAD, CDec./RAD, 10800, 'OutType','table', ...
        'RefCat','SREF', 'CatList',{'SCAT'}, 'MatchRadius',2, 'Verbose',false);
    verifyFalse(testCase, any(startsWith(T2.Properties.VariableNames,'NcoreShare_')), ...
        'NcoreShare_ must be absent without MarkShared.');
end
