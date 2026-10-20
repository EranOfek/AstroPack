function tests = test_cone_search
    % Unit tests for catsHTM.cone_search 'Method' and 'Columns' (issue #1403).
    %
    % Builds a small synthetic catsHTM catalog (4 columns, spread over many HTM
    % cells, including across RA=0) and checks that the default per-cell trimming
    % ('percell') returns exactly the same output as the original method
    % ('joined'), and that 'Columns' selects/reorders columns correctly.
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    RAD = 180./pi;
    Tmp = tempname;
    mkdir(Tmp);

    rng(11);
    N   = 4000;
    RAd = mod(-3 + 6.*rand(N,1), 360);   % [deg] 357..3 deg: straddles RA=0
    Decd = -3 + 6.*rand(N,1);            % [deg]
    Mag  = 12 + 8.*rand(N,1);
    Idx  = (1:N).';
    Data = [RAd./RAD, Decd./RAD, Mag, Idx];

    Old = pwd;
    cd(Tmp);
    VO.prep.build_htm_catalog(Data, 'CatName','CSTEST', 'HTM_Level',6, ...
        'ColCell',{'RA','Dec','Mag','Idx'}, 'ColUnits',{'rad','rad','mag',''});
    cd(Old);
    addpath(Tmp);
    evalin('base', 'clear CSTEST_HTM');   % drop a stale cached index

    testCase.TestData.Dir = Tmp;
end

function teardown(testCase)
    Tmp = testCase.TestData.Dir;
    evalin('base', 'clear CSTEST_HTM');
    if any(strcmp(strsplit(path, pathsep), Tmp))
        rmpath(Tmp);
    end
    if isfolder(Tmp)
        rmdir(Tmp, 's');
    end
end

%% Test Functions

function testPerCellEqualsJoined(testCase)
    % Default (percell) must reproduce the original (joined) output exactly,
    % including a cone across RA=0 and one that spans many cells.
    RAD = 180./pi;
    Cones = [0.5 0.5 600; 0 0 3600; 359.8 -1 1800; 1.5 2 7200];   % [deg deg arcsec]
    for I = 1:size(Cones,1)
        Args = {Cones(I,1)./RAD, Cones(I,2)./RAD, Cones(I,3)};
        [C1,N1,U1,D1] = catsHTM.cone_search('CSTEST', Args{:});
        [C2,N2,U2,D2] = catsHTM.cone_search('CSTEST', Args{:}, 'Method','joined');
        Msg = sprintf('cone %d', I);
        testCase.verifyGreaterThan(size(C1,1), 0, [Msg ': cone should not be empty.']);
        testCase.verifyEqual(C1, C2, [Msg ': Cat differs between percell and joined.']);
        testCase.verifyEqual(N1, N2, [Msg ': ColCell differs.']);
        testCase.verifyEqual(U1, U2, [Msg ': ColUnits differs.']);
        testCase.verifyEqual(D1, D2, [Msg ': D differs.']);
        testCase.verifyLessThan(max(D1), Cones(I,3)./(3600.*RAD), [Msg ': source outside cone.']);
    end
end

function testColumnsSelectAndOrder(testCase)
    % Columns returns the requested columns, in the requested order; names
    % absent from the catalog are ignored; both methods agree.
    Args = {0, 0, 3600};
    Full = catsHTM.cone_search('CSTEST', Args{:});
    [C, N, U] = catsHTM.cone_search('CSTEST', Args{:}, 'Columns',{'Idx','NoSuchCol','RA','Mag'});
    testCase.verifyEqual(N(:).', {'Idx','RA','Mag'}, 'ColCell not selected/ordered as requested.');
    testCase.verifyEqual(U(:).', {'','rad','mag'}, 'ColUnits not selected/ordered as requested.');
    testCase.verifyEqual(C, Full(:,[4 1 3]), 'Selected columns differ from the full-catalog columns.');
    CJ = catsHTM.cone_search('CSTEST', Args{:}, 'Columns',{'Idx','NoSuchCol','RA','Mag'}, 'Method','joined');
    testCase.verifyEqual(CJ, C, 'Columns: joined and percell disagree.');
    % char / string input accepted
    Cc = catsHTM.cone_search('CSTEST', Args{:}, 'Columns','Mag');
    Cs = catsHTM.cone_search('CSTEST', Args{:}, 'Columns',["Mag","Dec"]);
    testCase.verifyEqual(Cc, Full(:,3), 'char Columns input failed.');
    testCase.verifyEqual(Cs, Full(:,[3 2]), 'string-array Columns input failed.');
end

function testOnlyConeFalse(testCase)
    % OnlyCone=false: rows unchanged between methods, Columns still applied.
    Args = {0, 0, 1800, 'OnlyCone',false};
    C1 = catsHTM.cone_search('CSTEST', Args{:});
    C2 = catsHTM.cone_search('CSTEST', Args{:}, 'Method','joined');
    testCase.verifyEqual(C1, C2, 'OnlyCone=false: rows differ between methods.');
    C3 = catsHTM.cone_search('CSTEST', Args{:}, 'Columns',{'Mag'});
    testCase.verifyEqual(C3, C1(:,3), 'OnlyCone=false: Columns not applied.');
end

function testEmptyCone(testCase)
    % A cone far from the catalog patch returns an empty catalog, both methods.
    RAD = 180./pi;
    C1 = catsHTM.cone_search('CSTEST', 180./RAD, 60./RAD, 10);
    C2 = catsHTM.cone_search('CSTEST', 180./RAD, 60./RAD, 10, 'Method','joined');
    testCase.verifyTrue(isempty(C1), 'percell: empty cone should return empty.');
    testCase.verifyTrue(isempty(C2), 'joined: empty cone should return empty.');
end

function testShortColUnitsPadded(testCase)
    % A colcell whose ColUnits is shorter than ColCell must not crash Columns.
    File = fullfile(testCase.TestData.Dir, 'CSTEST_htmColCell.mat');
    S = load(File);
    S.ColUnits = S.ColUnits(1:2);
    save(File, '-struct', 'S');
    [C, N, U] = catsHTM.cone_search('CSTEST', 0, 0, 3600, 'Columns',{'Mag','RA'});
    testCase.verifyEqual(size(C,2), 2, 'Wrong number of columns.');
    testCase.verifyEqual(N(:).', {'Mag','RA'}, 'ColCell wrong.');
    testCase.verifyEqual(U(:).', {'','rad'}, 'Short ColUnits not padded with empty units.');
end

function testBadMethodErrors(testCase)
    % An unknown Method throws a clear error.
    testCase.verifyError(@() catsHTM.cone_search('CSTEST', 0, 0, 60, 'Method','foo'), ...
        'catsHTM:cone_search:Method', 'Unknown Method should error.');
end
