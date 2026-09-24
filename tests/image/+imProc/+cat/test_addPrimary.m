function tests = test_addPrimary
    % Unit tests for the issue #1180 overlap/ownership machinery:
    %   - imUtil.cut.gridSubImage exclusive (single-coverage) sections
    %   - imProc.cat.addPrimary 'primary' ownership column
    %   - imUtil.mask.setCoaddOverlap fed with the exclusive section
    tests = functiontests(localfunctions);
end

%% Fixture

function setup(testCase)
    % A small grid with real overlaps: 1000x900 frame, 300x301 tiles
    ImageSize = [1000 900];    % [X Y]
    SubSize   = [300 301];
    [CCDSEC, NSub, ~, NewNoOverlap, ~, ExclusiveCCDSEC, NewExclusive] = ...
        imUtil.cut.gridSubImage(ImageSize, SubSize);

    testCase.TestData.ImageSize    = ImageSize;
    testCase.TestData.SubSize      = SubSize;
    testCase.TestData.CCDSEC       = CCDSEC;
    testCase.TestData.NSub         = NSub;
    testCase.TestData.NewNoOverlap = NewNoOverlap;
    testCase.TestData.ExclusiveCCDSEC = ExclusiveCCDSEC;
    testCase.TestData.NewExclusive = NewExclusive;
end

%% gridSubImage exclusive sections

function testExclusiveComplementIsMultiCoverage(testCase)
    % The complement of the exclusive section within each tile must be
    % exactly the set of pixels covered by 2 or more tiles.
    ImageSize = testCase.TestData.ImageSize;
    CCDSEC    = testCase.TestData.CCDSEC;
    Excl      = testCase.TestData.ExclusiveCCDSEC;

    % pixel coverage count over the full frame [Y X]
    Cover = zeros(ImageSize(2), ImageSize(1));
    Ntile = size(CCDSEC,1);
    for K=1:1:Ntile
        Cover(CCDSEC(K,3):CCDSEC(K,4), CCDSEC(K,1):CCDSEC(K,2)) = ...
            Cover(CCDSEC(K,3):CCDSEC(K,4), CCDSEC(K,1):CCDSEC(K,2)) + 1;
    end

    for K=1:1:Ntile
        TileCover = Cover(CCDSEC(K,3):CCDSEC(K,4), CCDSEC(K,1):CCDSEC(K,2));
        InExcl    = false(size(TileCover));
        Xr = (Excl(K,1):Excl(K,2)) - CCDSEC(K,1) + 1;
        Yr = (Excl(K,3):Excl(K,4)) - CCDSEC(K,3) + 1;
        InExcl(Yr, Xr) = true;
        % single coverage inside the exclusive section, multi outside
        verifyTrue(testCase, all(TileCover(InExcl)==1, 'all'), ...
            sprintf('Tile %d: exclusive section contains multi-covered pixels', K));
        verifyTrue(testCase, all(TileCover(~InExcl)>=2, 'all'), ...
            sprintf('Tile %d: overlap region contains single-covered pixels', K));
    end
end

function testExclusiveInsideUniqueSection(testCase)
    % The exclusive section is contained in the unique (ownership) section
    NewNoOverlap = testCase.TestData.NewNoOverlap;
    NewExclusive = testCase.TestData.NewExclusive;
    verifyTrue(testCase, all(NewExclusive(:,1)>=NewNoOverlap(:,1)) && ...
                         all(NewExclusive(:,2)<=NewNoOverlap(:,2)) && ...
                         all(NewExclusive(:,3)>=NewNoOverlap(:,3)) && ...
                         all(NewExclusive(:,4)<=NewNoOverlap(:,4)));
end

%% addPrimary

function testExactlyOnePrimaryPerSource(testCase)
    % Random full-frame sources, replicated into every tile covering them:
    % each source must get primary==1 in exactly one tile.
    ImageSize    = testCase.TestData.ImageSize;
    CCDSEC       = testCase.TestData.CCDSEC;
    NewNoOverlap = testCase.TestData.NewNoOverlap;
    Ntile        = size(CCDSEC,1);

    rng(11);
    Nsrc  = 500;
    Xfull = 0.5 + rand(Nsrc,1).*ImageSize(1);
    Yfull = 0.5 + rand(Nsrc,1).*ImageSize(2);
    % add sources exactly on ownership boundaries (half-integer positions)
    Xfull = [Xfull; NewNoOverlap(2,2)+CCDSEC(2,1)-1+0.5; 100.5];
    Yfull = [Yfull; 200.0;                               NewNoOverlap(1,4)+0.5];
    Nsrc  = numel(Xfull);

    NPrimary = zeros(Nsrc,1);
    for K=1:1:Ntile
        % sources whose position falls inside this tile footprint
        In = Xfull >= CCDSEC(K,1)-0.5 & Xfull < CCDSEC(K,2)+0.5 & ...
             Yfull >= CCDSEC(K,3)-0.5 & Yfull < CCDSEC(K,4)+0.5;
        if any(In)
            Xloc = Xfull(In) - CCDSEC(K,1) + 1;
            Yloc = Yfull(In) - CCDSEC(K,3) + 1;
            AC = AstroCatalog({[Xloc, Yloc]}, 'ColNames',{'X','Y'});
            imProc.cat.addPrimary(AC, NewNoOverlap(K,:));
            verifyTrue(testCase, AC.isColumn('primary'));
            Primary = AC.getCol('primary');
            Ind = find(In);
            NPrimary(Ind) = NPrimary(Ind) + Primary;
        end
    end
    verifyTrue(testCase, all(NPrimary==1), ...
        sprintf('%d source(s) are not primary in exactly one tile', sum(NPrimary~=1)));
end

function testHeaderSectionAndNaN(testCase)
    % CCDSEC read from the UNIQSEC header keyword; NaN positions get 0;
    % second call replaces the column (idempotent).
    Sec = [11 110 6 95];
    AI  = AstroImage({zeros(100,120)});
    AI.HeaderData.replaceVal('UNIQSEC', imUtil.ccdsec.ccdsec2str(Sec));
    X   = [11; 10.4; 110.4; 110.6; NaN];
    Y   = [50;   50;    50;    50;  50];
    AI.CatData = AstroCatalog({[X, Y]}, 'ColNames',{'X','Y'});

    imProc.cat.addPrimary(AI);
    Primary = AI.CatData.getCol('primary');
    verifyEqual(testCase, Primary, [1; 0; 1; 0; 0]);

    % explicit section overrides, and the column is replaced in place
    imProc.cat.addPrimary(AI, [1 120 1 100]);
    Primary = AI.CatData.getCol('primary');
    verifyEqual(testCase, Primary, [1; 1; 1; 1; 0]);
    verifyEqual(testCase, sum(strcmp(AI.CatData.ColNames, 'primary')), 1);
end

%% setCoaddOverlap with the exclusive section

function testSetCoaddOverlapExclusive(testCase)
    % The Overlap bit re-set outside the exclusive section must equal
    % flag_ccdsec(size, EXCLSEC, false): the full overlap region.
    SubSize      = testCase.TestData.SubSize;
    NewExclusive = testCase.TestData.NewExclusive;
    BitInd = 25;

    K    = 8;   % an interior-ish tile
    Mask = zeros(SubSize(2), SubSize(1), 'uint32');
    Mask = imUtil.mask.setCoaddOverlap(Mask, NewExclusive(K,:), 'BitInd',BitInd);
    Ref  = imUtil.ccdsec.flag_ccdsec([SubSize(2), SubSize(1)], NewExclusive(K,:), false);
    verifyEqual(testCase, bitget(Mask, BitInd+1)>0, Ref);
end
