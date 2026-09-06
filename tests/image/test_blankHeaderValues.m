function tests = test_blankHeaderValues
    % Issue #1252: the unmeasured-value convention - a blank FITS card
    % (quoted all-blank string, or a value-less/undefined card) must read
    % as NaN on EVERY access path: the raw FITS.readHeader1 cell,
    % AstroHeader.getVal, getStructKey, and the no-conversion getValSimple.
    % In-memory empty values ('') must behave the same.
    tests = functiontests(localfunctions);
end

function setup(testCase)
    % Write an image whose header carries NaN (-> blank-string card), then
    % craft a value-less (FITS undefined) card into a copy at byte level.
    Tmp = [tempname, '.fits'];
    AI  = AstroImage({rand(8)});
    AI.HeaderData.replaceVal('TSTBLNK', NaN);      % writer renders '        '
    AI.HeaderData.replaceVal('TSTNUM',  17.5);     % control: real value
    AI.write1(Tmp);

    Raw = fileread(Tmp);
    P   = strfind(Raw, 'TSTBLNK');
    Card = Raw(P(1):P(1)+79);
    assert(contains(Card, ''''), 'expected a quoted blank card from the writer');

    % value-less card version
    Tmp2 = [tempname, '.fits'];
    Fid = fopen(Tmp2,'w');
    NewCard = ['TSTBLNK =', repmat(' ',1,71)];
    fwrite(Fid, [Raw(1:P(1)-1), NewCard, Raw(P(1)+80:end)]);
    fclose(Fid);

    testCase.TestData.FileBlankStr  = Tmp;
    testCase.TestData.FileValueless = Tmp2;
end

function teardown(testCase)
    delete(testCase.TestData.FileBlankStr);
    delete(testCase.TestData.FileValueless);
end

function testBlankStringCard(testCase)
    F  = testCase.TestData.FileBlankStr;
    HC = FITS.readHeader1(F, 1);
    I  = find(strcmp(HC(:,1),'TSTBLNK'));
    verifyTrue(testCase, isnumeric(HC{I,2}) && isnan(HC{I,2}), 'raw cell not NaN');
    H  = AstroHeader(F, 1);
    verifyTrue(testCase, isnan(H.getVal('TSTBLNK')));
    verifyTrue(testCase, isnan(H.getValSimple('TSTBLNK')));
    S = H.getStructKey({'TSTBLNK'});
    verifyTrue(testCase, isnan(S.TSTBLNK));
    verifyEqual(testCase, H.getValSimple('TSTNUM'), 17.5);   % real value intact
end

function testValuelessCard(testCase)
    F  = testCase.TestData.FileValueless;
    HC = FITS.readHeader1(F, 1);
    I  = find(strcmp(HC(:,1),'TSTBLNK'));
    verifyTrue(testCase, isnumeric(HC{I,2}) && isnan(HC{I,2}), 'raw cell not NaN');
    H  = AstroHeader(F, 1);
    verifyTrue(testCase, isnan(H.getVal('TSTBLNK')));
    verifyTrue(testCase, isnan(H.getValSimple('TSTBLNK')));
end

function testInMemoryEmpty(testCase)
    H = AstroHeader;
    H.replaceVal('TSTEMPTY', '');
    verifyTrue(testCase, isnan(H.getVal('TSTEMPTY')));
    verifyTrue(testCase, isnan(H.getValSimple('TSTEMPTY')));
    % a genuinely-string value stays a string
    H.replaceVal('TSTSTR', 'coadd');
    verifyEqual(testCase, H.getValSimple('TSTSTR'), 'coadd');
end

function testRoundTrip(testCase)
    % blank card -> NaN (memory) -> blank card (file): no literal 'NaN'
    % must ever reach the FITS file.
    F = testCase.TestData.FileValueless;
    H = AstroHeader(F, 1);
    verifyTrue(testCase, isnan(H.getValSimple('TSTBLNK')));
    AI = AstroImage({rand(8)});  AI.HeaderData = H;
    Out = [tempname, '.fits'];
    AI.write1(Out);
    Raw = fileread(Out);
    P   = strfind(Raw, 'TSTBLNK');
    Card = Raw(P(1):P(1)+79);
    verifyFalse(testCase, contains(Card, 'NaN'), 'literal NaN leaked into the card');
    verifyTrue(testCase, isnan(AstroHeader(Out,1).getValSimple('TSTBLNK')));
    delete(Out);
end
