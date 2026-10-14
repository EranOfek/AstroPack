function tests = test_AstroTransmission_getFilt
    % Unit tests for AstroTransmission.getFilt and the AstroSpec filter reader.
    %
    % getFilt is the modern entry point to the AstroPack filter catalogue
    % (cats.spec.filter.AstFilterCat). It was a stub until Oct 2026, which
    % also left AstroSpec.read2FilterMatrix's 'AstroTransmission' branch
    % calling a non-existent AstFilter.getFilt.
    %
    % Lives in tests/image/ rather than tests/image/@AstroTransmission/
    % because a plain function file inside a class folder is shadowed by the
    % class and cannot be loaded by runtests.

    tests = functiontests(localfunctions);
end

function testReturnsPopulatedAstroTransmission(testCase)
    T = AstroTransmission.getFilt('SDSS', 'g');

    testCase.verifyClass(T, 'AstroTransmission');
    testCase.verifyNumElements(T, 1);
    testCase.verifyEqual(T.Band, 'g');
    testCase.verifyEqual(T.Type, 'filter');
    testCase.verifyEqual(T.WaveUnits, 'A');
    testCase.verifyNotEmpty(T.Wave);
    testCase.verifyEqual(numel(T.Wave), numel(T.Tran));
end

function testFamilyWithoutBandReturnsAllBands(testCase)
    T = AstroTransmission.getFilt('LSST');

    testCase.verifyClass(T, 'AstroTransmission');
    % LSST is a 6-band system: u g r i z y.
    testCase.verifyEqual(numel(T), 6);
    testCase.verifyEqual(sort({T.Band}), {'g','i','r','u','y','z'});
end

function testUnknownNamesErrorWithGuidance(testCase)
    % The error must say which level failed, and name the alternatives.
    testCase.verifyError(@() AstroTransmission.getFilt('NoSuchSurvey','g'), ...
        'AstroTransmission:getFilt:UnknownFamily');
    testCase.verifyError(@() AstroTransmission.getFilt('SDSS','q'), ...
        'AstroTransmission:getFilt:UnknownBand');

    % The message is the useful part, so check it carries the alternatives.
    % Captured by try/catch - verifyError's extra outputs are not portable
    % across releases.
    testCase.verifySubstring(errMessage(@() AstroTransmission.getFilt('NoSuchSurvey','g')), ...
        'SDSS');
    testCase.verifySubstring(errMessage(@() AstroTransmission.getFilt('SDSS','q')), ...
        'u, g, r, i, z');
end

function testAstroSpecReaderUsesGetFilt(testCase)
    % read2FilterMatrix's 'AstroTransmission' branch used to call
    % AstFilter.getFilt, which does not exist; it now resolves and returns
    % the same [Wave, Tran] matrix as the default AstFilter branch.
    [CellAT, NameAT] = AstroSpec.read2FilterMatrix('ZTF', 'r', 'AstroTransmission');
    [CellAF, NameAF] = AstroSpec.read2FilterMatrix('ZTF', 'r');

    testCase.verifyClass(CellAT, 'cell');
    testCase.verifyEqual(NameAT, NameAF);
    testCase.verifyEqual(size(CellAT{1}, 2), 2);
    testCase.verifyEqual(CellAT{1}, CellAF{1}, 'AbsTol', 1e-10);
end

function Msg = errMessage(Fun)
    % Message of the error thrown by Fun, or '' if it did not throw.
    Msg = '';
    try
        Fun();
    catch ME
        Msg = ME.message;
    end
end
