function tests = test_predefSeqCompositeFun_filterSource
    % Unit tests for the 'FilterSource' argument of
    % imUtil.calib.predefSeqCompositeFun.
    %
    % Filter curves live in two AstroPack classes - the maintained
    % AstroTransmission and the older AstFilter - and 'FilterSource' decides
    % which one the MissionFilter component reads. It travels to
    % astro.transmission.filterTransmission as the CompositeFun optional
    % argument 'Source', and only when it is not the default, so a caller
    % that does not care about the store sees the catalogue entry it saw
    % before the argument existed.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testDefaultLeavesTheEntryUnchanged(testCase)
    % The regression guard: with the default store the MissionFilter entry
    % carries family and band only.
    [FunCat, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'LSST', 'FilterBand', 'i');
    [FunCatExpl, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'LSST', 'FilterBand', 'i', ...
        'FilterSource', 'AstroTransmission');

    testCase.verifyEqual(FunCat.MissionFilter.OptionalArgs, ...
        {'Family','LSST','Band','i'}, ...
        'The default filter store must not appear in the entry.');
    testCase.verifyEqual(FunCatExpl.MissionFilter, FunCat.MissionFilter, ...
        'Passing the default store explicitly changed the entry.');
end

function testNonDefaultStoreTravelsAsOptionalArg(testCase)
    % The other store is named in the entry, under the argument name
    % filterTransmission expects.
    [FunCat, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'ZTF', 'FilterBand', 'r', ...
        'FilterSource', 'AstFilter');

    testCase.verifyEqual(FunCat.MissionFilter.OptionalArgs, ...
        {'Family','ZTF','Band','r','Source','AstFilter'}, ...
        'The filter store did not reach the MissionFilter entry.');
end

function testLASTCatalogIsUnaffected(testCase)
    % Adding the argument must not perturb anything a LAST caller sees.
    [FunPlain, StagePlain] = imUtil.calib.predefSeqCompositeFun();
    [FunSrc,   StageSrc]   = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'SDSS', 'FilterBand', 'g', ...
        'FilterSource', 'AstFilter');

    testCase.verifyEqual(FunSrc.DefaultLASTFunList, FunPlain.DefaultLASTFunList, ...
        'The default LAST function list changed.');
    testCase.verifyEqual(FunSrc.LASTFunList, FunPlain.LASTFunList, ...
        'The LAST function list changed.');
    testCase.verifyEqual(StageSrc.LAST_Joint_2Iter_AtmosFirst_Split3, ...
                         StagePlain.LAST_Joint_2Iter_AtmosFirst_Split3, ...
        'The LAST optimisation sequence changed.');
end

function testUnknownStoreErrors(testCase)
    % Only the two store names are accepted.
    testCase.verifyError( ...
        @() imUtil.calib.predefSeqCompositeFun( ...
                'FilterFamily','ZTF', 'FilterBand','r', ...
                'FilterSource','NoSuchStore'), ...
        'MATLAB:validators:mustBeMember', ...
        'An unknown filter store should be rejected.');
end

function testBothStoresEvaluateToTheSameBand(testCase)
    % End to end through CompositeFun: whichever store the entry names, the
    % evaluated MissionFilter component is the same band - the two classes
    % are backed by the same catalogue file.
    Lambda = (3000:20:11000)';

    Tat = evalMissionFilter(Lambda, 'ZTF', 'r', 'AstroTransmission');
    Taf = evalMissionFilter(Lambda, 'ZTF', 'r', 'AstFilter');

    testCase.verifyEqual(Taf, Tat, 'AbsTol', 1e-12, ...
        'The two filter stores produced different transmissions.');
    testCase.verifyGreaterThan(max(Tat), 0.5, ...
        'The band does not transmit on this grid.');
end

%% Helper

function T = evalMissionFilter(Lambda, Family, Band, Source)
    % Evaluate the MissionFilter catalogue entry the way CompositeFun does:
    % handle plus the entry's own OptionalArgs.
    [FunCat, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', Family, 'FilterBand', Band, 'FilterSource', Source);
    Entry  = FunCat.MissionFilter;
    Handle = str2func(Entry.Handle);
    T = Handle(Lambda, Entry.Params(1), Entry.OptionalArgs{:});
end
