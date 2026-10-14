function tests = test_predefSeqCompositeFun_mission
    % Unit tests for the Mission (non-LAST filter) entries of
    % imUtil.calib.predefSeqCompositeFun.
    %
    % MissionFunList swaps the two LAST-specific components
    % (LASTTransmissionFixed and the skewed-Gaussian QE) for one tabulated
    % band chosen by argument, and the Mission_* optimisation sequences are
    % the LAST ones minus the QE-centre sub-stage. The LAST catalogue must
    % be bit-identical whether or not the new arguments are passed.

    tests = functiontests(localfunctions);
end

function testLASTCatalogIsUnaffected(testCase)
    % The regression guard that matters: adding the Mission arguments must
    % not perturb anything a LAST caller sees.
    [FunPlain, StagePlain] = imUtil.calib.predefSeqCompositeFun();
    [FunMiss,  StageMiss]  = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'SDSS', 'FilterBand', 'g');

    testCase.verifyEqual(FunMiss.DefaultLASTFunList, FunPlain.DefaultLASTFunList);
    testCase.verifyEqual(FunMiss.LASTFunList,        FunPlain.LASTFunList);
    testCase.verifyEqual(StageMiss.LAST_Joint_2Iter_AtmosFirst_Split3, ...
                         StagePlain.LAST_Joint_2Iter_AtmosFirst_Split3);
end

function testMissionFunListComposition(testCase)
    [FunCat, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'ZTF', 'FilterBand', 'r');
    FunList = FunCat.MissionFunList;

    Names = {FunList.Name};
    testCase.verifyEqual(numel(FunList), 7);
    testCase.verifyEqual(sort(Names), ...
        sort({'Normalization','Rayleigh','Aerosol','Ozone','Water','UMG','MissionFilter'}));

    % No LAST-specific component and, in particular, no skewness parameter.
    testCase.verifyFalse(any(strcmp(Names, 'QE_SkewedGaussian')));
    testCase.verifyFalse(any(strcmp(Names, 'LASTTransmissionFixed')));
    testCase.verifyFalse(any(contains(lower({FunList.Handle}), 'last')));

    AllParams = [FunList.ParamInfo];
    testCase.verifyFalse(any(strcmp({AllParams.Name}, 'Gamma')));
    testCase.verifyFalse(any(strcmp({AllParams.Name}, 'Center_Ang')));
end

function testFilterTravelsAsOptionalArgs(testCase)
    % Family/band are handle arguments, not fitted parameters - that is what
    % makes one list serve every mission.
    [FunCat, ~] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'LSST', 'FilterBand', 'i');
    Filt = FunCat.MissionFilter;

    testCase.verifyEqual(Filt.OptionalArgs, {'Family','LSST','Band','i'});
    testCase.verifyEqual(Filt.FitPar, false);
end

function testMissionSequencesHaveNoQEStage(testCase)
    [~, StageCat] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'SDSS', 'FilterBand', 'r');

    SeqNames = {'Mission_Joint_2Iter_AtmosFirst_Split2', ...
                'Mission_Joint_1Iter_AtmosFirst_Split2', ...
                'Mission_Joint_2Iter'};
    for I = 1:numel(SeqNames)
        testCase.verifyTrue(isfield(StageCat, SeqNames{I}), SeqNames{I});
        Stages = StageCat.(SeqNames{I}).Stages;
        for J = 1:numel(Stages)
            FP = Stages(J).FreeParams;
            if isstruct(FP)
                testCase.verifyFalse(any(strcmp({FP.Function}, 'QE_SkewedGaussian')), ...
                    sprintf('%s stage %d still fits the LAST QE', SeqNames{I}, J));
            end
        end
    end

    % The default Mission recipe mirrors the LAST default: same outer-loop
    % policy, one sub-stage fewer.
    Def = StageCat.Mission_Joint_2Iter_AtmosFirst_Split2;
    testCase.verifyEqual(Def.NumRepeats, 2);
    testCase.verifyEqual(numel(Def.Stages), ...
        numel(StageCat.LAST_Joint_2Iter_AtmosFirst_Split3.Stages) - 1);
end

function testModelBuildsAndEvaluatesTheBand(testCase)
    % End to end through CompositeFun: the OptionalArgs must reach the
    % handle, so the composite transmission vanishes well outside the band.
    [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun( ...
        'FilterFamily', 'SDSS', 'FilterBand', 'g');

    Lambda = (3000:20:11000)';
    Model = tools.math.fun.CompositeFun.model(FunCat.MissionFunList, ...
        'OptimizationSequence', StageCat.Mission_Joint_2Iter_AtmosFirst_Split2, ...
        'UseTran2D', false);

    T = Model.evaluateAllFunParInput(Lambda);
    testCase.verifySize(T, size(Lambda));
    testCase.verifyGreaterThan(max(T), 0);
    % Red of SDSS g the band is closed, so the product is zero.
    testCase.verifyEqual(max(T(Lambda > 7000)), 0, 'AbsTol', 1e-12);
end
