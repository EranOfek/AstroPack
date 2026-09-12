function tests = test_estimateFnFrZP
    % estimateFnFr must honour the NewZP/RefZP keyword choice, and the
    % pipelines must expose it (issue #1267).
    %   estimateFnFr does not fit anything: it reads one zero point from
    %   each header and divides them, so both images have to carry a zero
    %   point on the SAME absolute scale. The header values used here are
    %   real ones from a v1 coadd and the v4 reference covering it, where
    %   the two keyword choices differ by 0.17 mag - the flux-matching
    %   error measured on common stars before the references were rebuilt.
    tests = functiontests(localfunctions);
end

function setup(testCase)
    New = AstroImage({rand(32)});
    Ref = AstroImage({rand(32)});
    New.HeaderData.replaceVal({'PH_ZP','PT_ZP'}, [24.40, 24.25]);
    Ref.HeaderData.replaceVal({'PH_ZP','PT_ZP'}, [25.89, 25.57]);
    testCase.TestData.AD = AstroZOGY(New, Ref);
end

function testDefaultIsPH_ZP(testCase)
    [~, Fn, Fr] = testCase.TestData.AD.estimateFnFr;
    verifyEqual(testCase, Fn, 1);
    verifyEqual(testCase, 2.5*log10(Fr), 25.89-24.40, 'AbsTol',1e-6);
end

function testKeywordOverride(testCase)
    [~, ~, Fr] = testCase.TestData.AD.estimateFnFr('NewZP','PT_ZP', 'RefZP','PT_ZP');
    verifyEqual(testCase, 2.5*log10(Fr), 25.57-24.25, 'AbsTol',1e-6);
end

function testChoiceMatters(testCase)
    % the two conventions must not silently coincide
    [~, ~, Fr1] = testCase.TestData.AD.estimateFnFr;
    [~, ~, Fr2] = testCase.TestData.AD.estimateFnFr('NewZP','PT_ZP', 'RefZP','PT_ZP');
    verifyGreaterThan(testCase, abs(2.5*log10(Fr1/Fr2)), 0.1);
end

function testPipelinesExposeIt(testCase)
    % both transient pipelines must accept the keyword names, defaulting to
    % PH_ZP so existing runs are unchanged
    for F = {'pipeline.last.pipes.pipelineII', 'pipeline.last.transients.runTransientsPipe'}
        Src = fileread(which(F{1}));
        verifyTrue(testCase, contains(Src, "Args.NewZP = 'PH_ZP'"), ...
            sprintf('%s does not default NewZP to PH_ZP', F{1}));
        verifyTrue(testCase, contains(Src, "estimateFnFr('NewZP',Args.NewZP"), ...
            sprintf('%s does not forward NewZP to estimateFnFr', F{1}));
    end
end
