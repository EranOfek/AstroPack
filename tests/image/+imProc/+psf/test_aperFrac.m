function tests = test_aperFrac
    % Issue #1269: imProc.psf.aperFrac must not duplicate the PSF_AF_*
    % keywords when it runs on a header that already carries them (a coadd
    % inherits them from the single epoch it was built from, and pipelineI
    % calls aperFrac on both).
    tests = functiontests(localfunctions);
end

function setup(testCase)
    [X,Y] = meshgrid(-12:12);
    P = exp(-(X.^2+Y.^2)/8);  P = P./sum(P(:));
    AI = AstroImage({rand(64)});
    AI.PSFData.Data = P;
    testCase.TestData.AI = AI;
end

function testNoDuplicateOnSecondCall(testCase)
    AI = testCase.TestData.AI;
    [~,AI] = imProc.psf.aperFrac(AI);
    V1 = AI.HeaderData.getVal('PSF_AF_3');
    [~,AI] = imProc.psf.aperFrac(AI);     % the coadd path: keys already there
    Keys = AI.HeaderData.Data(:,1);
    for K = 1:4
        verifyEqual(testCase, nnz(strcmp(Keys, sprintf('PSF_AF_%d',K))), 1, ...
            sprintf('PSF_AF_%d appears more than once', K));
    end
    verifyEqual(testCase, AI.HeaderData.getVal('PSF_AF_3'), V1);
end

function testFractionsSane(testCase)
    [~,AI] = imProc.psf.aperFrac(testCase.TestData.AI);
    F = arrayfun(@(k) AI.HeaderData.getVal(sprintf('PSF_AF_%d',k)), 1:4);
    verifyTrue(testCase, all(diff(F)>=0), 'aperture fractions not increasing with radius');
    verifyTrue(testCase, all(F>0 & F<=1.0001), 'aperture fractions out of range');
end
