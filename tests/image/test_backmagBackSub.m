function tests = test_backmagBackSub
    % Tests for imProc.calib.backmag on background-subtracted images
    % (issue #1473): BACKMAG is NaN (a blank header card) when the image is
    % background-subtracted or its background level is not positive.
    tests = functiontests(localfunctions);
end

function AI = makeImage(MedBck)
    AI = AstroImage({zeros(10,10)});
    AI.HeaderData.replaceVal('MEDBCK',MedBck);
    AI.HeaderData.replaceVal('PT_ZP',25);
    AI.HeaderData.replaceVal('PIXSCALE',1.25);
end

function testSkyLevelGivesSurfaceBrightness(testCase)
    AI = imProc.calib.backmag(makeImage(120), 'KeyZP','PT_ZP');
    Expected = 25 - 2.5.*log10(120./1.25.^2);
    verifyEqual(testCase, AI.HeaderData.getVal('BACKMAG'), Expected, 'AbsTol',1e-10);
end

function testNegativeBackgroundGivesNaN(testCase)
    [AI, Res] = imProc.calib.backmag(makeImage(-0.86), 'KeyZP','PT_ZP');
    verifyTrue(testCase, isreal(Res.MagBack));
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('BACKMAG')));
end

function testZeroBackgroundGivesNaN(testCase)
    AI = imProc.calib.backmag(makeImage(0), 'KeyZP','PT_ZP');
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('BACKMAG')));
end

function testBackSubGivesNaN(testCase)
    % a positive residual of a subtracted background is not the sky either
    AI = imProc.calib.backmag(makeImage(0.1), 'KeyZP','PT_ZP', 'IsBackSub',true);
    verifyTrue(testCase, isnan(AI.HeaderData.getVal('BACKMAG')));
end

function testNaNIsWrittenAsBlankCard(testCase)
    AI = imProc.calib.backmag(makeImage(-1), 'KeyZP','PT_ZP');
    File = [tempname '.fits'];
    Cleanup = onCleanup(@() delete(File)); %#ok<NASGU>
    AI.write1(File, 'Image', 'FileType','fits');
    H = AstroHeader(File);
    verifyTrue(testCase, isnan(H.getVal('BACKMAG')));
end
