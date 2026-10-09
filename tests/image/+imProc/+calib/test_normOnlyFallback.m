function tests = test_normOnlyFallback
    % Unit tests for the Norm-only fallback and the keyword contract around it
    % (issue #1381).
    %
    % A full end-to-end calibration needs a real coadd plus catsHTM
    % calibrators, so the fallback cannot be driven from here. These tests
    % cover the parts that are reachable without data: the FitMode property
    % that records which model was fitted, the NormEstimator option that makes
    % the fallback's zero point an unweighted median, and blankCalibKeys,
    % which guarantees that a product whose calibration did not run carries
    % the same keyword set as one that did.
    % @TODO - drive the fallback end to end once a data fixture exists.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testFitModeDefaultIsFull(testCase)
    % A fresh PhotCalibTrans reports the full model until told otherwise.

    PC = PhotCalibTrans;
    testCase.verifyEqual(PC.FitMode, 'full', ...
        'Default PhotCalibTrans.FitMode should be ''full''.');
end

function testFitModeRecordsFallback(testCase)
    % FitMode is what the header writer keys the positional fit flags on, so it
    % must be settable to the reduced mode.

    PC = PhotCalibTrans;
    PC.FitMode = 'norm';
    testCase.verifyEqual(PC.FitMode, 'norm', ...
        'PhotCalibTrans.FitMode should record the Norm-only fallback.');
end

function testNormEstimatorRejectsInvalidValue(testCase)
    % fitPar validates NormEstimator through mustBeMember: only the
    % least-squares 'wmean' and the robust 'median' are accepted.

    CF = tools.math.fun.CompositeFun;
    testCase.verifyError(@() CF.fitPar([], [], 'NormEstimator', 'nonsense'), ...
        ?MException, 'An out-of-set NormEstimator should be rejected.');
end

function testBlankCalibKeysBlanksFitProducts(testCase)
    % Every quantity a successful fit would have produced is present and blank,
    % so a failed product is never missing a card that a calibrated one has.

    H = PhotCalibTrans.blankCalibKeys(AstroHeader(), {'MAG_PSF'});
    Blank = {'PT_ZP','PT_RMS','PT_ARMS','PT_CHI2','PT_DOF','PT_CTA','PT_DZPAB', ...
             'PT_P_V1','PT_P_F1','PT_P_NX1','APCOR_N','APCC_REF'};
    for I = 1:numel(Blank)
        testCase.verifyTrue(H.isKeyExist(Blank{I}), ...
            sprintf('%s should be present even when the fit did not run.', Blank{I}));
        testCase.verifyTrue(isnan(H.getVal(Blank{I})), ...
            sprintf('%s should be blank (NaN), not a value.', Blank{I}));
    end
end

function testBlankCalibKeysCoversEachApertureColumn(testCase)
    % The aperture-correction keys are emitted per magnitude column, so the set
    % matches what a successful fit would have written for the same catalog.

    H = PhotCalibTrans.blankCalibKeys(AstroHeader(), {'MAG_APER_1','MAG_PSF'});
    Expected = {'APC0_A1','APCX_A1','APCY_A1','APCXY_A1','APCC_A1','APCCE_A1', ...
                'APC0_PS','APCX_PS','APCY_PS','APCXY_PS','APCC_PS','APCCE_PS'};
    for I = 1:numel(Expected)
        testCase.verifyTrue(H.isKeyExist(Expected{I}), ...
            sprintf('%s should be written for its magnitude column.', Expected{I}));
    end
end

function testBlankCalibKeysKeepsConfigurationValues(testCase)
    % Configuration constants are known whatever the fit did, so they carry
    % real values rather than blanks.

    Const = struct('PT_REFSL', 1.642, 'PT_REFPV', 5500, 'PT_REFC', 1);
    H = PhotCalibTrans.blankCalibKeys(AstroHeader(), {}, 'Const', Const);
    testCase.verifyEqual(H.getVal('PT_REFSL'), 1.642, ...
        'PT_REFSL is configuration, not a fit product.');
    testCase.verifyEqual(H.getVal('PT_REFPV'), 5500, ...
        'PT_REFPV is configuration, not a fit product.');
    testCase.verifyEqual(H.getVal('PT_REFC'), 1, ...
        'PT_REFC is configuration, not a fit product.');
end
