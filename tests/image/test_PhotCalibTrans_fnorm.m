function tests = test_PhotCalibTrans_fnorm
    % Unit tests for the PT_FNORM header keyword (issue #1431): the fitted
    % Norm is an effective collecting area [m^2]; photCalibTransToHeader
    % writes PT_FNORM = Norm / CollectingArea, a dimensionless throughput.
    % The area is a parameter of the header writer only (default LAST's
    % pi*0.1397^2) and the keyword is always present, blank when undefined.
    % Author : D. Kovaleva (Oct 2026)
    % Example: runtests('tests/image/test_PhotCalibTrans_fnorm.m')
    tests = functiontests(localfunctions);
end

%% Test Functions

function testDefaultAreaIsLASTs(TestCase)
    verifyEqual(TestCase, PhotCalibTrans.CollectingAreaLAST, pi*0.1397^2, 'RelTol', 1e-12);
end

function testFNormWithTheDefaultArea(TestCase)
    PC = localPC(0.0276);
    H  = PC.photCalibTransToHeader(AstroHeader);
    verifyEqual(TestCase, H.getVal('PT_FNORM'), 0.0276 / (pi*0.1397^2), 'RelTol', 1e-12);
end

function testFNormWithAnotherArea(TestCase)
    % A Mission run passes its own telescope's area, e.g. the ZTF P48 pupil.
    PC = localPC(0.081);
    H  = PC.photCalibTransToHeader(AstroHeader, 'CollectingArea', pi*0.6096^2);
    verifyEqual(TestCase, H.getVal('PT_FNORM'), 0.081 / (pi*0.6096^2), 'RelTol', 1e-12);
end

function testNormFromModel(TestCase)
    verifyEqual(TestCase, PhotCalibTrans.normFromModel(localPC(0.0276).TransModel), 0.0276, 'RelTol', 1e-12);
    verifyTrue(TestCase, isnan(PhotCalibTrans.normFromModel([])));
end

function testFailureStampCarriesPTFNORM(TestCase)
    % A product whose calibration failed carries the key, blank.
    H = PhotCalibTrans.blankCalibKeys(AstroHeader, {});
    verifyTrue(TestCase, H.isKeyExist('PT_FNORM'));
    verifyTrue(TestCase, isnan(H.getVal('PT_FNORM')));
end

function testNonPositiveAreaIsRefused(TestCase)
    PC = localPC(0.0276);
    verifyError(TestCase, @() PC.photCalibTransToHeader(AstroHeader, 'CollectingArea', 0), ...
        'MATLAB:validators:mustBePositive');
end

%% Helpers

function PC = localPC(NormVal)
    % A PhotCalibTrans whose model holds only a Normalization function with
    % the given Norm [m^2] (header marked PT_NORMU so it is not converted).
    H = AstroHeader;
    H.insertKey({'PT_1_N', 'astro.transmission.normalization', ''; ...
                 'PT_1_V1', NormVal, ''; 'PT_1_F1', 1, ''; 'PT_NORMU', 'm2', ''});
    PC = PhotCalibTrans().photCalibTransFromHeader(H);
end
