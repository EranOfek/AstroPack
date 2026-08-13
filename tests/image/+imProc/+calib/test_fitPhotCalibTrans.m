function tests = test_fitPhotCalibTrans
    % Unit tests for the imProc.calib.fitPhotCalibTrans MagType option and the
    % underlying PhotCalibTrans flux->magnitude conversion (luptitude vs
    % magnitude).
    %
    % A full end-to-end fitPhotCalibTrans run needs a real coadd + catsHTM
    % calibrators, so it is not unit-testable here. These tests cover the
    % MagType mechanism fitPhotCalibTrans exposes: the PhotCalibTrans.MagType
    % property (default + validation), the static fluxToMag dispatcher, and the
    % fitPhotCalibTrans 'MagType' argument validation.
    % @TODO - add an end-to-end calibration test once a data fixture exists.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testMagTypeDefaultIsLup(testCase)
    % A fresh PhotCalibTrans defaults MagType to 'lup' (luptitude).

    PC = PhotCalibTrans;
    testCase.verifyEqual(PC.MagType, 'lup', ...
        'Default PhotCalibTrans.MagType should be ''lup''.');
end

function testMagTypeRejectsInvalidValue(testCase)
    % Assigning an out-of-set MagType is rejected by mustBeMember.

    PC = PhotCalibTrans;
    testCase.verifyError(@() setMagType(PC, 'xxx'), ?MException, ...
        'Invalid PhotCalibTrans.MagType should be rejected.');
end

function testFluxToMagMagNaNForNonPositive(testCase)
    % 'mag' returns NaN for non-positive flux and a finite value otherwise.

    ZP = 25;
    testCase.verifyTrue(isnan(PhotCalibTrans.fluxToMag('mag', -5, ZP)), ...
        'mag: negative flux must give NaN.');
    testCase.verifyTrue(isnan(PhotCalibTrans.fluxToMag('mag', 0, ZP)), ...
        'mag: zero flux must give NaN.');
    testCase.verifyTrue(isfinite(PhotCalibTrans.fluxToMag('mag', 100, ZP)), ...
        'mag: positive flux must be finite.');
end

function testFluxToMagLupFiniteForNegative(testCase)
    % 'lup' (luptitude) stays finite for negative flux (the whole point of it).

    testCase.verifyTrue(isfinite(PhotCalibTrans.fluxToMag('lup', -5, 25)), ...
        'lup: negative flux should stay finite.');
end

function testFluxToMagValue(testCase)
    % Standard-magnitude value: mag = -2.5*log10(Flux / 10^(0.4*ZP)).

    Mag = PhotCalibTrans.fluxToMag('mag', 100, 25);
    testCase.verifyEqual(Mag, 20, 'AbsTol', 1e-9, ...
        'mag value for Flux=100, ZP=25 should be 20.');
end

function testFluxToMagMatchesConvert(testCase)
    % fluxToMag dispatches to convert.magnitude / convert.luptitude using
    % Flux0 = 10^(0.4*ZP), so results match those functions exactly.

    Flux  = [100; 1e4; 1e6];
    ZP    = 22.5;
    Flux0 = 10.^(0.4 .* ZP);
    testCase.verifyEqual(PhotCalibTrans.fluxToMag('mag', Flux, ZP), ...
        convert.magnitude(Flux, Flux0), 'AbsTol', 1e-12, ...
        'mag path must match convert.magnitude.');
    testCase.verifyEqual(PhotCalibTrans.fluxToMag('lup', Flux, ZP), ...
        convert.luptitude(Flux, Flux0), 'AbsTol', 1e-12, ...
        'lup path must match convert.luptitude.');
end

function testFluxToMagVectorNaNMask(testCase)
    % In 'mag' mode only the non-positive entries become NaN (per-element).

    Flux = [100; -5; 0; 1e6];
    Mag  = PhotCalibTrans.fluxToMag('mag', Flux, 25);
    testCase.verifyEqual(isnan(Mag), [false; true; true; false], ...
        'mag: NaN mask must flag exactly the non-positive fluxes.');
end

function testFitPhotCalibTransRejectsInvalidMagType(testCase)
    % fitPhotCalibTrans validates its 'MagType' argument (arguments-block
    % mustBeMember fires before the body runs, so no image data is needed).

    testCase.verifyError( ...
        @() imProc.calib.fitPhotCalibTrans(AstroImage, 'MagType', 'xxx'), ...
        ?MException, ...
        'fitPhotCalibTrans should reject an invalid MagType.');
end

%% Helpers

function setMagType(PC, Val)
    % Assign MagType so the property set-validation can be exercised in a
    % function handle (verifyError needs a callable).
    PC.MagType = Val;
end
