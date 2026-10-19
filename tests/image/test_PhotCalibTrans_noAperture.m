function tests = test_PhotCalibTrans_noAperture
    % Unit tests for issue #1431: PhotCalibTrans carries no geometric
    % collecting area. The fit only ever constrains Aperture x Norm, so Norm
    % is now the effective collecting area [m^2]; the calibrator spectral
    % error is the relative error of the synthetic flux, a property of the
    % Gaia spectrum and the band alone; and Norm values restored from older
    % headers (no PT_NORMU) are converted from LAST's area.
    % Author : D. Kovaleva (Oct 2026)
    % Example: runtests('tests/image/test_PhotCalibTrans_noAperture.m')
    tests = functiontests(localfunctions);
end

%% Test Functions

function testNoCollectingArea(TestCase)
    verifyFalse(TestCase, isprop(PhotCalibTrans, 'Aperture'), ...
        'PhotCalibTrans still has an Aperture property.');
    verifyFalse(TestCase, isfield(PhotCalibTrans.missionDefaults('ZTF'), 'Aperture'), ...
        'The mission table still carries a collecting area.');
end

function testLegacyAreaIsLASTs(TestCase)
    verifyEqual(TestCase, PhotCalibTrans.LegacyNormArea, pi*0.1397^2, 'RelTol', 1e-12);
end

function testSpectralErrorIgnoresObservedFluxAndExposure(TestCase)
    % The old term divided a predicted-count error by the OBSERVED counts
    % and scaled with area and exposure time; the new one must not.
    PC = localSpecPC();
    E1 = spectralErr(PC, [1e4; 2e4; 5e4]);
    PC.ExpTime = 300;
    E2 = spectralErr(PC, [7e6; 1e3; 42]);
    verifyEqual(TestCase, E2, E1, 'RelTol', 1e-12);
end

function testSpectralErrorScalesWithTheGaiaError(TestCase)
    % Doubling the spectrum errors doubles sig_F/F (the magnitude error is
    % 2.5 log10(1+x), so compare x).
    PC = localSpecPC();
    X1 = 10.^(spectralErr(PC, ones(3,1)) / 2.5) - 1;
    PC.SpecData.SpecErr = 2 * PC.SpecData.SpecErr;
    X2 = 10.^(spectralErr(PC, ones(3,1)) / 2.5) - 1;
    verifyEqual(TestCase, X2, 2 * X1, 'RelTol', 1e-10);
end

function testSpectralErrorIsScaleFree(TestCase)
    % Scaling a spectrum together with its errors, or the band curve, must
    % not change a RELATIVE error.
    PC = localSpecPC();
    E1 = spectralErr(PC, ones(3,1));
    PC.SpecData.Spec    = 1e3 * PC.SpecData.Spec;
    PC.SpecData.SpecErr = 1e3 * PC.SpecData.SpecErr;
    E2 = spectralErr(PC, ones(3,1));
    verifyEqual(TestCase, E2, E1, 'RelTol', 1e-10);
    E3 = PC.propagateCalibratorMagErr(ones(3,1), [], 'WeightingMode', 'spectral', ...
        'SystematicErr', 0, 'RefTransmissionFun', @(L) 7 * localBand(L));
    verifyEqual(TestCase, E3, E1, 'RelTol', 1e-10);
end

function testLegacyHeaderNormIsConverted(TestCase)
    % A header without PT_NORMU predates #1431: Norm x LAST's area.
    PC = PhotCalibTrans().photCalibTransFromHeader(localHeader(0.3, false));
    verifyEqual(TestCase, localNorm(PC), 0.3 * pi*0.1397^2, 'RelTol', 1e-12);
end

function testNewHeaderNormIsKept(TestCase)
    PC = PhotCalibTrans().photCalibTransFromHeader(localHeader(0.0172, true));
    verifyEqual(TestCase, localNorm(PC), 0.0172, 'RelTol', 1e-12);
end

function testConvertLegacyNormWithoutNormalization(TestCase)
    % A model with no Normalization function is returned unchanged.
    PC = PhotCalibTrans().photCalibTransFromHeader(localHeader(0.3, false, 'astro.transmission.rayleighTransmission'));
    [~, Converted] = PhotCalibTrans.convertLegacyNorm(PC.TransModel);
    verifyFalse(TestCase, Converted);
end

%% Helpers

function E = spectralErr(PC, Flux)
    E = PC.propagateCalibratorMagErr(Flux, [], 'WeightingMode', 'spectral', ...
        'SystematicErr', 0, 'RefTransmissionFun', @localBand);
end

function T = localBand(L)
    % A smooth band on the model grid, shape only.
    T = exp(-0.5 * ((L(:) - 6200) / 900).^2);
end

function PC = localSpecPC()
    % Three synthetic calibrators: power-law spectra with wavelength-
    % dependent relative errors of 1-4 %.
    PC  = PhotCalibTrans();
    Wvl = (3360:20:10200)';
    A   = [-2; 0; 1.5];
    Spec    = 1e-17 * (Wvl.' / 5000) .^ A;                      % [3 x Nwvl]
    RelErr  = 0.01 + 0.03 * ((Wvl.' - 3360) / (10200 - 3360)) .* [1; 0.5; 0.2];
    PC.SpecData = struct('SpecWvl', Wvl, 'Spec', Spec, 'SpecErr', RelErr .* Spec);
end

function H = localHeader(NormVal, WithUnit, Handle)
    if nargin < 3
        Handle = 'astro.transmission.normalization';
    end
    H = AstroHeader;
    Keys = {'PT_1_N', Handle, ''; 'PT_1_V1', NormVal, ''; 'PT_1_F1', 1, ''};
    if WithUnit
        Keys(end+1,:) = {'PT_NORMU', 'm2', ''};
    end
    H.insertKey(Keys);
end

function N = localNorm(PC)
    N = PC.TransModel.Funs(1).Par(1);
end
