function tests = test_filterTransmission
    % Unit tests for astro.transmission.filterTransmission.
    %
    % Checks that a named survey band (SDSS / ZTF / LSST) is read from the
    % AstroPack filter catalogue, resampled onto an arbitrary wavelength
    % grid, zeroed outside its support, cached per filter, and that the
    % failure modes report what is wrong.

    tests = functiontests(localfunctions);
end

function testReturnsBandOnRequestedGrid(testCase)
    % The result follows Lambda in size and orientation, and is a bounded
    % normalized transmission.
    Lambda = (3000:20:11000)';
    T = astro.transmission.filterTransmission(Lambda, 1, 'Family','SDSS', 'Band','g');

    testCase.verifySize(T, size(Lambda));
    testCase.verifyTrue(all(isfinite(T)));
    testCase.verifyGreaterThanOrEqual(min(T), 0);
    testCase.verifyLessThanOrEqual(max(T), 1 + 1e-10);
    % A real band must actually transmit something on this grid.
    testCase.verifyGreaterThan(max(T), 0.5);
end

function testPeaksAtTheRightWavelength(testCase)
    % The transmission peak lands inside the nominal band, which is the
    % property the calibration depends on. SDSS g is ~4000-5500 Ang.
    Lambda = (3000:10:11000)';
    T = astro.transmission.filterTransmission(Lambda, 1, 'Family','SDSS', 'Band','g');

    [~, Imax] = max(T);
    testCase.verifyGreaterThan(Lambda(Imax), 4000);
    testCase.verifyLessThan(Lambda(Imax), 5500);
end

function testZeroOutsideTheBand(testCase)
    % Outside the tabulated support the transmission is zero, not an
    % extrapolation of the nearest edge.
    Lambda = (3000:20:11000)';
    T = astro.transmission.filterTransmission(Lambda, 1, 'Family','SDSS', 'Band','u');

    % Far red of SDSS u (~3000-4000 Ang): must be exactly zero.
    testCase.verifyEqual(T(Lambda > 8000), zeros(sum(Lambda > 8000), 1));
end

function testDistinctBandsDiffer(testCase)
    % The persistent cache is keyed on the filter identity as well as the
    % grid, so alternating bands within one session must not return the
    % previous band's curve.
    Lambda = (3000:20:11000)';
    Tg = astro.transmission.filterTransmission(Lambda, 1, 'Family','ZTF', 'Band','g');
    Tr = astro.transmission.filterTransmission(Lambda, 1, 'Family','ZTF', 'Band','r');
    Tg2 = astro.transmission.filterTransmission(Lambda, 1, 'Family','ZTF', 'Band','g');

    testCase.verifyNotEqual(Tg, Tr);
    testCase.verifyEqual(Tg2, Tg);          % cache returns the same curve
    % ZTF r is redder than ZTF g.
    testCase.verifyGreaterThan(weightedMean(Lambda, Tr), weightedMean(Lambda, Tg));
end

function testFamilyNameIsCaseInsensitive(testCase)
    Lambda = (3000:20:11000)';
    T1 = astro.transmission.filterTransmission(Lambda, 1, 'Family','LSST', 'Band','y');
    T2 = astro.transmission.filterTransmission(Lambda, 1, 'Family','lsst', 'Band','y');
    testCase.verifyEqual(T2, T1);
end

function testDummyParameterIsIgnored(testCase)
    % The second positional argument exists only for CompositeFun
    % compatibility; it must not change the curve.
    Lambda = (3000:20:11000)';
    T1 = astro.transmission.filterTransmission(Lambda, 1,  'Family','SDSS', 'Band','r');
    T2 = astro.transmission.filterTransmission(Lambda, 42, 'Family','SDSS', 'Band','r');
    testCase.verifyEqual(T2, T1);
end

function testMissingFilterErrors(testCase)
    Lambda = (3000:20:11000)';
    testCase.verifyError(@() astro.transmission.filterTransmission(Lambda), ...
        'astro:transmission:filterTransmission:NoFilter');
    testCase.verifyError(@() astro.transmission.filterTransmission(Lambda, 1, 'Family','SDSS'), ...
        'astro:transmission:filterTransmission:NoFilter');
end

function testUnknownNamesError(testCase)
    Lambda = (3000:20:11000)';
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, 'Family','NoSuchSurvey', 'Band','g'), ...
        'AstroTransmission:getFilt:UnknownFamily');
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, 'Family','SDSS', 'Band','q'), ...
        'AstroTransmission:getFilt:UnknownBand');
end

function W = weightedMean(Lambda, T)
    % Transmission-weighted mean wavelength - a one-line proxy for "which
    % band is redder".
    W = sum(Lambda .* T) ./ sum(T);
end
