function tests = test_filterTransmission_source
    % Unit tests for the 'Source' argument of astro.transmission.filterTransmission.
    %
    % Filter curves are held in AstroPack by two classes - the maintained
    % AstroTransmission and the older AstFilter - and 'Source' selects which
    % one is read. Both are backed by the same catalogue file
    % (cats.spec.filter.AstFilterCat), so the two must return the same
    % transmission; these tests pin that equality, the default, the cache
    % behaviour when the source alternates, and the failure modes of the
    % AstFilter branch (which reports unknown names on its own).

    tests = functiontests(localfunctions);
end

%% Test Functions

function testBothSourcesReturnTheSameCurve(testCase)
    % The two stores hold the same catalogue, so the curve must not depend
    % on which one is asked - this is what makes the switch safe to flip in
    % the middle of a calibration recipe.
    Lambda = (3000:20:11000)';
    Bands  = {'SDSS','g'; 'ZTF','r'; 'LSST','i'; 'GAIA','G'};

    for Ib = 1:size(Bands, 1)
        Family = Bands{Ib, 1};
        Band   = Bands{Ib, 2};
        Tat = astro.transmission.filterTransmission(Lambda, 1, ...
                  'Family', Family, 'Band', Band, 'Source', 'AstroTransmission');
        Taf = astro.transmission.filterTransmission(Lambda, 1, ...
                  'Family', Family, 'Band', Band, 'Source', 'AstFilter');

        testCase.verifyEqual(Taf, Tat, 'AbsTol', 1e-12, ...
            sprintf('%s %s differs between the two filter stores.', Family, Band));
        testCase.verifyGreaterThan(max(Tat), 0.5, ...
            sprintf('%s %s does not transmit on this grid.', Family, Band));
    end
end

function testDefaultSourceIsAstroTransmission(testCase)
    % Omitting 'Source' must keep the pre-existing behaviour.
    Lambda = (3000:20:11000)';
    Tdef = astro.transmission.filterTransmission(Lambda, 1, ...
               'Family','SDSS', 'Band','r');
    Tat  = astro.transmission.filterTransmission(Lambda, 1, ...
               'Family','SDSS', 'Band','r', 'Source','AstroTransmission');

    testCase.verifyEqual(Tdef, Tat, ...
        'The default source is not AstroTransmission.');
end

function testSourceIsPartOfTheCacheKey(testCase)
    % The persistent cache is keyed on the source as well as on the grid and
    % the filter identity. Alternating sources within one session must keep
    % returning the requested band, not whatever was cached last.
    Lambda = (3000:20:11000)';
    T1 = astro.transmission.filterTransmission(Lambda, 1, ...
             'Family','SDSS', 'Band','g', 'Source','AstroTransmission');
    T2 = astro.transmission.filterTransmission(Lambda, 1, ...
             'Family','SDSS', 'Band','z', 'Source','AstFilter');
    T3 = astro.transmission.filterTransmission(Lambda, 1, ...
             'Family','SDSS', 'Band','g', 'Source','AstroTransmission');

    testCase.verifyEqual(T3, T1, ...
        'Alternating the source corrupted the cached curve.');
    % SDSS z is redder than SDSS g - the second call really returned z.
    testCase.verifyGreaterThan(weightedMean(Lambda, T2), weightedMean(Lambda, T1), ...
        'The second call did not return the band it asked for.');
end

function testNormalizationAppliesToBothSources(testCase)
    % 'Normalization' is applied after the read, so it must behave the same
    % whichever store supplied the curve: 'peak' brings the band to a
    % maximum of 1 on its NATIVE grid (so the resampled maximum is just
    % below 1 unless Lambda happens to sample the peak), while 'area' leaves
    % the catalogue's unit-integral scaling, max ~1e-3.
    Lambda = (3000:10:11000)';
    for Source = {'AstroTransmission','AstFilter'}
        Tpeak = astro.transmission.filterTransmission(Lambda, 1, ...
                    'Family','ZTF', 'Band','g', 'Source',Source{1}, ...
                    'Normalization','peak');
        Tarea = astro.transmission.filterTransmission(Lambda, 1, ...
                    'Family','ZTF', 'Band','g', 'Source',Source{1}, ...
                    'Normalization','area');

        testCase.verifyLessThanOrEqual(max(Tpeak), 1 + 1e-12, ...
            sprintf('Peak normalization overshot 1 for source %s.', Source{1}));
        testCase.verifyGreaterThan(max(Tpeak), 0.99, ...
            sprintf('Peak normalization failed for source %s.', Source{1}));
        testCase.verifyLessThan(max(Tarea), 0.1, ...
            sprintf('Area normalization failed for source %s.', Source{1}));
        % Only the scale differs: the two curves have the same shape.
        testCase.verifyEqual(Tarea ./ max(Tarea), Tpeak ./ max(Tpeak), ...
            'AbsTol', 1e-12, ...
            sprintf('The two normalizations differ in shape for source %s.', Source{1}));
    end
end

function testUnknownSourceErrors(testCase)
    % Only the two store names are accepted, and the spelling is exact.
    Lambda = (3000:20:11000)';
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, ...
                'Family','SDSS', 'Band','g', 'Source','NoSuchStore'), ...
        'MATLAB:validators:mustBeMember', ...
        'An unknown filter store should be rejected.');
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, ...
                'Family','SDSS', 'Band','g', 'Source','astfilter'), ...
        'MATLAB:validators:mustBeMember', ...
        'The store name is case-sensitive.');
end

function testAstFilterSourceReportsUnknownNames(testCase)
    % The AstFilter branch does its own diagnostics (the AstroTransmission
    % branch gets them from getFilt), and must name the level that failed.
    Lambda = (3000:20:11000)';
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, ...
                'Family','NoSuchSurvey', 'Band','g', 'Source','AstFilter'), ...
        'astro:transmission:filterTransmission:UnknownFamily', ...
        'An unknown family should be reported as such.');
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, ...
                'Family','SDSS', 'Band','q', 'Source','AstFilter'), ...
        'astro:transmission:filterTransmission:UnknownBand', ...
        'An unknown band should be reported as such.');
end

function testAstFilterSourceStillNeedsFamilyAndBand(testCase)
    % The no-filter guard runs before the store is consulted.
    Lambda = (3000:20:11000)';
    testCase.verifyError( ...
        @() astro.transmission.filterTransmission(Lambda, 1, 'Source','AstFilter'), ...
        'astro:transmission:filterTransmission:NoFilter', ...
        'A missing family/band should be caught before the read.');
end

%% Helper

function W = weightedMean(Lambda, T)
    % Transmission-weighted mean wavelength - a one-line proxy for "which
    % band is redder".
    W = sum(Lambda .* T) ./ sum(T);
end
