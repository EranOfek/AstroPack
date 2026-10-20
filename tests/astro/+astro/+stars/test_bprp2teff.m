function tests = test_bprp2teff
    % Unit tests for astro.stars.bprp2teff.
    %
    % Table nodes, dereddening and its dust-layer scaling, the dwarf/giant
    % test, range flags, and consistency with astro.stars.tlogg2picklesClass.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testSun(testCase)
    % G2V color gives the solar Teff and log(g).

    [Teff, Logg] = astro.stars.bprp2teff(0.823);
    testCase.verifyEqual(Teff, 5770, 'AbsTol', 1, 'G2V color must give 5770 K.');
    % EEM G2V: M=1.00, R=1.012 -> log(g)=4.428
    testCase.verifyEqual(Logg, 4.428, 'AbsTol', 0.001, 'G2V must give the EEM table log(g).');
end

function testMonotonic(testCase)
    % Teff decreases monotonically with color over the whole table.

    Teff = astro.stars.bprp2teff(linspace(-0.7, 5.1, 500));
    testCase.verifyTrue(all(diff(Teff) < 0), 'Teff must decrease with BP-RP.');
end

function testDereddening(testCase)
    % The full Ebv is removed when no parallax is given.

    Ebv  = 0.3;
    Teff = astro.stars.bprp2teff(0.823 + 1.339*Ebv, 'Ebv', Ebv, 'GalLat', 30);
    testCase.verifyEqual(Teff, 5770, 'AbsTol', 1, 'Reddened G2V must deredden to 5770 K.');
end

function testDustLayer(testCase)
    % A 10 pc star at the pole gets almost no reddening; a 10 kpc star gets all.

    [~, ~, Res] = astro.stars.bprp2teff([1 1], 'G', [5 15], 'Plx', [100 0.1], ...
                                        'PlxErr', [0.01 0.01], 'Ebv', 0.1, 'GalLat', 90);
    testCase.verifyLessThan(Res.Ebv(1), 0.01, 'A nearby star must get a small fraction of Ebv.');
    testCase.verifyEqual(Res.Ebv(2), 0.1, 'AbsTol', 1e-6, 'A distant star must get the full Ebv.');
end

function testGiant(testCase)
    % A K star with M_G=0 is a giant and maps to a Pickles III template.

    [Teff, Logg, Res] = astro.stars.bprp2teff(1.2, 'G', 10, 'Plx', 1, 'PlxErr', 0.01);
    testCase.verifyTrue(Res.IsGiant, 'M_G=0 at BP-RP=1.2 must be a giant.');
    testCase.verifyEqual(Logg, 3.5, 'A giant must get GiantLogg.');
    R = astro.stars.tlogg2picklesClass(Teff, Logg);
    testCase.verifyEqual(R.lumclass, 'iii', 'A giant must map to a III template.');
end

function testNoParallaxIsDwarf(testCase)
    % Without a reliable parallax a star is a dwarf.

    [~, ~, Res] = astro.stars.bprp2teff(1.2, 'G', 10, 'Plx', 1, 'PlxErr', 0.5);
    testCase.verifyFalse(Res.PlxOK, 'Plx/PlxErr=2 must not be used.');
    testCase.verifyFalse(Res.IsGiant, 'A star without a usable parallax must be a dwarf.');
end

function testDwarfsMapToDwarfTemplates(testCase)
    % Every dwarf of the sequence, O to M, maps to a Pickles V template.

    [Teff, Logg] = astro.stars.bprp2teff(linspace(-0.7, 5.1, 100));
    R = arrayfun(@(T, L) astro.stars.tlogg2picklesClass(T, L), Teff, Logg);
    testCase.verifyTrue(all(strcmp({R.lumclass}, 'v')), 'All dwarfs must map to V templates.');
end

function testFlagsAndNaN(testCase)
    % Range flags and NaN propagation.

    [Teff, Logg, Res] = astro.stars.bprp2teff([-1 -0.3 1 6 NaN]);
    testCase.verifyEqual(Res.OutOfRange', [true false false true false], 'Out-of-range flags.');
    testCase.verifyEqual(Res.HotExtension', [true true false false false], 'Hot-extension flags.');
    testCase.verifyEqual(Teff([1 4]), [41400; 2420], 'AbsTol', 1, 'Out-of-range Teff is clipped.');
    testCase.verifyTrue(isnan(Teff(5)) && isnan(Logg(5)), 'NaN color must give NaN.');
end
