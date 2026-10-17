function tests = test_tlogg2picklesClass
    % Unit tests for astro.stars.tlogg2picklesClass.
    %
    % Luminosity-class boundary (DwarfLogg), the giant M4 branch, and that
    % every returned class exists in the Pickles library.

    tests = functiontests(localfunctions);
end

%% Test Functions

function testSunIsDwarf(testCase)
    % log(g)=4.44 must map to a dwarf template (it used to map to a giant).

    R = astro.stars.tlogg2picklesClass(5772, 4.44);
    testCase.verifyEqual(R.lumclass, 'v', 'A Sun-like log(g) must give a dwarf.');
end

function testGiant(testCase)
    % log(g) below DwarfLogg maps to a giant.

    R = astro.stars.tlogg2picklesClass(4800, 3.5);
    testCase.verifyEqual(R.lumclass, 'iii', 'log(g)=3.5 must give a giant.');
end

function testDwarfLoggArgument(testCase)
    % The boundary is an argument; the former boundary (4.5) is still reachable.

    R = astro.stars.tlogg2picklesClass(5772, 4.44, 'DwarfLogg', 4.5);
    testCase.verifyEqual(R.lumclass, 'iii', 'DwarfLogg=4.5 must restore the former mapping.');
    R = astro.stars.tlogg2picklesClass(5772, 4.0);
    testCase.verifyEqual(R.lumclass, 'v', 'log(g) equal to DwarfLogg must give a dwarf.');
end

function testGiantM4Reachable(testCase)
    % 3200 < Teff <= 3300 K giants map to M4 III (the branch was unreachable).

    R = astro.stars.tlogg2picklesClass(3250, 3.5);
    testCase.verifyEqual(R.class, 'm4', 'A 3250 K giant must map to m4.');
end

function testAllClassesHaveSpectra(testCase)
    % Every class the mapper can return must exist in the Pickles library.

    I   = Installer;
    Dir = I.getDataDir('PicklesStellarSpec');
    testCase.assumeTrue(isfolder(Dir), 'Skipping: Pickles library not installed.');
    Teff = logspace(log10(2000), log10(60000), 300);
    R    = astro.stars.tlogg2picklesClass(Teff, [3.5 4.5]);
    Name = unique(string(strcat({R.class}, {R.lumclass})));
    Missing = Name(~arrayfun(@(N) isfile(fullfile(Dir, "uk" + N + ".mat")), Name));
    testCase.verifyEmpty(Missing, 'Mapper returned classes with no Pickles spectrum file.');
end
