function tests = test_gaia2picklesClass
    % Unit tests for astro.stars.gaia2picklesClass.
    %
    % Luminosity class from the CMD position (V, IV, III, I), the no-parallax
    % default, color matching of the template, and the AstroSpec output.
    % Needs the boundary file from astro.stars.buildLumClassBoundaries.

    tests = functiontests(localfunctions);
end

%% Fixture

function setupOnce(testCase)
    File = fullfile(tools.os.getAstroPackDataPath, 'stars', 'PARSEC', 'lumclass_boundaries_parsec.mat');
    testCase.assumeTrue(isfile(File), 'Skipping: PARSEC boundary file not built.');
end

%% Test Functions

function testSunIsDwarf(testCase)
    % The Sun at 10 pc maps to a G dwarf template.

    [Name, Res] = astro.stars.gaia2picklesClass(0.82, 'G', 4.67, 'Plx', 100, 'PlxErr', 0.1);
    testCase.verifyEqual(Res.LumClass, "v", 'The Sun must be class V.');
    testCase.verifyTrue(startsWith(Name, "g"), 'The Sun must get a G template.');
end

function testRedClumpGiant(testCase)
    % A K star at M_G = 0 is a giant.

    [Name, Res] = astro.stars.gaia2picklesClass(1.2, 'G', 10, 'Plx', 1, 'PlxErr', 0.01);
    testCase.verifyEqual(Res.LumClass, "iii", 'M_G=0 at BP-RP=1.2 must be class III.');
    testCase.verifyTrue(endsWith(Name, "iii"), 'A giant must get a III template.');
end

function testSubgiant(testCase)
    % M_G = 3 at BP-RP = 0.98 (2.5 mag above the main sequence) is a subgiant.

    [~, Res] = astro.stars.gaia2picklesClass(0.98, 'G', 13, 'Plx', 1, 'PlxErr', 0.01);       % M_G = 3
    testCase.verifyEqual(Res.LumClass, "iv", 'A star 2.5 mag above the MS at G colors must be class IV.');
end

function testSupergiant(testCase)
    % M_G = -7 at BP-RP = 1.0 is a supergiant.

    [~, Res] = astro.stars.gaia2picklesClass(1.0, 'G', 3, 'Plx', 1, 'PlxErr', 0.01);         % M_G = -7
    testCase.verifyEqual(Res.LumClass, "i", 'M_G=-7 must be class I.');
end

function testLuminousODwarfStaysDwarf(testCase)
    % An O dwarf on the main sequence is V although it is brighter than the supergiant limit.

    [~, Res] = astro.stars.gaia2picklesClass(-0.69, 'G', 5.8, 'Plx', 1, 'PlxErr', 0.01);  % M_G = -4.2 = MS
    testCase.verifyEqual(Res.LumClass, "v", 'A main-sequence O star must be class V.');
end

function testNoParallaxIsDwarf(testCase)
    % Without a usable parallax the class is V.

    [~, Res] = astro.stars.gaia2picklesClass(1.2, 'G', 10, 'Plx', 1, 'PlxErr', 0.5);
    testCase.verifyEqual(Res.LumClass, "v", 'Plx/PlxErr=2 must give class V.');
    [~, Res] = astro.stars.gaia2picklesClass(1.2);
    testCase.verifyEqual(Res.LumClass, "v", 'No parallax must give class V.');
end

function testTemplateColorMatch(testCase)
    % The chosen dwarf template's synthetic color is close to the input color.

    B = load(fullfile(tools.os.getAstroPackDataPath, 'stars', 'PARSEC', 'lumclass_boundaries_parsec.mat'));
    Color = linspace(0, 2.5, 26)';
    Name  = astro.stars.gaia2picklesClass(Color);
    [~, Loc] = ismember(Name, B.Templates.Name);
    testCase.verifyLessThan(max(abs(B.Templates.BP_RP(Loc) - Color)), 0.25, ...
                            'Template colors must follow the input colors.');
end

function testSpecOutputSharesHandles(testCase)
    % Stars with the same template share one AstroSpec object.

    [Name, ~, Spec] = astro.stars.gaia2picklesClass([0.82 0.82 1.2]);
    testCase.verifyEqual(numel(Spec), 3, 'One AstroSpec element per star.');
    testCase.verifyTrue(Spec(1) == Spec(2), 'Same template must share one handle.');
    testCase.verifyEqual(string(Spec(3).ObjName), Name(3), 'ObjName must be the template name.');
end

function testNaNColor(testCase)
    % NaN color gives an empty name, and the spectra output refuses it.

    Name = astro.stars.gaia2picklesClass([0.82 NaN]);
    testCase.verifyEqual(Name(2), "", 'NaN color must give an empty name.');
    testCase.verifyError(@() spectra([0.82 NaN]), 'astro:stars:gaia2picklesClass:NaNColor', ...
                         'Spectra must not be built for NaN colors.');
end

%% Helpers

function Spec = spectra(Color)
    [~, ~, Spec] = astro.stars.gaia2picklesClass(Color);
end
