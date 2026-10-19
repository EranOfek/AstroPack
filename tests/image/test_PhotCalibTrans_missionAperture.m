function tests = test_PhotCalibTrans_missionAperture
    % Unit tests for the telescope collecting area of a mission run:
    % PhotCalibTrans.missionDefaults(...).Aperture and
    % PhotCalibTrans.resolveAperture, which calibrate uses to choose it.
    % The area sets the photon scale the fitted Norm is measured against, so a
    % mission run that kept LAST's 0.061 m^2 reported a Norm 19x too large for
    % ZTF. Precedence: explicit argument > area set on the object by hand >
    % mission table > LAST default; a LAST run never changes.
    % Author : D. Kovaleva (Oct 2026)
    % Example: runtests('tests/image/test_PhotCalibTrans_missionAperture.m')
    tests = functiontests(localfunctions);
end

%% Test Functions

function testTableValues(TestCase)
    % Unobstructed entrance pupils: LAST 27.94 cm, ZTF/P48 48" = 1.219 m.
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('LAST').Aperture, pi*0.1397^2, 'RelTol',1e-12);
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('ZTF').Aperture,  pi*0.6096^2, 'RelTol',1e-12);
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('ztf').Aperture,  pi*0.6096^2, 'RelTol',1e-12, ...
        'Mission names are case-insensitive.');
end

function testLASTEntryMatchesTheClassDefault(TestCase)
    % The table's LAST value and the property default must be the same
    % number, or resolveAperture would mistake an untouched object for one
    % whose area was set by hand.
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('LAST').Aperture, PhotCalibTrans().Aperture, ...
        'RelTol',1e-12);
end

function testUnlistedMissionHasNoArea(TestCase)
    verifyEmpty(TestCase, PhotCalibTrans.missionDefaults('SDSS').Aperture);
end

function testExplicitArgumentWins(TestCase)
    [A, Src] = PhotCalibTrans.resolveAperture('ZTF', 2.5, pi*0.1397^2);
    verifyEqual(TestCase, A, 2.5);
    verifyEqual(TestCase, Src, 'argument');
end

function testMissionTableReplacesTheLASTDefault(TestCase)
    % The regression: ZTF without 'Aperture' used to keep LAST's area.
    [A, Src] = PhotCalibTrans.resolveAperture('ZTF', [], pi*0.1397^2);
    verifyEqual(TestCase, A, pi*0.6096^2, 'RelTol',1e-12);
    verifyEqual(TestCase, Src, 'mission');
end

function testHandSetAreaIsKept(TestCase)
    % An area the user put on the object deliberately is not overwritten.
    [A, Src] = PhotCalibTrans.resolveAperture('ZTF', [], 0.9);
    verifyEqual(TestCase, A, 0.9);
    verifyEqual(TestCase, Src, 'object');
end

function testLASTRunIsUnchanged(TestCase)
    % LAST keeps whatever the object holds - default or not.
    [A, Src] = PhotCalibTrans.resolveAperture('LAST', [], pi*0.1397^2);
    verifyEqual(TestCase, A, pi*0.1397^2, 'RelTol',1e-12);
    verifyEqual(TestCase, Src, 'object');
    [A, Src] = PhotCalibTrans.resolveAperture('LAST', [], 0.05);
    verifyEqual(TestCase, A, 0.05);
    verifyEqual(TestCase, Src, 'object');
end

function testUnlistedMissionIsFlagged(TestCase)
    % No table entry and no argument: LAST's area stays, flagged so
    % calibrate can log that Norm is not a throughput of this telescope.
    [A, Src] = PhotCalibTrans.resolveAperture('SDSS', [], pi*0.1397^2);
    verifyEqual(TestCase, A, pi*0.1397^2, 'RelTol',1e-12);
    verifyEqual(TestCase, Src, 'unlisted');
end
