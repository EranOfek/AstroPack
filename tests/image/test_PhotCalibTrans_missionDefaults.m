function tests = test_PhotCalibTrans_missionDefaults
    % Unit tests for PhotCalibTrans.missionDefaults - the per-mission
    % reference-spectrum slope and calibrator window used by calibrate and
    % imProc.calib.fitPhotCalibTrans.
    % Author : D. Kovaleva (Oct 2026)
    % Example: runtests('tests/image/test_PhotCalibTrans_missionDefaults.m')
    tests = functiontests(localfunctions);
end

function testLASTIsUnchanged(TestCase)
    % LAST must keep exactly the values the pipeline has always used.
    Def = PhotCalibTrans.missionDefaults('LAST');
    verifyEqual(TestCase, Def.RefSpecSlope, 1.642);
    verifyEqual(TestCase, Def.MagRange, [12 16]);
    verifyTrue(TestCase, Def.Known);
end

function testDefaultArgumentIsLAST(TestCase)
    % No argument -> LAST, so a bare call cannot change a LAST run.
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults(), ...
                          PhotCalibTrans.missionDefaults('LAST'));
end

function testZTFIsABFlat(TestCase)
    % Non-LAST missions use an AB-flat reference spectrum. The calibrator
    % window is NOT a mission property, so ZTF keeps the catalogue's.
    Def = PhotCalibTrans.missionDefaults('ZTF');
    verifyEqual(TestCase, Def.RefSpecSlope, 0, ...
        'A non-LAST mission must start from an AB-flat reference spectrum.');
    verifyEqual(TestCase, Def.MagRange, [12 16], ...
        'ZTF must use the calibrator catalogue window.');
    verifyTrue(TestCase, Def.Known, 'ZTF has an entry in the table.');
end

function testWindowIsTheSameForEveryMission(TestCase)
    % MagRange is a property of the CALIBRATOR CATALOGUE, not of the
    % instrument: outside G = [12 16] there are too few Gaia XP sampled
    % spectra, whatever the mission. Magnitudes a given mission cannot
    % measure well are removed by the flag and S/N gates instead.
    Missions = {'LAST','ZTF','SDSS','LSST','PAN-STARRS/PS1','NoSuchSurvey'};
    for Im = 1:numel(Missions)
        Def = PhotCalibTrans.missionDefaults(Missions{Im});
        verifyEqual(TestCase, Def.MagRange, [12 16], ...
            sprintf('Mission %s does not use the calibrator catalogue window.', ...
                    Missions{Im}));
    end
end

function testMissionNameIsCaseInsensitive(TestCase)
    % Mission names reach this from a filter-family argument, where case is
    % the user's choice.
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('ztf'), ...
                          PhotCalibTrans.missionDefaults('ZTF'));
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('last'), ...
                          PhotCalibTrans.missionDefaults('LAST'));
end

function testUnknownMissionIsABFlatAndFlagged(TestCase)
    % An unlisted mission gets the neutral AB-flat slope and the catalogue's
    % window, and is still flagged as unlisted so the caller can say the name
    % was not in the table.
    Def = PhotCalibTrans.missionDefaults('SDSS');
    verifyEqual(TestCase, Def.RefSpecSlope, 0, ...
        'An unlisted mission must start from an AB-flat reference spectrum.');
    verifyEqual(TestCase, Def.MagRange, [12 16], ...
        'An unlisted mission must still get the calibrator catalogue window.');
    verifyFalse(TestCase, Def.Known, 'SDSS has no entry in the table.');
end
