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

function testZTFIsABFlatWithItsOwnWindow(TestCase)
    % Non-LAST missions use an AB-flat reference spectrum; ZTF's window was
    % measured on real frames (saturation below r~13.5, SN floor beyond r~19).
    Def = PhotCalibTrans.missionDefaults('ZTF');
    verifyEqual(TestCase, Def.RefSpecSlope, 0);
    verifyEqual(TestCase, Def.MagRange, [14 19]);
    verifyTrue(TestCase, Def.Known);
end

function testMissionNameIsCaseInsensitive(TestCase)
    % Mission names reach this from a filter-family argument, where case is
    % the user's choice.
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('ztf'), ...
                          PhotCalibTrans.missionDefaults('ZTF'));
    verifyEqual(TestCase, PhotCalibTrans.missionDefaults('last'), ...
                          PhotCalibTrans.missionDefaults('LAST'));
end

function testUnknownMissionHasNoWindow(TestCase)
    % An unlisted mission gets the neutral AB-flat slope but NO calibrator
    % window: that one has to be measured, and the caller warns instead.
    Def = PhotCalibTrans.missionDefaults('SDSS');
    verifyEqual(TestCase, Def.RefSpecSlope, 0);
    verifyEmpty(TestCase, Def.MagRange);
    verifyFalse(TestCase, Def.Known);
end
