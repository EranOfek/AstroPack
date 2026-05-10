function tests = Test_ultrasat_slew_calc_processRequest
    % Tests for ultrasat.services.slew_calc.processRequest (bridge JSON contract)

    tests = functiontests(localfunctions);
end

%% Test Functions

function testBatchMissingPairsHasErrorStatus(testCase)
    In = struct('action', 'slew_batch', 'time', '');
    Out = ultrasat.services.slew_calc.processRequest(In);
    verifyEqual(testCase, Out.status, 'error');
    verifyTrue(testCase, isfield(Out, 'results'));
    verifyEqual(testCase, Out.results, []);
end

function testSlewRollIgnoredForSlewTime(testCase)
    % Same RA/Dec with different roll should yield identical slew (RA/Dec-only model)
    InBase = struct( ...
        'action', 'slew', ...
        'from', struct('ra', 10, 'dec', -20, 'roll', 0), ...
        'to', struct('ra', 12.5, 'dec', -21.2, 'roll', 0), ...
        'time', '');
    InRoll = InBase;
    InRoll.from.roll = 0;
    InRoll.to.roll = 45;

    Out0 = ultrasat.services.slew_calc.processRequest(InBase);
    Out1 = ultrasat.services.slew_calc.processRequest(InRoll);

    verifyEqual(testCase, Out0.status, 'ok');
    verifyEqual(testCase, Out1.status, 'ok');
    verifyEqual(testCase, Out1.slew, Out0.slew, 'AbsTol', 1e-9);
    verifyEqual(testCase, Out1.direct, Out0.direct);
end
