function tests = test_ScheduleManagerClient_getTargets_01()
% Offline unit tests for ScheduleManagerClient.getTargets request shaping.
tests = functiontests(localfunctions);
end


function test_defaultParams_and_endpoint(tc)
client = ultrasat.api.clients.TestScheduleManagerClient();
resp = client.getTargets(); %#ok<NASGU>

tc.verifyEqual(client.LastEndpoint, '/get-targets');
tc.verifyTrue(isstruct(client.LastParams));
tc.verifyTrue(isfield(client.LastParams, 'start_time'));
tc.verifyTrue(isfield(client.LastParams, 'end_time'));
tc.verifyFalse(isfield(client.LastParams, 'limit'));

tc.verifyClass(client.LastParams.start_time, 'datetime');
tc.verifyClass(client.LastParams.end_time, 'datetime');
tc.verifyEqual(char(client.LastParams.start_time.TimeZone), 'UTC');
tc.verifyEqual(char(client.LastParams.end_time.TimeZone), 'UTC');

tc.verifyEqual(client.LastParams.start_time, datetime(2020, 1, 1, 'TimeZone', 'UTC'));
tc.verifyEqual(client.LastParams.end_time, datetime(2040, 12, 31, 23, 59, 59, 'TimeZone', 'UTC'));
end


function test_limitIsIncluded_whenProvided(tc)
client = ultrasat.api.clients.TestScheduleManagerClient();
resp = client.getTargets([], [], 100); %#ok<NASGU>

tc.verifyEqual(client.LastEndpoint, '/get-targets');
tc.verifyTrue(isfield(client.LastParams, 'limit'));
tc.verifyEqual(client.LastParams.limit, 100);
end


function test_responseTimesAreConvertedFromIsoStrings(tc)
client = ultrasat.api.clients.TestScheduleManagerClient();
resp = client.getTargets();

tc.verifyTrue(isfield(resp, 'targets'));
tc.verifyEqual(numel(resp.targets), 1);

t = resp.targets(1);
tc.verifyClass(t.start_time, 'datetime');
tc.verifyClass(t.end_time, 'datetime');
tc.verifyEqual(char(t.start_time.TimeZone), 'UTC');
tc.verifyEqual(char(t.end_time.TimeZone), 'UTC');

tc.verifyEqual(t.start_time, datetime(2029, 2, 1, 0, 0, 0, 'TimeZone', 'UTC'));
tc.verifyEqual(t.end_time, datetime(2029, 7, 31, 0, 0, 0, 'TimeZone', 'UTC'));
end

