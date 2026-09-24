classdef TestScheduleManagerClient < ultrasat.api.clients.ScheduleManagerClient
    properties
        LastEndpoint = ''
        LastParams = struct()
        StubResponse = []
    end

    methods
        function obj = TestScheduleManagerClient()
            obj@ultrasat.api.clients.ScheduleManagerClient('http://example.invalid');
            obj.StubResponse = struct( ...
                'status', 'ok', ...
                'targets', struct( ...
                    'target_id', 'hcs-2029-02-01T00:00:00+00:00', ...
                    'name', 'HCS-target', ...
                    'start_time', '2029-02-01T02:00:00+02:00', ...
                    'end_time', '2029-07-31T03:00:00+03:00' ...
                ) ...
            );
        end

        function response = postRequest(obj, endpoint, params) %#ok<INUSL>
            obj.LastEndpoint = endpoint;
            obj.LastParams = params;
            response = obj.StubResponse;
        end
    end
end

