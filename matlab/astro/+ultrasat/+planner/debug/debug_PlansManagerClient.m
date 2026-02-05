%==========================================================================
% ULTRASAT Planner - Debug MissionApiClient (FastAPI plans_manager)
% Run from MATLAB: cd to planner; run('astro/+ultrasat/+planner/debug/debug_PlansManagerClient.m')
% Or: ultrasat.planner.debug.debug_PlansManagerClient
%
% Requires: FastAPI plans_manager running (e.g. uvicorn soc.mission.plans_manager.api:app --port 8321)
% Set SOC_API_BASE (e.g. http://localhost:8321), SOC_API_KEY, namespace as needed.
%==========================================================================

function debug_PlansManagerClient()
    fprintf('========== DEBUG PLANS MANAGER CLIENT ==========\n');
    fprintf('Uses MissionApiClient with FastAPI plans_manager.\n');
    fprintf('Set SOC_API_BASE, SOC_API_KEY if required.\n\n');

    apiUrl = getenv('SOC_API_BASE');
    if isempty(apiUrl)
        apiUrl = 'http://localhost:8321';
    end
    apiKey = getenv('SOC_API_KEY');
    namespace = 'OPER';

    client = ultrasat.api.MissionApiClient('ApiUrl', apiUrl, 'Namespace', namespace, 'ApiKey', apiKey);
    fprintf('Client: ApiUrl=%s, Namespace=%s\n\n', client.ApiUrl, client.Client.Namespace);

    % getPlansList
    fprintf('---- getPlansList([], [], []) ----\n');
    try
        response = client.getPlansList([], [], []);
        fprintf('ok=%d, status=%s\n', response.ok, getfield(response, 'status', ''));
        if response.ok && isfield(response, 'plans') && ~isempty(response.plans)
            fprintf('Plans count: %d\n', numel(response.plans));
            if numel(response.plans) >= 1
                p1 = response.plans(1);
                if iscell(response.plans), p1 = response.plans{1}; end
                fprintf('First plan pk=%s title=%s\n', num2str(p1.pk), getfield(p1, 'title', ''));
            end
        else
            fprintf('Plans count: 0\n');
        end
    catch ME
        fprintf('Error: %s\n', ME.message);
    end

    fprintf('\n========== DEBUG PLANS MANAGER CLIENT DONE ==========\n');
end
