%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.debug.debug_processRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Debug processRequest health and dry_run forced photometry.
%
% Run by      : ultrasat.services.on_demand.debug.debug_processRequest()
%==========================================================================

function debug_processRequest()
    fprintf('\n========== DEBUG ON_DEMAND processRequest ==========\n');

    debugHealth();

    Root = resolveOnDemandRoot();
    WorkDir = fullfile(Root, 'forced_photometry', 'work', '2099', '01', '01', 'debug-fp-0001');
    mkdir(WorkDir);

    RequestPath = fullfile(WorkDir, 'request.json');
    copyfile( ...
        fullfile(fileparts(mfilename('fullpath')), '..', 'examples', '02_forced_photometry_request.json'), ...
        RequestPath);

    WorkFolderJson = ultrasat.tools.pathForJson(WorkDir);
    Input = struct();
    Input.action = 'forced_photometry';
    Input.request_id = 'debug-fp-0001';
    Input.request_file = ultrasat.tools.pathForJson(RequestPath);
    Input.work_folder = WorkFolderJson;
    Input.IpcWorkerId = 'debug_worker';

    fprintf('\n--- forced_photometry dry_run ---\n');
    Output = ultrasat.services.on_demand.processRequest(Input);
    fprintf('status  : %s\n', Output.status);
    fprintf('message : %s\n', Output.message);
    if isfield(Output, 'result_file')
        fprintf('result  : %s\n', Output.result_file);
    end

    fprintf('========== DEBUG ON_DEMAND processRequest DONE ==========\n');
end

% -------------------------------------------------------------------------

function debugHealth()
    fprintf('\n--- health ---\n');
    Input = struct('action', 'health');
    Output = ultrasat.services.on_demand.processRequest(Input);
    fprintf('status  : %s\n', Output.status);
    fprintf('message : %s\n', Output.message);
    if ~strcmp(Output.status, 'ok')
        error('debug_processRequest:HealthFailed', 'health expected ok');
    end
end

% -------------------------------------------------------------------------

function Root = resolveOnDemandRoot()
    Root = getenv('ON_DEMAND_PATH');
    if isempty(Root)
        Root = fullfile(tempdir, 'on_demand_debug');
        setenv('ON_DEMAND_PATH', Root);
        fprintf('ON_DEMAND_PATH unset; using %s\n', Root);
    end
end