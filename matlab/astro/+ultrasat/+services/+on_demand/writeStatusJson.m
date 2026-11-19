%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.writeStatusJson
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Create or merge workdir status.json (paths serialized for JSON).
%==========================================================================

function StatusFile = writeStatusJson(WorkFolder, Patch)
    % writeStatusJson Write status.json under WorkFolder.

    arguments
        WorkFolder (1,1) string
        Patch struct
    end

  WorkFolder = string(ultrasat.tools.localPathForIo(WorkFolder));
    if ~isfolder(WorkFolder)
        mkdir(WorkFolder);
    end

    StatusFile = fullfile(WorkFolder, 'status.json');
    Status = defaultStatusTemplate();
    if isfile(StatusFile)
        try
            Status = jsondecode(fileread(StatusFile));
        catch
        end
    end

    Fn = fieldnames(Patch);
    for K = 1:numel(Fn)
        Val = Patch.(Fn{K});
        if isstring(Val) || ischar(Val)
            if any(strcmp(Fn{K}, {'message', 'state', 'action', 'worker_id', 'request_id'}))
                Status.(Fn{K}) = char(Val);
            else
                Status.(Fn{K}) = char(ultrasat.tools.pathForJson(Val));
            end
        else
            Status.(Fn{K}) = Val;
        end
    end

    Text = jsonencode(Status, 'PrettyPrint', true);
    TmpFile = [StatusFile, '.tmp'];
    fid = fopen(TmpFile, 'wt');
    if fid < 0
        error('OnDemand:StatusWrite', 'Cannot write %s', StatusFile);
    end
    fprintf(fid, '%s', Text);
    fclose(fid);
    movefile(TmpFile, StatusFile, 'f');
end

% -------------------------------------------------------------------------

function S = defaultStatusTemplate()
    S = struct( ...
        'state', 'queued', ...
        'request_id', '', ...
        'action', '', ...
        'created_at', '', ...
        'claimed_at', '', ...
        'started_at', '', ...
        'completed_at', '', ...
        'worker_id', '', ...
        'reclaim_count', 0, ...
        'heartbeat_at', '', ...
        'message', '');
end