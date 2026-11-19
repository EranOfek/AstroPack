%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.writeResultJson
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Write durable result.json in workdir with pathForJson on paths.
%==========================================================================

function ResultFile = writeResultJson(WorkFolder, Payload)
    % writeResultJson Serialize result envelope to work_folder/result.json.

    arguments
        WorkFolder (1,1) string
        Payload struct
    end

    WorkFolder = string(ultrasat.tools.localPathForIo(WorkFolder));
    if ~isfolder(WorkFolder)
        mkdir(WorkFolder);
    end

    Payload = pathForJsonStruct(Payload);
    ResultFile = fullfile(WorkFolder, 'result.json');
    Text = jsonencode(Payload, 'PrettyPrint', true);
    TmpFile = [ResultFile, '.tmp'];
    fid = fopen(TmpFile, 'wt');
    if fid < 0
        error('OnDemand:ResultWrite', 'Cannot write %s', ResultFile);
    end
    fprintf(fid, '%s', Text);
    fclose(fid);
    movefile(TmpFile, ResultFile, 'f');
    ResultFile = ultrasat.tools.pathForJson(ResultFile);
end

% -------------------------------------------------------------------------

function S = pathForJsonStruct(S)
    Fn = fieldnames(S);
    for K = 1:numel(Fn)
        Val = S.(Fn{K});
        if ischar(Val) || isstring(Val)
            if endsWith(Fn{K}, '_file') || endsWith(Fn{K}, '_folder') || strcmp(Fn{K}, 'work_folder')
                S.(Fn{K}) = ultrasat.tools.pathForJson(Val);
            end
        elseif isstruct(Val)
            S.(Fn{K}) = pathForJsonStruct(Val);
        end
    end
end