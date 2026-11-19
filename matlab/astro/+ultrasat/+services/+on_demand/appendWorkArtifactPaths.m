%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.appendWorkArtifactPaths
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Add work_folder, result_file, status_file to IPC output (pathForJson).
%==========================================================================

function Output = appendWorkArtifactPaths(Output, WorkFolder, ResultFile)
    arguments
        Output struct
        WorkFolder (1,1) string
        ResultFile (1,1) string = ""
    end

    WorkFolderJson = ultrasat.tools.pathForJson(WorkFolder);
    Output.work_folder = WorkFolderJson;
    Output.status_file = ultrasat.tools.pathForJson(fullfile(WorkFolder, 'status.json'));
    if nargin >= 3 && strlength(ResultFile) > 0
        Output.result_file = ResultFile;
    elseif isfield(Output, 'status') && strcmp(Output.status, 'ok')
        Output.result_file = ultrasat.tools.pathForJson(fullfile(WorkFolder, 'result.json'));
    end
end