%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.printJsonlSummary.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Print summary of debug JSONL output files
%==========================================================================

function printJsonlSummary()
    % printJsonlSummary  List debug JSONL files with line counts and sample lines.
    %
    % Example:
    %   soc.monitor.debug.printJsonlSummary();
    fprintf('--- JSONL summary ---\n');
    if ispc
        Folder = 'C:/SOC/monitor/debug_jsonl';
    else
        Folder = '/var/opt/soc/monitor/debug_jsonl';
    end
    Files = dir(fullfile(Folder, 'pipeline_monitor_*.jsonl'));
    if isempty(Files)
        fprintf('No JSONL files found in %s\n', Folder);
        return;
    end
    for I = 1:numel(Files)
        FullPath = fullfile(Files(I).folder, Files(I).name);
        fprintf('File: %s (%d bytes)\n', FullPath, Files(I).bytes);
        Lines = readJsonlLines(FullPath);
        fprintf('  Line count: %d\n', numel(Lines));
        MaxShow = min(3, numel(Lines));
        for J = 1:MaxShow
            fprintf('  [%d] %s\n', J, Lines{J});
        end
    end
end

function Lines = readJsonlLines(Filename)
    Lines = {};
    Fid = fopen(Filename, 'r');
    if Fid < 0
        return;
    end
    Cleaner = onCleanup(@() fclose(Fid));
    while true
        Line = fgetl(Fid);
        if ~ischar(Line)
            break;
        end
        if strlength(string(Line)) > 0
            Lines{end + 1} = Line; %#ok<AGROW>
        end
    end
end
