%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_validateLvcJsonOnShare.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Description : GW share smoke test for lvc_*.json (jsondecode). Report on share root.
%
% Run:
%   ultrasat.services.too_planner.debug.debug_validateLvcJsonOnShare()
%==========================================================================

function debug_validateLvcJsonOnShare(Args)
    % Same validation as debug_loadLvcJsonFiles; writes lvc_json_validate.csv on ParentFolder.

    arguments
        Args.ParentFolder (1,1) string = ""
        Args.MaxFiles (1,1) double = Inf
        Args.StopOnFirstError (1,1) logical = false
    end

    if strlength(strtrim(Args.ParentFolder)) > 0
        Parent = char(strtrim(Args.ParentFolder));
    else
        Parent = fullfile(getRwshareRoot(), 'gw_coverage_experiment_2026_08');
    end

    ultrasat.services.too_planner.debug.debug_loadLvcJsonFiles( ...
        ParentFolder=Parent, ...
        MaxFiles=Args.MaxFiles, ...
        StopOnFirstError=Args.StopOnFirstError);

    Src = fullfile(Parent, 'lvc_json_load_report.csv');
    Dest = fullfile(Parent, 'lvc_json_validate.csv');
    if isfile(Src)
        copyfile(Src, Dest, 'f');
        fprintf('Share report: %s\n', Dest);
    else
        fprintf('Warning: load report missing: %s\n', Src);
    end
end


function Root = getRwshareRoot()
    if ispc
        Root = '\\socsrv\rwshare_data';
    else
        Root = '/data/rwshare_data';
    end
end