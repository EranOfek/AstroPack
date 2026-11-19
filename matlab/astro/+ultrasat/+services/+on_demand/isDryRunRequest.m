%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.isDryRunRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : True when workdir request requests dry_run (integration tests).
%==========================================================================

function Tf = isDryRunRequest(WorkReq)
    Tf = false;
    if ~isfield(WorkReq, 'dry_run')
        return;
    end
    Val = WorkReq.dry_run;
    if islogical(Val)
        Tf = Val;
    elseif isnumeric(Val)
        Tf = Val ~= 0;
    elseif ischar(Val) || isstring(Val)
        Tf = any(strcmpi(string(Val), ["true", "1", "yes"]));
    end
end