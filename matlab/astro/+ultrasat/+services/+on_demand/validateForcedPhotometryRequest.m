%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.validateForcedPhotometryRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Validate workdir request.json for forced_photometry (SPEC §10.1).
%==========================================================================

function validateForcedPhotometryRequest(WorkReq)
    requireField(WorkReq, 'action', 'forced_photometry');
    requireField(WorkReq, 'request_id', []);
    requireField(WorkReq, 'image_file', []);
    requireField(WorkReq, 'sources', []);
    if ~isstruct(WorkReq.sources) && ~iscell(WorkReq.sources)
        error('OnDemand:Validation', 'forced_photometry: sources must be an array');
    end
    if isempty(WorkReq.sources)
        error('OnDemand:Validation', 'forced_photometry: sources must not be empty');
    end
end

% -------------------------------------------------------------------------

function requireField(S, Name, ExpectedAction)
    if ~isfield(S, Name) || isempty(S.(Name))
        error('OnDemand:Validation', 'forced_photometry: missing %s', Name);
    end
    if ~isempty(ExpectedAction) && ~strcmp(S.(Name), ExpectedAction)
        error('OnDemand:Validation', 'forced_photometry: action must be %s', ExpectedAction);
    end
end