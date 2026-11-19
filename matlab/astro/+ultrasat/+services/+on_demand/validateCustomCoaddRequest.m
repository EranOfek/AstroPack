%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.validateCustomCoaddRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Validate workdir request.json for custom_coadd (SPEC §10.2).
%==========================================================================

function validateCustomCoaddRequest(WorkReq)
    Act = 'custom_coadd';
    if isfield(WorkReq, 'action')
        ActNorm = char(WorkReq.action);
        if strcmp(ActNorm, 'custom_coaddition')
            ActNorm = 'custom_coadd';
        end
        if ~strcmp(ActNorm, Act)
            error('OnDemand:Validation', 'custom_coadd: action must be custom_coadd');
        end
    else
        error('OnDemand:Validation', 'custom_coadd: missing action');
    end

    if ~isfield(WorkReq, 'request_id') || isempty(WorkReq.request_id)
        error('OnDemand:Validation', 'custom_coadd: missing request_id');
    end
    if ~isfield(WorkReq, 'image_files') || isempty(WorkReq.image_files)
        error('OnDemand:Validation', 'custom_coadd: missing image_files');
    end
    if ~iscell(WorkReq.image_files) && ~isstring(WorkReq.image_files) && ~ischar(WorkReq.image_files)
        error('OnDemand:Validation', 'custom_coadd: image_files must be an array');
    end
end