function Link=getSpecLink(RA, Dec, Args)
    % get link to DESI spectra viewer given target ID or RA/Dec
    % Input  : - Either TargetID or RA
    %          - Dec. If empty, then treat RA as TargetID.
    %            Default is [].
    % Output : - A cell array of links to the target viewer.
    % Author : Eran Ofek (Jun 2023)
    % Example: Link=VO.desi.getSpecLink(int64(616089230757593610));
    arguments
        RA
        Dec           = [];
        Args.Cat      = cats.spec.sources.DESI_EDR;
        Args.BaseURL  = 'https://www.legacysurvey.org/viewer/desi-spectrum/edr/targetid';
    end

    if isempty(Dec)
        % assume  RA contains target ID
        TargetID = RA;
    else
        % search RA/Dec in DESI local table
        error('RA/Dec input option not yet available');
        TargetID = NaN;

    end

    Ntarget = numel(TargetID);
    Link    = cell(Ntarget,1);
    for Itarget=1:1:Ntarget
        Link{Itarget} = sprintf('%s%d',Args.BaseURL, TargetID(Itarget));
    end

end
