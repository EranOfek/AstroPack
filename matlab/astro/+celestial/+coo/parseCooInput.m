function [RA, Dec] = parseCooInput(RA, Dec, Args)
    % Parse RA/Dec coordinates
    % Input  : - RA in [rad], [deg], [sexagesinmal], or [object name]
    %          - Dec in [rad], [deg], [sexagesinmal. If empty, then RA is
    %            object name.
    %          * ...,key,val,...
    %            'InUnits' - Default is 'deg'.
    %            'OutUnits' - Default is 'deg'.
    %            'NameServer' - Default is 'simbad'.
    % Output : - RA
    %          - Dec
    % Author : Eran Ofek (Oct 2021)
    % Example: [RA, Dec] = celestial.coo.parseCooInput(1, 1, 'InUnits','rad', 'OutUnits','deg')
    
    arguments
        RA
        Dec
        Args.InUnits    = 'deg';
        Args.OutUnits   = 'deg';
        Args.NameServer = 'simbad';
    end
    
    if isempty(Dec) && ischar(RA)
        % call name server
        switch lower(Args.NameServer)
            case 'simbad'
                [RA, Dec] = VO.name.server_simbad(RA);
                Args.InUnits = 'deg';
            case 'ned'
                [RA, Dec] = VO.name.server_ned(RA);
                Args.InUnits = 'deg';
            otherwise
                error('Unknown NameServer option');
        end
    else
    
        if ischar(RA) || iscell(RA)
            RA = celestial.coo.convertdms(RA, 'SH', 'r');
            Args.InUnits = 'rad';
        end
        if ischar(Dec) || iscell(Dec)
            Dec = celestial.coo.convertdms(Dec, 'SD', 'R');
            Args.InUnits = 'rad';
        end
    end
    
    Factor = convert.angular(Args.InUnits, Args.OutUnits);
    RA     = RA.*Factor;
    Dec    = Dec.*Factor;
    
end