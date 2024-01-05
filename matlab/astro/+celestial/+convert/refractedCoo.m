function [OutRA, OutDec] = refractedCoo(InRA, InDec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - R.A., [deg|rad|sex] or object name.
    %            If second input is provided and RA is not numeric, then
    %            will assume input is in sexagesinal coordinates.
    %          - Dec. [deg|rad|sex]. If empty, then will interpret the
    %            first input argument as an object name.
    %            Default is [].
    %          * ...,key,val,... 
    %            'InUnits' - Default is 'deg'.
    %            'OutUnits' - Default is 'deg'.
    %            'Server' - If input is object name, then this is the name
    %                   server that will be used: @VO.name.server_simbad|
    %                   @VO.name.server_ned.
    %                   Default is @VO.name.server_simbad
    % Output : - 
    % Author : Eran Ofek (2024 Jan) 
    % Example: 

    arguments
        InRA
        InDec                  = [];
        Args.InUnits           = 'deg';  % 'deg'|'rad'|'sex'|'ned'|'simbad'|
        Args.OutUnits          = 'deg';  % 'deg'|'rad'
        Args.Server            = @VO.name.server_simbad;
        
    end

    [RA, Dec]=celestial.convert.cooResolve(InRA, InDec, 'InUnits',Args.InUnits, 'OutUnits','rad', 'Server',Args.Server); % [rad]
    
    % apply refraction
    
    
end
