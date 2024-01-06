function [OutRA, OutDec] = cooResolve(InRA, InDec, Args)
    % Convert units for input coordinates or resolve object name into coordinates.
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
    % Output : - Output RA.
    %          - Output Dec.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [RA, Dec]=celestial.convert.cooResolve(1,1)
    %          [RA, Dec]=celestial.convert.cooResolve('m31')
    %          [RA, Dec]=celestial.convert.cooResolve('12:31:10.1','-20:10:10');

    arguments
        InRA
        InDec                  = [];
        Args.InUnits           = 'deg';  % 'deg'|'rad'|'sex'|'ned'|'simbad'|
        Args.OutUnits          = 'deg';  % 'deg'|'rad'
        Args.Server            = @VO.name.server_simbad;
    end

    if isempty(InDec)
        Args.InUnits = 'name';
    else
        if ~isnumeric(InRA)
            Args.InUnits = 'sex';
        end
    end

    switch lower(Args.InUnits)
        case 'name'
            [OutRA, OutDec] = Args.Server(InRA, Args.OutUnits(1));
        
        case 'sex'
            OutRA  = celestial.coo.convertdms(InRA, 'SH', Args.OutUnits(1));
            OutDec = celestial.coo.convertdms(InDec, 'SD', Args.OutUnits(1));
        otherwise
            AngFactor = convert.angular(Args.InUnits, Args.OutUnits);
            OutRA     = AngFactor .* InRA;
            OutDec    = AngFactor .* InDec;

    end





end
