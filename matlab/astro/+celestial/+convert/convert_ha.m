function [Out] = convert_ha(In, JD, Args)
    % Convert HA/RA to RA/HA.
    % Input  : - H.A. or R.A.
    %          - JD. Default is celestial.time.julday();
    %          * ...,key,val,... 
    %            'InUnits' - Input units. Default is 'deg'.
    %            'OutUnits' - Output units. Default is 'deg'.
    %            'Long' - east longitude of observer. Default is 35.
    %            'LongUnits' - Units of longitude. Default is 'deg'.
    %            'TypeLST' - LST type (see celestial.time.lst).
    %                   Default is 'a'.
    %            'OutRange' - Output range. Options are:
    %                   'pi' - beteen -pi to pi
    %                   '2pi' - between 0 to 2*pi
    % Output : - R.A., or H.A.
    % Author : Eran Ofek (2024 Jan) 
    % Example: celestial.convert.convert_ha(15, 2451545)

    arguments
        In
        JD                    = celestial.time.julday();
        Args.InUnits          = 'deg';
        Args.OutUnits         = 'deg';
        
        Args.Long             = 35;
        Args.LongUnits        = 'deg';
        Args.TypeLST          = 'a';
        Args.OutRange         = 'pi'
    end
    
    In = convert.angular(Args.InUnits, 'rad', In);  % [rad]
    
    Args.Long = convert.angular(Args.LongUnits, 'rad', Args.Long);
    LST       = celestial.time.lst(JD, Args.Long, Args.TypeLST);  % fraction of day
    LST       = 2.*pi.*LST;   % [rad]
    
    Out = LST - In; % [rad]
        
    switch Args.OutRange
        case 'pi'
            Out = mod(Out, 2.*pi);
            F   = Out>pi;
            Out(F) = Out(F) - 2.*pi;
        case '2pi'
            Out = mod(Out, 2.*pi);
        otherwise
            error('Unknown OutRange option');
    end
    
    Out = convert.angular('rad', Args.OutUnits, Out);
end

