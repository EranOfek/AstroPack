function [Ang_SOT, Ang_STO, Ang_TSO] = anglesFromDistances(R_Obs, R_Target, Delta, OutUnits)
    % Given Distances between 3 bodies, return the angles.
    % Input  : - (R_Obs) An array of Sun-Observer distances.
    %          - (R_Target) An array of Sun-Target distances.
    %          - (Delta) An array of Observer-Target distances
    %          - Output units: 'rad'|'deg'. Default is 'rad'.
    % Output : - Sun-Observer-Target angles.
    %          - Sun-Target-Observer angles.
    %          - target-Sun-Observer angles.
    % Author : Eran Ofek (Nov 2023)
    % Example: [Ang_SOT, Ang_STO, Ang_TSO] = celestial.SolarSys.anglesFromDistances(1, 1, [1 1.1])
    %          [Ang_SOT, Ang_STO, Ang_TSO] = celestial.SolarSys.anglesFromDistances(1, 1, [1 1.1],'deg')
    
    arguments
        R_Obs       % Sun-Observer
        R_Target    % Sun-Target
        Delta       % Target-Observer
        OutUnits    = [];
    end    
    
    % calculate angles
    Ang_SOT = acos((R_Obs.^2 + Delta.^2 - R_Target.^2)./(2.*R_Obs.*Delta));  % [deg]
    % Observer-Target-Sun
    Ang_STO = acos((R_Target.^2 + Delta.^2 - R_Obs.^2)./(2.*R_Target.*Delta));   % [deg]

    if nargout>2
        Ang_TSO   = pi - Ang_SOT - Ang_STO;
    else
        Ang_TSO   = [];
    end

    if ~isempty(OutUnits)
        Factor  = convert.angular('rad',OutUnits);
        Ang_SOT = Ang_SOT.*Factor;
        Ang_STO = Ang_STO.*Factor;
        Ang_TSO = Ang_TSO.*Factor;
    end
end
