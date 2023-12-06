function Force = ple_force(TargetXYZ, JD, CooFrame, IncludeSun)
    % Calculate the net Sun+planets G*M on a solar system body.
    % Input  : - Target body positiopn vector.
    %            This is a 3 X Ntimes matrix, where 3 is the numbr of
    %            coordinates, and Ntimes the number of times.
    %          - Vector of JD. Number of elements is Ntimes or 1. 
    %          - Output type (always cartesian): 
    %               'Ecdate' - Ecliptic of date.
    %               'Eqdate' - Equatorial mean equinox of date.
    %               'EqJ2000' - Equatorial mean equinox of J2000.
    %            Default is 'EqJ2000'
    %          - A logical indicating if to include the Sun force.
    %            Default is true.
    % Output : - A 3 X Ntimes matrix of force vectors on the body in each
    %            epoch (JD). Units: Solar-mass * au / day^2
    % Author : Eran Ofek (Oct 2021)
    % Example: Force = celestial.SolarSys.ple_force([2 2 2]', 2451545)
    
    arguments
        TargetXYZ
        JD
        CooFrame     = 'EqJ2000';  % 'EcDate' | 'EqDate' | 'EqJ2000'
        IncludeSun logical    = true;
    end
    G = (constant.G./(constant.au).^3 .*86400.^2 .* constant.SunM);  % [G: au^3 SunM^-1 day^-2]
    
    Msun  = 1.98847e33;  % [gram]
    Mpl   = [0.330103e27, 4.86731e27, 5.97217e27, 0.641691e27, 1898.125e27, 568.317e27, 86.8099e27, 102.4092e27]./Msun;  % [solar mass]
%     Mpl(3) = 0; %test
    Msun  = 1;
    
    GMpl  = G .* Mpl;
    GMsun = G .* Msun;
    
    
    % planets coordinates
    % return: 3 X Nplanets X Ntimes
    PlXYZ = celestial.SolarSys.ple_xyzAll(JD, CooFrame);
%     PlXYZ(:,3,:) = PlXYZ(:,3,:)+6e9/constant.au; %test
    Npl   = size(PlXYZ, 2);
%     Njd   = numel(JD); 
    Njd   = size(TargetXYZ,2); % to support 1 date for multiple targets
    
    % transform planets coordinates to target reference frame
    PlXYZ = PlXYZ - reshape(TargetXYZ, [3, 1, Njd]);
    
    % Distances
    PlDist = sqrt(sum(PlXYZ.^2,1));
    
    % calc force and sum over planets
    ForcePl = squeeze(sum(GMpl .* PlXYZ./(PlDist.^3), 2));
    
    if IncludeSun
        % Sun
        SunDist  = sqrt(sum(TargetXYZ.^2,1));
        ForceSun = squeeze(sum(GMsun .* (-TargetXYZ)./(SunDist.^3), 3));
    else
        ForceSun = 0;
    end
    
    % total force
    Force    = ForcePl + ForceSun;
    
end