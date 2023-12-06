function [Radius, Area] = asteroid_radius(H,Albedo,Mag,R,Delta)
    % Calculate asteroid radius from magnitudes
    % Input  : - Planetary absolute magnitude (H).
    %          - Albedo. Default is 0.15.
    %          - Magnitude. If given than override H.
    %          - R [AU]
    %          - Delta [AU]
    % Output : - Radius [km]
    %          - Area [km^2]
    % Author : Eran Ofek (Nov 2022)
    % Example: [Radius, Area] = celestial.SolarSys.asteroid_radius(18,0.15);
    %          [Radius, Area] = celestial.SolarSys.asteroid_radius([],0.15,12,1,0.075);
    
    arguments
        H
        Albedo   = 0.15;
        Mag      = [];
        R        = [];
        Delta    = [];
    end
    
    if ~isempty(R) && ~isempty(Delta) && ~isempty(Mag)
        H = Mag - (5.*log10(R) + 5.*log10(Delta));
    end
    Radius = 0.5.*10.^(3.1236-0.5.*log10(Albedo)-0.2.*H);
    Area   = pi.*Radius.^2;
    
end
