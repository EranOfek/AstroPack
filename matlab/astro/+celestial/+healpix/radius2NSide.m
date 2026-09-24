function [NSide] = radius2NSide(Radius)
    % Given angular radius, find the most appropriate NSide for cone search.
    % Input  : - Radius [rad]
    % Output : - NSide;
    % Author : Eran Ofek (2026 Jun) 
    % Example: celestial.healpix.radius2NSide(1./206000)
    
    NSide = 2.^floor(log(1./Radius)./log(2));
end
