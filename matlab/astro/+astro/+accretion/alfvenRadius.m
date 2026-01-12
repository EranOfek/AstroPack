function [R_al, Vff, Rho, L] = alfvenRadius(Ms, B, Mdot, Rl)
    % Estimate of the Alfvan radius in the equatorial plane of an aligned NS
    % Input  : - Mass of central object [solar mass]. Default is 1.4
    %          - Surface magnetic field of dipole [G]. Default is 1e12.
    %          - Mass accretion rate [solar mass/yr]. Default is 1e-7.
    %          - (Rl) Radius in which to calculate luminosity (binding energy).
    %            Defaulys is 1e6 [cm].
    % Output : - Alfvan radius [cm].
    %          - Free fall velocity [cm/s] at Alfavn radius.
    %          - Density [g/cm^3] at alfvan radius.
    %          - Luminosity calculated based on the potential energy at
    %            radius Rl [erg/s].
    % Author : Eran Ofek (2026 Jan) 
    % Example: [R_al, Vff, Rho, L] = astro.accretion.alfvenRadius(1.4, 1e12, 1e-7, false)

    arguments
        Ms   = 1.4;
        B   = 1e12;
        Mdot = 1e-7;
        Rl   = 1e6;
    end

    M      = Ms.*constant.SunM;
    Mdot   = Mdot.*constant.SunM./(365.25.*86400);    % solar mass/yr -> g/s
    Mdot17 = Mdot./1e17;

    % if IsL
    %     % Use L  [15.1.6]
    %     L37 = Mdot./1e37;
    %     R_al = 3.5e8.*L37.^(-2./7) .*(B./1e12).^(4./7) .* Ms.^(1./7) .* (Rs./1e6).^(-2./7); % [cm]
    %     Mdot = L37.*1e37./(constant.G.*M./Rs);
    % 
    % else
    
    % use Mdot [15.1.4]
    R_al = 3.2e8.*Mdot17.^(-2./7) .*(B./1e12).^(4./7) .* Ms.^(-1./7);  % [cm]
    

    Vff = sqrt(2.*constant.G.*M./R_al);
    Rho = Mdot./(4.*pi.*Vff.*R_al.^2);
    L   = Mdot.*(constant.G.*M./Rl);

end
