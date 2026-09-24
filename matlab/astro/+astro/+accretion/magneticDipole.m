function [Br, Bt, Babs, B_phi, B_rho, B_z] = magneticDipole(R, Theta, Rs, Bsurf, Args)
    % Dipole magnetic field strength as a function of R and Theta
    % Input  : - (R) distance from dipole [cm]. Good approximation for R>3*Rs
    %          - (Theta) polar anle [deg].
    %          - (Rs) star radius [cm]. Default is 1e6.
    %          - (Bsurf) magnetif field on stellar surface [G].
    %            Default is 1e12.
    % Output : - Magnetic field in the radial direction [G].
    %          - Magnetic field in the theta direction [G].
    %          - Abs value of magnetic field.
    %          - Magnetic field in the phi direction [G]. Always 0.
    %          - Magnetic field in the rho direction (distance from dipole
    %            axis) [G].
    %          - Magnetic field in the z direction (from dipole center
    %            towards dipole direction) [G]. 
    % Author : Eran Ofek (2026 Jan) 
    % Example: [Br, Bt, Babs, B_phi, B_rho, B_z] = astro.accretion.magneticDipole(1e6,90)

    arguments
        R                   % [cm]
        Theta               % [deg]
        Rs       = 1e6;     % [cm]
        Bsurf    = 1e12;    % [Gauss]
        Args.A                 = [];
        Args.B                 = [];
    end

    % convert to SI units
    Rstar = Rs./100;  % [m]
    R     = R./100;   % [m]
    B     = Bsurf./1e4;  % [T]


    Mu0 = 4.*pi.*1e-7; % [N A^-2] [H m^-1]

    %B = Mu0 .*Q_m. * r_hat./(4.*pi.* r.^2);

    Q_m = B.*4.*pi.*Rstar.^2./(Mu0);
    M   = Q_m.*Rstar;

    Br = Mu0./(4.*pi) .* 2.*M.*cosd(Theta)./(R.^3);
    Bt = Mu0./(4.*pi) .* M.*sind(Theta)./(R.^3);
    Babs = Mu0./(4.*pi) * M./(R.^3) .* sqrt(1+3.*cosd(Theta).^2);

    % cylindrical coordinates
    Z     = R.*cosd(Theta);
    Rho   = R.*sind(Theta);
    B_phi = 0;
    % R = sqrt(Rho.^2 + Z.^2)
    RhoZ2 = (Rho.^2 +Z.^2).^(5./2);
    B_rho = Mu0./(4.*pi) .* 3.*M.*Rho.*Z./RhoZ2;
    B_z   = Mu0./(4.*pi) .* M.*(2.*Z.^2 - Rho.^2)./RhoZ2;

    % convert back to Gauss [cgs units]
    Br = Br.*1e4;
    Bt = Bt.*1e4;
    Babs = Babs.*1e4;
    B_phi = B_phi.*1e4;
    B_rho = B_rho.*1e4;
    B_z   = B_z.*1e4;

end
