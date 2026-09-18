function [Result] = accretionDisk(Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2025 Dec) 
    % Example: R=astro.accretion.accretionDisk();

    arguments
        Args.Mass      = 1;      % [solar mass]
        Args.Mdot      = 3e-9;   % [solar mass/year]
        Args.Alpha     = 0.1;
        Args.NR        = -100;    % step R, minus for logarithmic
        Args.RI        = 10;     % inner disk radius
        Args.RD        = 1e5;  % outer disk radius
        Args.UnitsR    = 'SR';   % 'SR' - Swartchikd radius, 'NS' - 15km, 'cm' - cm
        Args.Beta      = 1;

        Args.Lambda    = (1000:100:1e4).'.*1e-8;  % [cm]
    end
    SunM = constant.SunM;
    G    = constant.G;
    SEC_YEAR = 86400.*365.25;
    C    = constant.c;

    % units conversion
    % convert to cgs
    MassSM = Args.Mass;
    Mass = Args.Mass .* SunM;   % [gr]
    
    Mdot = Args.Mdot .* SunM./SEC_YEAR;   % [gr/s]
    Beta = Args.Beta;   % |beta|<=1,  ~1 for BH
    switch lower(Args.UnitsR)
        case 'sr'
            ConvR = 2.*G.*Mass./(C.^2);  % [cm]
        case 'ns'
            ConvR = 15.*1e5;   % [cm]
        case 'cm'
            ConvR = 1;
        otherwise
            error('Unknown UnitsR option');
    end
    RI = Args.RI.* ConvR;
    RD = Args.RD.* ConvR;
    RS = 2.*G.*Mass./(C.^2);  % swarchild radius

    % Define radial grid
    if Args.NR<0
        R = logspace(log10(RI), log10(RD), -Args.NR);
    else
        R = linspace(RI, RD, Args.NR);
    end
    Result.R = R;
    Rrs      = R./RS;  % R in RS units

    % inard rate of angular momentum
    Result.Jplus = Mdot.*sqrt(G.*Mass.*R);   % [14.5.13]

    % rate at which angular momentum is consumed by the accretor
    Result.Jminus = Beta .* Result.Jplus;   % [14.5.13]

    CG    = 1 - sqrt(6./Rrs);   % [14.5.37]

    % Total emission as a function of radius / one side only
    %Result.Fr = 3.*Mdot.*G.*Mass.*(1 - Beta.* sqrt(RI./R))./(8.*pi.*R.^3);   % [14.5.17]
    Result.Fr = 5e26 .* MassSM.^(-2) .* (Mdot./1e17) .* Rrs.^(-3) .* CG;  % [erg cm^-2 s^-1] [14.5.37]


    % Total luminosity
    Result.L = (1.5 - Beta).* G.*Mass.*Mdot./RI;


    
    % surface density of disk at radius R
    % \approx 2*h*rho [14.5.7]
    Result.Sigma = 7.*(1./Args.Alpha).* (MassSM) .* (Mdot./1e17).^-1 .* Rrs.^1.5 ./CG;  % [g cm^-2] [14.5.37]
    Result.Rho   = 3e-5 .* (1./Args.Alpha) .* (MassSM) .* (Mdot./1e17).^02 .* Rrs.^1.5  .* CG.^(-2);   % [g cm^-3]  [14.5.37]
    % interior disk Temperature(!)
    Result.T     = 5e7 .* (Args.Alpha .* MassSM).^(-1./4) .* Rrs.^(-3./8);   % [K]  [14.5.37]
    % optical depth by electron scattering
    Result.TauES = 3.* (1./Args.Alpha) .* MassSM .* 1./(Mdot./1e17) .* Rrs.^1.5 ./CG;  % [] [14.5.37]
    Result.H     = 1e5 .* (Mdot./1e17) .* CG;  % [cm] [14.5.37]

    % Optically thick disk temperature:
    %Result.Ts1 = (4.*Result.Fr./constant.sigma).^(1./4);  % why factor of 4?
    Result.Ts = 5e7 .* sqrt(MassSM) .* (Mdot./1e17).^(1./4) .* (R./RS).^(-3./4) .* CG.^(1./4);

    Result.Kappa_ff = 0.64.*1e23.*Result.Rho.*Result.T.^(-7./2);  % [cm^2/g] [Rho and T in cgs]  [14.5.26]

    % kappa_es >> kappa_ff
    Result.Kappa_es = 0.34;
    Result.Teff = Result.Ts;
    FlagKappa   = Result.Kappa_es>Result.Kappa_ff;
    Result.TeffK = Result.Ts .* (Result.Kappa_ff./Result.Kappa_es).^(1./8);  % [14.5.54]
    Result.Teff(FlagKappa) = Result.TeffK(FlagKappa);
    % weird results:
    Result.FrK = 6.2e19.*Result.Rho.^(1./2) .* Result.Ts.^(9./4);  % [erg cm^-2 s^-1] [14.5.53]

    Result.Lambda = Args.Lambda;
    Nu = constant.c./Args.Lambda;
    X = constant.h.*Nu./(constant.kB.*Result.Ts);
    Result.Fnu = X.^1.5 .* exp(-X./2)./sqrt(exp(X) - 1);

    % disk divided into 3 regions: [14.5.35]
    % 1. Outer region: gas pressure dominates radiation pressure, opacity
    %    dominated by free free
    % 2. A middle region: as pressure dominates radiation pressure, opacity
    %    is mainly due to electron scattering
    % 3. An inner region: radiation pressure dominates gas pressure, opacity
    %    is mainly due to electron scattering
   
    % In the following CG\approx1
    % outer to middle radius:
    Result.R_om = RS .* 4e3.* ( (Mdot./1e17)./MassSM ).^(2./3);   % [cm] [14.5.35]
    Result.R_omDR = Result.R_om./RD;  % [disk radius units]
    % middle to inner radius:
    Result.R_mi = RS .* 80 .* Args.Alpha.^(2./21) .* MassSM.^(-2./3) .* ( Mdot./1e17).^(16./21);  % [cm]  [14.5.36] 
    Result.R_miDR = Result.R_mi./RD; % [disk radius units]

    % inner region:
    TsI = 2e9 .* Args.Alpha.^(2./9) .* MassSM.^(-10./9) * (Mdot./1e17).^(8./9) .* Rrs.^(-17./9) .* CG.^(8./9);
    FlagInner = R<Result.R_mi;
    Result.Teff(FlagInner) = TsI(FlagInner);





end
