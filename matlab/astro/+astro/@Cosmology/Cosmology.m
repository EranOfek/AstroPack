% Cosmology class - 

classdef Cosmology < Component
    % Parent class for all components

    % Properties
    properties (SetAccess = public)
        H0          = [];
        OmegaK      = [];
        OmegaM      = [];
        OmegaL      = [];
        OmegaB      = [];
        OmegaRad    = [];
        Tau         = [];
        W           = [];
        Sigma8      = [];
        Ns          = [];
        Age         = [];
        OmegaM_H2   = [];
        OmegaB_H2   = [];
        
        ErrH0          = [];
        ErrOmegaK      = [];
        ErrOmegaM      = [];
        ErrOmegaL      = [];
        ErrOmegaB      = [];
        ErrOmegaRad    = [];
        ErrTau         = [];
        ErrW           = [];
        ErrSigma8      = [];
        ErrNs          = [];
        ErrAge         = [];
        ErrOmegaM_H2   = [];
        ErrOmegaB_H2   = [];
        
    end

    %--------------------------------------------------------
    methods % Constructor

        function Obj = Cosmology(Name)
            % Constructor for the Cosmology class
            % Input  : - Cosmology  name.
            %            Default is 'planck2015'
            % Output : - An astro.Cosmology objcect.
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Name = 'planck2015';
            end
            
            if isempty(Name)
                Obj.H0 = [];
            else
                Obj = astro.Cosmology.parameters(Name);
            end
            
        end
    end


    methods (Static)
        function Obj=parameters(Name)
            % Create and populate an astro.Cosmology object with cosmoilogical parameters
            % Input  : - Name: 'wmap3'|...|'planck2015'
            % Output : - An astro.Cosomology object.
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Name = 'planck2015';
            end
            
            Obj = astro.Cosmology([]);
            
            switch lower(Name)
                case 'wmap3'
                    %------------------------------------------
                    % WMAP + SNLS colsmological parameters
                    % Spergel et al. (2007; astro-ph/0603449)
                    %------------------------------------------

                    % Hubble parameter
                    Obj.H0         = 70.4;
                    Obj.ErrH0      = [-1.6 +1.5];
          
                    Obj.OmegaM     = 0.268;
                    Obj.ErrOmegaM  = [-0.018 +0.018];

                    Obj.OmegaB     = 0.03105;
                    Obj.ErrOmegaB  = [NaN NaN];

                    Obj.OmegaK     = 0.986;
                    Obj.ErrOmegaK  = [-0.017 +0.017];

                    Obj.OmegaL     = 0.716;
                    Obj.ErrOmegaL  = [-0.055 +0.055];

                    % Omega radiation
                    Obj.OmegaRad    = 0.0;
                    Obj.ErrOmegaRad = [-0.0 0.0];

                    % Equation of state (quintessence)
                    Obj.W            = -1; %OmegaM_H2    = 0.1324;
                    Obj.ErrW         = [-0.0 0.0];

                    % OmegaM x h^2
                    Obj.OmegaM_H2    = 0.1324;
                    Obj.ErrOmegaM_H2 = [-0.0041 +0.0042];

                    % OmegaB x h^2
                    Obj.OmegaB_H2    = 0.02186;
                    Obj.ErrOmegaB_H2 = [-0.00068 +0.00068];

                    % Optical depth
                    Obj.Tau        = 0.073;
                    Obj.ErrTau     = [-0.028 +0.027];

                    % Power-law slope
                    Obj.Ns         = 0.947;
                    Obj.ErrNs      = [-0.015 +0.015];

                    % sigma8
                    Obj.Sigma8     = 0.776;
                    Obj.ErrSigma8  = [-0.032 +0.031];

                case 'wmap5'
                    %--------------------------------------------
                    % WMAP5 + SNLS + BO colsmological parameters
                    % Komatsu et al. (2008; astro-ph/0803.0547)
                    %--------------------------------------------

                    % Hubble parameter
                    Obj.H0         = 70.1;
                    Obj.ErrH0      = [-1.3 +1.3];

                    Obj.OmegaM     =   0.268;
                    Obj.ErrOmegaM  = [-0.018 +0.018];

                    Obj.OmegaB     = 0.0462;
                    Obj.ErrOmegaB  = [-0.0015 +0.0015];

                    Obj.OmegaK     =  1;
                    Obj.ErrOmegaK  = [NaN NaN];

                    Obj.OmegaL     = 0.721;
                    Obj.ErrOmegaL  = [-0.015 +0.015];

                    % Omega radiation
                    Obj.OmegaRad    = 0.0;
                    Obj.ErrOmegaRad = [-0.0 0.0];

                    % Equation of state (quintessence)
                    Obj.W            = -1;
                    Obj.ErrW         = [-0.0 0.0];

                    % OmegaM x h^2
                    Obj.OmegaM_H2    = 0.1369;
                    Obj.ErrOmegaM_H2 = [-0.0037 +0.0037];

                    % OmegaB x h^2
                    Obj.OmegaB_H2    = 0.02265;
                    Obj.ErrOmegaB_H2 = [-0.00059 +0.00059];

                    % Optical depth
                    Obj.Tau        = 0.084;
                    Obj.ErrTau     = [-0.016 +0.016];

                    % Power-law slope
                    Obj.Ns         = 0.960;
                    Obj.ErrNs      = [-0.013 +0.014];

                    % sigma8
                    Obj.Sigma8     = 0.817;
                    Obj.ErrSigma8  = [-0.016 +0.016];

                    % Age
                    Obj.Age        = 13.73;          % [Gyr]
                    Obj.ErrAge     = [-0.12 0.12];   % [Gyr]

                case 'wmap9'
                    %--------------------------------------------
                    % WMAP9 + BAO + H0 colsmological parameters
                    % Hinshaw et al. (2013)
                    %--------------------------------------------

                    % Hubble parameter
                    Obj.H0         = 69.33;
                    Obj.ErrH0      = [-0.88 +0.88];
                    Obj.OmegaM     =   0.2408+0.0471;
                    Obj.ErrOmegaM  = [-0.009 +0.009];

                    Obj.OmegaB     = 0.0471;
                    Obj.ErrOmegaB  = [-0.001 +0.001];

                    Obj.OmegaK     =  1;
                    Obj.ErrOmegaK  = [NaN NaN];

                    Obj.OmegaL     = 1- 0.2408-0.0471;
                    Obj.ErrOmegaL  = [-0.009 +0.009];

                    % Omega radiation
                    Obj.OmegaRad    = 0.0;
                    Obj.ErrOmegaRad = [-0.0 0.0];

                    % Equation of state (quintessence)
                    Obj.W            = -1;
                    Obj.ErrW         = [-0.0 0.0];

                    % OmegaM x h^2
                    %Obj.OmegaM_H2    = 0.1369;
                    %Obj.ErrOmegaM_H2 = [-0.0037 +0.0037];

                    % OmegaB x h^2
                    Obj.OmegaB_H2    = 0.02266;
                    Obj.ErrOmegaB_H2 = [-0.00043 +0.00043];

                    % Optical depth
                    Obj.Tau        = 0.088;
                    Obj.ErrTau     = [-0.013 +0.013];

                    % Power-law slope
                    Obj.Ns         = 0.971;
                    Obj.ErrNs      = [-0.01 +0.01];

                    % sigma8
                    Obj.Sigma8     = 0.817;
                    Obj.ErrSigma8  = [-0.016 +0.016];

                    % Age
                    Obj.Age        = 13.75;          % [Gyr]
                    Obj.ErrAge     = [-0.085 0.085];   % [Gyr]

                case 'planck2015'
                    % Planck full mission cosmological parameters
                    % https://arxiv.org/abs/1502.01589
                    % TT, TE, EE+low P, +lensing+ext

                    Obj.H0 = 67.74;
                    Obj.ErrH0 = [0.46 0.46];

                    Obj.OmegaM = 0.3089;
                    Obj.ErrOmegaM = [0.0062 0.0062];

                    Obj.OmegaRad = 0;

                    Obj.OmegaL = 0.6911;
                    Obj.ErrOmegaL = [0.0062 0.0062];

                    Obj.OmegaK     =  0;
                    Obj.ErrOmegaK  = [NaN NaN];
                    
                    Obj.OmegaB_H2  = 0.02230;
                    Obj.ErrOmegaB_H2 = [0.00014 0.00014];

                    Obj.Sigma8 = 0.8159;
                    Obj.ErrSigma8 = [0.0086 0.0086];

                    %Obj.z_re = 8.8;
                    %Obj.Errz_re = [1.1 1.2];

                    Obj.Age = 13.799;
                    Obj.ErrAge = [0.021 0.021];

                otherwise
                    error('Unknown ParType option');
            end
            
            
        end
        
    end

    methods  % cosmological functions
        function Result = e_z(Obj, Z)
            % Calculate E(z) cosmological function
            % Description: Calculate E(z) cosmological function, which is
            %              proportional to the time derivative of the logarithm
            %              of the scale factor.
            % Input  : - Redshift.
            %          - Cosmological parameters vector:
            %            [Omega_M Omega_Lambda Omega_Radiation],
            %            default for Omega_radiation is 0.
            % Output : - E(z).
            % Author : Eran Ofek (Apr 2023)
            % Example: C.e_z(rand(3,3));

            Result = 1./sqrt(Obj.OmegaRad.*(1+Z).^4 + Obj.OmegaM.*(1+Z).^3 + Obj.OmegaK.*(1+Z).^2 + Obj.OmegaL);
        end
        
        function Result = ad_dist(Obj, Z1, Z2, Args)
            % Calculate the filled beam angular diameter distance between two redshifts
            % Description: Calculate the filled beam angular diameter distance
            %              between two redshifts along the line of sight.
            %              Use the cosmological parameters indicated by the
            %              class.
            % Input  : - An astro.Cosmology object.
            %          - A vector of z1 (or end of interation).
            %          - A vector of z2. If empty, then integrate from z=0
            %            to the z indicated in the previous argument.
            %            Default is [].
            %          * ...,key,val,...
            %            'OutUnits' - Output units. Default is 'pc'.
            %            'AbsTol' - Integration tol. Default is 1e-4.
            % Output : - Angular diameter distance (in OutUnits units).
            % Author : Eran Ofek (Apr 2023)
            
            arguments
                Obj
                Z1
                Z2             = [];
                Args.OutUnits  = 'pc';
                Args.AbsTol    = 1e-4;
            end
            
            if isempty(Z2)
                Z2 = Z1;
                Z1 = 0;
            end
            
            Nz1 = numel(Z1);
            Nz2 = numel(Z2);
            
            Nz  = max(Nz1, Nz2);
            Z1  = Z1(:).*ones(Nz,1);
            Z2  = Z2(:).*ones(Nz,1);

            H0 = Obj.H0.*100000./(constant.pc.*1e6); % convert H0 to cgs
            R0 = constant.c./H0;  % cm.

            IntVal = zeros(Nz,1);
            for I=1:1:Nz
               % integrate inv_e_z from z1 to z2
               IntVal(I) = integral(@Obj.e_z, Z1(I), Z2(I), 'AbsTol',Args.AbsTol); %,Tol,[],CosmoPars(2:end));
            end

            OmegaTot  = Obj.OmegaM + Obj.OmegaL + Obj.OmegaRad;

            if (OmegaTot>1)
                % k=+1   close
                Chi12 = sqrt(abs(OmegaTot - 1)).*IntVal;
                Dist  = R0.*sin(Chi12)./((1+Z2).*sqrt(OmegaTot-1));
            elseif (OmegaTot<1)
                % k=-1   open
                Chi12 = sqrt(abs(OmegaTot - 1)).*IntVal;
                Dist  = R0.*sinh(Chi12)./((1+Z2).*sqrt(1-OmegaTot));
            else
                % k=0    flat
                Chi12 = IntVal;
                Dist  = R0.*Chi12./(1+Z2);
            end

            % convert cm to OutUnits
            Result = convert.length('cm', Args.OutUnits, Dist);
        end
        
        function [DL, DM] = lum_dist(Obj, Z, Args)
            % Calculate luminosity distance and distance modulus.
            % Description: Compute luminosity distance from redshift and cosmological
            %              parameters.
            % Input  : - An astro.Cosmology object.
            %          - Array of redshifts.
            %          * ...,key,val,...
            %            'OutUnits' - Output units of distance.
            %                   Default is 'pc'.
            %            'AbsTol' - Integration abs. tol. for convergence.
            %                   Default is 1e-4.
            % Output : - Luminosity distance (default in pc).
            %          - Distance modulus [mag].
            % Reference : Perlmutter et al. 1997 ApJ, 483, 565
            %             Oke & Sandage 1968 ApJ, 154, 21
            %             Peterson, B.M., 1997, AGN, p.165
            % Author : Eran Ofek (Jul 2001)
            % Example: C=astro.Cosmology;
            %          [DL, DM]=C.lum_dist([2 1])
            
            arguments
                Obj
                Z
                Args.OutUnits  = 'pc';
                Args.AbsTol    = 1e-4;
            end
           
            C  = constant.c;    % speed of light [cm/s]
            Pc = constant.pc;   % Parsec [cm]

            H0       = Obj.H0;
            OmegaM   = Obj.OmegaM;
            OmegaL   = Obj.OmegaL;
            OmegaRad = Obj.OmegaRad;

            % convert H0 to cm/sec/sec
            H0 = H0.*100000./(Pc.*1e6);


            % where is OmegaR?
            if ((OmegaM+OmegaL)>1)
               %Lx = inline('sin(x)','x');
               Lx = @(x) sin(x);
               K  = 1 - OmegaM - OmegaL;
            elseif ((OmegaM+OmegaL)<1)
               %Lx = inline('sinh(x)','x');
               Lx = @(x) sinh(x);
               K  = 1 - OmegaM - OmegaL;
            else
               % OmegaM + OmegaL == 1
               %Lx = inline('x','x');
               Lx = @(x) x;
               K  = 1;
            end

            N  = numel(Z);
            DL = zeros(size(Z));
            for I=1:1:N
               ZI = Z(I);

               %Int = inline('((1+ZI).^2.*(1+OmegaM.*ZI) - ZI.*(2+ZI).*OmegaL).^(-1./2)','ZI','OmegaM','OmegaL');
               Int = @(ZI) ((1+ZI).^2.*(1+OmegaM.*ZI) - ZI.*(2+ZI).*OmegaL).^(-1./2);

               %DL_Int = quad(Int,0,ZI,[],[],OmegaM,OmegaL);
               DL_Int = integral(Int,0,ZI, 'AbsTol',Args.AbsTol);

               DL(I) = (C.*(1+ZI)./(H0.*sqrt(abs(K)))).*Lx(sqrt(abs(K)).*DL_Int);

            end

            % luminosity distance in Parsecs:
            DL = convert.length('cm',Args.OutUnits, DL);

            % distance modulus:
            if (nargout>1)
               DM = 5.*log10(DL./10);
            end
            
        end
        
        function T = lookback_time(Obj, Z1, Z2)
            % Compute the cosmological lookback time between z1 and z2
            % Description: Compute the cosmological lookback time, between two events
            %              in redshift z1 and z2, and given the cosmology.
            %              (Assuming matter dominated universe - Z<1000).
            % Input  : - An astro.Cosmology object.
            %          - An array of z1
            %          - An array of z2. If empty, thenn z1=0 and the
            %            z1 argument specify z2.
            % Output : - Lookback time [seconds].
            % Reference : Lahav et al. 1991, MNRAS, 251, 128
            % Author : Eran Ofek (Jul 2001)
            % Example: C=astro.Cosmology;
            %          C.lookback_time([2 1])
            %          C.lookback_time([0.5 0.5],[1 2])
           
            arguments
                Obj
                Z1
                Z2     = [];
            end
            
            C  = constant.c;       % speed of light [cm/sec]
            Pc = constant.pc;      % Parsec [cm]
            
            H0       = Obj.H0;
            OmegaM   = Obj.OmegaM;
            OmegaL   = Obj.OmegaL;
            OmegaRad = Obj.OmegaRad;

            % convert H0 to cm/sec/sec
            H0_cgs = H0.*100000./(Pc.*1e6);

            if isempty(Z2) 
                Z2 = Z1;
                Z1 = zeros(size(Z2));
            end

            A_high = 1./(1+Z1);
            A_low  = 1./(1+Z2);

            % lookback time integrand (still need to divide by H0)
            %lookback_fun = inline('sqrt(A./(OmegaL.*A.^3 + (1-OmegaM-OmegaL).*A + OmegaM))','A','OmegaM','OmegaL');
            lookback_fun = @(A,OmegaM,OmegaL) sqrt(A./(OmegaL.*A.^3 + (1-OmegaM-OmegaL).*A + OmegaM));

            T = zeros(size(Z1));
            for I=1:1:numel(T)
               T(I) = quadl(lookback_fun,A_low(I),A_high(I),[],[] ,OmegaM, OmegaL);
               T(I) = T(I)./H0_cgs;
            end
                        
        end
        
    end

    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Component class
    end
end
