function [C_TauTag, TauTag] = lightTimeCorrection(U, E, Q)
    % Calculate the c\tau' parameters required for light time convergence
    % calculation. Here:
    %   u_B - The Barycentric position of the object.
    %   E_B - The Barycentric position of the earth.
    %   S_B - The Barycentric position of the Sun.
    %   E_H - The Heliocenric position of the Earth (E_B - S_B)
    % U = u_B(t-tau) - E_B(t)
    % Q = u_B(t-tau) - S_B(t-tau)  
    % For definitions and formulae, see Explanatory Supplement to the Astronomical
    % Alamanac (Seidelmann 2006), chapter 3.315, p. 148.
    % Input  : - A 3 x N matrix of U, or a 1 X N vector of |U|.
    %          - A 3 X N matrix of E_H, or a 1 X N vector of |E_H|.
    %          - A 3 X N matrix of Q, or a 1 X N vector of !Q|.
    % Output : - The c\tau' parameter.
    % Author : Eran Ofek (Sep 2021)
    % Example: C_TauTag = celestial.Kepler.lightTimeCorrection(1, 1, 2)
    
    arguments
        U          % 3XN
        E          % 3XN
        Q          % 3XN
    end
    
    Mu = 1.32712438e20;  % [m^3 s^-2]
    Mu = Mu./((constant.au./100).^3 ) .* 86400.^2;
    C  = constant.c .*86400./constant.au;  % [au/day]
    
    if size(U,1)==1
        AbsU = U;
    else
        AbsU = sum(U.^2, 1);
    end
    if size(E,1)==1
        AbsE = E;
    else
        AbsE = sum(E.^2, 1);
    end
    if size(Q,1)==1
        AbsQ = Q;
    else
        AbsQ = sum(Q.^2, 1);
    end
    
    C_TauTag = AbsU + (2.*Mu./(C.^2)).*log( (AbsE + AbsU + AbsQ)./(AbsE - AbsU + AbsQ) );
    TauTag   = C_TauTag./C;
end