function dydt=freePrecession(t,y,L,I1,I2,I3)
    % The diff. equations for triaxial rigid body free precession
    % Input  : - t
    %          - y
    %          - L
    %          - I1
    %          - I2
    %          - I3
    % Output : - dy/dt
    % Author : Eran Ofek (Feb 2023)

    arguments
        t
        y
        L  = 1;
        I1 = 1;
        I2 = 0.9;
        I3 = 0.9;
    end
    
    Theta = y(1);
    Phi   = y(2);
    Psi   = y(3);
    
    dydt    = zeros(3,1);
    dydt(3) = (1./I3 - sin(Psi).^2./I1 - cos(Psi).^2./I2).*L.*cos(Theta);
    dydt(2) = (sin(Psi).^2./I1 + cos(Psi).^2./I2).*L;
    dydt(1) = (1./I1 - 1./I2).*L.*sin(Theta).*sin(Psi).*cos(Psi);
    
end
