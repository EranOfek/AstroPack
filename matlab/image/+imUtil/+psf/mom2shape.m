function [AB, El, C] = mom2shape(X2, Y2, XY)
    % Calculate shape (A, B, Theta, Elongation,...) from 2nd moments
    % Input  : - X^2 moment.
    %          - Y^2 moment.
    %          - X*Y moment.
    % Output : - Structure with:
    %            .Theta [rad]
    %            .A
    %            .B
    %          - Structure with:
    %            .Elongation = A/B
    %            .Ellipticity = 1 - B/A
    %          - Structure with:
    %            .CXX
    %            .CYY
    %            .CXY
    % Author : Eran Ofek (Jan 2022)
    % Example: [AB, El, C] = imUtil.psf.mom2shape(1,1,0);
   
    Diff2 = X2 - Y2;
    Sum2  = X2 + Y2;
    
    AB.Theta = 0.5.*atan(2.*XY./Diff2);
    AB.A     = sqrt(0.5.*Sum2 + sqrt( (0.5.*Diff2).^2 + XY.^2));
    AB.B     = sqrt(0.5.*Sum2 - sqrt( (0.5.*Diff2).^2 + XY.^2));
    
    if nargout>1
        El.Elongation  = AB.A./AB.B;
        El.Ellipticity = 1 - AB.B./AB.A;
        
        if nargout>2
            C.CXX = (cos(AB.Theta)./AB.A).^2 + (sin(AB.Theta)./AB.B).^2;
            C.CYY = (sin(AB.Theta)./AB.A).^2 + (cos(AB.Theta)./AB.B).^2;
            C.CXY = 2.*cos(AB.Theta).*sin(AB.Theta).*(1./(AB.A.^2) - 1./(AB.B.^2));
        end
    end
    
end