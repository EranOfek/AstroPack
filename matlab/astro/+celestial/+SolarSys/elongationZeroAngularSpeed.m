function [Elon0, V_pElon] = elongationZeroAngularSpeed(A_Obj,A_Earth, Elon)
    % Estimate the elongation (ang. dist. from Sun on ecliptic) of zero ang. speed points.
    %   Assuming circular orbit for object and observer.
    % Input  : - Scalar semi major axis of object [au] assuming circular orbit.
    %          - Scalar semi major axis of t=observer [au]. Default is 1.
    %          - Optional elongation [deg] in which to evaluate projected
    %            velocity. Default is 90.
    % Output : - Elonagtion (ang. dist. from Sun on ecliptic) of zero ang. speed points.
    % Author : Eran Ofek (Jul 2022)
    % Example: celestial.SolarSys.elongationZeroAngularSpeed(10)
   
    arguments
        A_Obj
        A_Earth    = 1;
        Elon       = 90;
    end
    
    ResObj  = celestial.Kepler.kepler3law(constant.SunM, 'a', A_Obj.*constant.au);
    V_Obj   = ResObj.v;
    
    ResEar  = celestial.Kepler.kepler3law(constant.SunM, 'a', A_Earth.*constant.au);
    V_Earth = ResEar.v;
    
    
    E = (10:1:100).';
    
    V_p = V_Earth.*sind(E - 90) + V_Obj.*cos(  asin(A_Earth./A_Obj) .* sind(E) );
    %plot(E, V_p)
    
    Elon0 = interp1(V_p, E, 0, 'cubic');
    
    V_pElon = V_Earth.*sind(Elon - 90) + V_Obj.*cos(  asin(A_Earth./A_Obj) .* sind(Elon) );
    
   
end
