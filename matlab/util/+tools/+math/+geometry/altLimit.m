function [Beta] = altLimit(l,H1,H2,R)
    % Calculate altitude limit for telescope located at distance l from a wall.
    %     
    % Input  : - Distance between telescope center of motion and wall.
    %          - Height of telescope center of motion.
    %          - Height of obstraction/wall.
    %          - Radius (distance) from center of motion and telescope edge
    %            perpendicular to optical axis.
    % Output : - Alt limit for obstraction [rad].
    % Author : Eran Ofek (2024 Jun) 
    % Example: [Beta] = tools.math.geometry.altLimit(l,H1,H2,R)

    arguments
        l   = 135+205; %185;  % 135
        H1  = 120; %165; %120;
        H2  = 220; %346; %250; %346; %120;
        R   = 58; %50; %65; %50;
    end

    Phi = atan((H2-H1)./l);
    lt  = l./cos(Phi);
    Alpha = asin(R./lt);
    Beta = Phi + Alpha;


end
