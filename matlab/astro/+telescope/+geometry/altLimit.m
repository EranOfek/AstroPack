function [Beta] = altLimit(l,H1,H2,R)
    % Calculate altitude limit for telescope located at distance l from a wall.
    %     
    % Input  : - Distance between telescope center of motion and wall.
    %          - Height of telescope center of motion.
    %          - Height of obstraction/wall.
    %          - Radius (distance) from center of motion and telescope edge
    %            perpendicular to optical axis.
    % Output : - Alt limit for obstraction [deg].
    % Author : Eran Ofek (2024 Jun) 
    % Example: [Beta] = telescope.geometry.altLimit(l,H1,H2,R)

    arguments
        l   = 135+205; %185;  % 135
        H1  = 120; %165; %120;
        H2  = 220; %346; %250; %346; %120;
        R   = 58; %50; %65; %50;
    end

    RAD = 180./pi;
    Phi = atan((H2-H1)./l);
    lt  = l./cos(Phi);
    Alpha = asin(R./lt);
    Beta = Phi + Alpha;
    Beta = Beta.*RAD;

    % Based on a formula I gave Ofer:
    % d = 135 
    % distance from unit to wall
    % CG_height = 120 
    % wall_height = 220
    % R=R_mirror
    % H=wall_height-CG_height
    % R=50;
    % obs_angle = real(-log(-(R - (- H^2 + R^2 - d^2)^(1/2))/(H + d*1i))*1i) * RAD
    
    
end
