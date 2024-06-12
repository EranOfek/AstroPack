function [Result] = altLimit2(l,r1,r2,DH)
    % Calculate the altitude limit of two telescopic system obstracting one another.
    %     
    % Input  : - Distance between telescopes.
    %          - Radius (distance) of mirror edge, of 1st telescope, from the pointing axis
    %            that crosses the center of motion.
    %          - Radius (distance) of mirror edge, of 2nd telescope, from the pointing axis
    %            that crosses the center of motion.
    %          - Hight of telescope 2 center of motion relative to
    %            telescope 1. Default is 0.
    % Output : - Altitude limit for 1st telescope [deg].
    % Author : Eran Ofek (2024 Jun) 
    % Example: telescope.geometry.altLimit2

    arguments
        l    = 205; %325; %205;
        r1   = 52.5; %75; %55; %75;
        r2   = 52.5; %75; %55; %75;
        DH   = 0;
    end

    alpha = (0:1:90)';
    A = atand((r1.*cosd(alpha) + r2.*sind(90-alpha) + DH)./( l - r1.*sind(alpha) -r2.*cosd(90-alpha)  ));
    Z = tools.find.find_local_zeros(alpha, A-alpha);
    Result = Z(:,1);
end
