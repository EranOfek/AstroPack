function [Result] = altLimit2(l,r1,r2,DH)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jun) 
    % Example: tools.math.geometry.altLimit2

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
