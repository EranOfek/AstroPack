function Result = satellite_ang_speed(H, Alt, Args)
    % Estimate satellite angular speed as a function of Alt and H
    % Input  : - Sat. Height [km]. Default is 500.
    %          - Sat. Alt. [deg]. Default is 45.
    %          * ...,key,val,...
    %            'EarthR' - Earth radius [km]. Default is 6371.
    %            'Vel' - sat. velocity [km/s]. Default is 7.7.
    % Output : - A structure with the following fields:
    %            .Dist - dist. to sat. [km].
    %            .Gamma - Vel. projected angle [deg].
    %            .AngSpeed -Sat, ang. speed [deg/s].
    % Author : Eran Ofek (Jul 2023)
    % Example: Result = celestial.earth.satellite_ang_speed
    
    
    arguments
        H    = 500;            % km
        Alt  = 45;
        Args.EarthR   = 6371;  % km
        Args.Vel      = 7.7;   % km/s
    end
    RAD = 180./pi;
    
    Gamma = 90 - asind(Args.EarthR.*sind(Alt+90)./(Args.EarthR + H));
    
    A = 1;
    B = -2.*Args.EarthR.*cosd(Alt + 90);
    C = Args.EarthR.^2 - (Args.EarthR+H).^2;
    
    Dist  = (-B + sqrt(B.^2 - 4.*A.*C))./(2.*A);
    
    Result.Dist  = Dist;
    Result.Gamma = Gamma;
    Result.AngSpeed = Args.Vel.*cosd(Gamma)./Result.Dist .*RAD;  % deg/s
    
end
