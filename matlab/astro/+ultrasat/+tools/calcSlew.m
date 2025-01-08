function [T_sec,DirectSlewBool] = calcSlew(RA_1,Dec_1,RA_2,Dec_2,Args)
% Calculate the ULTRASAT slew time between two coordinates. 
% Package: ultrasat.tools
% Description: 
% Input  : - Start point RA.
%          - Start point Dec.
%          - End point RA.
%          - End point Dec.
%       * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'Units' - Units of input coordinates {'g'|'rad'|'deg'}
%                      'g' - radians or sexagesimal.
%                      'rad' - radians only.
%                      'deg' - degrees only.
%                      Defaults is 'rad'
%            'CheckTrajectory' - Boolean, check if the trajectory violates
%                                the Sun/Earth/Moon minimum distance limits
%                                during slew.
%                                Defaults 'false'.
%            'TrajStepDeg' - Step size in 'deg' of trajectory to check.
%                            Default 1.
%            'TrajStepDeg' - Step size in 'deg' of trajectory to check.
%                            Default 1.
%            'JD' - Julian day of the slew. Arbitrary default.
%            'SunDistSlew' - minimum angluar distance in 'Deg' from the Sun
%                            during slew. Default 70.
%            'MoonDistSlew' - minimum angluar distance in 'Deg' from the
%                             Moon during slew. Default 14.
%            'EarthDistSlew' - minimum angluar distance in 'Deg' from the
%                              Earth during slew. Default 14.
% Output : - Slew time in [sec]
%          - Boolean if direct slew trajectory (false indicates
%            Sun/Moon/Earth minimum distance required indirect trajectroy)
% License: GNU general public license version 3
%     By : Yossi Shvartzvald                    Dec 2024
% Example:ultrasat.tools.
% [T_sec,DirectSlew] = ultrasat.tools.calcSlew(0,0,-pi,0);
% [T_sec,DirectSlew] = ultrasat.tools.calcSlew(0,0,-pi,0,'CheckTrajectory',true);
%
    arguments
        RA_1
        Dec_1
        RA_2
        Dec_2
        Args.Units              =   'rad';
        Args.CheckTrajectory    =   false;
        Args.TrajStepDeg        =   1; %Deg
        Args.JD                 =   celestial.time.julday('2028-01-01T00:00:00');
        Args.SunDistSlew        =   70;   % [deg]
        Args.MoonDistSlew       =   14;   % [deg]
        Args.EarthDistSlew      =   14;   % [deg]
    end

    RAD = 180./pi;
    
    switch lower(Args.Units)
         case 'rad'
            % do nothing
         case 'g'
            RA_1  = celestial.coo.convertdms(RA_1,'gH','r');
            Dec_1 = celestial.coo.convertdms(Dec_1,'gD','r');
            RA_2  = celestial.coo.convertdms(RA_2,'gH','r');
            Dec_2 = celestial.coo.convertdms(Dec_2,'gD','r');
         case 'deg'
            RA_1   = RA_1./RAD;
            Dec_1  = Dec_1./RAD;
            RA_2   = RA_2./RAD;
            Dec_2  = Dec_2./RAD;
         otherwise
            error('Unknown Units option');
    end
    
    
    % calcuate the distance
    [Dist,PA]=celestial.coo.sphere_dist(RA_1,Dec_1,RA_2,Dec_2,'rad'); % all in radians
    
    DirectSlewBool = true;
    if Args.CheckTrajectory
        [TrajCoo(:,2),TrajCoo(:,1)]  = reckon(Dec_1*RAD,RA_1*RAD,(0:Args.TrajStepDeg:Dist*RAD),PA*RAD);        
        F = TrajCoo(:,1)<0;
        TrajCoo(F,1) = TrajCoo(F,1) + 360.;
        
        Vis    = ultrasat.ULTRASAT_restricted_visibility(Args.JD, TrajCoo./RAD,...
                'MinSunDist',Args.SunDistSlew/RAD,'MinMoonDist',Args.MoonDistSlew/RAD,'MinEarthDist',Args.EarthDistSlew/RAD);
            
        DirectSlewBool = all(Vis.SunLimits .* Vis.MoonLimits .* Vis.EarthLimits);
    end
    
    Dist_deg = Dist*RAD;
    % calc the slew time
    if Dist_deg<10
        T_sec = -0.002093*Dist_deg.^4 + 0.06241*Dist_deg.^3 - 0.7613*Dist_deg.^2 + 6.633*Dist_deg + 11.22;
    elseif Dist_deg<36
        T_sec = 0.0003436*Dist_deg.^3 - 0.03864*Dist_deg.^2 + 2.482*Dist_deg + 21.75;
    else
        if DirectSlewBool
            T_sec = 1*Dist_deg + 41;
        else
            T_sec = 1*Dist_deg + 77;
        end
    end
end
