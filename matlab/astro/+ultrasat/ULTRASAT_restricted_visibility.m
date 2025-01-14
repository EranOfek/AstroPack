function ULTRASAT_vis=ULTRASAT_restricted_visibility(JD,Coo,Args)
% Calcualte the visibilty and compare to the limits of a list of Coo at a vecotr of JD 
% Package: ultrasat
% Input  : - a vector of julian dates
%          - Atwo column matric of Coordinate [RA,Dec]
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'CooUnits' - Units of input coordinates {'g'|'rad'|'deg'}
%                         'g' - radians or sexagesimal.
%                         'rad' - radians only.
%                         'deg' - degrees only.
%                          Defaults is 'rad'
%            'MinSunDist' - Minimum distance from the Sun, in [degrees].
%                           Default is 70.
%            'MinMoonDist' - Minimum distance from the Moon, in [degrees]
%                           Default is 34.
%            'MinEarthDist' - Minimum distance from the Earth, in [degrees]
%                           Default is 56.
%            'MinDistOffset' - Offset of the minimum distances from the Sun/Earth/Moon, in [degrees].
%                              0 for center of FoV (i.e., pointing), 7 to within the 7deg FoV (though not exact - adjust sepratly each limit and do not check together)
%                              Default is 0.
%            'Power_MaxSunDist' - Maximum distance from the Sun for positive Power balance (i.e., beyond this it is Hard Obs), in [degrees].
%                                 Default is 130 (90+45; 45 for the solar panels).
%            'Comm_MinEarthDist' - Minimum distance from the Earth allowing for real-time dowloand, in [degrees].
%                                  Default is 0 (i.e. no limit).
%            'Comm_MaxEarthDist' - Maximum distance from the Earth allowing for real-time dowloand, in [degrees].
%                                  Default is 180 (i.e. no limit).
% Output : - Visibility structure with: {'SunAngDist';'EarthAngDist';'MoonAngDist';'JD';'Coo';...
%                                        'SunRA';'SunDec';'EarthRA';'EarthDec';'MoonRA';'MoonDec';
%                                        'SunLimits';'EarthLimits';'MoonLimits';'PowerLimits';'CommLimits'}
% License: GNU general public license version 3
%     By : Yossi Shvartzvald                    updated Jan 2025
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: N1  = [220./RAD, 66./RAD];
%          S1  = [ 42./RAD,-66./RAD];
%          Coo = [N1;S1];
%          JD  = celestial.time.julday([1 1 2025 0]) + (0:0.1:365)';
%          ULTRASAT_vis=ultrasat.ULTRASAT_restricted_visibility(JD,Coo);
% Reliable: 
%--------------------------------------------------------------------------
 
    arguments
        JD
        Coo
        Args.CooUnits    ='rad';
        Args.MinSunDist  = 70; % [deg]
        Args.MinMoonDist  = 34; % [deg]
        Args.MinEarthDist  = 56; % [deg]
        Args.MinDistOffset  = 0; % [deg] 
        Args.Power_MaxSunDist   = 130; % [deg] 
        Args.Comm_MinEarthDist   = 0; % [deg] Currently (i.e. 0) means no limit
        Args.Comm_MaxEarthDist = 180; % [deg] Currently (i.e. 180) means no limit
    end

    RAD = 180./pi;
    
    switch lower(Args.CooUnits)
         case 'rad'
            % do nothing
         case 'g'
            Coo(:,1)  = celestial.coo.convertdms(Coo(:,1),'gH','r');
            Coo(:,2) = celestial.coo.convertdms(Coo(:,2),'gD','r');
         case 'deg'
            Coo   = Coo./RAD;
         otherwise
            error('Unknown Units option');
    end

    % subtract offset and convert limits to radians
    MinSunDist = (Args.MinSunDist-Args.MinDistOffset)./RAD;
    MinMoonDist = (Args.MinMoonDist-Args.MinDistOffset)./RAD;
    MinEarthDist = (Args.MinEarthDist-Args.MinDistOffset)./RAD;

    Power_MaxSunDist = Args.Power_MaxSunDist./RAD; 
    Comm_MinEarthDist = Args.Comm_MinEarthDist./RAD; 
    Comm_MaxEarthDist = Args.Comm_MaxEarthDist./RAD; 

    % retrieve visibilty and check limits
    ULTRASAT_vis = ultrasat.GEO_object_visibility(JD,Coo);

    ULTRASAT_vis.SunLimits   = ULTRASAT_vis.SunAngDist   > MinSunDist;
    ULTRASAT_vis.EarthLimits = ULTRASAT_vis.EarthAngDist > MinEarthDist;
    ULTRASAT_vis.MoonLimits  = ULTRASAT_vis.MoonAngDist  > MinMoonDist;

    ULTRASAT_vis.PowerLimits = ULTRASAT_vis.SunAngDist   < Power_MaxSunDist;
    ULTRASAT_vis.CommLimits  = ULTRASAT_vis.EarthAngDist > Comm_MinEarthDist & ...
                               ULTRASAT_vis.EarthAngDist < Comm_MaxEarthDist;

end