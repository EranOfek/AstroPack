function Roll = expectedRoll(RA,Dec,JD,Args)
% Calculate ULTRASAT's expected roll (aka position angle) for a given sky coordinates at time JD.  slew time between two coordinates. 
% Package: ultrasat.tool
% Description: 
% Input  : - Target RA.
%          - Target Dec.
%          - Julian day
%          - End point Dec.
%       * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'InUnits' - Units of input coordinates {'g'|'rad'|'deg'}
%                      'g' - radians or sexagesimal.
%                      'rad' - radians only.
%                      'deg' - degrees only.
%                      Defaults is 'deg'
%            'OutUnits' -- Units of input coordinates {'g'|'rad'|'deg'}
%                      'g' - radians or sexagesimal.
%                      'rad' - radians only.
%                      'deg' - degrees only.
%                      Defaults is 'deg'
% Output : - Roll
% License: GNU general public license version 3
%     By : Yossi Shvartzvald                    Jan 2025
% Example:Roll =  ultrasat.tools.expectedRoll(0,0,juliandate(datetime('now')));
%
    arguments
        RA
        Dec
        JD 
        Args.InUnits              =   'deg';
        Args.OutUnits              =   'deg';        
    end

    RAD = 180./pi;
    
    switch lower(Args.InUnits)
         case 'rad'
            % do nothing
         case 'g'
            RA  = celestial.coo.convertdms(RA,'gH','r');
            Dec = celestial.coo.convertdms(Dec,'gD','r');
         case 'deg'
            RA   = RA./RAD;
            Dec  = Dec./RAD;
         otherwise
            error('Unknown Units option');
    end
    
    [SunRA,SunDec]     = celestial.SolarSys.suncoo(JD,'j');
    
    [~,Roll_westward,~] = celestial.coo.sphere_dist_fast(RA,Dec,SunRA,SunDec);
    
    Roll =     2*pi-Roll_westward; 
    
    switch lower(Args.OutUnits)
         case 'rad'
            % do nothing
         case 'g'
            Roll  = celestial.coo.convertdms(Roll,'r','gH');
         case 'deg'
            Roll   = Roll.*RAD;
         otherwise
            error('Unknown Units option');
    end
    
end
