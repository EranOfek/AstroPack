function [SoftMaxSunAngDist,isHard,Hard_dur_min] = PowerLimits(t,Args)
% Calc the maximum angle for positive power balance with solar panels
% (i.e., soft). Optionanly, return the maximum duration of a HardToO to 
% specfic coordinates in minutes
% Package: ultrasat
% Input  : - vector of datetimes to check
%           * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'coo' - coordinates of targets 
% Output :  - vector of maximum allow angle
%           - boolean if Hard
%           - duration of hardTOO [minutes]
% License: GNU general public license version 3
%     By : Yossi Shvartzvald                    updated May 2026
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: 
%           N1  = [220./RAD, 0./RAD]; S1  = [ 42./RAD,-66./RAD];Coo = [N1;S1];
%           t = datetime('20-Mar-2031 05:03:02');t2 = datetime('15-Dec-2039 05:25:02');
%           [SoftMaxSunAngDist,isHard,Hard_dur_min] = ultrasat.PowerLimits([t t t2],'Coo',Coo(1,:))
%           [SoftMaxSunAngDist,isHard,Hard_dur_min] = ultrasat.PowerLimits(t,'Coo',Coo)
%           [SoftMaxSunAngDist,isHard,Hard_dur_min] = ultrasat.PowerLimits(t2,'Coo',Coo)
% Reliable: 
%--------------------------------------------------------------------------
 
    arguments
        t datetime

        Args.DOD    =  0; % defult is full battery.
        Args.Coo    =  []; % coo to calcualt Hard duration

        Args.maxAng = 71+66; % Maximum soft angle(-> in Winter, correspond to minimal distance)
        Args.minAng = 71+60; % Minimum soft angle(-> in Summer, correspond to maximal distance)

        Args.base_dur_min = 180; % All calculation relative to this value
        
        Args.maxDOD = 0.8; % max allowed dod for which base_dur_min was calcualted for zero sun on solar panels


    end
    RAD = 180./pi;

    perihelion = 0.9833; % minimum Sun-Earth distance in AU
    aphelion   = 1.0167; % maximum Sun-Earth distance in AU

    % get Sun-Earth distance at t
    [SunRA,SunDec,SunDist,~,~]=celestial.SolarSys.suncoo(juliandate(t),'j');

    SoftMaxSunAngDist = (SunDist.^2-perihelion^2)*(Args.maxAng-Args.minAng)/(aphelion^2-perihelion^2)...
                        +Args.minAng;

    isHard = false;
    Hard_dur_min = [];

    if ~isempty(Args.Coo)
        SunAngDist = celestial.coo.sphere_dist_fast(Args.Coo(:,1)',Args.Coo(:,2)',SunRA,SunDec);
        isHard = (SunAngDist.*RAD)>SoftMaxSunAngDist;

        [~,~,eclipse_dur] = ultrasat.Eclipse_times('Times',t);
        if isempty(eclipse_dur)
            eclipse_dur = zeros(size(t));
        else
            eclipse_dur = minutes(eclipse_dur);
        end

        Hard_dur_min = inf(size(isHard));
        Hard_dur_min(isHard) = (Args.base_dur_min - eclipse_dur(isHard))  .* ...
                               (Args.maxDOD - Args.DOD)/Args.maxDOD .*...
                               (cos(SoftMaxSunAngDist(isHard)/RAD-pi/2) - cos(pi/2) )./...
                               (cos(SoftMaxSunAngDist(isHard)/RAD-pi/2) - cos(SunAngDist(isHard)-pi/2)  )...
                               + eclipse_dur(isHard).*(Args.maxDOD - Args.DOD)/Args.maxDOD;

    end


end