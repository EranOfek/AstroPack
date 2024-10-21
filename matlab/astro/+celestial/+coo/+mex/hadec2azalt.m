% Convert HA/Dec to Az/Alt (fast mex version)
%   See also: celestial.coo.hadec2azalt celestial.coo.ha2alt
% Package: +celestial.coo
% Input  : - HA [rad]
%          - Dec [rad]
%          - Lat [rad]
% Output : - Az [same as input]
%          - Alt [same as input]
% Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS
% -fopenmp" hadec2azalt.cpp
% Author : Eran Ofek (Oct 2024)
% Example: [Az,Alt]=celestial.coo.mex.hadec2azalt(HA,Dec,Lat)