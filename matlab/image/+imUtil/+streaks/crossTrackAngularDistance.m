function [ct,at1,at2,ca] = crossTrackAngularDistance(RA,Dec,RAp,Decp)
% compute the angular distances of the point(s) [Rap,Decp] from the unit great
% circle defined by the two points RA,Dec
% returns: ct - cross track distance (angle on the great circle
%                   perpendicular to the base great circle)
%          at1,2 - along track distances (angle on the same base great
%                  circle, fom the first and second base point)
%          ca    - angle between the two great circles defined by the base
%                  and the first two given points
% angles are given in radians
% Author: Enrico Segre, november 2026
% formulae adapted from https://via-technology.aero/navigation/spherical-vector-geometry/
arguments
    RA (1,2) double
    Dec (1,2) double
    RAp double
    Decp double
end

v=[cos(RA).*cos(Dec); sin(RA).*cos(Dec); sin(Dec)];

% pole vector
c=cross(v(:,1),v(:,2));
pole=c/sqrt(c'*c);

% maybe this could be vectorized
ct=NaN(size(RAp));
at1=ct;
at2=at1;
vp=[cos(RAp).*cos(Decp); sin(RAp).*cos(Decp); sin(Decp)];
for i=1:numel(RAp)
    dp=vp(:,i)'*pole;
    ct(i)=asin(dp);
    pplane=vp(:,i)-dp*pole;
    xp=pplane/sqrt(pplane'*pplane);
    at1(i)=asin(cross(v(:,1),xp)'*pole);
    at2(i)=asin(cross(v(:,2),xp)'*pole);
end

if numel(RAp)>=2
    cp=cross(vp(:,1),vp(:,2));
    polep=cp/sqrt(cp'*cp);
    pp=cross(pole,polep);
    ca=asin(sqrt(pp'*pp));
else
    ca=NaN;
end

