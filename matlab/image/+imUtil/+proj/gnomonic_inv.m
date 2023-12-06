function [Lon,Lat]=gnomonic_inv(X,Y,Center,R,OutUnits)
% tangential (gnomonic) projection - spherical to planar
% Package: +imUtil.tan
% Input  : - An array of longitudes.
%          - An array of latitudes.
%          - Center projection point [Long, Lat].
%            Default is [0 0].
%          - A scalar that represents the scale of X/Y.
%            For example, 180./pi means that X/Y are in units of deg
%            per unit-pix.
%            1, means that X/Y are in radians.
%            Default is 180./pi;
%          - A scalr that controls the units of the output coordinates
%            relative to degrees.
%            1 - will output the X,Y coordinates in degrees (i.e., at the
%            limit of small distances, unit distance equal 1 deg).
%            3600 - will output the X,Y coordinates in arcsec...
%            Default is 1.
%          - Input coordinates and Center units.
%            Default is 'deg'.
% Output : - X coordinates.
%          - Y coordinates.
%      By: Eran O. Ofek                         May 2020
% Example: Lon = rand(10,1); Lat=rand(10,1);
%          [X,Y]=imUtil.proj.gnomonic(Lon,Lat,[0.5 0.5],1,'deg')
%          [Lon1,Lat1]=imUtil.proj.gnomonic_inv(X,Y,[0.5 0.5],1,'deg')

if nargin<5
    OutUnits = 'deg';
    if nargin<4
        R = 180./pi;
    end
end

RAD = 180./pi;

% R is scale [rad/unit-pix]
X = X./R;
Y = Y./R;
%Center = Center./R;   % bug

Rho   = sqrt(X.^2+Y.^2);
C     = atan(Rho);

Lon0 = Center(1);
Lat0 = Center(2);

Lat  = asin(cos(C).*sin(Lat0) + Y.*sin(C).*cos(Lat0)./Rho);
Lon  = Lon0 + atan(X.*sin(C)./(Rho.*cos(Lat0).*cos(C) - Y.*sin(Lat0).*sin(C)));

Lat(Rho==0) = Lat0;
Lon(Rho==0) = Lon0;


ConvFactor = convert.angular('rad',OutUnits);
Lon     = Lon.*ConvFactor;
Lat     = Lat.*ConvFactor;

