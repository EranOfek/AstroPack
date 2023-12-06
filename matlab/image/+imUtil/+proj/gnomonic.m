function [X,Y]=gnomonic(Lon,Lat,Center,R,InUnits)
% tangential (gnomonic) projection - spherical to plannar
% Package: +imUtil.tan
% Input  : - An array of longitudes.
%          - An array of latitudes.
%          - Center projection point [Long, Lat].
%            Default is [0 0].
%          - The radius of the unit sphere which scales the output.
%            Effectively this is the scale of the output coordinates.
%            For example, 1 - output is close to radians.
%                         180./pi - output is close to deg.
%            Default is 180./pi;
%          - Input coordinates and Center units.
%            Default is 'deg'.
% Output : - X coordinates.
%          - Y coordinates.
%      By: Eran O. Ofek                         May 2020
% Example: Lon = rand(10,1); Lat=rand(10,1);
%          [X,Y]=imUtil.proj.gnomonic(Lon,Lat,[0.5 0.5])

if nargin<5
    InUnits = 'deg';
    if nargin<4
        R = 180./pi;
    end
end

ConvFactor = convert.angular(InUnits,'rad');
Lon     = Lon.*ConvFactor;
Lat     = Lat.*ConvFactor;
Center  = Center.*ConvFactor;


Lon0  = Center(1);
Lat0  = Center(2);
% R is really R.*S (R-radius, S-scale factor)
CosC = sin(Lat0).*sin(Lat) + cos(Lat0).*cos(Lat).*cos(Lon-Lon0);
X = R.*cos(Lat).*sin(Lon-Lon0)./CosC;
Y = R.*(cos(Lat0).*sin(Lat) - sin(Lat0).*cos(Lat).*cos(Lon-Lon0))./CosC;



