function [PixCand] = findNeighbors(NSide, Pix, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Feb) 
    % Example: 

    arguments
        NSide
        Pix
        Args.IncludeSelf       = false;
        Args.Type              = 'nested';
    end
    RAD = 180./pi;


    [Lon, Lat] = celestial.healpix.pix2ang(NSide, Pix, 'Type',Args.Type);
    [LatList,Npix]=celestial.healpix.latitudeRings(NSide);
    [~,Il] = min(abs(LatList-Lat));
    SmallRadiusLength = 2.*pi*cos(Lat);
    DistLon = SmallRadiusLength./Npix(Il);
    Az  = (0:15:360)./RAD;
    SizeAz = size(Az);
    DistLon = repmat(DistLon, SizeAz);
    Lat     = repmat(Lat, SizeAz);
    Lon     = repmat(Lon, SizeAz);

    [LatCand, LonCand] = reckon(Lat, Lon, DistLon, Az, 'radians');

    PixCand = celestial.healpix.ang2pix(NSide, LonCand, LatCand, 'Type',Args.Type);

    PixCand = unique(PixCand);
    
end
