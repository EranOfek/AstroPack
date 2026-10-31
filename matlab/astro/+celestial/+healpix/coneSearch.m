function [Result,PixLon,PixLat] = coneSearch(NSide, Lon, Lat, Radius, Args)
    % cone search for healpix pixels (fast version).
    %   Return all the pixel indices that may be in the cone search.
    %   The list may contains nearby irrelevant pixels.
    %   This function use a simple annulus based search.
    %   For slower version see: celestial.healpix.coneSearchRecur
    %
    %   See also a faster mex version: celestial.healpix.mex.coneSearch
    %
    % Input  : - Nside.
    %          - Longitude (scalar).
    %          - Latitude (scalar).
    %          - Search radius (scalar).
    %          * ...,key,val,... 
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'CooUnits' - Input coordinate units. Default is 'rad'.
    %            'RadiusUnits' - Input search radius units. Default is 'rad'.
    %            'Oversampling' - Number of sample points per equal-area
    %                   pixel radius (radially and along each ring).
    %                   Larger is slower and more complete. With 2, no
    %                   overlapping pixel was missed in tests against
    %                   healpy query_disc (Nside 4-4096, radius 0.05-10 deg).
    %                   Default is 2.
    % Output : - Column vector of pixel indices.
    % Author : Eran Ofek (2025 Jan); A.M. Krassilchtchikov (2026 Oct)
    % Example: celestial.healpix.coneSearch(16,1,1,0.01)

    arguments
        NSide
        Lon
        Lat
        Radius
        Args.Type        = 'nested';
        Args.CooUnits    = 'rad';
        Args.RadiusUnits = 'rad';
        Args.Oversampling = 2;
    end

    if strcmp(Args.Type, 'nested')
        IsNested = true;
    else
        IsNested = false;
    end
    
    if ~strcmpi(Args.CooUnits,'rad')
        Factor = convert.angular(Args.CooUnits,'rad');
        Lon    = Factor.*Lon;
        Lat    = Factor.*Lat;
    end
    if ~strcmpi(Args.RadiusUnits,'rad')
        Factor = convert.angular(Args.RadiusUnits,'rad');
        Radius = Factor.*Radius;
    end
    
    
    ApproxPixRadius = celestial.healpix.pixRadius(NSide);
    
    Nr = ceil(Args.Oversampling.* Radius./ApproxPixRadius);
    RadVec = linspace(ApproxPixRadius.*0.1, Radius+0.5.*ApproxPixRadius, Nr);
    
    NpaVec       = ceil(Args.Oversampling .* 2.*pi.*RadVec./ApproxPixRadius);  % points per ring
    ApproxNumPix = 1 + sum(NpaVec);
    
    %Result   = zeros(ApproxNumPix,1);
    AllLon   = zeros(ApproxNumPix,1);
    AllLat   = zeros(ApproxNumPix,1);
    
    % the cone centre itself (the only sure point for cones smaller than a pixel)
    AllLon(1) = mod(Lon, 2.*pi);
    AllLat(1) = Lat;
    K = 1;
    for Ir=1:1:Nr
        Npa = NpaVec(Ir);
        Az  = linspace(0,2.*pi,Npa).';
%         [LatP, LonP] = reckon(Lon, Lat, RadVec(Ir), Az, 'radians');
        [LatP, LonP] = reckon(Lat, Lon, RadVec(Ir), Az, 'radians');
        LonP(LonP<0) = LonP(LonP<0) + 2*pi;
    
        NumPix = numel(LonP);
        AllLon(K+1:K+NumPix) = LonP;
        AllLat(K+1:K+NumPix) = LatP;
        K = K + NumPix;
        
        %if IsNested
        %    Pix = celestial.healpix.mex.ang2pix_nested(NSide, LonP, LatP);
        %else
        %    Pix = celestial.healpix.mex.ang2pix_ring(NSide, LonP, LatP);
        %end
        %NumPix = numel(Pix);
        %Result(K+1:K+NumPix) = Pix;
        %K = K + NumPix;
        
        %Result = [Result; Pix];
        
    end
    AllLon = AllLon(1:K);
    AllLat = AllLat(1:K);
    
    if IsNested
        Result = celestial.healpix.mex.ang2pix_nested(NSide, AllLon, AllLat);
    else
        Result = celestial.healpix.mex.ang2pix_ring(NSide, AllLon, AllLat);
    end
        
    %Result = Result(1:K);
    Result = unique(Result);
    
    if nargout>1
        if IsNested
            [PixLon, PixLat] = celestial.healpix.mex.pix2ang_nested(NSide, double(Result));
        else
            [PixLon, PixLat] = celestial.healpix.mex.pix2ang_ring(NSide, double(Result));
        end
    end
    
end
