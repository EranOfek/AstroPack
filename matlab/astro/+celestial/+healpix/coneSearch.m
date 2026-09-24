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
    % Output : - Column vector of pixel indices.
    % Author : Eran Ofek (2025 Jan) 
    % Example: celestial.healpix.coneSearch(16,1,1,0.01)

    arguments
        NSide
        Lon
        Lat
        Radius
        Args.Type        = 'nested';
        Args.CooUnits    = 'rad';
        Args.RadiusUnits = 'rad';
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
    
    Nr = ceil(2.5.* Radius./ApproxPixRadius);
    RadVec = linspace(ApproxPixRadius.*0.1, Radius+0.5.*ApproxPixRadius, Nr);
    
    PixelArea    = 4.*pi./(12.*NSide.^2);
    ApproxNumPix = ceil(5.*  pi.*Radius.^2./PixelArea);  % factor 5 oversampling
    
    %Result   = zeros(ApproxNumPix,1);
    AllLon   = zeros(ApproxNumPix,1);
    AllLat   = zeros(ApproxNumPix,1);
    
    K = 0;
    for Ir=1:1:Nr
        Npa = ceil(2.5 .* 2.*pi.*RadVec(Ir)./ApproxPixRadius);
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
            [PixLon, PixLat] = celestial.healpix.mex.pix2ang_ring(NSide, dpuble(Result));
        end
    end
    
end
