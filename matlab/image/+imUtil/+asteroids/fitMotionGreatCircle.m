function [Result] = fitMotionGreatCircle(Time, Lon, Lat, Args)
    % Fit Long(time) and Lat(time) to a great circle motion
    % Input  : - Vector of time.
    %          - Array of longitude. Columns correspondg to objects, and
    %            rows to time.
    %          - Array of latitude.
    %          * ...,key,val,... 
    %            'RefTime' - Refernce time. If empty use mid time.
    %                   Default is [].
    %            'InCooUnits' - Default is 'deg'.
    %            'OutCooUnits' - Default is 'deg'.
    % Output : - 
    
    % Author : Eran Ofek (2025 Jun) 
    % Example: R=imUtil.asteroids.fitMotionGreatCircle(JD,RA,Dec);

    arguments
        Time
        Lon
        Lat
        Args.RefTime           = [];
        Args.InCooUnits        = 'deg';
        Args.OutCooUnits       = 'deg';
    end

    if isempty(Args.RefTime)
        Args.RefTime = (Time(1)+Time(end)).*0.5;
    end

    [Time, SI] = sort(Time);
    Lon        = Lon(SI,:);
    Lat        = Lat(SI,:);

    ConvFactor = convert.angular(Args.InCooUnits,'rad');
    Lon        = ConvFactor.*Lon;
    Lat        = ConvFactor.*Lat;

    N = numel(Time);

    Time = Time - Args.RefTime;
    
    Result = struct('Lon0', [], 'Lat0', [], 'Omega', [], 'PA', [], ...
                    'dLon', [], 'dLat', [], 'RMS', []);
    
    % Convert to Cartesian unit vectors
    [X, Y, Z] = sph2cart(Lon, Lat, 1);  % spherical to Cartesian
    [X, Y, Z] = celestial.coo.coo2cosined(Lon, Lat);

    H  = [ones(N,1), Time];
    Px = H\X;
    Py = H\Y;
    Pz = H\Z;

    ModelX = H*Px;
    ModelY = H*Py;
    ModelZ = H*Pz;

    [ModelLon, ModelLat] = celestial.coo.cosined2coo(ModelX, ModelY, ModelZ);

    ResidLon = mod(Lon - ModelLon, 2.*pi);
    ResidLat = Lat - ModelLat;

    LonRMS   = std(ResidLon);
    LatRMS   = std(ResidLat);

    ConvFactor = convert.angular('rad',Args.OutCooUnits);
    Result.RefT = Args.RegTime;
    Result.DeltaTime   = Time(end) - Time(1);
    Result.MidLat      = (ModelLat(end,:) + ModelLat(1,:)).*0.5;
    Result.RateLat     = (ModelLat(end,:) - ModelLat(1,:))./Result.DeltaTime;
    Result.RateLon     = (ModelLon(end,:) - ModelLon(1,:))./Result.DeltaTime;
    Result.RateLonCos  = Result.RateLon.*cos(MidLat)./Result.DeltaTime;

    Result.MidLat      = ConvFactor .* Result.MidLat;
    Result.RateLat     = ConvFactor .* Result.RateLat;
    Result.RateLon     = ConvFactor .* Result.RateLon;
    Result.RateLonCos  = ConvFactor .* Result.RateLonCos;

    
end

