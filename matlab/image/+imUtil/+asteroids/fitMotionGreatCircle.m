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
    %            'ModelTime' - List of times, relative to RefTime at which
    %                   to evaluate the position (ModelLon/Lat0).
    %                   Default is 0.
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
        Args.ModelTime         = 0;
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

    ResidLon = Lon - ModelLon;
    ResidLat = Lat - ModelLat;

    % position at ref time
    H0 = [ones(numel(Args.ModelTime),1), Args.ModelTime(:)];
    ModelX0 = H0*Px;
    ModelY0 = H0*Py;
    ModelZ0 = H0*Pz;

    [ModelLon0, ModelLat0] = celestial.coo.cosined2coo(ModelX0, ModelY0, ModelZ0);

    ConvFactor  = convert.angular('rad',Args.OutCooUnits);
    Result.RefT = Args.RefTime;
    Result.N    = N;
    Result.DeltaTime   = Time(end) - Time(1);
    Result.MidLat      = (ModelLat(end,:) + ModelLat(1,:)).*0.5;
    Result.ModelLon0   = ModelLon0;
    Result.ModelLat0   = ModelLat0;
    Result.RateLat     = (ModelLat(end,:) - ModelLat(1,:))./Result.DeltaTime;
    Result.RateLon     = (ModelLon(end,:) - ModelLon(1,:))./Result.DeltaTime;
    Result.RateLonCos  = Result.RateLon.*cos(Result.MidLat);
    Result.LonRMS      = std(ResidLon).*cos(Result.MidLat);
    Result.LatRMS      = std(ResidLat);

    Result.ModelLon0   = ConvFactor .* Result.ModelLon0;
    Result.ModelLat0   = ConvFactor .* Result.ModelLat0;
    Result.MidLat      = ConvFactor .* Result.MidLat;
    Result.RateLat     = ConvFactor .* Result.RateLat;
    Result.RateLon     = ConvFactor .* Result.RateLon;
    Result.RateLonCos  = ConvFactor .* Result.RateLonCos;
    Result.LonRMS      = ConvFactor .* Result.LonRMS;
    Result.LatRMS      = ConvFactor .* Result.LatRMS;
    Result.RMS         = sqrt(Result.LonRMS.^2 + Result.LatRMS.^2);
    Result.ModelTime   = Args.ModelTime(:);
    
end

