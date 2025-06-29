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
    % Output : - 
    %       .Lon0     - longitude at RefTime [rad]
    %       .Lat0     - latitude at RefTime [rad]
    %       .Omega    - angular speed [rad/unit time]
    %       .PA       - position angle of motion [rad] E of N
    %       .dLon     - projected motion in longitude [rad/unit time]
    %       .dLat     - projected motion in latitude [rad/unit time]
    %       .RMS      - root mean square angular deviation [rad]
    % Author : Eran Ofek (2025 Jun) 
    % Example: 

    arguments
        Time
        Lon
        Lat
        Args.RefTime           = [];
        Args.InCooUnits        = 'deg';
    end

    if isempty(Args.RefTime)
        Args.RefTime = (Time(1)+Time(end)).*0.5;
    end

    ConvFactor = convert.angular('Args.InCooUnits','rad');
    Lon        = ConvFactor.*Lon;
    Lat        = ConvFactor.*Lat;

    N = numel(Time);

    Time = Time - Args.RefTime;
    
    M = size(Lon,2);
    Result = struct('Lon0', [], 'Lat0', [], 'Omega', [], 'PA', [], ...
                    'dLon', [], 'dLat', [], 'RMS', []);
    
    % Convert to Cartesian unit vectors
    [x, y, z] = sph2cart(Lon, Lat, 1);  % spherical to Cartesian
    
    for m = 1:M
        P = [x(:,m), y(:,m), z(:,m)];   % Nx3 trajectory
        V = polyfit(Time, P, 1);       % Linear fit in 3D: P = V(1)*Time + V(2)
        Vvec = V(1,:);                 % velocity vector
        
        % Normalize motion vector
        Vvec = Vvec / norm(Vvec);
    
        % Position at reference time
        P0 = polyval(V, 0);
        P0 = P0 / norm(P0);  % ensure on unit sphere
    
        % Project Vvec onto plane tangent to unit sphere at P0
        Vtan = Vvec - dot(Vvec, P0)*P0;
        Omega = norm(Vtan);
    
        % Convert P0 to spherical coords
        [Lon0, Lat0, ~] = cart2sph(P0(1), P0(2), P0(3));
    
        % Tangent basis
        e_lon = [-sin(Lon0), cos(Lon0), 0];
        e_lat = [-cos(Lon0)*sin(Lat0), -sin(Lon0)*sin(Lat0), cos(Lat0)];
    
        dLon = dot(Vtan, e_lon);
        dLat = dot(Vtan, e_lat);
        PA = atan2(dLon, dLat);  % E of N
    
        % --- Compute global RMS ---
        % Reconstruct predicted trajectory
        Pred = polyval(V, Time);
        Pred = Pred ./ vecnorm(Pred, 2, 2);  % normalize each row
    
        % Angular distance from actual to predicted
        dotprod = sum(P .* Pred, 2);
        dotprod = min(max(dotprod, -1), 1);  % clip for safety
        AngErr = acos(dotprod);              % angular error per time
        RMS = sqrt(mean(AngErr.^2));         % root mean square
    
        % Store results
        Result(m).Lon0 = Lon0;
        Result(m).Lat0 = Lat0;
        Result(m).Omega = Omega;
        Result(m).PA = PA;
        Result(m).dLon = dLon;
        Result(m).dLat = dLat;
        Result(m).RMS = RMS;
    end
end

