function orbitIntegrationLightTime1(Time, OrbEl, Args)
    %
    % Example: celestial.SolarSys.orbitIntegrationLightTime1(2451545,OrbEl)
    
    arguments
        Time
        OrbEl
        Args.TimeScale          = 'TDB';
        Args.ObserverEphem      = [];
        Args.GeoPos             = [];
        Args.RefEllipsoid       = 'WGS84';
        Args.TolKepler          = 1e-8;
        Args.TolInt             = 1e-10;
        Args.TolLT              = 1e-6;   % [s]
        Args.MaxIterLT          = 10;
        Args.EarthEphem         = 'INPOP';
        Args.INPOP              = [];
    end
    RAD  = 180./pi;
    Caud = constant.c.*86400./constant.au;  % speed of light [au/day]
            
    
    Nt      = numel(Time);
    
    if ~(Nt==1 || Ntarget==1)
        error('Number of epochs or number of targets must be 1');
    end

    if isa(OrbEl, 'celestial.OrbitalEl')
        [Nu0]  = keplerSolve(OrbEl, OrbEl.Epoch, 'Tol',Args.TolKepler);
        [V0,X0] = trueAnom2rectVel(OrbEl, Nu0, [], []);
        if numel(unique(OrbEl.Epoch))>1
            error('Using orbitIntegrationLightTime1 Epoch must be unique');
        end
        StartEpoch = OrbEl.Epoch;
        Ntarget     = OrbEl.numEl;
    else
        error('OrbEl option currently must be of type celestial.OrbitalEl');
    end
   
    X_B = X0;
    V_B = V0;
    
    
    % divide requested T to those >Epoch and those <Epoch
    Tbackward = Time(Time<=StartEpoch);
    Tforward  = Time(Time>StartEpoch);

    Tbackward = sort(Tbackward, 'descend');
    Tforward  = sort(Tforward, 'ascend');

    NtBackward = numel(Tbackward);
    NtForward  = numel(Tforward);

    for It=1:1:NtBackward
        
        % Calculate the Earth position at the requested epoch
        if ~isempty(Args.ObserverEphem)
            E_H    = Args.ObserverEphem(It,1:3)';
            E_dotH = Args.ObserverEphem(It,4:6)';
        else
            [E_H, E_dotH, Args.INPOP]=celestial.SolarSys.earthObserverPos(Time(It), 'GeoPos',Args.GeoPos,...
                                                                            'EarthEphem',Args.EarthEphem,...
                                                                            'INPOP',Args.INPOP,...
                                                                            'TimeScale',Args.TimeScale,...
                                                                            'OutUnits','au',...
                                                                            'RefEllipsoid',Args.RefEllipsoid);                    


        end
        
        
        % Integrate position to Time(It)
        [X_B, V_B] = celestial.SolarSys.orbitIntegration([StartEpoch, Time(It)],...
                                                             X_B,...
                                                             V_B,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt);
        %
        U_B = X_B;
        % Topocentric coordinates
        U = U_B - E_H;  % U_B(t-tau)
        % Q = U_B - S_B; % U_B(t-tau) - S_B(t-tau)

        % Observer-Distance object
        Delta = sqrt(sum(U.^2, 1));
            
        % Light Time correction - single iteration only
        LightTime = Delta./Caud;
        
        [X_B, V_B] = celestial.SolarSys.orbitIntegration([Time(It), Time(It)-LightTime],...
                                                         X_B,...
                                                         V_B,...
                                                         'RelTol',Args.TolInt,...
                                                         'AbsTol',Args.TolInt);
        %
        
        
        % Barycentric distance
        R     = sqrt(sum(U_B.^2, 1));
        
        
        
        
    end
                
                                                             
                                                             
end
