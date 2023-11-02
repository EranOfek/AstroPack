function orbitIntegrationLightTime1
    %
    
    arguments
        Time
        OrbEl
        StartEpochs
    end
    
    Nt      = numel(Time);
    Ntarget = numEl(Obj);
    if ~(Nt==1 || Ntarget==1)
        error('Number of epochs or number of targets must be 1');
    end

    if isa(OrbitalEl, 'celestial.OrbitalEl')
        [Nu0]  = keplerSolve(OrbEl, OrbEl.Epoch, 'Tol',Args.Tol);
        [V0,X0] = trueAnom2rectVel(OrbEl, Nu0, [], []);
        if numel(unique(OrbEl.Epoch))>1
            error('Using orbitIntegrationLightTime1 Epoch must be unique');
        end
        UniqueEpoch = Obj.Epoch;
    else
        error('OrbEl option currently must be of type celestial.OrbitalEl');
    end
   
    X_B = X0;
    V_B = V0;
    
    
    % divide requested T to those >Epoch and those <Epoch
    Tbackward = T(T<=Epoch);
    Tforward  = T(T>Epoch);

    Tbackward = sort(Tbackward, 'descend');
    Tforward  = sort(Tforward, 'ascend');

    NtBackward = numel(Tbackward);
    NtForward  = numel(Tforward);

    for It=1:1:NtBackward
        LightTimeNotConverged = true;
        LightTime             = 0;
        Iter                  = 0;

        % Integrate position to Time(It)
        [X_B, V_B] = celestial.SolarSys.orbitIntegration([StartEpochs, Time(It)],...
                                                             X_B,...
                                                             V_B,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt);
        while LightTimeNotConverged
            Iter = Iter + 1;
            % Integrate position to Time-LightTime
            [X_B, V_B] = celestial.SolarSys.orbitIntegration([Time(It), Time(It)-LightTime],...
                                                             X_B,...
                                                             V_B,...
                                                             'RelTol',Args.TolInt,...
                                                             'AbsTol',Args.TolInt);
            %
            % rectangular ecliptic coordinates of Earth with equinox of J2000
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

            % Topocentric position of object
            U = U_B - E_H;  % U_B(t-tau)
            % Q = U_B - S_B; % U_B(t-tau) - S_B(t-tau)

            % Observer-Distance object
            Delta = sqrt(sum(U.^2, 1));

            % Update LightTime
            PrevLightTime = LightTime;
            LightTime = Delta./Caud;
            % FFU: more accuratly - use:
            % celestial.Kepler.LightTimeCorrection
            if all(abs(LightTime - PrevLightTime))<Args.TolLT || Iter>Args.MaxIterLT
                LightTimeNotConverged = false;
            end
            % Barycentric distance
            R     = sqrt(sum(U_B.^2, 1));
        end
    end
                
                                                             
                                                             
end
