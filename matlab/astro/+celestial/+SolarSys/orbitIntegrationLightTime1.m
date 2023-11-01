function orbitIntegrationLightTime1
    %
    
    arguments
        OrbitalElObj
        StartEpochs
    end
    
    Nt      = numel(Time);
    Ntarget = numEl(Obj);
    if ~(Nt==1 || Ntarget==1)
        error('Number of epochs or number of targets must be 1');
    end
    
    if Args.Integration
        [Nu0]  = keplerSolve(OrbitalElObj, Obj.Epoch, 'Tol',Args.Tol);
        [V0,X0] = trueAnom2rectVel(Obj,Nu0,[],[]);
        [UniqueEpochs,~,IndEpochs] = unique(Obj.Epoch); % devide to groups with same initial epoch
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
            
            [X_B, V_B] = celestial.SolarSys.orbitIntegration([StartEpochs, Time(It)],...
                                                                 X_B,...
                                                                 V_B,...
                                                                 'RelTol',Args.TolInt,...
                                                                 'AbsTol',Args.TolInt);
            while LightTimeNotConverged
                Iter = Iter + 1;
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
                    
                    switch lower(Args.EarthEphem)
                        case 'vsop87'
                            [E_H,E_dotH] = celestial.SolarSys.calc_vsop87(Time(It), 'Earth', 'a', 'd');
                        case 'inpop'
%                           % error('INPOP is not implemented yet - use vsop87');
                            IN = celestial.INPOP;  % need to make it singelton
                            IN.populateTables({'Ear','Sun'});
                            IN.populateTables({'Ear','Sun'},'FileData','vel');
                            %
                            E_H    = IN.getPos('Ear',Time(It),'IsEclipticOut',true) - IN.getPos('Sun',Time(It),'IsEclipticOut',true);
                            E_dotH = IN.getVel('Ear',Time(It),'IsEclipticOut',true) - IN.getVel('Sun',Time(It),'IsEclipticOut',true);
                            
                            % convert to eclipic coordinates
                            
                        otherwise
                            error('Unknown EarthEphem option');
                            
                    end
                    end
                    Gau = celestial.coo.topocentricVector(Time(It), Args.GeoPos, 'OutUnits','au',...
                                                                             'RefEllipsoid',Args.RefEllipsoid,...
                                                                             'Convert2ecliptic',true,...
                                                                             'Equinox','J2000');

                    E_H = E_H + Gau;
                    

                    U = U_B - E_H;  % U_B(t-tau)
                    % Q = U_B - S_B; % U_B(t-tau) - S_B(t-tau)

                    Delta = sqrt(sum(U.^2, 1));

                    PrevLightTime = LightTime;
                    LightTime = Delta./Caud;
                    % more accuratly - use:
                    % celestial.Kepler.LightTimeCorrection

                    if all(abs(LightTime - PrevLightTime))<Args.TolLT || Iter>Args.MaxIterLT
                        LightTimeNotConverged = false;
                    end
                end
                R     = sqrt(sum(U_B.^2, 1));
                
                
                                                             
                                                             
end
