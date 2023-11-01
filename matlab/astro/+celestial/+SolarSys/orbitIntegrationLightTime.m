function orbitIntegrationLightTime
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

    % for each initial epoch
    NinitEpoch = numel(UniqueEpochs);
    for IinitEpoch=1:1:NinitEpooch
        % select all targets with the same initital epoch
        IndTargets = find(IndEpochs == IinitEpoch);
        NtargetsEpoch = numel(IndTargets);   % index of targets with the same epoch
        Epoch = UniqueEpochs(IinitEpoch);
        
    
        % divide requested T to those >Epoch and those <Epoch
        
        
    % Orbital integration
    
    % loop for each group with same initial epoch
    for It=1:1:Nt
        LightTimeNotConverged = true;
        LightTime             = 0;
        Iter                  = 0;              
        while LightTimeNotConverged
            Iter = Iter + 1;

            
            U_B = zeros(3,Ntarget);                
            for Iepoch = 1:numel(StartEpochs)
                IndTargets = find(IndEpochs == Iepoch);
                NtargetsEpoch = numel(IndTargets);
                % if light times are different
                if NtargetsEpoch>1 && numel(LightTime)>1 && any(LightTime(IndTargets)~=LightTime(IndTargets(1)))

                    X_B = X0(:,IndTargets);
                    V_B = V0(:,IndTargets);

                    % first integrate all targets to minimal
                    % time (maximal light time)
                    [MaxLightTime,ImaxLightTime] = max(LightTime(IndTargets));
                    %[X_B,V_B] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch),Time(It)-MaxLightTime]...
                    %        ,X_B,V_B, 'RelTol',Args.TolInt,'AbsTol',Args.TolInt);
                    [X_B,V_B] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch),Time(It)]...
                            ,X_B,V_B, 'RelTol',Args.TolInt,'AbsTol',Args.TolInt);

                    % then integrate one by one according to
                    % light time
        %                                 for Itarget = 1:numel(IndTargets)
        %                                      [X_B(:,Itarget),~] = celestial.SolarSys.orbitIntegration([Time(It)-MaxLightTime,Time(It)-LightTime(IndTargets(Itarget))]...
        %                                         ,X_B(:,Itarget),V_B(:,Itarget), 'RelTol',Args.TolInt,'AbsTol',Args.TolInt);
        %                                      
        %                                 end    
                    U_B(:,IndTargets) = X_B;
                else % if light times are equal integrate all at once
                    if numel(LightTime)>1
                        IndLightTime = IndTargets(1);
                    else
                        IndLightTime =1;
                    end
                    %[U_B(:,IndTargets),~] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch),Time(It)-LightTime(IndLightTime)],...
                    %                                    X0(:,IndTargets),V0(:,IndTargets),'RelTol',Args.TolInt,'AbsTol',Args.TolInt);
                    [U_B(:,IndTargets),~] = celestial.SolarSys.orbitIntegration([StartEpochs(Iepoch), Time(It)],...
                                                        X0(:,IndTargets),V0(:,IndTargets),'RelTol',Args.TolInt,'AbsTol',Args.TolInt);

                end
            end
        end
end
