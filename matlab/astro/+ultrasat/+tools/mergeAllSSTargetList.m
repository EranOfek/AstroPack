function [UniqTargets, Nexp] = mergeAllSSTargetList(UTargets, Args)
    % merge 2 same targets in UniqTargets, fill Args.Nexposure 
    % Input  : - target list (a vector, may contain 0s)    
    %          * ...,key,val,... 
    % Output : - merged target list 
    %          - a vector of exposure counts   
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: [UniqTargets, Nexp] = ultrasat.tools.mergeAllSSTargetList(UTargets);  
    arguments
        UTargets
        Args.Nexp = 3; % number of exposures per visit
    end
    %
    UniqTargets = [];
    Nexp        = [];      
    
    Nt = numel(UTargets);
    It = 0;
    while It < Nt
        It = It+1;
        if UTargets(It) == 0  % empty slot
            UniqTargets = [UniqTargets 0];
            Nexp        = [Nexp 0]; 
            continue
        elseif It < Nt-1 % before the last slot
            if UTargets(It) == UTargets(It+1) 
                 UniqTargets = [UniqTargets UTargets(It)];
                 Nexp  = [Nexp 2*Args.Nexp]; % double exposure
                 It    = It+1;               % skip the next slot
            else
                UniqTargets = [UniqTargets UTargets(It)];
                Nexp  = [Nexp Args.Nexp];
            end
        else % the last slot 
            UniqTargets = [UniqTargets UTargets(It)];
            Nexp  = [Nexp Args.Nexp];              
        end                    
    end
end
