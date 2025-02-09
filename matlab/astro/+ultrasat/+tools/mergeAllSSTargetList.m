function [UniqTargets, Nexposures, Dither] = mergeAllSSTargetList(UTargets, Args)
    % merge type 1 targets in UniqTargets, fill Args.Nexposure and Args.Dither (mark the dither of type 1 and 2 targets)
    %     Optional detailed description
    % Input  : - target list (a vector, may contain 0s)    
    %          * ...,key,val,... 
    % Output : - merged target list 
    %          - a vector of exposure counts
    %          - a vector of dither patterns 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: [UniqTargets, Nexposure, Dither] = ultrasat.tools.mergeAllSSTargetList(UTargets);  
    arguments
        UTargets
        Args.Nexp = 3; % number of exposures per visit
    end
    %
    UniqTargets = [];
    Nexposures  = [];
    Dither      = [];
    
    Nt = numel(UTargets);
    It = 0;
    while It < Nt
        It = It+1;
        if UTargets(It) == 0                  % empty
            UniqTargets = [UniqTargets UTargets(It)];
            Nexposures  = [Nexposures 0];
            Dither      = [Dither 0];
        elseif It < Nt-2
            if UTargets(It) == UTargets(It+3) % type 2
                UniqTargets = [UniqTargets UTargets(It) UTargets(It+1) UTargets(It+2) UTargets(It+3)];
                Nexposures  = [Nexposures Args.Nexp Args.Nexp Args.Nexp Args.Nexp];
                Dither      = [Dither 1 2 3 4];
                It = It+3;
            elseif UTargets(It) == UTargets(It+1) % type 1
                UniqTargets = [UniqTargets UTargets(It)];
                Nexposures  = [Nexposures 2*Args.Nexp];
                Dither      = [Dither 0];
                It = It+1;
            end
        elseif UTargets(It) == UTargets(It+1) % type 1
            UniqTargets = [UniqTargets UTargets(It)];
            Nexposures  = [Nexposures 2*Args.Nexp];
            Dither      = [Dither 0];
            It = It+1;
        else
            error('Incorrect target list');
        end
    end
end
