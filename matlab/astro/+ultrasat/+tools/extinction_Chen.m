function A_U = extinction(RA, Dec, Args)
    % calculate averaged extinction in the ULTRASAT band for a list of coordinates
    %
    % Input : - a vector of RA [deg]
    %         - a vector of Dec [deg]
    % Output : - a vector ULTRASAT band extinction (A_U) averaged over the FOV
    arguments
        RA
        Dec
        Args.AveragedExt = 'C:/AstroPack/Data/ULTRASAT/A_USat_aver7deg_hp49152.mat';  % ~/matlab/data/ULTRASAT/A_USat_aver7deg_hp49152.mat'; % interpolation function        
    end
    %
    load(Args.AveragedExt); % load the interpolation function A_Uaver7deg
    A_U = A_Uaver7deg(RA, Dec);    
end