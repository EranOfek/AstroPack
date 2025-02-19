function A_U = extinction(RA, Dec, Args)
    % calculate averaged extinction in the ULTRASAT band for a list of coordinates
    %
    % Input :  - a vector of RA [deg]
    %          - a vector of Dec [deg]
    % Output : - a vector ULTRASAT band extinction (A_U) averaged over the FOV
    % Example: A_U = ultrasat.tools.extinction(RA,Dec);
    arguments
        RA
        Dec
		
		% Temporary solution for development, @Todo @Yossi
		if ispc
 		    Args.AveragedExt = 'C:/AstroPack/Data/ULTRASAT/A_USat_aver7deg_hp49152.mat';
		else
            Args.AveragedExt = '~/matlab/data/ULTRASAT/A_USat_aver7deg_hp49152_v2.mat'; % interpolation function        
		end
    end
    %
    load(Args.AveragedExt); % load the interpolation function A_Uaver7deg
    A_U = A_Uaver7deg(RA, Dec);        
end