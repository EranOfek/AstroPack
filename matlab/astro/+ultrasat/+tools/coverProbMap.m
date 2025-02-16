function [RA, Dec] = coverProbMap(SkyMap, Args)
    % optimal coverage of a probability sky map with (circular) exposures
    %     Optional detailed description
    % Input  : - a normalized probability map: table of HEALPIX numbers and corresponding probabilities  
    %          * ...,key,val,... 
    % Output : - a set of [RA, Dec] coordinates of FOV centers 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: Map = '~/ULTRASAT/SkyGrid/LVC/2024/04/01/lvc_2024_04_01_00_40_58_000000.csv';
    %         [RA, Dec] = ultrasat.tools.coverProbMap(Map,'MaxExp',4); 
    arguments
        SkyMap      
        Args.MaxExp            = []; % maximal number of exposures to use
        Args.MinProb           = []; % minimal cumulative probability covered
               
        Args.CleanThresh       = 0.1; % cleaning probability [sr(-1)] 
    end        
    % read the alert map from a CSV file and filter out points < 0.1 sr(-1)
    Map0 = readtable(SkyMap);
    Map1 = Map0(Map0.PROBDENSITY > Args.CleanThresh,:); 
end
