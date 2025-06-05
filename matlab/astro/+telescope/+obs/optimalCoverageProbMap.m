function [FOVcenters, IdxCenters, CoveredPD, FOVmap, Nstop] = optimalCoverageProbMap(Map, PD, Rfov, Args)
    % optimal coverage of a sky probability map with circular FOVs
    %     a fast greedy algorithm based on a KD-tree search 
    %     the probability density may be weighted with the map pixel area
    %     NB: the algorithm places the FOV centers only on the input map
    %     pixels, and never between them! 
    % Input  : - a source map on the sky: a 2-column matrix of [RA, Dec] [deg]
    %          - a vector of probability density at each of the map points [arb. units.]
    %          - the telescope FOV radius [deg]
    %          * ...,key,val,... 
    %          'PixSize' - area of map pixels (should be the same length as PD)
    %          'Uniq' - HEALpix UNIQ indices of map pixels (should be the same length as PD)
    %          'Nmax' - maximal number of FOVs
    %          'Overlap' - [0, 1]: 0 = disjoint FOVs only (no reuse), 1 = full overlap allowed (not practical) 
    %          'TargetCoverage' - [0, 1]: desired total PD coverage 
    %          'Plot' - boolean 
    % Output : - FOV centers [RA, Dec]
    %          - indices of FOV centers in the original list
    %          - total summed weighted PD covered by all the FOVs
    %          - cell array: FOVmap{k} = indices of points covered by FOV k
    %          - number of FOVs actually required to cover the desired % of the total PD
    % Author : A.M. Krassilchtchikov (2025 Jun) 
    % Example: Data = '~/matlab/data/ULTRASAT/lvc_2024_04_01_00_40_58_000000.csv';
    %          T = readtable(Data); Map = [T.RA, T.DEC]; PD = T.PROBDENSITY; Rfov = 7;
    %          [FOVcenters, IdxCenters, CoveredPD, FOVmap, Nstop] = ...
    %              telescope.obs.optimalCoverageProbMap(Map, PD, Rfov,'Uniq',T.UNIQ,'Nmax', 10, 'Plot',1);
    arguments
        Map
        PD                    
        Rfov
        Args.PixSize         = [];       % area of map pixels (should be the same length as PD)
        Args.Uniq            = [];       % HEALpix UNIQ indices of map pixels (should be the same length as PD)  
        Args.Nmax            = 1000;     % maximal number of FOVs
        Args.Overlap         = 0.0;      % [0, 1]: 0 = disjoint FOVs only (no reuse), 1 = full overlap allowed (not practical) 
        Args.TargetCoverage  = 1;        % [0, 1]: desired total PD coverage 
        Args.Plot            = false;
    end
    %
    RAD  = 180/pi; SRAD = RAD * RAD;
    NsideAreaDeg = [2, 859.4366926962348; ... % area in deg(2)
                    4, 214.8591731740587; ...
                    8, 53.714793293514674; ...
                   16, 13.428698323378669; ...
                   32, 3.357174580844667; ...
                   64, 0.8392936452111668; ...
                  128, 0.2098234113027917; ...
                  256, 0.052455852825697924; ...
                  512, 0.013113963206424481; ...
                 1024, 0.0032784908016061202; ...
                 2048, 0.0008196227004015301;...
                 4096, 0.00020490567510038252; ...
                 8192, 5.122641877509563e-05; ...
                16384, 1.2806604693773907e-05];                 
    % Convert the coordinates to Cartesian on a unit sphere:
    RA  = Map(:,1)/RAD;
    Dec = Map(:,2)/RAD;
    V   = [cos(Dec) .* cos(RA), cos(Dec) .* sin(RA), sin(Dec)];
    ChordR = 2 * sind(Rfov / 2);
    
    % Scale the PD according to the pixel size:
    if ~isempty(Args.PixSize)
        PD = PD .* PixSize;
    elseif ~isempty(Args.Uniq)
        Ind  = floor(log(Args.Uniq/4)/(2*log(2)));
        PD   = NsideAreaDeg(Ind(:,1),2).* PD / SRAD; % [usually the original PD is per SRAD]            
    end

    % Build a KD-tree for a fast 3D neighbor search:
    Tree = KDTreeSearcher(V);

    % Initialize:
    M = numel(RA);
    Coverage_count = zeros(M,1);
    PD_effective = PD;
    IdxCenters = [];
    FOVmap = {};
    CoveredPD = 0;   
    Nstop = Args.Nmax;
    
    % Find the FOV positions:
    for k = 1:Args.Nmax
        Best_idx = -1;
        Best_score = -inf;
        Best_neighbors = [];

        for i = 1:M
            if PD_effective(i) == 0
                continue
            end
            Neighbors = rangesearch(Tree, V(i,:), ChordR);
            Neighbors = Neighbors{1};
            Score = sum(PD_effective(Neighbors));
            if Score > Best_score
                Best_score = Score;
                Best_idx = i;
                Best_neighbors = Neighbors;
            end
        end

        if Best_idx == -1
            break
        end

        IdxCenters(end+1) = Best_idx;
        FOVmap{end+1} = Best_neighbors;
        CoveredPD = CoveredPD + sum(PD(Best_neighbors));
        Coverage_count(Best_neighbors) = Coverage_count(Best_neighbors) + 1;
        PD_effective(Best_neighbors) = PD(Best_neighbors) .* Args.Overlap.^Coverage_count(Best_neighbors);

        if CoveredPD >= Args.TargetCoverage * sum(PD)
            Nstop = k;
            break
        end
    end
    
    % Collect the output:
    FOVcenters = [RA(IdxCenters)*RAD, Dec(IdxCenters)*RAD];
    
    %
    if Args.Plot
        figure; clf; hold on;
        plot.ungridded_image(RA*RAD,Dec*RAD,PD);
        plot(FOVcenters(:,1),FOVcenters(:,2),"*",'Color','red');
        plot.skyCircles(FOVcenters(:,1), FOVcenters(:,2), 'Rad', Rfov,'Color','white');
    end
end