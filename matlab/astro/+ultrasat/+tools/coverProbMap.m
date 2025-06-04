function [RA, Dec, Stat] = coverProbMap(SkyMap, Args)
    % optimal coverage of a probability sky map with (circular) exposures
    %     Optional detailed description
    % Input  : - a normalized probability map: table of HEALPIX indices and corresponding probabilities  
    %          * ...,key,val,... 
    % Output : - a set of [RA, Dec] coordinates of FOV centers 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: Map = '~/ULTRASAT/SkyGrid/LVC/2024/04/01/lvc_2024_04_01_00_40_58_000000.csv';
    %         [RA, Dec, ~] = ultrasat.tools.coverProbMap(Map,'MaxTarg',4); 
    %         ultrasat.tools.coverProbMap(Map,'Verbosity',0,'Experimental',1,'ProbThresh',0.01,'MaxTarg',10); % experimental

    arguments
        SkyMap      
        Args.MaxTarg           = 4;    % maximal number of exposures (unique targets) to use
        Args.MinProb           = 0.5;  % minimal cumulative probability covered
        Args.MinAddedProb      = 0.05; % once adding one more target increases the sum covered probability 
                                       % by less than this value, we does not need it and stop               
        Args.FOVradius         = 7.0;  % [deg] 
        Args.CleanThresh       = 0.1;  % cleaning probability [sr(-1)] 
        Args.ProbThresh        = 0.03; % the limiting probability per ULTRASAT FOV (determines the maximal number of FOVs)
        
        Args.Verbosity         = 2;    
        Args.DrawMaps          = true;
        Args.CalcCoverageCurve = true;
        Args.Experimental      = false;
    end        
    %
    Sr  = (180/pi)^2;  % deg(2)
    FOV = pi*Args.FOVradius.^2; % deg(2) approximate area     
    PD  = Args.ProbThresh * ( Sr / FOV ); % the limiting probability per [sr] (as on the original maps)
    
    Stat.NCover      = 0; % number of exposures
    Stat.CoveredArea = 0; % covered area 

    % read the alert map as input or from a CSV file and filter out points < 0.1 sr(-1)
    if istable(SkyMap)
        Map0 = SkyMap;
    else
        Map0 = readtable(SkyMap);
    end
    Map1 = Map0(Map0.PROBDENSITY > Args.CleanThresh,:);      
    
        if Args.Verbosity > 1
            fprintf('Alert map: %s \n',SkyMap)           
            [Prob, Area] = sumProbability(Map0);
            fprintf('Initial probability: %.2f on an area of %.1f deg^2 \n',Prob,Area)
        end    
        if Args.Verbosity > 0
            [Prob, Area] = sumProbability(Map1);
            fprintf('Cleaned probability: %.2f on an area of %.1f deg^2 \n',Prob,Area)
        end  
        
    % extract a region with probability per ULTRASAT FOV is over Args.ProbThresh 
    Map = Map1(Map1.PROBDENSITY > PD,:);    
    [Prob, Area] = sumProbability(Map);
    
        if Args.Verbosity > 0
            fprintf('Extracted probability: %.2f on area of %.1f deg^2 \n',Prob,Area)
        end
        if Prob < 1e-6      
           if Args.Verbosity > 0
               fprintf('No FOV above Args.ProbThresh found \n');
           end
           [RA, Dec, Stat] = deal([]);
           return 
        end    
        if Args.DrawMaps
            figure(1); subplot(3,1,1); plot(Map1.RA,Map1.DEC,'*')
            subplot(3,1,2); plot(Map.RA,Map.DEC,'*')
            subplot(3,1,3); plot.ungridded_image(Map.RA,Map.DEC,Map.PROBDENSITY);
        end
        
    %%%%%%%%%%%%%%% experimental coverage function:
    if Args.Experimental
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
            Ind  = floor(log(Map.UNIQ/4)/(2*log(2)));
            SRAD = (180/pi)^2; % sq. deg. in srad
            ProbPerDeg = Map.PROBDENSITY ./ SRAD; % probability per deg^2          
            WeightedProb = NsideAreaDeg(Ind(:,1),2).*ProbPerDeg;          
            Overlap  = 0.1;% 1 -- absolute overlap, 0 -- no overlap allowed
            Coverage = 1;  % percent of filtered probability covered
            [RA_out, Dec_out, centers_idx, coverage_PD, disk_map, stop_k] = place_probability_disks(Map.RA, Map.DEC, WeightedProb,...
                Args.MaxTarg, Args.FOVradius, 'Overlap',Overlap,'Plot',1,'TargetCoverage',Coverage);
%              [RA_out, Dec_out, centers_idx, coverage_PD, disk_map, stop_k] = place_probability_disks(Map.RA, Map.DEC, Map.PROBDENSITY,...
%                 Nmax, Args.FOVradius, 'Overlap',Overlap,'Plot',1,'TargetCoverage',Coverage);
    end
    %%%%%%%%%%%%%%%
        
    % cover the region with targets
    Targets0 = coverSky(Map,'FOVradius',Args.FOVradius,'DrawMaps',Args.DrawMaps);
    Ntarg0   = size(Targets0, 2);
    
        if Args.Verbosity > 1
            fprintf('The target area is covered with %d FOVs \n',Ntarg0)
        end  
        
    % sort the targets by covered probability (with no overlap treatment yet!) 
    [~, Ind] = sort([Targets0.Pr], 'descend'); 
     
    if Args.CalcCoverageCurve
        It = 0;
        Nthresh = numel(Args.MinProb);        
        Stat.NCover(1:Nthresh) = 0;
        Stat.CoveredArea(1:Nthresh) = 0;
        CoveredProb = zeros(1,Ntarg0);
        while It < Ntarg0 && Stat.NCover(Nthresh) < 1
            It = It+1;
            Targets = Targets0(Ind(1:It));  % select first It targets
            TargCoo = cell2mat(arrayfun(@(x) x.Coo, Targets, 'UniformOutput', false)');
            [CoveredProb(It), CoveredArea] = sumProbability(Map,'Targets',TargCoo,'FOVradius',Args.FOVradius);  
            if It > 1
                DeltaCoveredProb = CoveredProb(It)-CoveredProb(It-1); % covered probability added with Target(It)
            else
                DeltaCoveredProb = CoveredProb(It);
            end
            for i = 1:Nthresh
                if ( CoveredProb(It) > Args.MinProb(i) || DeltaCoveredProb < Args.MinAddedProb) &&  Stat.NCover(i) < 1
                    Stat.NCover(i) = It;
                    Stat.CoveredArea(i) = CoveredArea;
                end                
            end            
        end                
    end
    
    % select no more than Args.MaxTarg targets with highest probability
    if Stat.NCover(end) < 1 % if the required probability has not been reached, take all the exposure
        fprintf('The required probability %.1f has not been reached!\n',Args.MinProb(Nthresh));        
        Stat.Ntarg = min(Ntarg0,Args.MaxTarg);
        Targets = Targets0(Ind(1:Stat.Ntarg)); 
    else
        Stat.Ntarg = min(Stat.NCover,Args.MaxTarg);
        Targets = Targets0(Ind(1:Stat.Ntarg));  % take the first Stat.Ntarg targets from the ordered list
    end
        
    TargCoo = cell2mat(arrayfun(@(x) x.Coo, Targets, 'UniformOutput', false)');
    Stat.CoveredProb = sumProbability(Map,'Targets',TargCoo,'FOVradius',Args.FOVradius); % NB! sumProbability deals with overlaps
    
%         if Args.Verbosity > 1
            fprintf('Selected %d FOVs with highest probability \n',Stat.Ntarg)
            fprintf('Covered probability: %.2f \n',Stat.CoveredProb) % with tiny overlaps, so might be > 1
%         end    
        if Args.DrawMaps        
            for Itarg = 1:Stat.NCover
                plot.skyCircles(Targets0(Ind(Itarg)).Coo(1),Targets0(Ind(Itarg)).Coo(2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','green');
            end
            for Itarg = 1:Stat.Ntarg
                plot.skyCircles(Targets(Itarg).Coo(1),Targets(Itarg).Coo(2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','red');
            end
        end
    
    % extract the output lists:
    RA  = arrayfun(@(t) t.Coo(1), Targets);
    Dec = arrayfun(@(t) t.Coo(2), Targets);
    Stat.IndividualCoveredProb = arrayfun(@(t) t.Pr, Targets);
end

%%% internal functions may be later replaced to calls to external tools

function Targets = coverSky(Map, Args)
    %
    arguments
        Map
        Args.FOVradius        = 7; % deg 
        Args.InitialGridFile  = '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_350_rep1.txt'
        Args.DrawMaps logical = true;
    end
    %
    RAD = 180/pi;    
    Grid0 = readmatrix(Args.InitialGridFile);
    Np    = length(Grid0);
    
        if Args.DrawMaps 
            figure(2); clf
            axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
            plotm(Map.DEC./RAD,Map.RA./RAD,'*')
        end           
    % find all the 7-deg all-sky grid pixels intersecting with any of the alert pixels
    ITarg = 0;
    for Ip = 1:Np        
        Rd = celestial.coo.sphere_dist_fast(Grid0(Ip,1)/RAD,Grid0(Ip,2)/RAD,Map.RA./RAD,Map.DEC./RAD);        
        Ind = Rd < Args.FOVradius/RAD;
        if sum(Ind) > 0
            ITarg = ITarg + 1;
            Targets(ITarg).Pr  = sumProbability(Map(Ind,:)); % probability of the points inside the FOV
            Targets(ITarg).Coo = Grid0(Ip,:);
                if Args.DrawMaps
                    plot.skyCircles(Grid0(Ip,1),Grid0(Ip,2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','blue');
                end
%               fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
        end
    end
end

function [SumProb, SumArea] = sumProbability(Map, Args)
    % sum the probability and area of a set of healpix points of varying resolution
    arguments
        Map
        Args.Targets    = []; % a list of (RA,Dec) pairs
        Args.FOVradius  = 7;  % [deg] 
    end
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

    RAD  = 180/pi;   % deg
    SRAD = RAD*RAD;  % deg^2
    
    if ~isempty(Args.Targets) % if a set of targets is given, limit the map to the area contained within this set of FOVs
        Np = height(Map);
        Map.Select(1:Np) = 0; % create a selection column in the table
        for Ip = 1:Np
            Rd = celestial.coo.sphere_dist_fast(Args.Targets(:,1)/RAD,Args.Targets(:,2)/RAD,Map.RA(Ip)./RAD,Map.DEC(Ip)./RAD);            
            if sum(Rd < Args.FOVradius/RAD) > 0 % the point lies within one of the FOVs
                Map.Select(Ip) = 1;
            end
        end        
        Map = Map(Map.Select > 0,:);        
    end
    
    ProbPerDeg = Map.PROBDENSITY ./ SRAD; % probability per deg^2
    
    Ind  = floor(log(Map.UNIQ/4)/(2*log(2)));    
    SumProb = sum(NsideAreaDeg(Ind(:,1),2).*ProbPerDeg);    
    SumArea = sum(NsideAreaDeg(Ind(:,1),2));       % deg^2
end

%%% an experimental function to cover the probability density: 

function [RA_out, Dec_out, centers_idx, coverage_PD, disk_map, stop_k] = place_probability_disks(RA, Dec, PD, N, R, Args)
% PLACE_PROBABILITY_DISKS Greedy placement of N spherical disks (radius R degrees)
% to cover maximum probability density on the sphere with optional overlap 
%
% Inputs:
%   RA, Dec - vectors of coordinates in degrees (length M)
%   PD      - vector of probability density values (length M)
%   N       - maximum number of disks to place
%   R       - radius of each disk in degrees
%   Overlap - overlap factor in [0, 1], where:
%             0 = disjoint disks only (no reuse),
%             1 = fully overlapping allowed (pure greedy).
% Optional:
%   'TargetCoverage' - desired total PD coverage (0 to 1), default = 1
%   'Plot'           - if true, show Aitoff projection plot
%
% Outputs:
%   RA_out, Dec_out - coordinates (in degrees) of selected disk centers
%   centers_idx     - indices of selected centers in original list
%   coverage_PD     - total summed PD covered by all disks
%   disk_map        - cell array: disk_map{k} = indices of points covered by disk k
%   stop_k          - index at which coverage target was achieved
    arguments
        RA
        Dec
        PD
        N                    
        R
        Args.Overlap         = 0;
        Args.TargetCoverage  = 1;
        Args.Plot            = false;
    end
    % Convert RA/Dec to radians
    RA_rad = deg2rad(RA(:));
    Dec_rad = deg2rad(Dec(:));
    PD = PD(:);
    M = length(PD);

    % Convert to 3D unit vectors
    x = cos(Dec_rad) .* cos(RA_rad);
    y = cos(Dec_rad) .* sin(RA_rad);
    z = sin(Dec_rad);
    V = [x, y, z];

    % Build KD-tree for fast 3D neighbor search
    tree = KDTreeSearcher(V);

    % Convert angular radius to 3D chord distance
    chord_radius = 2 * sind(R / 2);

    % Initialize
    coverage_count = zeros(M,1);
    PD_effective = PD;
    centers_idx = [];
    disk_map = {};
    coverage_PD = 0;
    total_PD = sum(PD);
    stop_k = N;

    for k = 1:N
        best_idx = -1;
        best_score = -inf;
        best_neighbors = [];

        for i = 1:M
            if PD_effective(i) == 0
                continue
            end
            neighbors = rangesearch(tree, V(i,:), chord_radius);
            neighbors = neighbors{1};
            score = sum(PD_effective(neighbors));
            if score > best_score
                best_score = score;
                best_idx = i;
                best_neighbors = neighbors;
            end
        end

        if best_idx == -1
            break
        end

        centers_idx(end+1) = best_idx;
        disk_map{end+1} = best_neighbors;
        coverage_PD = coverage_PD + sum(PD(best_neighbors));
        coverage_count(best_neighbors) = coverage_count(best_neighbors) + 1;
    %     PD_effective(best_neighbors) = PD(best_neighbors) .* (1 - Args.Overlap).^coverage_count(best_neighbors);
        PD_effective(best_neighbors) = PD(best_neighbors) .* Args.Overlap.^coverage_count(best_neighbors);

        if coverage_PD >= Args.TargetCoverage * total_PD
            stop_k = k;
            break
        end
    end

    RA_out  = RA(centers_idx);
    Dec_out = Dec(centers_idx);

    % Optional Plotting (Aitoff Projection)
    if Args.Plot
        figure; hold on; grid on
        % Convert RA to [-180,180] for Aitoff
        RA_plot = mod(RA + 180, 360) - 180;
        [x_proj, y_proj] = aitoff_projection(RA_plot, Dec);
        scatter(x_proj, y_proj, 5, PD, 'filled');
        colorbar; title('Probability Density and Disk Centers')
        % Plot circles
        for i = 1:length(centers_idx)
            [xc, yc] = aitoff_projection(RA_plot(centers_idx(i)), Dec(centers_idx(i)));
            plot(xc, yc, 'rx', 'MarkerSize', 8, 'LineWidth', 2);
            draw_aitoff_circle(RA_plot(centers_idx(i)), Dec(centers_idx(i)), R);
        end
        xlabel('RA (Aitoff)'); ylabel('Dec'); axis equal
    end
end

function [x, y] = aitoff_projection(ra, dec)
% Convert degrees to radians
    ra = deg2rad(ra);
    dec = deg2rad(dec);

    alpha = ra / 2;
    d = dec;
    cos_d = cos(d);
    denom = sqrt(1 + cos(cos_d .* cos(alpha)));
    x = 2 * cos_d .* sin(alpha) ./ denom;
    y = sin(d) ./ denom;
end

function draw_aitoff_circle(RA_center, Dec_center, R)
% Draws a small circle of radius R (degrees) around (RA_center, Dec_center)
% in Aitoff projection.
    steps = 200;
    angles = linspace(0, 2*pi, steps);
    RA_center = deg2rad(RA_center);
    Dec_center = deg2rad(Dec_center);
    R = deg2rad(R);

    % Compute circle on the sphere
    circle_RA = zeros(steps, 1);
    circle_Dec = zeros(steps, 1);

    for j = 1:steps
        angle = angles(j);
        % Great circle offset using spherical trigonometry
        circle_Dec(j) = asin(sin(Dec_center) * cos(R) + cos(Dec_center) * sin(R) * cos(angle));
        dRA = atan2(sin(angle) * sin(R) * cos(Dec_center), cos(R) - sin(Dec_center) * sin(circle_Dec(j)));
        circle_RA(j) = RA_center + dRA;
    end

    % Convert back to degrees and project
    circle_RA = rad2deg(circle_RA);
    circle_Dec = rad2deg(circle_Dec);
    circle_RA = mod(circle_RA + 180, 360) - 180;
    [xc, yc] = aitoff_projection(circle_RA, circle_Dec);
    plot(xc, yc, 'r-', 'LineWidth', 1);
end



