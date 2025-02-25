function [RA, Dec, Stat] = coverProbMap(SkyMap, Args)
    % optimal coverage of a probability sky map with (circular) exposures
    %     Optional detailed description
    % Input  : - a normalized probability map: table of HEALPIX numbers and corresponding probabilities  
    %          * ...,key,val,... 
    % Output : - a set of [RA, Dec] coordinates of FOV centers 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: Map = '~/ULTRASAT/SkyGrid/LVC/2024/04/01/lvc_2024_04_01_00_40_58_000000.csv';
    %         [RA, Dec, ~] = ultrasat.tools.coverProbMap(Map,'MaxTarg',4); 
    arguments
        SkyMap      
        Args.MaxTarg           = 4;    % maximal number of exposures (unqi targets) to use
        Args.MinProb           = 0.5;  % minimal cumulative probability covered
        Args.MinAddedProb      = 0.05; % once adding one more target increases the sum covered probability 
                                       % by less than this value, we does not need it and stop               
        Args.FOVradius         = 7.0;  % [deg] 
        Args.CleanThresh       = 0.1;  % cleaning probability [sr(-1)] 
        Args.ProbThresh        = 0.03; % the limiting probability per ULTRASAT FOV (determines the maximal number of FOVs)
        
        Args.Verbosity         = 2;    
        Args.DrawMaps          = true;
        Args.CalcCoverageCurve = true;
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
    % find all the 7-deg grid pixels intersecting with any of the alert pixels
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

