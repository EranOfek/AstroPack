function Result = plannerToO(AlertMapCSV, Args)
    % automatic planner of a ToO
    %     Optional detailed description
    % Input  : - a CSV file with the alert map
    %          - 
    %          * ...,key,val,... 
    %        'ProbThresh' - minimal probability density 
    %        'MaxTargets' - maximal number of targets in a ToO
    %        'Cadence'    - number of target exposures 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2024 Jul) 
    % Example:
    % ultrasat.planner.plannerToO('~/ULTRASAT/SkyGrid/LVC/01/lvc_2024_04_01_00_40_58_000000.csv');
    %
    arguments
        AlertMapCSV            = '~/ULTRASAT/SkyGrid/LVC/2024/04/01/lvc_2024_04_01_00_40_58_000000.csv'; 
        Args.FOVradius         = 7;   % deg
        Args.CleanThresh       = 0.1; % cleaning probability [sr(-1)] 
        Args.ProbThresh        = 0.1; % the limiting probability per ULTRASAT pointing (determines the maximal number of FOVs)
        Args.MinCoveredProb    = 0.5; % the required minimal cumulative probability to be covered
        Args.ThresholdFAR      = 1.3e-7; % [s(-1)] 1.3e-7 ~ one false alarm in 3 months % 3.17e-9 -- 1 in 10 years 
        Args.MaxTargets        = 4;
        Args.Cadence           = 3;
        Args.OutName           = 'ToOplan.json';
        Args.ShowCoverageCurve = false; 
        Args.Verbosity         = 1;
        Args.DrawMaps logical  = true;
        Args.MockAlerts        = false;
        Args.TestAlerts        = false;
        Args.StatisticsOnly    = false;
    end
    
    Sr = (180/pi)^2;  % deg(2)
    FOV = pi*7^2;     % deg(2)
    
    PD  = Args.ProbThresh * ( Sr / FOV ); % the limiting probability per sr (as on the original maps)
    
    % set the structure:
    Result.FileName    = AlertMapCSV;
    Result.CoveredProb = 0;
    Result.Ntarg       = 0;
    Result.Targets     = "";
    Result.MinCoveredProb = Args.MinCoveredProb;
    Result.NCover      = 0;    
    Result.CoveredArea = 0;
    Result.AlertJD     = 0;
    
    % read some of the parameters from the FITS header:
%     AH = AstroHeader(strrep(AlertMapCSV, '.csv', '.fits'),2);
%     Result.Distmean = AH.getVal('Distmean');
%     Result.Diststd  = AH.getVal('Diststd');
           
    Jdata = jsondecode(fileread(strrep(AlertMapCSV, '.csv', '.fits.json')));
    try
        Result.Distmean = Jdata.DISTMEAN;
    catch
        Result.Distmean = NaN;
    end
    try      
        Result.Diststd  = Jdata.DISTSTD;        
    catch
        Result.Diststd  = NaN;
    end
    % read event parameters from a JSON file:
    Jdata = jsondecode(fileread(strrep(AlertMapCSV, '.csv', '.json')));
    Result.Type = Jdata.alert_type;
    Result.Superevent = Jdata.superevent_id;
    Result.Instrument = Jdata.event.instruments;
    Result.DateObs = Jdata.event.time;
    Result.AlertTime = Jdata.time_created;    
    Result.FAR = Jdata.event.far;    
        
    % filtering by the FAR value 
    if Result.FAR > Args.ThresholdFAR         
        if Args.Verbosity > 0
            fprintf('FAR above the threshold \n');
        end
        return
    end
    
    % filtering out mock and test alerts
    if ~Args.MockAlerts && Result.Superevent(1) == 'M' % filter out Mocks 
        if Args.Verbosity > 0
            fprintf('Mock alert filtered out\n');
        end
        return
    end
    if ~Args.TestAlerts && Result.Superevent(1) == 'T' % filter out Tests             
        if Args.Verbosity > 0
            fprintf('Test alert filtered out\n');
        end
        return
    end
    if Args.MockAlerts && Result.Superevent(1) == 'S' % filter out real events
        if Args.Verbosity > 0
            fprintf('Real alert filtered out\n');
        end
        return    
    end 
        
    % read the alert map from a CSV file and filter out points < 0.1 sr(-1)
    Map0 = readtable(AlertMapCSV);
    Map1 = Map0(Map0.PROBDENSITY > Args.CleanThresh,:); 
     
        if Args.Verbosity > 1
            fprintf('Alert CSV map: %s \n',AlertMapCSV)           
            [Prob, Area] = sumProbability(Map0);
            fprintf('Initial probability: %.2f on area of %.1f deg^2 \n',Prob,Area)
        end

    [Prob, Area] = sumProbability(Map1);
    if Args.Verbosity > 0
        fprintf('Cleaned probability: %.2f on area of %.1f deg^2 \n',Prob,Area)
    end
    
    % extract a region with probability over Args.ProbThresh 
    Map = Map1(Map1.PROBDENSITY > PD,:);
    
    [Prob, Area] = sumProbability(Map);
    if Args.Verbosity > 0
        fprintf('Extracted probability: %.2f on area of %.1f deg^2 \n',Prob,Area)
    end
    
    if Prob < 1e-6      
       if Args.Verbosity > 0
           fprintf('No region above Args.ProbThresh found \n');
       end
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
    
    % sort the targets by covered probability (with no overlap treatment!) 
    [~, Ind] = sort([Targets0.Pr], 'descend'); 
     
    if Args.ShowCoverageCurve
        It = 0;
        Nthresh = numel(Args.MinCoveredProb);        
        Result.NCover(1:Nthresh) = 0;
        Result.CoveredArea(1:Nthresh) = 0;
        while It < Ntarg0 && Result.NCover(Nthresh) < 1
            It = It+1;
            Targets = Targets0(Ind(1:It));  % select first It targets
            TargCoo = cell2mat(arrayfun(@(x) x.Coo, Targets, 'UniformOutput', false)');
            [CoveredProb, CoveredArea] = sumProbability(Map,'Targets',TargCoo,'FOVradius',Args.FOVradius);  
            for i = 1:Nthresh
            if CoveredProb > Args.MinCoveredProb(i) &&  Result.NCover(i) < 1 
                Result.NCover(i) = It;
                Result.CoveredArea(i) = CoveredArea;
            end
            end
        end
    end
    
    if Args.StatisticsOnly
        %%%%% !!!! TEMPORARY STOP FOR COLLECTING STATISTICS ONLY !!!!
        return
    end
            
    % select no more than Args.MaxTargets targets with highest probability
    Result.Ntarg = min(Result.NCover,Args.MaxTargets);
    
    Targets = Targets0(Ind(1:Result.Ntarg));   % take only first Result.Ntarg from the ordered list   
        
%     Result.CoveredProb = sum([Targets.Pr]); %% This is not correct due to overlaps!
    TargCoo = cell2mat(arrayfun(@(x) x.Coo, Targets, 'UniformOutput', false)');
    Result.CoveredProb = sumProbability(Map,'Targets',TargCoo,'FOVradius',Args.FOVradius);
    
    if Args.Verbosity > 1
        fprintf('Selected %d FOVs with highest probability \n',Result.Ntarg)
        fprintf('Covered probability (with overlap, can be > 1!): %.2f \n',Result.CoveredProb)
    end
    
    % illustration:
    if Args.DrawMaps        
        for Itarg = 1:Result.NCover
            plot.skyCircles(Targets0(Ind(Itarg)).Coo(1),Targets0(Ind(Itarg)).Coo(2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','green');
        end
        for Itarg = 1:Result.Ntarg
            plot.skyCircles(Targets(Itarg).Coo(1),Targets(Itarg).Coo(2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','red');
        end
    end
    
    % replicate the targets according to the Cadence
    Targets = repmat(Targets, 1, Args.Cadence);

    % schedule the targets
    Targets = schedulerToO(Targets);
    
    % write the plans to a JSON file, removing the probabilities     
    Result.Targets = jsonencode(rmfield(Targets,'Pr'));
    
    FID = fopen(Args.OutName,'w'); 
    fprintf(FID,Result.Targets);
    fclose(FID);
    
end

%%% internal functions will be later replaced to calls to external tools

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
            % illustration:
            if Args.DrawMaps
                plot.skyCircles(Grid0(Ip,1),Grid0(Ip,2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','blue');
            end
%            fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
        end
    end

end

function Targets = schedulerToO(Targets, Args)
    % schedule a list of targets
    %
    % Input : - a list of target coordinates
    %         -
    %         * ...,key,val,...
    % Output: 

    Nt = size(Targets);
    
    % find the nearest visibility window for each of the targets 
    
    % order targets
    
    Targets(1).StartTime = 2567.8; % JD
    Targets(2).StartTime = 2567.9; % JD
    
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
    SRAD = RAD*RAD;  % deg(2)
    
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
    
    Prob = Map.PROBDENSITY ./ SRAD; % probability per deg^2
    
    Ind  = floor(log(Map.UNIQ/4)/(2*log(2)));
    
    SumProb = sum(NsideAreaDeg(Ind(:,1),2).*Prob);    
    SumArea = sum(NsideAreaDeg(Ind(:,1),2));  % deg^2
end
