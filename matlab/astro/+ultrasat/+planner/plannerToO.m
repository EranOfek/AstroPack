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
    %
    arguments
        AlertMapCSV            = 'LVC/01/lvc_2024_04_01_00_40_58_000000.csv'; 
        Args.CleanThresh       = 0.1; % cleaning probability [sr(-1)] 
        Args.ProbThresh        = 0.2; % the limiting probability per ULTRASAT pointing 
        Args.MaxTargets        = 4;
        Args.Cadence           = 3;
        Args.OutName           = 'ToOplan.json';
    end
    
    Sr = (180/pi)^2;  % deg(2)
    FOV = pi*7^2;     % deg(2)
    
    PD  = Args.ProbThresh * ( Sr / FOV );

    % read the alert map from a CSV file and filter out points < 0.1 sr(-1)
    Map0 = readtable(AlertMapCSV);
    
    SumProb = sumProbability(Map0);
    
    Map1 = Map0(Map0.PROBDENSITY > Args.CleanThresh,:);
    
    % extract a region with probability over Args.ProbThresh 
    Map = Map1(Map1.PROBDENSITY > PD,:);
    
    figure(1)
    subplot(3,1,1); plot(Map1.RA,Map1.DEC,'*')
    subplot(3,1,2); plot(Map.RA,Map.DEC,'*')
    subplot(3,1,3); plot.ungridded_image(Map.RA,Map.DEC,Map.PROBDENSITY);
    
    
    % cover the region with targets
    Targets = coverSky(Map);
    
    % replicate the targets according to the Cadence
    Targets = repmat(Targets, 1, Args.Cadence);

    % schedule the targets
    Targets = schedulerToO(Targets);
    
    % write the plans to a JSON file     
    Result = jsonencode(Targets);
    
    FID = fopen(Args.OutName,'w'); 
    fprintf(FID,Result);
    fclose(FID);
    
end

%%% internal functions will be replaced later to calls to external tools

function Targets = coverSky(Map, Args)
    %
    arguments
        Map
        Args.FOVradius       = 7; % deg 
        Args.InitialGridFile = '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_350_rep1.txt'
    end
    %
    RAD = 180/pi;
    
    Grid0 = readmatrix(Args.InitialGridFile);
    Np    = length(Grid0);
    Grid  = zeros(Np,2);
    
    figure(2); clf
    axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
    
    plotm(Map.DEC./RAD,Map.RA./RAD,'*') 
    
    % find all the 7-deg grid pixels intersecting with any of the alert pixels
    ITarg = 0;
    for Ip = 1:Np
        Rd = celestial.coo.sphere_dist_fast(Grid0(Ip,1)/RAD,Grid0(Ip,2)/RAD,Map.RA./RAD,Map.DEC./RAD);
        if any(Rd < Args.FOVradius/RAD) 
            ITarg = ITarg + 1;
            Targets(ITarg).Coo = Grid0(Ip,:); 
            % illustration:
            plot.skyCircles(Grid0(Ip,1),Grid0(Ip,2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','blue');
            fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
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

function SumProb = sumProbability(Map)

    NsideRad = [2, 27.585653017957394; ... % radius of healpix in deg
        4, 14.5722306700779; ...
        8,  7.47282699728271; ...
        16,  3.7823672156460226; ...
        32,  1.902601860011511; ...
        64,  0.9541480607387777; ...
        128,  0.47778497003680387; ...
        256,  0.23907012000928965; ...
        512,  0.11957945660469947; ...
        1024,  0.059800825955419704; ...
        2048,  0.02990318720521464; ...
        4096,  0.014952287136343813; ...
        8192,  0.007476316948721642; ...
        16384,  0.00373820181913693; ...
        32768,  0.0018691117457205135; ...
        65536,  0.0009345585818920722; ...
        131072,  0.00046727996820400576; ...
        262144,  0.00023364015341454294; ...
        524288,  0.00011682011904060968; ...
        1048576,  5.841007009601502e-05];

    RAD  = 180/pi;      % deg
    SRAD = (180/pi)^2;  % deg(2)
    
    Uniq = Map.UNIQ;
    Prob = Map.PROBDENSITY ./ SRAD; % probability per deg^2
    
    Ind  = floor(log(Uniq/4)/(2*log(2)))-1;
    PixRad = NsideRad(Ind(:,1),2);
    
    SumProb = sum(pi*PixRad.^2.*Prob);
end