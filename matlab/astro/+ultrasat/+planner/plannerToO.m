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
        AlertMapCSV            = 'alert.csv'; 
        Args.ProbThresh        = 0.9; 
        Args.MaxTargets        = 5;
        Args.Cadence           = 3;
        Args.OutName           = 'ToOplan.json';
    end

    % read the alert map from a CSV file
    Map = readtable(AlertMapCSV);
    
    % extract a region with probability over Args.ProbThresh 
    Map = Map(Map.PROBDENSITY > Args.ProbThresh,:);
    
    % cut the region into targets 
    Targets(1).Coo = [10, -80];
    Targets(2).Coo = [11, -81];
    
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