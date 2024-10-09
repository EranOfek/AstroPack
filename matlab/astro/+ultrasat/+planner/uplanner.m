classdef uplanner < Component 
    
    properties(Access = public)
        Type                     % HCS, LCS, AllSS, DDT, TOO 
        StartTime       
        EndTime
        Plan
        UniqTargList
        Vis 
        
        % HCS, LCS
        DailyWindow     
        Cadence         
        
        % AllSS
        AllSSHighLatThresh = 30; % |RA| [deg]
        HighLatVisits      = 16; % 1 visit = 3 x 300 s 
        LowLatVisits       =  2;
        EpochPerVisit      =  3;       
        DitherPattern      = '2x2';
        
        % TOO
        TOOMaxTargets      =  4;
        TOOProbMap      
        
        Scheduled                % date or empty
        Validated                % date or empty
        Status             = 'draft';
    end

    properties(Access = private)
        
    end
    %
    methods  % Constructor
        function Obj = uplanner(Args)
            % object constructor
            % example: P = ultrasat.planner.uplanner;
            arguments                
                Args.Type        = '';   % plan type: HCS, LCS, AllSS, DDT, TOO  
                Args.StartTime   = '2028-01-01T00:00:01';
                Args.EndTime     = '2031-12-31T23:23:59';
                
                Args.PlanColumns = {'TargInd','Tstart','JDstart','ExpTime','Tiles',...
                                    'MoonDist','SunDist','EarthDist','SlewDist','OverlapTargets'};
                Args.TargColumns = {'RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj'};
                
                Args.DailyWindow = [];   % length in hours
                Args.Cadence     = [];   % cadence in days                
                
                Args.TOOMaxTargets = 4; 
            end
            %
            if ~isempty(Args.StartTime) 
                Obj.StartTime = Args.StartTime;
            end
            %
            if ~isempty(Args.EndTime)             
                Obj.EndTime   = Args.EndTime;
            end
            %
            if isempty(Args.Type)
                Obj.Type = 'DDT';
            else
                Obj.Type = Args.Type;
            end
            % 
            if ~isempty(Args.DailyWindow)
                Obj.DailyWindow = Args.DailyWindow;
            end
            %
            if ~isempty(Args.Cadence) 
                Obj.Cadence = Args.Cadence;
            end
            %
            if ~isempty(Args.TOOMaxTargets) 
                Obj.TOOMaxTargets = Args.TOOMaxTargets;
            end
            % 
            Obj.Plan = table([],[],[],[],[],[],[],[],[],[],'VariableNames', Args.PlanColumns); 
            %
            Obj.UniqTargList = table([],[],[],[],[],[],[],'VariableNames', Args.TargColumns); 
            %
        end
    end
    %
    methods % Building the plans          
        %
        function buildDDT(Obj, Args)
            % build a plan for a list of DDT targets
            arguments
                Obj
                Args.Coo
            end
            % check visibility within the given time interval for each of the targets
            % 
            % fill in the target list 
            %           
            % show which observations in the existing plan are to be replaced 
            % 
            % select 1 visibility window for each of the targets and write them to Obj.Plan 
        end
        %
        function buildTOO(Obj, Args)
            % build a plan for a TOO map 
            arguments
                Obj 
                Args.Map 
            end
        end
        %
    end
    %
    methods % Auxiliary functions
        %
        function calcVisibility(Obj, Args)
            % calculate visibility for the given period and time bin
            arguments
                Obj
                Args.Coo               = []; % 2-column matrix of [RA, Dec] in [deg]               
                Args.TimeBin           = []; % [days] 
                Args.SunDist           = 70; % [deg]
                Args.MoonDist          = 34; % [deg]
                Args.EarthDist         = 56; % [deg]
            end
            %
            RAD = 180/pi;
            %
            if ~isempty(Args.TimeBin)
                Obj.TimeBin = Args.TimeBin;
            end
            %
            if ~isempty(Args.Coo)
                Obj.Coo = Args.Coo;            
            end
            if isempty(Obj.Coo)
                error('No coordinates found as function input or object property')
            else
                Obj.NCoo = size(Obj.Coo,1); % number of sky points 
            end
            %
            Obj.JD    = Obj.StartDate + (0:Obj.TimeBin:(Obj.EndDate-Obj.StartDate))';
            Obj.NumJD = numel(Obj.JD);
            
            Obj.Vis  = ultrasat.ULTRASAT_restricted_visibility(Obj.JD,Obj.Coo/RAD,...
                'MinSunDist',Args.SunDist/RAD,'MinMoonDist',Args.MoonDist/RAD,'MinEarthDist',Args.EarthDist/RAD);
            
            Obj.CombVis      = Obj.Vis.SunLimits .* Obj.Vis.MoonLimits .* Obj.Vis.EarthLimits;
            Obj.CombVisPower = Obj.CombVis .* Obj.Vis.PowerLimits; 
        end
    end
    % 
    methods(Static)
        Result = debug()
            % unitTest
        Result = unitTest()
            % unitTest
    end
end
