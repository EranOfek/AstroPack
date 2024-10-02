classdef uplanner < Component 
    
    properties(Access = public)
        Type
        StartTime       
        EndTime
        Plan
        UniqTargList
        SurvProp
        AllSSProp
%         TOOProp
        MaxTargets      = [];
        TOOProbMap      = [];
        Scheduled       = [];
        Validated       = [];
        Status          = 'draft';
    end

    properties(Access = private)
        
    end
    %
    methods  % Constructor
        function Obj = uplanner(Args)
            % object constructor
            % example: P = ultrasat.planner.uplanner;
            arguments                
                Args.Type        = '';   % plan type  
                Args.StartTime   = '2028-01-01T00:00:01';
                Args.EndTime     = '2031-12-31T23:23:59';
                Args.PlanColumns = {'TargInd','Tstart','JDstart','ExpTime','Tiles',...
                                    'MoonDist','SunDist','EarthDist','SlewDist','OverlapTargets'};
                Args.TargColumns = {'RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj', 'Vis'};
%                 Args.SurvColumns = {'DailyWindow', 'Cadence', 'EpochsPerVisit'};
%                 Args.AllSSColumns= {'HighLatThresh', 'HighLatVisits', 'LowLatVisits', 'DitherPattern'};
%                 Args.TOOColumns  = {'MaxTargets','ProbMapName'};
            end
            %
            if isempty(Args.Type)
                Obj.Type = 'DDT';
            else
                Obj.Type = Args.Type;
            end
            %
            Obj.StartTime = Args.StartTime;
            Obj.EndTime   = Args.EndTime;
            %
            Obj.Plan = table([],[],[],[],[],[],[],[],[],[],'VariableNames', Args.PlanColumns);
            %
            Obj.UniqTargList = table([],[],[],[],[],[],[],[],'VariableNames', Args.TargColumns);
            %
%             Obj.SurvProp = table([],[],[],'VariableNames', Args.SurvColumns);
            % 
%             Obj.AllSSProp = table([],[],[],[],'VariableNames', Args.AllSSColumns);
            %
%             Obj.TOOProp = table([],[],'VariableNames', Args.TOOColumns);
        end
    end
    %
    methods %  
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
        %
        function findVisibilitySlots(Obj,Args)
            % find visibility slots 
            arguments
                Obj
                Args.UninterruptedDays = [];   % days   % later make all these different for each sky point
                Args.NumExp            = 3;
                Args.Expo              = 300;  % [s]   
                Args.NSlots            = 1;  
            end
            SecPerDay = 86400;
            % calculate the desired exposure in time bins 
            if ~isempty(Args.UninterruptedDays)  % find visibility for an uninterrupted period in time bins
                Obj.ExpBins = Args.UninterruptedDays/Obj.TimeBin; 
            else                                 % find visibility for an uninterrupted period in exposures
                Obj.ExpBins = Args.NumExp .* Args.Expo ./ SecPerDay /Obj.TimeBin; % [days] 
            end
            
            % find first NSlots slots of length Obj.ExpD for each of the Obj.Coo points
            Obj.StartSlot(Args.NSlots,Obj.NCoo) = 0;
            
            for Ip = 1:Obj.NCoo           % loop by coordinates (objects)
                It    = 0; 
                ISlot = 0;
                while ISlot < Args.NSlots % loop by slots               
                    ISlot   = ISlot + 1; 
                    SlotVis = 0;                     
                    while It < Obj.NumJD  % loop by time marks 
                        It = It + 1;                         
                        if Obj.CombVis(It,Ip) == 1
                            SlotVis = SlotVis + 1;
                        else
                            SlotVis = 0;
                        end
                        if SlotVis > Obj.ExpBins 
                            Obj.StartSlot(ISlot,Ip) = It-SlotVis+1;
                            break;
                        end
                    end
                end
            end
            Obj.StartSlotTime = Obj.StartDate + Obj.StartSlot .* Args.Expo ./ SecPerDay;  
        end
    end

    methods(Static)
        Result = debug()
            % unitTest
        Result = unitTest()
            % unitTest
    end
end
