classdef Visibility < PlannerToolBase
    %
    properties(Access = public)   
        StartDate % all the object times are JD 
        EndDate   
        Coo 
        StartSlotTime % possible start time(s) for each sky point 
    end

    properties(Access = private)
        TimeBin = 0.01; % [days] 0.01 day = 864 s ~ 3 x 300 s 
        JD
        NumJD
        ExpBins
        NCoo
        Vis
        CombVis
        CombVisPower
        StartSlot
    end
    %
    methods  % Constructor
        function Obj = Visibility(Args)
            % object constructor
            % example: V = ultrasat.planner.plannerTools.Visibility('StartDate',[2028 01 01]);
            %          V.calcVisibility('Coo',[90 45])
            %          V.findVisibilitySlots('NSlots',10)
            %          V.StartSlotTime or
            %          int16(celestial.time.jd2date(V.StartSlotTime,'h')) 
            arguments                
                Args.StartDate   = [];   % JD (UTC) or any format compatible with celestial.time.date2jd
                Args.Period      = 180;  % [days] find visibility windows within this period of time 
                Args.Coo         = [];   % a 2-column matrix of [RA, Dec] in [deg]
            end
            %
            if ~isempty(Args.StartDate)
                if isnumeric(Args.StartDate) && isscalar(Args.StartDate) && isreal(Args.StartDate) 
                    Obj.StartDate = Args.StartDate;
                else
                    Obj.StartDate = celestial.time.date2jd(Args.StartDate);
                end
            else
                Obj.StartDate = celestial.time.date2jd; % take the current moment in UTC 
            end
            Obj.EndDate = Obj.StartDate + Args.Period; 
            
            Obj.Coo = Args.Coo;
        end
    end
    %
    methods % visibility functions 
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
