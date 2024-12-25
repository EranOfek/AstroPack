%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of functions:
% - ultrasat.planner.uplanner(Args): Constructor
%
% - Obj.set.Type(Type)             : Setter. Verify allowed Type
% - Obj.set.StartTime(StartTime)   : Setter. Also sets TimeZone of StartTime
% - Obj.set.EndTime(EndTime)       : Setter. Also sets TimeZone of EndTime
%
% - Obj.buildHCS                   : Build a plan for a HCS field. 
%                                    All relevant parameters should be set before calling this function
%                                    (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTargList) ==1)
%
% - Obj.buildLCS(Args)             : Build a plan for a Targetlist of LCS fields. If a list is not provided, uses all targets in the unique target list.
%                                    Fill in a daily window of observations and move to the next day. 
%                                    All relevant parameters should be set before calling this function
%                                    (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit/DailyWindowStartTime/DailyWindowMaxDuration/ height(Obj.UniqTargList)>0)
%
% - Obj.buildTOO(Args)             : Build a plan for a TOO list. Allow to enter all paramters as Args (but can also use those that are in Obj) 
%                                    Looping over a list of targets within a time window set by TOOStartTime and TOOWindowDuration
%                                    TODO - should add optimal covarge plan(s) of ProbabiltyMap.
%
% - Obj.addDDT2Plan(TargetList,StartTime,Args)  : Add to the plan a list of DDT targets (TargetList) as a group, starting at StartTime.
%                                                 Mutliple additions to the Plan are allowed. 
%                                                 However, no pre-defined loops over the list (within a window or within days)
%
% - Obj.buildAllSS(Args)           : TODO - write build All Sky-Survey function (currently empty function)
%
%
% - Obj.addUniqTargets(RA, Dec, Args)                       : Add a list of [RA,Dec] coordinates (in degrees) to the unique targetList, 
%                                                             and calls Obj.updateTargetProperties and Obj.updateTargetVisibility
% - Obj.scheduleTargets(UniqTargetIndexes,StartTime,Args)   : Schedule a group of targets, starting at StartTime following by the rest, taking into account slew time between targets.
%                                                             TODO- allow to provide a list of StartTime, one for each of target in the list.
% - Obj.retrieveMissionApprovedPlan(Args)                   : Retrive the mission approved plan in a given time window (default window is Obj.CheckTimes) 
%                                                             and populate the fields of Obj.MissionApprovedPlan.
%                                                             Alternativly, allows also to provide a uplanner object (taking its plan as the MissionApprovedPlan) or struct of approved targets.
%
% - Obj.clearUniqueTargets                                  : Clear the unique target list, as well as the plan and visibility object
% - Obj.clearPlan                                           : Clear the plan
% - Obj.clearMissionApprovedPlan                            : Clear the Mission Approved Plan table
%
%
% - Obj.adjustGroupStartTime(Args)                          : Adjust the start time of a group in the plan by 3 options: 
%                                                                  a given NewStartTime, a given ShiftTime, or relative to a target in the OverLap targets list.
%                                                             If no GroupList is provided, will adjust all groups in the plan, one by one.
% - Obj.updateTargetProperties(Args)                        : Fill for each of the unique targets the following properties: extinction (A_U), calibrating objects within FoV (CalObj),
%                                                               (TODO) reference images  within FoV (RefImageIDs), external surveys overlaping with the FoV (ExtSurveys),
%                                                               specific known objects (e.g., planets, massive stars, blazars) within the FOV (FieldObj)
%                                                             TODO - should allow to update only selected targets (i.e., new targets)
% - Obj.updateTargetVisibility(Args)                        : Calcuate visibility for all unique targets for a given time window (default window is Obj.CheckTimes)
% - Obj.adjustCheckTimes(CheckStartTime,CheckEndTime)       : Set Obj.CheckTimes and then calls Obj.updateTargetVisibility and Obj.retrieveMissionApprovedPlan
%
% - Obj.schedule                                            : Set Obj.Status to 'draft' and Obj.Scheduled time to 'now'. (called from Obj.scheduleTargets)
% - Obj.validate                                            : TODO - send plan to the validator. In addition, set Obj.Status to 'validated' and Obj.Validated time to 'now'
% - Obj.submit                                              : TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.Submitted time to 'now'
%
% - Res = Obj.showCalibObj(Ind,Args)                        : Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Additional functions to be considered:
% - planSelfConsistencyCheck               : Verify that the plan schedule is self consistent
% - retrieveExecutedObsMap                 : Retrieve of executed observations maps for a given field / coordinate
% - plotPlan                                            : Plot the plan targets on a sky map, optionally with the overalpping targets, calibrating stars, refernce images, Sky Catalogs, extinction map, executed obs maps, etc.
% - plotUniqTarg                                    : Plot the UniqTarget targets on a sky map, optionally with the calibrating stars, refernce images, extinction map, Sky Catalogs, executed obs maps, etc.
% - plotVisibility                                       : Display the visibilty constrains of the targets
% - expectedRoll                                     : Calculate the expcted roll angle
% several optimized plannaing functions\tools (e.g., covarge of an area, plan AllSS - 2 options, mutiple ToO plans)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classdef uplanner < Component 
    % 
    properties(Access = public)
        Type                char            % HCS, LCS, AllSS, DDT, TOO 
        StartTime           datetime   % start of the whole plan
        EndTime             datetime   %   end of the whole plan
        Plan                                    % target list 
        UniqTargList                       % unique target list
        
        CheckTimes(2,1)     datetime   ={'2028-01-01 00:00:00','2028-07-01 00:00:00'};
        Vis                                     % visibility matrix         
        MissionApprovedPlan          % Approved Mission Plan retrvied  from C&C 
        
        DefEpochsPerVisit   uint8       =  3; 
        Exptime             duration    = seconds(300); %[s]
        Tiles               string        = ['1','2','3','4']; %
        DefSlewBuffer       duration    = seconds(5);
        FullTileReadTime    duration    = seconds(15); % Time from start read of first row to last. This time will be added to each row in plan (before slew to next target..
        
        % LCS / AllSS
        DailyWindowStartTime    duration    =  duration(10,00,00); % [hrs]   
        DailyWindowMaxDuration  duration    =  hours(3);       % [hrs]
        
        % AllSS
        AllSSHighLatThresh  double      = 30; % |RA| [deg]
        HighLatVisits       uint8       = 16; % 1 visit = 3 x 300 s 
        LowLatVisits        uint8       =  2;      
        DitherPattern                   = '2x2';
        
        % TOO
        TOOStartTime              datetime    =  datetime('now'); % [hrs]   
        TOOWindowDuration  duration    =  hours(3);       % [hrs]
        %TOOMaxTargets          uint8       =  4;   % Unused for now - check if needed later
        %TOOProbMap                                 % Unused for now - check if needed later 
        
        N_uniqueTargets     uint8       =  0; % number of unique targets
        N_planTargets       uint8       =  0; % number of targets in the plan
        
        Rfov                            =  10; % [deg] FOV radius conservative, w/o roll information
        
        CalibObj                        = []; % table of calibration objects 
        CalibDir           
        
        Scheduled           datetime    % date or empty
        Validated           datetime    % date or empty
        Submitted           datetime    % date or empty
        Status              char        = 'draft';
        
        AstPlanner          char        % name of the Astronomer-Planner
    end
    % 
    properties(Hidden, Constant)
        Plan_AllowedTypes  = {'HCS', 'LCS', 'AllSS', 'DDT', 'TOO'};
        
        SysTimeZone        = 'UTC';
        
        Plan_DefVarNames   = {'Name','UniqTargInd','Group','RA', 'Dec','Roll','Tiles',...
                              'Tstart','Tend','JDstart','JDend','ExpTime','Nexposures','TotalDuration','SlewTimeBefore',...
                              'NoComm','HardObs','MoonDist','SunDist','EarthDist','Zody','LimMag','OverlapTargets'};
        Plan_DefVarTypes   = {'char','uint8','uint8','double','double','double','string',...
                              'datetime','datetime','double','double','duration','double','duration','duration',...
                              'logical','logical','double','double','double','double','double','cell'};
                                                                
        Target_DefVarNames = {'Name','RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj'};
        Target_DefVarTypes = {'char','double','double', 'double', 'cell', 'cell', 'cell', 'cell'};  
        
        MissionApprovedPlan_VarNames   = {'TargetID','RA', 'Dec','Roll',...
                              'Tstart','Tend','ExpTime','Nexposures','TotalDuration'};
        MissionApprovedPlan_VarTypes   = {'char','double','double','double',...
                              'datetime','datetime','duration','double','duration'};        
        
        ObsSunDist           = 70;   % [deg]
        ObsMoonDist          = 34;   % [deg]
        ObsEarthDist         = 56;   % [deg]
        
    end 
    % 
    methods  % Constructor
        function Obj = uplanner(Args)
            % object constructor
            % example: up = ultrasat.planner.uplanner('AstPlanner','YS');
            arguments                
                Args.Type        = '';   % plan type: HCS, LCS, AllSS, DDT, TOO  
                
                Args.AstPlanner  = '';
                
                Args.CalObj      = '~/matlab/data/ULTRASAT/starlib23_table.mat';  % the calibration objects' list 
                Args.CalDir      = '~/matlab/data/ULTRASAT/Calib/';               % the catibration objects' spectra    
            end
            %          
            if isempty(Args.AstPlanner) 
                error('Planner Name is missing');
            else
                Obj.AstPlanner= Args.AstPlanner;  
            end
            %
            if ~isempty(Args.Type)
                Obj.Type = Args.Type;               
            end
            % 
            Obj.StartTime.TimeZone = Obj.SysTimeZone;
            Obj.EndTime.TimeZone = Obj.SysTimeZone;
            %
            Obj.Plan = table('Size',[Obj.N_planTargets,numel(Obj.Plan_DefVarNames)],'VariableNames', Obj.Plan_DefVarNames,...
                                'VariableTypes',Obj.Plan_DefVarTypes);
                            
            Obj.Plan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.Plan.Tend.TimeZone = Obj.SysTimeZone;
            %
            Obj.UniqTargList = table('Size',[Obj.N_uniqueTargets,numel(Obj.Target_DefVarNames)],'VariableNames', Obj.Target_DefVarNames,...
                                'VariableTypes',Obj.Target_DefVarTypes); 
            %
            Obj.MissionApprovedPlan = table('Size',[0,numel(Obj.MissionApprovedPlan_VarNames)],'VariableNames', Obj.MissionApprovedPlan_VarNames,...
                                'VariableTypes',Obj.MissionApprovedPlan_VarTypes);           
                            
            Obj.MissionApprovedPlan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.MissionApprovedPlan.Tend.TimeZone = Obj.SysTimeZone;                            
            %
            load(Args.CalObj); % load the calibration objects' table
            Obj.CalibObj = CalibObj;
            Obj.CalibDir = Args.CalDir;
        end
    end 
    %
    methods % Setters/Getters
        function set.Type(Obj, Type)
            % setter for Plan Type - verify Type is from the allowed list
            if any(strcmp(Type,Obj.Plan_AllowedTypes))
                Obj.Type = Type;
            else
                error('Unknown Plan Type')
            end
        end
        %
        function set.StartTime(Obj, StartTime)
            % setter for StartTime - make sure in UTC
            Obj.StartTime = datetime(StartTime);
            Obj.StartTime.TimeZone = Obj.SysTimeZone;
        end
        %
        function set.EndTime(Obj, EndTime)
            % setter for EndTime - make sure in UTC
            Obj.EndTime = datetime(EndTime);
            Obj.EndTime.TimeZone = Obj.SysTimeZone;
        end
    end
    %
    methods % Building the plans          
        %
        function buildHCS(Obj)
            % Build a plan for a HCS field. 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTargList) ==1)
            
            % Verify all relevant parameters are set
            
            if ~strcmp(Obj.Type,'HCS')
                error('Plan Type is not HCS');
            end
            if isempty(Obj.StartTime) || isempty(Obj.EndTime) || isempty(Obj.Exptime) || isempty(Obj.Tiles)
                error('Missing params (StartTime/EndTime/Exptime/Tiles)');
            end
            if Obj.StartTime > Obj.EndTime
                error('StartTime is after EndTime');
            end
            if height(Obj.UniqTargList) ~=1
                error('HCS requires one single target');
            end
                  
            % Calc number of exposures within the plan time 
            Nexposures = floor((Obj.EndTime-Obj.StartTime)/Obj.Exptime);
            
            % Schedule HCS field
            Obj.scheduleTargets(1,Obj.StartTime,'Nexposures',Nexposures);
            
            % make a schedule 
            % show which observations in the existing plan are to be replaced 
                % this is not needed for the HCS?
            % validate the plan
%            Obj.validate
            % submit the plan as JSON and save the plan in a .mat object
%            Obj.submit
        end
        %
        function buildLCS(Obj,Args)
            % Build a plan for a Targetlist of LCS fields. If a list is not provided, uses all targets in the unique target list.
            % Fill in a daily window of observations and move to the next day. 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit/DailyWindowStartTime/DailyWindowMaxDuration/ height(Obj.UniqTargList)>0)
            arguments
                Obj
                Args.TargetList = [];
            end
           
            % Verify all relevant parameters are set
            
            if ~strcmp(Obj.Type,'LCS')
                error('Plan Type is not LCS');
            end
            if isempty(Obj.StartTime) || isempty(Obj.EndTime) || isempty(Obj.Exptime) || isempty(Obj.Tiles) || isempty(Obj.DefEpochsPerVisit)
                error('Missing params (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit)');
            end
            if isempty(Obj.DailyWindowStartTime) || isempty(Obj.DailyWindowMaxDuration)
                error('Missing LCS window params (DailyWindowStartTime/DailyWindowMaxDuration)');
            end
            if Obj.StartTime > Obj.EndTime
                error('StartTime is after EndTime');
            end
            if Obj.DailyWindowMaxDuration > hours(24)
               error('Daily window is LONGER than a DAY'); 
            end
            if height(Obj.UniqTargList) == 0
                error('LCS reuire at least one target');
            end         
            
            if isempty(Args.TargetList)
                Args.TargetList = 1:height(Obj.UniqTargList);
            end
                
            %Calc expected number of targets fit in single window
            NUtarg = numel(Args.TargetList);

            MaxTargPerWindow = floor(Obj.DailyWindowMaxDuration / (double(Obj.DefEpochsPerVisit) * Obj.Exptime + Obj.DefSlewBuffer + Obj.FullTileReadTime + seconds(100))); % last argument is conservative slew time
             
            CurrStartTime = dateshift(Obj.StartTime,'start','day') + Obj.DailyWindowStartTime;
            if CurrStartTime < Obj.StartTime
                CurrStartTime = CurrStartTime+1;
            end
            Obj.StartTime = CurrStartTime;
            
            MaxEndTime = Obj.EndTime;
            
            CurrGroup = 1;
            CurrFirstTargetInd = 1;
            
            while (CurrStartTime+Obj.DailyWindowMaxDuration) < MaxEndTime
                LastTarget = min(NUtarg,CurrFirstTargetInd+MaxTargPerWindow-1);
                
                % Schedule daily LCS fields
                Obj.scheduleTargets(Args.TargetList(CurrFirstTargetInd:LastTarget),CurrStartTime,'Group',CurrGroup);
                
                % Set next day params
                CurrGroup = CurrGroup +1;
                
                CurrFirstTargetInd = LastTarget +1;
                if CurrFirstTargetInd > NUtarg
                    CurrFirstTargetInd = 1;
                end
                
                CurrStartTime = CurrStartTime +1; % add 1 day           
            end               
            
        end
        %
        function buildTOO(Obj, Args)
            % Build a plan for a TOO list. Allow to enter all paramters as Args (but can also use those that are in Obj) 
            % Looping over a list of targets within a time window set by TOOStartTime and TOOWindowDuration
            % TODO - should add optimal covarge plan(s) of ProbabiltyMap.
            arguments
                Obj 
                Args.Map                           = [];
                Args.RA                             = [];
                Args.Dec                            = [];
                Args.Name                        = {};
                Args.TOOStartTime            = [];
                Args.TOOWindowDuration = [];
                Args.EpochsPerVisit           = [];
                Args.ExpTime                     = [];
                Args.SlewBuffer                  = [];
                Args.Tiles                            = [];
            end
            
            if ~strcmp(Obj.Type,'TOO')
                error('Plan Type is not TOO');
            end
            
            if ~isempty(Args.TOOStartTime)
                Obj.TOOStartTime = Args.TOOStartTime;
            end
            if ~isempty(Args.TOOWindowDuration)
                Obj.TOOWindowDuration = Args.TOOWindowDuration;
            end
            if ~isempty(Args.EpochsPerVisit)
                Obj.DefEpochsPerVisit = Args.EpochsPerVisit;
            end      
             if ~isempty(Args.ExpTime)
                Obj.ExpTime = Args.ExpTime;
            end                 
            if ~isempty(Args.SlewBuffer)
                Obj.DefSlewBuffer = Args.SlewBuffer;
            end     
             if ~isempty(Args.Tiles)
                Obj.Tiles = Args.Tiles;
            end     
            
            
            Obj.StartTime = Obj.TOOStartTime;
            Obj.EndTime = Obj.TOOStartTime + Obj.TOOWindowDuration;
            
            if ~isempty(Args.Map)
                % TODO - do somethng with a map
            elseif ~isempty(Args.RA) && ~isempty(Args.Dec) && numel(Args.RA)==numel(Args.Dec)
                Obj.addUniqTargets(Args.RA,Args.Dec,'Name',Args.Name);                
            else
                error('No TOO targets/map');
            end
            
            % Check visibility - TODO: later change error to active action
            if ~all(Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits ,1)
                error('Issue with Sun/Earth/Moon limits');
            end
            
            % Loop over the targets within the window
            NTargets = numel(Args.RA);
            
            MaxTargInWindow = floor(Obj.TOOWindowDuration / (double(Obj.DefEpochsPerVisit) * Obj.Exptime + Obj.DefSlewBuffer + Obj.FullTileReadTime + seconds(100))); % last argument is conservative slew time
            
            Obj.scheduleTargets([repmat(1:NTargets,1,floor(MaxTargInWindow/NTargets)) 1:mod(MaxTargInWindow,NTargets)]',Obj.StartTime);
            
        end
        %
        function addDDT2Plan(Obj, TargetList,StartTime,Args)
            % Add to the plan a list of DDT targets (TargetList) as a group, starting at StartTime.
            % Mutliple additions to the Plan are allowed. 
            % However, no pre-defined loops over the list (within a window or within days)
            arguments
                Obj
                TargetList
                StartTime
                Args.Group     = [];
            end
            
            if ~strcmp(Obj.Type,'DDT')
                error('Plan Type is not DDT');
            end            
            
            if isempty(Args.Group)
                if isempty(Obj.Plan)
                    Args.Group = 1;
                else
                    Args.Group = max(Obj.Plan.Group)+1;
                end
            end
            
            Obj.scheduleTargets(TargetList,StartTime,'Group',Args.Group);
            
        end
        %
        function buildAllSS(Obj, Args)
            % TODO - write build All Sky-Survey function (currently empty function)
            arguments
                Obj
                Args
            end
        end
    end
    %
    methods % Auxiliary functions
        %
        function addUniqTargets(Obj, RA, Dec, Args)
            % Add a list of [RA,Dec] coordinates (in degrees) to the unique targetList, 
            % and calls Obj.updateTargetProperties and Obj.updateTargetVisibility
            arguments
                Obj
                RA        = 0; % [deg]
                Dec       = 0; % [deg]
                Args.Name = ''; % Target name (optional)
                Args.File = ''; % coordinate file name % ~/test.coo
            end
            %
            if ~isempty(Args.File)
                Coo = readmatrix(Args.File,'FileType','text');
                RA  = Coo(:,1); Dec = Coo(:,2);
            end
            %
            NUtarg = numel(RA); % the number of unique targets to be added
            NU0    = height(Obj.UniqTargList);
            %
            Obj.UniqTargList.RA( NU0+1:NU0+NUtarg) =  RA; 
            Obj.UniqTargList.Dec(NU0+1:NU0+NUtarg) = Dec;
            %
            if ~isempty(Args.Name)
                Obj.UniqTargList.Name(NU0+1:NU0+NUtarg) = Args.Name;
            end
            %
            Obj.N_uniqueTargets = height(Obj.UniqTargList);
            %
            Obj.updateTargetProperties;
            %
            Obj.updateTargetVisibility;
        end
        %
        function scheduleTargets(Obj, UniqTargetIndexes,StartTime,Args)
            % Schedule a group of targets, starting at StartTime following by the rest, taking into account slew time between targets.
            % TODO- allow to provide a list of StartTime, one for each of target in the list.
            arguments
                Obj
                UniqTargetIndexes
                StartTime
                Args.Nexposures = [];
                Args.Exptime = []; % 
                Args.Tiles = []; % 
                Args.Group = []; % Target name (optional)
            end
            %
           
            RAD = 180/pi;
            
            if isempty(Args.Nexposures)
                Args.Nexposures = Obj.DefEpochsPerVisit;
            end
            if isempty(Args.Exptime)
                Args.Exptime = Obj.Exptime;
            end
            if isempty(Args.Tiles)
                Args.Tiles = Obj.Tiles;
            end
            
            NUtarg = numel(UniqTargetIndexes);
            NProws    = height(Obj.Plan);
            
            % Add first plan row 
            for ii = 1:NUtarg
            
                Plan_row = NProws+ii;
                TardetInd = UniqTargetIndexes(ii);

                Obj.Plan.Name(Plan_row) = Obj.UniqTargList.Name(TardetInd);
                Obj.Plan.UniqTargInd(Plan_row) = TardetInd;
                Obj.Plan.RA(Plan_row) = Obj.UniqTargList.RA(TardetInd);
                Obj.Plan.Dec(Plan_row) = Obj.UniqTargList.Dec(TardetInd);
                Obj.Plan.ExpTime(Plan_row) = Args.Exptime;
                Obj.Plan.Tiles(Plan_row) = Args.Tiles;
                Obj.Plan.Nexposures(Plan_row) = Args.Nexposures;
                Obj.Plan.TotalDuration(Plan_row) = Obj.Plan.Nexposures(Plan_row) * Obj.Plan.ExpTime(Plan_row) + Obj.FullTileReadTime;

                if ii == 1
                    Obj.Plan.Tstart(Plan_row) = StartTime;
                else
                    [T_sec,~] = ultrasat.tools.calcSlew(Obj.Plan.RA(Plan_row-1),Obj.Plan.Dec(Plan_row-1),Obj.Plan.RA(Plan_row),Obj.Plan.Dec(Plan_row),...
                                                        'Units','deg','CheckTrajectory',true);
                    Obj.Plan.SlewTimeBefore(Plan_row) = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;
                    Obj.Plan.Tstart(Plan_row) = Obj.Plan.Tend(Plan_row-1) + Obj.Plan.SlewTimeBefore(Plan_row);
                end
                
                Obj.Plan.Tend(Plan_row) = Obj.Plan.Tstart(Plan_row) + Obj.Plan.TotalDuration(Plan_row);
                Obj.Plan.JDstart(Plan_row) = juliandate(Obj.Plan.Tstart(Plan_row));
                Obj.Plan.JDend(Plan_row) = juliandate(Obj.Plan.Tend(Plan_row));

                TargetVis = ultrasat.ULTRASAT_restricted_visibility(Obj.Plan.JDstart(Plan_row), [Obj.Plan.RA(Plan_row) Obj.Plan.Dec(Plan_row)]./RAD,...
                    'MinSunDist',Obj.ObsSunDist/RAD,'MinMoonDist',Obj.ObsMoonDist/RAD,'MinEarthDist',Obj.ObsEarthDist/RAD);
                
                if ~all([TargetVis.EarthLimits , TargetVis.MoonLimits , TargetVis.SunLimits])
                    error('Issue with Sun/Earth/Moon limits');
                end

                Obj.Plan.NoComm(Plan_row) = ~all(TargetVis.CommLimits);
                Obj.Plan.HardObs(Plan_row) = ~all(TargetVis.PowerLimits);
                
                
                Obj.Plan.MoonDist(Plan_row) = TargetVis.MoonAngDist*RAD;
                Obj.Plan.SunDist(Plan_row) = TargetVis.SunAngDist*RAD;
                Obj.Plan.EarthDist(Plan_row) = TargetVis.EarthAngDist*RAD;

                % ADD Calc Zody,LimMag  
                
                % Search for overlapping targets. TODO - currently does not
                % load the MissionApprovedPlan if not exist
                if ~isempty(Obj.MissionApprovedPlan)                    
                    Obj.Plan.OverlapTargets{Plan_row} = find((Obj.Plan.Tstart(Plan_row) > Obj.MissionApprovedPlan.Tstart & Obj.Plan.Tstart(Plan_row) < Obj.MissionApprovedPlan.Tend) |...
                                                        (Obj.Plan.Tend(Plan_row)   > Obj.MissionApprovedPlan.Tstart & Obj.Plan.Tend(Plan_row)   < Obj.MissionApprovedPlan.Tend));
                end
                
            end
            
            if ~isempty(Args.Group)
                Obj.Plan.Group((NProws+1):(NProws+NUtarg)) = Args.Group;
            end
            
            % update Number of target in the plan;
            Obj.N_planTargets = height(Obj.Plan);
            
            % update Start & End time of the plan;
            Obj.StartTime = min(Obj.Plan.Tstart);
            Obj.EndTime = max(Obj.Plan.Tend);
            
            % Timestamp of schedule
            Obj.schedule;
        end
        %
        function retrieveMissionApprovedPlan(Obj,Args)
            % Retrive the mission approved plan in a given time window (default window is Obj.CheckTimes) 
            % and populate the fields of Obj.MissionApprovedPlan.
            % Alternativly, allows also to provide a uplanner object (taking its plan as the MissionApprovedPlan) or struct of approved targets.
            arguments
                Obj
                Args.inputPlan = []; 
                Args.WindowStartTime = []; 
                Args.WindowEndTime = []; 
                Args.Mclient soc.api.MissionClient 
            end        
            
            %for now, allow to get a uPlan and use it as refernce
            if isa(Args.inputPlan,'table')
                Obj.clearMissionApprovedPlan;
                
                Obj.MissionApprovedPlan.RA(1:height(Args.inputPlan))  = 0; 
                Obj.MissionApprovedPlan.RA  =  Args.inputPlan.RA ;
                Obj.MissionApprovedPlan.Dec  =  Args.inputPlan.Dec ;
                Obj.MissionApprovedPlan.Roll  =  Args.inputPlan.Roll ;
                Obj.MissionApprovedPlan.Tstart  =  Args.inputPlan.Tstart ;
                Obj.MissionApprovedPlan.Tend  =  Args.inputPlan.Tend ;
                Obj.MissionApprovedPlan.ExpTime  =  Args.inputPlan.ExpTime ;
                Obj.MissionApprovedPlan.Nexposures  =  Args.inputPlan.Nexposures ;
                Obj.MissionApprovedPlan.TotalDuration  =  Args.inputPlan.TotalDuration ;
                return
            end
            
            if isempty(Args.WindowStartTime)
                Args.WindowStartTime  = Obj.CheckTimes(1);
            end
            
            if isempty(Args.WindowEndTime)
                Args.WindowEndTime  = Obj.CheckTimes(2);
            end
            
            % Retrive struct or uses provided one
            if isstruct(Args.inputPlan)
                structPlan = Args.inputPlan;
            else
                structPlan = Args.Mclient.getApprovedTargets(Args.WindowStartTime, Args.WindowEndTime);
            end
            
            TargetsTable = struct2table(structPlan.targets);
            
             Obj.clearMissionApprovedPlan;
            
            Obj.MissionApprovedPlan.RA(1:height(TargetsTable))  = 0; 
            Obj.MissionApprovedPlan.TargetID = TargetsTable.target_id;
            Obj.MissionApprovedPlan.RA  =  TargetsTable.ra ;
            Obj.MissionApprovedPlan.Dec  =  TargetsTable.decl ;
            Obj.MissionApprovedPlan.Roll  =  TargetsTable.roll ;
            Obj.MissionApprovedPlan.Tstart  = datetime(TargetsTable.start_time,'Format','yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z','TimeZone',Obj.SysTimeZone);
            Obj.MissionApprovedPlan.Tend  =  datetime(TargetsTable.end_time,'Format','yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z','TimeZone',Obj.SysTimeZone);
            Obj.MissionApprovedPlan.ExpTime  =  seconds(TargetsTable.exposure);
            Obj.MissionApprovedPlan.Nexposures  =  TargetsTable.image_count;
            Obj.MissionApprovedPlan.TotalDuration  =  seconds(TargetsTable.total_seconds);            
            
        end
        %
        function clearUniqueTargets(Obj)
            % Clear the unique target list, as well as the plan and visibility object 
            
            % Remove all unique targets
            Obj.UniqTargList(:,:) = [];
            % clean the number of unique targets
            Obj.N_uniqueTargets = 0;
            % clear the plan
            Obj.clearPlan;
            % clean the visibility
            Obj.Vis = [];
        end
        %
        function clearPlan(Obj)
            % Clear the plan
            
            % remove the plan
            Obj.Plan(:,:) = [];
            % clean the number of unique targets
            Obj.N_planTargets = 0;
        end    
         %
        function clearMissionApprovedPlan(Obj)
            % Clear the Mission Approved Plan table
            Obj.MissionApprovedPlan(:,:) = [];
        end    
        %
        function adjustGroupStartTime(Obj,Args)
            % Adjust the start time of a group in the plan by 3 options: 
            %       a given NewStartTime, a given ShiftTime, or relative to a target in the OverLap targets list.
            % If no GroupList is provided, will adjust all groups in the plan, one by one.
            arguments
                Obj
                Args.GroupList                                 = [];
                Args.NewStartTime                      =[];
                Args.ShiftTime              duration  = seconds(inf);
            end
            
            if isempty(Args.GroupList)
                Args.GroupList = unique(Obj.Plan.Group);  % Apply to 
            end
            
            for Gind= 1:numel(Args.GroupList)
                ShiftTime = Args.ShiftTime;
                
                Plan_rows = find(Obj.Plan.Group==Args.GroupList(Gind));
                if ~isempty(Args.NewStartTime)
                        ShiftTime = Args.NewStartTime - Obj.Plan.Tstart(Plan_rows(1));
                end
                
                if isinf(ShiftTime)
                    %calcuate the shift based on the overlaptargets
                    ShiftTime = seconds(0); % in case it doesn't find any match - does not shift the time
                    
                    OTlist = Obj.Plan.OverlapTargets{Plan_rows(1)};
                    for ii = 1:numel(OTlist)
                        CurrOTind = OTlist(ii);
                        % check if starttime of entire group within curr overlap target window
                        if (Obj.Plan.Tstart(Plan_rows(1)) > Obj.MissionApprovedPlan.Tstart(CurrOTind) && Obj.Plan.Tstart(Plan_rows(1)) < Obj.MissionApprovedPlan.Tend(CurrOTind))
                            
                            [T_sec,~] = ultrasat.tools.calcSlew(Obj.MissionApprovedPlan.RA(CurrOTind),Obj.MissionApprovedPlan.Dec(CurrOTind),Obj.Plan.RA(Plan_rows(1)),Obj.Plan.Dec(Plan_rows(1)),...
                                                        'Units','deg','CheckTrajectory',true);
                            
                            SlewTime = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;
                            
                            OT_Tend = Obj.MissionApprovedPlan.Tend(CurrOTind);
                            OT_ExpTime = Obj.MissionApprovedPlan.ExpTime(CurrOTind);
                            
                            
                            OT_Tend_close = OT_Tend + round((Obj.Plan.Tstart(Plan_rows(1))-OT_Tend-SlewTime)./OT_ExpTime)*OT_ExpTime;
                            
                            ShiftTime = OT_Tend_close + SlewTime - Obj.Plan.Tstart(Plan_rows(1));
                            
                            Obj.Plan.SlewTimeBefore(Plan_rows(1)) = SlewTime;
                        end
                    end
                end
                
                
                %apply shift
                Obj.Plan.Tstart(Plan_rows) = Obj.Plan.Tstart(Plan_rows) + ShiftTime;
                Obj.Plan.Tend(Plan_rows) = Obj.Plan.Tend(Plan_rows) + ShiftTime;
                
                Obj.Plan.JDstart(Plan_rows) = juliandate(Obj.Plan.Tstart(Plan_rows));
                Obj.Plan.JDend(Plan_rows) = juliandate(Obj.Plan.Tend(Plan_rows));
            end
            
        end  
        %
        function updateTargetProperties(Obj, Args)
            % Fill for each of the unique targets the following properties: extinction (A_U), calibrating objects within FoV (CalObj),
            % (TODO) reference images  within FoV (RefImageIDs), external surveys overlaping with the FoV (ExtSurveys),
            % specific known objects (e.g., planets, massive stars, blazars) within the FOV (FieldObj)
            %
            % TODO - should allow to update only selected targets (i.e., new targets)
            arguments
                Obj    
                Args.ExtSurveyMaps = '~/matlab/data/ULTRASAT/ExtSurveyMaps.mat';
                Args.FieldObjects  = '~/matlab/data/ULTRASAT/FieldObjects.mat';
            end              
            % target coordinates 
            RA = Obj.UniqTargList.RA; Dec = Obj.UniqTargList.Dec; 
            
            % extinction 
            Obj.UniqTargList.A_U = ultrasat.tools.extinction(RA, Dec); 
            
            % load the lists of external important objects and survey maps
            load(Args.ExtSurveyMaps); % 'SurveyMaps' table
            load(Args.FieldObjects);  % 'Known_Obj_large', 'Known_Obj_small' tables

            for iT = 1:Obj.N_uniqueTargets % loop over targets 
                RA0 = Obj.UniqTargList.RA(iT); Dec0 = Obj.UniqTargList.Dec(iT);                
                % make a circular FOV region
                FOV = ultrasat.tools.getFOVcircle(RA0,Dec0,'Radius',Obj.Rfov,'Plot',0);  
                FOVp = polyshape(FOV);  % a polyshape is useful to test intersections
                
                % select calibration objects 
                Ind = celestial.search.isPointInsidePolygon(Obj.CalibObj.RA, Obj.CalibObj.Dec, FOV);
                Obj.UniqTargList.CalObj{iT} = num2cell(find(Ind>0));
                
                % select reference images
%                 Ind = celestial.search.isPointInsidePolygon(Obj.RefIma.RA, Obj.RefIma.Dec,FOV); 
%                 Obj.UniqTargList.RefImageIDs{iT} = num2cell(find(Ind>0));

                % select external surveys 
                Ind = overlaps(SurveyMaps.Shape,FOVp);
                Obj.UniqTargList.ExtSurveys{iT} = num2cell(find(Ind>0));
               
                % select specific objects falling into the FOV
                Ind = celestial.search.isPointInsidePolygon(Known_Obj_small.RA, Known_Obj_small.Dec, FOV);
                Field.Small = num2cell(find(Ind>0));
                % also extract
                Ind = celestial.search.isPointInsidePolygon(Known_Obj_large.WG3_det_trans_planets.ra, Known_Obj_large.WG3_det_trans_planets.dec,FOV);
                Field.TransPlanets = num2cell(find(Ind>0));
                Ind = celestial.search.isPointInsidePolygon(Known_Obj_large.WG5_Massive_Stars.RA, Known_Obj_large.WG5_Massive_Stars.Dec,FOV);
                Field.MassiveStars = num2cell(find(Ind>0));
                Ind = celestial.search.isPointInsidePolygon(Known_Obj_large.WG5_AllClusters.RA, Known_Obj_large.WG5_AllClusters.DEC,FOV);
                Field.Clusters = num2cell(find(Ind>0));                
                Ind = celestial.search.isPointInsidePolygon(Known_Obj_large.WG7_Blazars.RA, Known_Obj_large.WG7_Blazars.Dec,FOV);
                Field.Blazars = num2cell(find(Ind>0));    
                %
                Obj.UniqTargList.FieldObj{iT} = Field;
            end            
        end
        %
        function updateTargetVisibility(Obj, Args)
            % Calcuate visibility for all unique targets for a given time window (default window is Obj.CheckTimes)
            arguments
                Obj                     
                Args.TimeBin           = 0.01; % [days] 
                Args.WindowStartTime = []; 
                Args.WindowEndTime = []; 
            end
            %
            RAD = 180/pi;          
            %
            if isempty(Args.WindowStartTime)
                Args.WindowStartTime = Obj.CheckTimes(1);
            end
            
            if isempty(Args.WindowEndTime)
                Args.WindowEndTime = Obj.CheckTimes(2);
            end
            
            StartJD = juliandate(Args.WindowStartTime);
            EndJD   = juliandate(Args.WindowEndTime);
            VisJD  = StartJD + (0:Args.TimeBin:(EndJD-StartJD))';                         
            Obj.Vis    = ultrasat.ULTRASAT_restricted_visibility(VisJD, [Obj.UniqTargList.RA Obj.UniqTargList.Dec]./RAD,...
                'MinSunDist',Obj.ObsSunDist/RAD,'MinMoonDist',Obj.ObsMoonDist/RAD,'MinEarthDist',Obj.ObsEarthDist/RAD);             
%             Obj.CombVis      = Obj.Vis.SunLimits .* Obj.Vis.MoonLimits .* Obj.Vis.EarthLimits;  
%             Obj.CombVisPower = Obj.CombVis .* Obj.Vis.PowerLimits; 
        end
        %
        function adjustCheckTimes(Obj,CheckStartTime,CheckEndTime)
            % Set Obj.CheckTimes and then calls Obj.updateTargetVisibility and Obj.retrieveMissionApprovedPlan
            Obj.CheckTimes = [CheckStartTime;CheckEndTime];
            Obj.updateTargetVisibility;
            Obj.retrieveMissionApprovedPlan;
        end
        %
        function schedule(Obj)
            % Set Obj.Status to 'draft' and Obj.Scheduled time to 'now'. (called from Obj.scheduleTargets)
            Obj.Status    = 'draft';
            Obj.Scheduled = datetime('now','TimeZone', 'UTC');    
        end
        %
        function validate(Obj)
            % TODO - send plan to the validator. In addition, set Obj.Status to 'validated' and Obj.Validated time to 'now'
            Obj.Status    = 'validated';
            Obj.Validated = datetime('now','TimeZone', 'UTC');     
        end        
        %
        function submit(Obj)
            %  TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.Submitted time to 'now'
            Obj.Status    = 'submitted';
            Obj.Submitted = datetime('now','TimeZone', 'UTC'); 
        end
        %
        function Res = showCalibObj(Obj,TargInd,Args)
            % Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
            % Input : - object indexes
            %        ..key,val..
            %       'PlotSpectrum' - logical, def. false
            %       'Band' - spectral interval for plotting in [nm], e.g. [230 300]  
            % Output: - a subset of the main calibration objects' table
            % Exapmle: P = ultrasat.planner.uplanner;
            %          P.buildHCS('CooFile','~/hcs.coo');
            %          Tab = P.showCalibObj(2) 
            % or
            %          P.showCalibObj(2, 'PlotSpectrum',true); 
            arguments
                Obj
                TargInd               = [];
                Args.PlotSpectrum = false;
                Args.subInd2plot  = 1;
                Args.WaveRange    = []; % [nm] range for spectrum plotting, e.g. [230 300] 
            end
            %
            if isempty(TargInd)
                TabInd = unique(Cell2Vec([Obj.UniqTargList.CalObj{:}]));
                Res = Obj.CalibObj(TabInd,:);
            else
                TabInd = [Obj.UniqTargList.CalObj{TargInd}{:}]; % 
                Res = Obj.CalibObj(TabInd,:); 
            end
            if Args.PlotSpectrum
                Fname = sprintf('%s/%s.fits',Obj.CalibDir,Res.obj{Args.subInd2plot});
                Ftab  = fitsread(Fname,'binarytable');
                Spec  = [Ftab{1} Ftab{6} Ftab{7}];                
                figure; clf                                
                errorbar(Spec(:,1),Spec(:,2),Spec(:,3),'.'); xlabel '\lambda [A]'; ylabel 'F [erg/cm(2)/s/A]'; set(gca, 'YScale', 'log');
                if ~isempty(Args.WaveRange)
                    xlim(Args.WaveRange.*10);
                end
                Title = sprintf('%s: Teff = %.0f, log(g) = %.1f',Res.obj{1},Res.Teff_K_,Res.logG); title(Title)        
            end            
        end
        %
    end
    % 
    methods(Static)
        Result = debug()
            % unitTest
            function Result = unitTest()
                % unitTest
                Result=false;
                %

                % Example for creating HCS survey:
                  HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Field','RA','Dec'},'RowNames',{'S1','N2','N3'}');
                  upHCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','HCS');
                  upHCS.StartTime = '2028-01-01 12:00:00';
                  upHCS.EndTime = '2028-07-01 12:00:00';
                  upHCS.addUniqTargets(HCS_fields.RA('S1'),HCS_fields.Dec('S1'),'Name',HCS_fields.Field('S1'));
                  upHCS.buildHCS;


                % Example for creating LCS survey:
                  LCS_grid = readtable('~/matlab/data/ULTRASAT/LCS_nonoverlapping_grid.csv');
                  upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');
                  upLCS.StartTime = '2024-12-04 00:00:00';
                  upLCS.EndTime = '2025-01-16 12:00:00';
                  upLCS.DailyWindowStartTime = duration('09:58:00');
                  F = LCS_grid.V45==1 & LCS_grid.A_U_1==1;
                  upLCS.addUniqTargets(LCS_grid.RA(F),LCS_grid.Dec(F),'Name',num2cell(LCS_grid.Field(F)));

                  upLCS.updateTargetVisibility('WindowStartTime',upLCS.StartTime,'WindowEndTime',upLCS.EndTime);
                  F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits ,1));

                 % Fakely retrive upHCS ar apprvoed target list
                  upLCS.retrieveMissionApprovedPlan('inputPlan',upHCS.Plan);
                  
                  % check with struct
                  load('~/matlab/data/ULTRASAT/api_response.mat');

                  upLCS.retrieveMissionApprovedPlan('inputPlan',response);
                  
                  upLCS.buildLCS('TargetList',F2);

                  upLCS.adjustGroupStartTime;  % Check adjustments relative to Approved List


                % Example for TOO plan:
                  upTOO = ultrasat.planner.uplanner('AstPlanner','YS','Type','TOO');
                  upTOO.buildTOO('RA',HCS_fields.RA,'Dec',HCS_fields.Dec,'Name',HCS_fields.Field);

                % Example DDT plan (very basic):
                  upDDT = ultrasat.planner.uplanner('AstPlanner','YS','Type','DDT');
                  upDDT.addUniqTargets(HCS_fields.RA,HCS_fields.Dec,'Name',num2cell(HCS_fields.Field));
                  upDDT.addDDT2Plan([1,2],'2028-01-01 12:00:00');
                  upDDT.addDDT2Plan([3,2],'2028-01-05 00:10:00'); 

                 %
                 Result=true;
                %
                  
            end

    end
end
