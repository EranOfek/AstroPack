%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of functions:
% - ultrasat.planner.uplanner(Args): Constructor
%
% - constructAllSSgrid(Args):       Construct or load the AllSS grid (incl. the UniqTarg property)  
%
% - Obj.set.Type(Type)             : Setter. Verify allowed Type
% - Obj.set.StartTime(StartTime)   : Setter. Also sets TimeZone of StartTime
% - Obj.set.EndTime(EndTime)       : Setter. Also sets TimeZone of EndTime
%
% - Obj.buildHCS(Args)             : Build a plan for a HCS field. 
%                                    All relevant parameters should be set before calling this function
%                                    (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTarg) ==1)
%                                    TODO: allow to select a target from UniqTarg
%
% - Obj.buildLCS(Args)             : Build a plan for a Targetlist of LCS fields. If a list is not provided, uses all targets in the unique target list.
%                                    Fill in a daily window of observations and move to the next day. 
%                                    All relevant parameters should be set before calling this function
%                                    (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit/DailyWindowStartTime/DailyWindowMaxDuration/ height(Obj.UniqTarg)>0)
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
% - Obj.editUniqTarg(UniqTargInd,Args)                      :  Edit a given UniqTargInd in the uniqTarg table.Only allows to edit Name, RA, Dec
%                                                                                     Update the UniqTarg properties and visibility.
%                                                                                     If  UniqTargInd already shceduled in the Plan, updates all row with this UniqTarg
% - Obj.delUniqTarg(UniqTargInd,Args)                       : Check if UniqTargInd is in the Plan. If not, delete it from UniqTarg table.
%                                                  If in plan, by default will return an error. If specifcally asked to delete anyway, will do that and update the group to be continous 
%                                                  (though won't change the Group start time) 
% - Obj.saveUniqTargCooList(FileName)                     : Write the [Name, Ra, Dec] of the uniqe target into FileName 
% - Obj.clearUniqueTargets                                  : Clear the unique target list, as well as the plan and visibility object
%
% - Obj.scheduleTargets(UniqTargetIndexes,StartTime,Args)   : Schedule a group of targets, starting at StartTime following by the rest, taking into account slew time between targets.
%                                                             TODO- allow to provide a list of StartTime, one for each of target in the list.
% - Obj.editPlanRow(Plan_row,Args)                          : Allow to directly edit only the following fields in a plan row:ExpTime, Tiles, Nexposures.
%                                                                                 Will update row properties if needed (due to edited fields) or if asked directly (even if no fields were edited)
%                                                                                 If plan_row is part of a group, update the properties of relevant other rows
% - Obj.delPlanRow(Plan_row)                         : detlete the plan row and Check if part of a group. if so, adjust group (if needed).
% - Obj.clearPlan                                           : Clear the plan
%
% - Obj.retrieveMissionApprovedPlan(Args)                   : Retrive the mission approved plan in a given time window (default window is Obj.CheckTimes) 
%                                                             and populate the fields of Obj.MissionApprovedPlan.
%                                                             Alternativly, allows also to provide a uplanner object (taking its plan as the MissionApprovedPlan) or struct of approved targets.
%
% - Obj.clearMissionApprovedPlan                            : Clear the Mission Approved Plan table
%
%
% - [CheckStatus,badPlanRow] = Obj.planSelfConsistencyCheck(Args)       : Verify that the plan schedule is self consistent. 
%                                                                                                                       TODO- validate Ntargets in plan and uniqTarg
% - Obj.adjustGroupStartTime(Args)                          : Adjust the start time of a group in the plan by 3 options: 
%                                                                  a given NewStartTime, a given ShiftTime, or relative to a target in the OverLap targets list.
%                                                             If no GroupList is provided, will adjust all groups in the plan, one by one.
% - Obj.updateTargetProperties(Args)                        : Fill for each of the unique targets the following properties: extinction (A_U), calibrating objects within FoV (CalObj),
%                                                               (TODO) reference images  within FoV (RefImageIDs), external surveys overlaping with the FoV (ExtSurveys),
%                                                               specific known objects (e.g., planets, massive stars, blazars) within the FOV (FieldObj)
% - Obj.updatePlanRowProperties(Plan_row, Args)     : Calcaulte and fill for a given plan row the following properties -
%                                                                                       TotalDuration, Tend, JDstart, JDend, ExpectedRoll,  NoComm, HardObs, MoonDist, SunDist, EarthDist,OverlapTargets
%                                                                                    If asked to CalcStartTimeFromPrevTarget then also calcuates - SlewTimeBefore, Tstart
%                                                                                    Return error If there's issue with  Sun/Earth/Moon limits
% - Obj.updateTargetVisibility(Args)                        : Calcuate visibility for all unique targets for a given time window (default window is Obj.CheckTimes)
%                                                                                TODO- consider updating only selected targets (i.e., new)
% - Obj.adjustCheckTimes(CheckStartTime,CheckEndTime)       : Set Obj.CheckTimes and then calls Obj.updateTargetVisibility and Obj.retrieveMissionApprovedPlan
%
% - Obj.schedule                                            : Set Obj.Status to 'draft' and Obj.ScheduledTime time to 'now'. (called from Obj.scheduleTargets)
% - Obj.validate(Args)                                      : TODO - send plan to the validator. In addition, set Obj.Status to 'validated' and Obj.ValidatedTime to 'now'
% - Obj.clearValidationData                                 : Clears valiation data from Plan table, delete the ValidationTime and ValidationResponse and change status back to draft
% - Obj.submit(Args)                                        : TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.SubmittedTime to 'now'
%
% - planStruct = planTable2struct(Obj,Args)                 : Return a struct array of a conversion of the Obj.Plan table, in the correct naming and format for validation/submission
%
% - Res = Obj.showCalibObj(UniqTargInd,Args)                        : Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
%
% - Obj.plotVisibility(UniqTargInd,Args)                            : Plot the visibilty of a UniqTarg
%
% - Obj.plotMapPlan(Args)                                           : plotting on a map relevant properties and info from the plan
%                                                                     TODO - Change to map projection later
% Static methods:
% - CheckTimes = getDefaultCheckTimes()                              : Get the default Check times.  TODO - update if needed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Additional functions to be considered:
% - retrieveExecutedObsMap                 : Retrieve of executed observations maps for a given field / coordinate
% several optimized planning functions\tools (e.g., covarge of an area, plan AllSS - 2 options, mutiple ToO plans)
% add msglog for all functions - expecially for trycatch
% Verify all param range/valid values (e.g., Exp time >readtime)
% 
% 4. In all error messages (including planSelfConsistencyCheck), give more information, i.e. which rows overlap etc.
%
% 5. clearValidationData - link to edit\delete plan row
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classdef uplanner < Component 
    % 
    properties(Access = public)
        Pk                  int32 = 0;          % Primary key of the plan in database, 0 if not set yet
        Title               char                % Name of the object
        Type                char                % HCS, LCS, AllSS, DDT, TOO 
        StartTime           datetime            % start of the whole plan
        EndTime             datetime            %   end of the whole plan
        Plan                                    % table of the Plan (target per row) 
        UniqTarg                                % table of unique targets
        
        CheckTimes(2,1)     datetime            % times to be used for visibilty and mission approval retrival
        Vis                                     % visibility matrix         
        MissionApprovedPlan                     % Approved Mission Plan retrvied  from C&C 
        
        DefEpochsPerVisit               = 3; 
        Exptime             duration    = seconds(300);      %[s]
        Tiles               string      = ['1','2','3','4']; %
        DefSlewBuffer       duration    = seconds(5);
        FullTileReadTime    duration    = seconds(15); % Time from start read of first row to last. This time will be added to each row in plan (before slew to next target..
        
        % LCS / AllSS
        DailyWindowStartTime    duration =  duration(23,00,00); % [hrs]   
        DailyWindowMaxDuration  duration =  hours(3);           % [hrs]
        
        % AllSS
        AllSSgridFile                   = 'AllSS_grid_361.txt'; % the default AllSS grid
        PointTypeCriterion              = 'b'; % 'b' -- by the Galactic latitute or 'a_u' -- by the A_U (ULTRASAT band extinction) 
        AllSSHighLatThresh              = 30; % |b| [deg]
        HighLatVisits                   =  4; % 1 visit = 3 x 300 s 
        LowLatVisits                    =  2;      
        DitherPattern                   = '2x2';  % not used as of yet
        DitherLeg                       = 0;      % [deg] dither leg size
        ExtragalMinIntervals            = [0 0 0];% minimal intervals in days between extragalactic visits
        DailySlots                                % number of slots in a day
        MaxDailyVisits                            % maximal allowed number of daily visits (determined from DailyWindowMaxDuration) 
        EmptyDay                        = false;  % 1 empty day in a week (visibility set to 0 for all slots)
        BufferEarthDist                 = 0;      % buffer distances for visibility predictions
        BufferSunDist                   = 0;
        BufferMoonDist                  = 0;
        SchedStatus                               % a table of AllSS points with the scheduling status marked 
        
        % TOO
        TOOStartTime       datetime     =  datetime('now'); % [hrs]   
        TOOWindowDuration  duration     =  hours(3);        % [hrs]
        TOOMaxTargets                   =  4;               % maximal number of target fields
        TOOMinAddedProb                 =  0.05;            % minimal covered probability difference between N and N+1 targets employed
        TOOMinCoveredProb               =  0.9;             % minimal covered probability
        TOOAlertProbMap                                     % input probability map 
        
        TOOUsedTargets                                      % the number of actually employed targets
        TOOCoveredProb                                      % actually covered probability (all targets)
        TOOCoveredByTarget                                  % actually covered probability (vector: per target)
        
        N_uniqueTargets                 =  0; % number of unique targets
        N_planTargets                   =  0; % number of targets in the plan
        
        Rfov                            =  10; % [deg] FOV radius conservative, w/o roll information
        
        BaseDataDir                      % Base directory for data needed for uplanner
        
        CalibObj                        = []; % table of calibration objects 
        CalibDir                             % the catibration objects' spectra directory 

        RetrivedMissionTime     datetime    % date or empty        
        ScheduledTime           datetime    % date or empty
        ValidatedTime           datetime    % date or empty
        SubmittedTime           datetime    % date or empty
        Status                  char        = 'draft';

        ValidationResponse  struct      % sturct containing the latest response from validator (corresponding to  ValidatedTime)
        
        AstPlanner              char        % name of the Astronomer-Planner
        Mclient                             % API client - MissionClient / MissionClientSim
    end
    % 
    properties(Hidden, Constant)
        Plan_AllowedTypes  = {'HCS', 'LCS', 'AllSS', 'DDT', 'TOO'};
        
        SysTimeZone        = 'UTC';
        
        Plan_DefVarNames   = {'Name','UniqTargInd','Group','RA', 'Dec','ExpectedRoll','Tiles',...
                              'Tstart','Tend','JDstart','JDend','ExpTime','Nexposures','TotalDuration','SlewTimeBefore',...
                              'NoComm','HardObs','MoonDist','SunDist','EarthDist','Zody','LimMag','OverlapTargets',...
                              'ValidationStatus','PowerStatus','ObrdStatus','Tend_ValidationEstimate','Roll_ValidationEstimate',...
                              'ValidationWarning','MissionApproveStatus','MissionApproveComment'};
        Plan_DefVarTypes   = {'string','double','double','double','double','double','string',...
                              'datetime','datetime','double','double','duration','double','duration','duration',...
                              'logical','logical','double','double','double','double','double','cell',...
                              'string','string','string','datetime','double',...
                              'cell','string','cell'};
                                                                
        Target_DefVarNames = {'Name', 'RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj', 'HealpixArray','DitherGroup'};
        Target_DefVarTypes = {'string', 'double', 'double', 'double', 'cell', 'cell', 'cell', 'cell', 'cell', 'double'};  
        
        MissionApprovedPlan_VarNames   = {'Name','pk','TargetID','RA', 'Dec','Roll',...
                              'Tstart','Tend','ExpTime','Nexposures','TotalDuration'};
        MissionApprovedPlan_VarTypes   = {'string','uint64','char','double','double','double',...
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
                
                Args.BaseDataDir = '~/matlab/data/ULTRASAT/'; % Base directory for data needed for uplanner
                Args.CalObjFile  = 'starlib23_table.mat';     % the calibration objects' list (within  BaseDataDir)
                Args.CalSubDir   = 'Calib/';                  % the catibration objects' spectra directory (within  BaseDataDir)
                
                Args.AllSSgridFile = [];                      % an alternative AllSS grid (the default is in the properties)
                Args.ExtragalDitherLeg = [];                  % an alternative dither leg size for the AllSS grid
                Args.Save          = [];
                Args.Load          = [];
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
            Obj.EndTime.TimeZone   = Obj.SysTimeZone;
            %
            Obj.CheckTimes = ultrasat.planner.uplanner.getDefaultCheckTimes();
            Obj.CheckTimes.TimeZone = Obj.SysTimeZone;
            %
            Obj.Plan = table('Size',[Obj.N_planTargets,numel(Obj.Plan_DefVarNames)],'VariableNames', Obj.Plan_DefVarNames,...
                                'VariableTypes',Obj.Plan_DefVarTypes);
                            
            Obj.Plan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.Plan.Tend.TimeZone = Obj.SysTimeZone;
            Obj.Plan.Tend_ValidationEstimate.TimeZone = Obj.SysTimeZone;
            %
            Obj.UniqTarg = table('Size',[Obj.N_uniqueTargets,numel(Obj.Target_DefVarNames)],'VariableNames', Obj.Target_DefVarNames,...
                                'VariableTypes',Obj.Target_DefVarTypes); 
            %
            Obj.MissionApprovedPlan = table('Size',[0,numel(Obj.MissionApprovedPlan_VarNames)],'VariableNames', Obj.MissionApprovedPlan_VarNames,...
                                'VariableTypes',Obj.MissionApprovedPlan_VarTypes);           
                            
            Obj.MissionApprovedPlan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.MissionApprovedPlan.Tend.TimeZone = Obj.SysTimeZone;                            
            %
            
            Obj.BaseDataDir = Args.BaseDataDir;
            Obj.CalibDir = fullfile(Obj.BaseDataDir ,Args.CalSubDir);
            
            load(fullfile(Obj.BaseDataDir ,Args.CalObjFile)); % load the calibration objects' table     
            Obj.CalibObj = CalibObj;
            
            if strcmpi(Obj.Type,'AllSS') % construct the AllSS grid
                if ~isempty(Args.AllSSgridFile)
                    Obj.AllSSgridFile = Args.AllSSgridFile;
                end
                if ~isempty(Args.ExtragalDitherLeg)
                    Obj.DitherLeg = Args.ExtragalDitherLeg;
                end
                Obj.constructAllSSgrid('Save',Args.Save,'Load',Args.Load);
            end
        end
        %
        function Obj = constructAllSSgrid(Obj, Args)
            % construction of the AllSS grid 
            arguments
                Obj
                Args.Verbosity = 1;
                Args.Save      = [];
                Args.Load      = [];
            end
            if isempty(Args.Load)
                % read the main grid file
                Grid = readtable(fullfile(Obj.BaseDataDir,Obj.AllSSgridFile));
                
                % determine the two types of sky points
                RAD = 180/pi;
                if strcmpi(Obj.PointTypeCriterion,'b')       % distinction according to the Galactic latitude
                    [~, Grid.b] = celestial.coo.convert_coo(Grid.RA./RAD,Grid.Dec./RAD,'j2000.0','g');
                    Extragal = abs(Grid.b.*RAD) > Obj.AllSSHighLatThresh;
                elseif strcmpi(Obj.PointTypeCriterion,'a_u') % distinction accoring to the averaged A_U
                    Grid.A_U = ultrasat.tools.extinction(Grid.RA,Grid.Dec);
                    Extragal = Grid.A_U < 1;
                else
                    error('Unknown point type criterion');
                end
                                if Args.Verbosity > 0
                                    fprintf('Adding unique targets...\n'); tic
                                end
                % dither the extragalactic points:
                [DitheredGrid, DitherGroup] = ultrasat.tools.ditherGrid(Grid(Extragal,:),'Leg',Obj.DitherLeg,...
                    'Ngrid',4,'Pattern',Obj.DitherPattern);
                
               % add the galactic points to the unique targets list:
                Obj.addUniqTargets(Grid.RA(~Extragal),Grid.Dec(~Extragal),'Name',num2cell(Grid.id(~Extragal)),...
                    'UpdateVisibility',false);
                % add the extragalactic points to the unique targets list:
                Obj.addUniqTargets(DitheredGrid.RA,DitheredGrid.Dec,'Name',num2cell(DitheredGrid.id),...
                    'DitherGroup',DitherGroup,'UpdateVisibility',false);
                
                                if Args.Verbosity > 0
                                    fprintf('%d unique targets added in %.0f s \n',height(Obj.UniqTarg),toc);
                                end
                % fill the scheduled status table
                Obj.SchedStatus = table(Obj.UniqTarg.Name,Obj.UniqTarg.RA,Obj.UniqTarg.Dec,Obj.UniqTarg.DitherGroup,...
                    repmat(0,1,Obj.N_uniqueTargets)','VariableNames',{'Name','RA','Dec','DithGroup','Status'});
            else
                load(Args.Load)
                Obj.UniqTarg    = UniqTarg;        
                Obj.SchedStatus = SchedStatus;     
                Obj.N_uniqueTargets = height(Obj.UniqTarg);
            end
            % save the unique target list grid in the file named Args.Save
            if ~isempty(Args.Save)
                UniqTarg    = Obj.UniqTarg;
                SchedStatus = Obj.SchedStatus;
                save(Args.Save,'UniqTarg','SchedStatus');
            end
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
        function buildHCS(Obj,Args)
            % Build a plan for a HCS field, using a single selected UniqTarget 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTarg) >=1)
            arguments
                Obj
                Args.HCS_UniqTarg = 1; % Default is the first line if not selected
            end               

            
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
            if height(Obj.UniqTarg) < 1
                error('HCS requires a unique target target');
            end            
            if numel(Args.HCS_UniqTarg) ~=1
                error('HCS requires one single target');
            end


            % Calc number of exposures within the plan time 
            Nexposures = floor((Obj.EndTime-Obj.StartTime)/Obj.Exptime);
            
            % Schedule HCS field
            Obj.scheduleTargets(Args.HCS_UniqTarg,Obj.StartTime,'Nexp',Nexposures);
        end
        %
        function buildLCS(Obj,Args)
            % Build a plan for a Targetlist of LCS fields. If a list is not provided, uses all targets in the unique target list.
            % Fill in a daily window of observations and move to the next day. 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit/DailyWindowStartTime/DailyWindowMaxDuration/ height(Obj.UniqTarg)>0)
            arguments
                Obj
                Args.TargetList = [];
            end
           
            % Verify that all the relevant parameters are set
            
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
            if height(Obj.UniqTarg) == 0
                error('LCS reuire at least one target');
            end         
            
            if isempty(Args.TargetList)
                Args.TargetList = 1:height(Obj.UniqTarg);
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
            arguments
                Obj 
                Args.Map               = [];                
                Args.RA                = [];
                Args.Dec               = [];
                Args.Name              = {};
                Args.TOOStartTime      = [];
                Args.TOOWindowDuration = [];
                Args.EpochsPerVisit    = [];
                Args.ExpTime           = [];
                Args.SlewBuffer        = [];
                Args.Tiles             = [];
                Args.TimeBin           = 0.01; % [d] the time bin for visibility checks
                Args.Verbosity         = 0;
                Args.DrawMaps          = 0;
            end
            
            if ~strcmp(Obj.Type,'TOO')
                error('Plan Type is not TOO');
            end
            
            if isempty(Args.Map)
                Args.Map = Obj.TOOAlertProbMap;
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
                        
            Obj.StartTime  = Obj.TOOStartTime;
            Obj.EndTime    = Obj.TOOStartTime + Obj.TOOWindowDuration;
            Obj.CheckTimes = [Obj.StartTime, Obj.EndTime];
            
            if ~isempty(Args.Map)
                [RA, Dec, Stat] = ultrasat.tools.coverProbMap(Args.Map,...
                    'MaxTarg',Obj.TOOMaxTargets,'MinProb',Obj.TOOMinCoveredProb,'MinAddedProb',Obj.TOOMinAddedProb,...
                    'Verbosity',Args.Verbosity,'DrawMaps',Args.DrawMaps); 
                Names = num2cell(1:numel(RA)); % may add "TOOfield.." to the name? 
                Obj.addUniqTargets(RA, Dec,'Name',Names); 
                
                Obj.TOOUsedTargets = Stat.Ntarg; 
                Obj.TOOCoveredProb = Stat.CoveredProb;
                Obj.TOOCoveredByTarget = Stat.IndividualCoveredProb;
            elseif ~isempty(Args.RA) && ~isempty(Args.Dec) && numel(Args.RA)==numel(Args.Dec)
                [RA, Dec] = deal(Args.RA, Args.Dec);
                Obj.addUniqTargets(RA, Dec,'Name',Args.Name);                
            else
                error('No TOO targets/map');
            end
            
            % Check visibility and shift the window if needed            
%             if ~all(Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits ,1)
            if ~all(Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits,'all')
                fprintf('Visibility issue: immediate observation is not possible\n');              
                % scan 6 months ahead and find the first occurence of an Obj.TOOWindowDuration window:
                Obj.CheckTimes = [Obj.StartTime, Obj.StartTime + calmonths(6)]; 
                Obj.updateTargetVisibility('TimeBin',Args.TimeBin);
                Nbins  = ceil(Obj.TOOWindowDuration/days(Args.TimeBin)); 
                Limits = Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits;
%                 CombinedLimits = prod(Limits,2);
                % find a period of Obj.TOOWindowDuration length where CombinedLimits is 1:                
%                 Ind   = tools.find.findGroupOfConsecutiveVals(CombinedLimits, 1, Nbins, 1);
                for i=1:Obj.TOOUsedTargets
                    Ind(i,:)   = tools.find.findGroupOfConsecutiveVals(Limits(:,i), 1, Nbins, 1);
                end
                if ~isempty(Ind)                    
%                     Obj.StartTime  = datetime(Obj.Vis.JD(Ind(1)),'ConvertFrom','juliandate','TimeZone','UTC');
%                     Obj.EndTime    = datetime(Obj.Vis.JD(Ind(end)),'ConvertFrom','juliandate','TimeZone','UTC');                    
                    StartSlot = min(Ind,[],'all');    % find the earliest slot for 1 target
                    FirstTarg = find(Ind==StartSlot); % and the target number
                    Obj.StartTime = datetime(Obj.Vis.JD(StartSlot),'ConvertFrom','juliandate','TimeZone','UTC');
                    Obj.EndTime   = datetime(Obj.Vis.JD(StartSlot+Nbins-1),'ConvertFrom','juliandate','TimeZone','UTC');
                    Obj.delUniqTarg(1:Obj.TOOUsedTargets); % remove all the targets and add the nearest one only
                    Obj.addUniqTargets(RA(FirstTarg), Dec(FirstTarg),'Name',Names(FirstTarg));                    
                    fprintf('The nearest visibility window is found at %s\n',Obj.StartTime);                    
                    fprintf('for 1 target covering %.2f probability\n',Obj.TOOCoveredByTarget(FirstTarg));        
                else
                    error('No visibility window for the TOO can be found within the next 6 months');
                end
            end
            
            % Loop over the targets within the window
            NTargets = height(Obj.UniqTarg);
            
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
                Args.Group = [];
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
            % AllSS builder
            arguments
                Obj
                Args.AverageSlew            = 60;       % [s] estimate of the average slew time within a daily AllSS block
                Args.AllowPartial           = true;     % allow incomplete scheduling
                Args.MergeSameTargets       = true;     % merge 2 sequential visits of the same target into 1 visit of double Nexp
                Args.MaxBranch              = 0;        % SWITCHED OFF maximal number of branches to try before skipping a point
                Args.Verbose                = false;
            end
            % For the 361 sky points of the AllSS we need no less than 180*(2+16) = 3240 visits. 
            % As the scheduling cannot be ideal, let us assume that we need to try ~3600 visits, 
            % that is, allow for a maximum of 20 visits a day. 
            % If the average slot length for a visit could be ~ 3 x 300 + 71 (for retargeting) = 971 seconds,
            % the daily AllSS slot length will be ~ 5.39 hours, the total number of slots in a day will be 89. 
            % (if we dedicate a week for AllSS only, this may become 24 hrs)                        
            MinimalVisitSlot = double(Obj.DefEpochsPerVisit) * Obj.Exptime + Obj.FullTileReadTime ...
                               + Obj.DefSlewBuffer + seconds(Args.AverageSlew);  % the minimal size of the visit slot             
            Obj.DailySlots   = floor(days(1)/MinimalVisitSlot);                  % the maximal number of slots in a day
            VisitSlot        = 1/Obj.DailySlots;                                 % slot length in days     
            Obj.MaxDailyVisits = floor(Obj.DailyWindowMaxDuration/days(VisitSlot)); % no more then this number of AllSS visits per day
            
            % fill in the visibility matrix and determine visibility limits for each point and each time slot 
            Obj.CheckTimes = [Obj.StartTime, Obj.EndTime];
            Obj.updateTargetVisibility('TimeBin',VisitSlot,...
                'ObsSunDist',  Obj.ObsSunDist  +Obj.BufferSunDist,...
                'ObsMoonDist', Obj.ObsMoonDist +Obj.BufferMoonDist,...
                'ObsEarthDist',Obj.ObsEarthDist+Obj.BufferEarthDist);                             
            Limits = Obj.Vis.SunLimits .* Obj.Vis.EarthLimits .* Obj.Vis.MoonLimits .* Obj.Vis.PowerLimits;    
            
            % apply additional visibility constraints
            if Obj.EmptyDay % 1 empty day each week
                Ind = [];
                for k = 7*Obj.DailySlots:7*Obj.DailySlots:size(Limits,1)
                    Ind = [Ind, k:k+Obj.DailySlots-1]; 
                end
                Ind = Ind(Ind <= size(Limits,1)); 
                Limits(Ind,:) = 0;
            end
            
            % exclude already scheduled points (for multiple calls of the builder)
            F = Obj.SchedStatus.Status > 0; 
            Limits(:,F) = 0;                 
            
            % schedule the AllSS points in the averaged same length time slots
            [DailyTab, PointTabSorted, ~, Schedule] = ultrasat.tools.distributeAllSS(...
                Limits, Obj.UniqTarg.DitherGroup, Obj.MaxDailyVisits, Obj.DailySlots,...
                'VisitsByType',[Obj.LowLatVisits Obj.HighLatVisits],'FieldNames',Obj.UniqTarg.Name,....
                'MinIntervals',Obj.ExtragalMinIntervals, 'AllowPartial',Args.AllowPartial,'MaxBranch',Args.MaxBranch,...
                'Verbose',Args.Verbose);
            
            % warn if some of the points were not scheduled:
            VisitsToSchedule = sum(Obj.UniqTarg.DitherGroup(~F)==0)*Obj.LowLatVisits + sum(Obj.UniqTarg.DitherGroup(~F)>0)*Obj.HighLatVisits; 
            ScheduledVisits  = sum(Schedule~=0);            
            if ScheduledVisits < VisitsToSchedule               
                fprintf('Failed to schedule %d visits of %d\n',VisitsToSchedule-ScheduledVisits,VisitsToSchedule)               
            end                        
            
            % for each of the pre-scheduled days run the actual scheduler accounting for real retargeting times 
            NDays  = floor(size(Limits,1)/Obj.DailySlots);            
            for IDay = 1:NDays
                if Args.Verbose
                    fprintf('Planning AllSS targets for day %d\n',IDay);
                end
                if DailyTab.StartSlot(IDay) > 0
                    Ind  = (IDay-1)*Obj.DailySlots+DailyTab.StartSlot(IDay);
                    StartJD = Obj.Vis.JD(Ind);  
                    if Args.MergeSameTargets % may lead to shifts causing visibility errors!                     
                        [UniqTargets, Nexp] = ultrasat.tools.mergeAllSSTargetList(DailyTab.Points{IDay},'Nexp',Obj.DefEpochsPerVisit);
                    else
                        UniqTargets = DailyTab.Points{IDay};
                        Nexp = repmat(Obj.DefEpochsPerVisit,1,numel(UniqTargets));
                    end                 
                    % split the target list into parts according to positions of the 0s:
                    Ind0 = find(UniqTargets<1); % find all the zeros
                    if isempty(Ind0) % no zeros = no holes 
                        Obj.scheduleTargets(UniqTargets,...
                            datetime(StartJD,'ConvertFrom','juliandate','TimeZone','UTC'),...
                            'Group',IDay,'Nexp',Nexp);
                    else % split into groups:
                        NHoles=numel(Ind0);
                        for ii = 1:NHoles+1
                            if ii==1
                                T1 = 1; T2 = Ind0(ii)-1;
                            elseif ii == NHoles+1
                                T1 = Ind0(NHoles)+1; T2 = numel(UniqTargets);
                            else
                                T1 = Ind0(ii-1)+1;   T2 = Ind0(ii)-1;
                            end
                            if T2 >= T1 % there are some targets between T1 and T2
                                Obj.scheduleTargets(UniqTargets(T1:T2),...
                                    datetime(Obj.Vis.JD(Ind+T1-1),'ConvertFrom','juliandate','TimeZone','UTC'),...
                                    'Group',1000+IDay*10+ii,'Nexp',Nexp(T1:T2));
                            end
                        end
                    end
                end
            end % planning days
            
            PointTab = sortrows(PointTabSorted,{'PointNum'}); 
            Obj.SchedStatus.Status = (PointTab.Visits == PointTab.Filled) | (Obj.SchedStatus.Status>0); % mark the scheduled points             
        end % buildAllSS
    end % methods block
    %
    methods % Auxiliary functions
        %
        function addUniqTargets(Obj, RA, Dec, Args)
            % Add a list of [RA,Dec] coordinates (in degrees) to the unique targetList, 
            % and calls Obj.updateTargetProperties and Obj.updateTargetVisibility
            arguments
                Obj
                RA           = 0;    % [deg]
                Dec          = 0;    % [deg]
                Args.TimeBin = 0.01; % [day] % this is close to 1 visit 
                Args.Name    = '';   % Target name (optional)
                Args.File    = '';   % coordinate file name % ~/test.coo
                Args.ObsSunDist   = [];
                Args.ObsMoonDist  = [];
                Args.ObsEarthDist = [];
                Args.DitherGroup  = [];
                Args.UpdateVisibility = true; % update visibility immediately
            end
            %
            if ~isempty(Args.File)
                cooFile = readtable(Args.File);
                colRA = find(strcmp(cooFile.Properties.VariableNames,'RA'));
                colDec = find(strcmp(cooFile.Properties.VariableNames,'Dec'));
                colName = find(strcmp(cooFile.Properties.VariableNames,'Name'));
                Ncol = numel(cooFile.Properties.VariableNames);
                
                if isempty(colRA) || isempty(colDec)
                    if Ncol==3
                        colName = 1;
                        colRA = 2;
                        colDec =3;
                    else
                        colRA = 1;
                        colDec =2;
                    end
                    
                end
                RA  = table2array(cooFile(:,colRA)); 
                Dec = table2array(cooFile(:,colDec));
                if ~isempty(colName)
                    Args.Name = string(table2array(cooFile(:,colName)));
                end
            end
            %
            NUtarg = numel(RA); % the number of unique targets to be added
            NU0    = height(Obj.UniqTarg);
            %
            Obj.UniqTarg.RA( NU0+1:NU0+NUtarg) =  RA; 
            Obj.UniqTarg.Dec(NU0+1:NU0+NUtarg) = Dec;
            %
            if ~isempty(Args.Name)
                Obj.UniqTarg.Name(NU0+1:NU0+NUtarg) = Args.Name;
            end
            %
            if ~isempty(Args.DitherGroup)
                Obj.UniqTarg.DitherGroup(NU0+1:NU0+NUtarg) = Args.DitherGroup;
            end
            %
            Obj.N_uniqueTargets = height(Obj.UniqTarg);
            %
            Obj.updateTargetProperties('TargList',NU0+1:NU0+NUtarg);
            %
            if Args.UpdateVisibility
                Obj.updateTargetVisibility('TimeBin',Args.TimeBin,...
                    'ObsSunDist',Args.ObsSunDist,'ObsMoonDist',Args.ObsMoonDist,'ObsEarthDist',Args.ObsEarthDist);
            end
        end
        %
        function editUniqTarg(Obj,UniqTargInd,Args)           
            % Edit a given UniqTargInd in the uniqTarg table.Only allows to edit Name, RA, Dec
            % Update the UniqTarg properties and visibility.
            % If  UniqTargInd already shceduled in the Plan, updates all row with this UniqTarg
            arguments
                Obj
                UniqTargInd
                Args.Name   = '';
                Args.RA   = [];
                Args.Dec   = [];
            end
            
            CooChanged = false;
            
            % update fields in UniqTarg table, if needed 
            
            if ~isempty(Args.Name)
                Obj.UniqTarg.Name(UniqTargInd) = Args.Name;
            end
            
            if ~isempty(Args.RA)
                Obj.UniqTarg.RA(UniqTargInd) = Args.RA;
                CooChanged = true;
            end
            
            if ~isempty(Args.Dec)
                Obj.UniqTarg.Dec(UniqTargInd) = Args.Dec;
                CooChanged = true;
            end
            
            if CooChanged
                Obj.updateTargetProperties('TargList',UniqTargInd);
                Obj.updateTargetVisibility;
            end
            
            % find if UniqTargInd in the Plan
            Plan_rows = find(Obj.Plan.UniqTargInd==UniqTargInd);
            
            for ii = 1:numel(Plan_rows)
                Obj.Plan.Name(Plan_rows(ii)) = Obj.UniqTarg.Name(UniqTargInd);
                
                if CooChanged
                    Obj.Plan.RA(Plan_rows(ii)) = Obj.UniqTarg.RA(UniqTargInd);
                    Obj.Plan.Dec(Plan_rows(ii)) = Obj.UniqTarg.Dec(UniqTargInd);
                    Obj.editPlanRow(Plan_rows(ii),'updateRowsProp',true);
                end
            end            
        end
        %
        function delUniqTarg(Obj,UniqTargInd,Args)                               
            % Check if UniqTargInd is in the Plan. If not, delete it from UniqTarg table.
            % If in plan, by default will return an error. If specifcally asked to delete anyway, will do that and update the group to be continous 
            % (though won't change the Group start time) 
            arguments
                Obj
                UniqTargInd
                Args.abort_if_in_plan    = true; % 
            end
            
            Plan_rows = find(Obj.Plan.UniqTargInd==UniqTargInd);
            
            if Args.abort_if_in_plan && ~isempty(Plan_rows) 
                error('UniqTargInd is in Plan - aborting deletion');
            else
                Obj.UniqTarg(UniqTargInd,:) = [];

                Glist = unique(Obj.Plan.Group(Plan_rows));
                
                Obj.Plan(Plan_rows,:)=[];

                for ii = 1:numel(Glist)
                    % edit the group
                    G = find(Obj.Plan.Group==Glist(ii),1); % find first group member, if any...
                    if ~isempty(G)
                        Obj.editPlanRow(G,'updateRowsProp',true);
                    end
                end               
            end
            
            Obj.Plan.UniqTargInd(Obj.Plan.UniqTargInd>UniqTargInd) = Obj.Plan.UniqTargInd(Obj.Plan.UniqTargInd>UniqTargInd)-1;
            
            Obj.N_uniqueTargets = height(Obj.UniqTarg);
            
            %
            Obj.updateTargetVisibility; % consider remove specific UniqTarg                
        end
        %
        function saveUniqTargCooList(Obj,FileName)
            % Write the [Name, Ra, Dec] of the uniqe target into FileName  
            colRA = find(strcmp(Obj.UniqTarg.Properties.VariableNames,'RA'));
            colDec = find(strcmp(Obj.UniqTarg.Properties.VariableNames,'Dec'));
            colName = find(strcmp(Obj.UniqTarg.Properties.VariableNames,'Name'));
            writetable(Obj.UniqTarg(:,[colName,colRA,colDec]),FileName,'QuoteStrings',1)
        end
        %
        function clearUniqueTargets(Obj)
            % Clear the unique target list, as well as the plan and visibility object 
            
            % Remove all unique targets
            Obj.UniqTarg(:,:) = [];
            % clean the number of unique targets
            Obj.N_uniqueTargets = 0;
            % clear the plan
            Obj.clearPlan;
            % clean the visibility
            Obj.Vis = [];
        end

        %
        function scheduleTargets(Obj, UniqTargetIndexes,StartTime,Args)
            % Schedule a group of targets, starting at StartTime following by the rest, taking into account slew time between targets.
            % TODO- allow to provide a list of StartTime, one for each of target in the list.
            arguments
                Obj
                UniqTargetIndexes
                StartTime             % datetime object
                Args.Nexp       = []; % number of exposures taken in a row
                Args.Exptime    = []; % exposure time
                Args.Tiles      = []; % active tile numbers               
                Args.Group      = -1; % Group Ind. -1 for no group
            end
            %            
            if isempty(Args.Nexp)
                Args.Nexp = Obj.DefEpochsPerVisit;
            end
            if isempty(Args.Exptime)
                Args.Exptime = Obj.Exptime;
            end
            if isempty(Args.Tiles)
                Args.Tiles = Obj.Tiles;
            end
            
            NUtarg = numel(UniqTargetIndexes);
            NProws = height(Obj.Plan);
            
            if numel(Args.Nexp) < NUtarg
                Args.Nexp = repmat(Args.Nexp(1),1,NUtarg);
            end
            if numel(Args.Exptime) < NUtarg
                Args.Exptime = repmat(Args.Exptime(1),1,NUtarg);
            end
            if numel(Args.Tiles) < NUtarg
                Args.Tiles = repmat(Args.Tiles(1),1,NUtarg);
            end
            
            % Chen (05/10/2025)
            % Avoid Warning: "The assignment added rows to the table, but did not assign values to all of the table's existing variables. Those variables are extended with rows containing default values".
            lastRowNeeded = NProws + NUtarg;
            if height(Obj.Plan) < lastRowNeeded

                % Create filler table
                nToAdd = lastRowNeeded - height(Obj.Plan);
                filler = table('Size',[nToAdd numel(Obj.Plan_DefVarNames)], ...
                       'VariableNames', Obj.Plan_DefVarNames, 'VariableTypes', Obj.Plan_DefVarTypes);

                % Set proper timezone
                filler.Tstart.TimeZone = Obj.SysTimeZone;
                filler.Tend.TimeZone = Obj.SysTimeZone;
                filler.Tend_ValidationEstimate.TimeZone = Obj.SysTimeZone;                

                % Extend the Plan tale
                Obj.Plan = [Obj.Plan; filler];
            end
            
            % Add plan rows one be one
            for ii = 1:NUtarg
            
                Plan_row = NProws+ii;
                curr_UniqTargInd = UniqTargetIndexes(ii);
                
                Obj.Plan.Name(Plan_row) = Obj.UniqTarg.Name(curr_UniqTargInd);
                Obj.Plan.UniqTargInd(Plan_row) = curr_UniqTargInd;
                Obj.Plan.RA(Plan_row)  = Obj.UniqTarg.RA(curr_UniqTargInd); 
                Obj.Plan.Dec(Plan_row) = Obj.UniqTarg.Dec(curr_UniqTargInd); 
                Obj.Plan.ExpTime(Plan_row) = Args.Exptime(ii);
                Obj.Plan.Tiles(Plan_row)   = Args.Tiles(ii);
                Obj.Plan.Nexposures(Plan_row) = Args.Nexp(ii);

                if ii == 1
                    Obj.Plan.Tstart(Plan_row) = StartTime;
                    Obj.updatePlanRowProperties(Plan_row);
                else
                    Obj.updatePlanRowProperties(Plan_row,'CalcStartTimeFromPrevTarget',true);
                end 
            end
            
            %
            Obj.Plan.Group((NProws+1):(NProws+NUtarg)) = Args.Group;
            
            % update Number of target in the plan;
            Obj.N_planTargets = height(Obj.Plan);
            
            % update Start & End time of the plan;
            Obj.StartTime = min(Obj.Plan.Tstart);
            Obj.EndTime = max(Obj.Plan.Tend);
            
            % Timestamp of schedule
            Obj.schedule;
        end

        %
        function editPlanRow(Obj,Plan_row,Args)
            % Allow to directly edit only the following fields in a plan row:ExpTime, Tiles, Nexposures.
            % Will update row properties if needed (due to edited fields) or if asked directly (even if no fields were edited)
            % If plan_row is part of a group, update the properties of relevant other rows
            arguments
                Obj
                Plan_row
                Args.ExpTime    duration      =seconds(inf);
                Args.Tiles                 = [];
                Args.Nexposures     = [];
                Args.updateRowsProp = false; % If set to true - update Rows (and group memebrs prop., if ExpTime or Nexposures were edited, it will update rows anwyay.
            end
            
            updateRowsProp = Args.updateRowsProp;
            
            if ~isempty(Args.Tiles)
                Obj.Plan.Tiles(Plan_row) = Args.Tiles;
            end
            if ~isinf(Args.ExpTime)
                Obj.Plan.ExpTime(Plan_row) = Args.ExpTime;
                updateRowsProp = true;
            end
            if ~isempty(Args.Nexposures)
                Obj.Plan.Nexposures(Plan_row) = Args.Nexposures;
                updateRowsProp = true;
            end          
            
            if updateRowsProp
                G = Obj.Plan.Group(Plan_row);
                
                if G == -1
                    Obj.updatePlanRowProperties(Plan_row);
                else % part of a group
                    Glist = find(Obj.Plan.Group==G);
                    if Plan_row==Glist(1) % first in the group
                        Obj.updatePlanRowProperties(Plan_row);
                    else
                       Obj.updatePlanRowProperties(Plan_row,'CalcStartTimeFromPrevTarget',true);
                    end
                    
                    Glist = Glist(Glist>Plan_row); % only following group members
                    for ii = 1:numel(Glist)
                        Obj.updatePlanRowProperties(Glist(ii),'CalcStartTimeFromPrevTarget',true);
                    end

                end
            end            
        end
        %
        function delPlanRow(Obj,Plan_row)   
            % detlete the plan row and Check if part of a group. if so, adjust group (if needed).
            
            % extract the group of the row to be deleted
            G = Obj.Plan.Group(Plan_row);
            
            % Delete the row and update the number of 
            Obj.Plan(Plan_row,:) = [];
            Obj.N_planTargets = height(Obj.Plan);
            
            if Obj.Plan.Group(Plan_row)==G && G~=-1 % if the next plan row is part of the same group
                Obj.editPlanRow(Plan_row,'updateRowsProp',true);
            end
             
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
        function retrieveMissionApprovedPlan(Obj,Args)
            % Retrive the mission approved plan in a given time window (default window is Obj.CheckTimes) 
            % and populate the fields of Obj.MissionApprovedPlan.
            % Alternativly, allows also to provide a uplanner object (taking its plan as the MissionApprovedPlan) or struct of approved targets.
            arguments
                Obj
                Args.inputPlan = []; 
                Args.WindowStartTime = []; 
                Args.WindowEndTime = []; 
            end        
            
            %for now, allow to get a uPlan and use it as refernce
            if isa(Args.inputPlan,'table')
                Obj.clearMissionApprovedPlan;
                
                Obj.MissionApprovedPlan.RA(1:height(Args.inputPlan))  = 0; 
                Obj.MissionApprovedPlan.RA  =  Args.inputPlan.RA ;
                Obj.MissionApprovedPlan.Dec  =  Args.inputPlan.Dec ;
                Obj.MissionApprovedPlan.Roll  =  Args.inputPlan.ExpectedRoll ;
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
                if isempty(Obj.Mclient)
                    error('Obj.Mclient must be set'); 
                end

                structPlan = Obj.Mclient.getApprovedTargets(Args.WindowStartTime, Args.WindowEndTime);
            end
                       
            Obj.clearMissionApprovedPlan;
            Obj.RetrivedMissionTime = datetime('now','TimeZone', Obj.SysTimeZone);            

            if isempty(structPlan.targets)
                return;
            end

            TargetsTable = struct2table(structPlan.targets, 'AsArray', true);            
            
            Obj.MissionApprovedPlan.RA(1:height(TargetsTable))  = 0; 
            Obj.MissionApprovedPlan.Name(1:height(TargetsTable))  = TargetsTable.name; 
            Obj.MissionApprovedPlan.pk(1:height(TargetsTable))  = TargetsTable.pk; 
            Obj.MissionApprovedPlan.TargetID = TargetsTable.target_id;
            Obj.MissionApprovedPlan.RA       = TargetsTable.ra ;
            Obj.MissionApprovedPlan.Dec      = TargetsTable.decl ;
            Obj.MissionApprovedPlan.Roll     = TargetsTable.roll ;
            Obj.MissionApprovedPlan.Tstart   = Obj.parseIsoDatetime(TargetsTable.start_time);
            Obj.MissionApprovedPlan.Tend     = Obj.parseIsoDatetime(TargetsTable.end_time);
            Obj.MissionApprovedPlan.ExpTime  = seconds(TargetsTable.exposure);
            Obj.MissionApprovedPlan.Nexposures = TargetsTable.image_count;
            Obj.MissionApprovedPlan.TotalDuration = seconds(TargetsTable.total_seconds);               
        end
         %
        function clearMissionApprovedPlan(Obj)
            % Clear the Mission Approved Plan table
            Obj.MissionApprovedPlan(:,:) = [];
            Obj.RetrivedMissionTime = datetime([],[],[]);
        end    
        %
        function [CheckStatus,badPlanRow] = planSelfConsistencyCheck(Obj,Args)
            % Verify that the plan schedule is self consistent
            arguments
                Obj
                Args.timingPrecision = seconds(0.01);
            end
            
            tmpPlan = Obj.Plan;
            
            tmpPlan = sortrows(tmpPlan,'Tstart');
            
            % Validate that Obj.Start time and the first start time in the plan agree
            if abs(Obj.StartTime-tmpPlan.Tstart(1))>Args.timingPrecision
                fprintf('Bad Start Time of Entire Object\n');
                CheckStatus = false;
                badPlanRow = tmpPlan(1,:);
                return
            end
            
            for Plan_row = 1:height(tmpPlan)
                % calculate and validate times between targets
                if Plan_row>1
                    
                    [T_sec,~] = ultrasat.tools.calcSlew(tmpPlan.RA(Plan_row-1),tmpPlan.Dec(Plan_row-1),tmpPlan.RA(Plan_row),tmpPlan.Dec(Plan_row),...
                                                        'Units','deg','CheckTrajectory',true);
                    tmpSlewTimeBefore = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;
                    tmpTstart = currTend + tmpSlewTimeBefore;                    
 
                    if (tmpPlan.Tstart(Plan_row)-tmpTstart)<-Args.timingPrecision

                        fprintf('Bad timing between rows\n');
                        CheckStatus = false;
                        badPlanRow = tmpPlan(Plan_row,:);
                        return               
                    end                    
                end
                % calcaute and validate relative time within the plan row 
                tmpTotalDuration = tmpPlan.Nexposures(Plan_row) * tmpPlan.ExpTime(Plan_row) + Obj.FullTileReadTime;
                tmpTend = tmpPlan.Tstart(Plan_row) + tmpTotalDuration;
                
                tmpJDstart = juliandate(tmpPlan.Tstart(Plan_row));
                tmpJDend = juliandate(tmpTend);
                
                if abs(tmpPlan.TotalDuration(Plan_row)-tmpTotalDuration)>Args.timingPrecision || ...
                   abs(tmpPlan.Tend(Plan_row)-tmpTend)>Args.timingPrecision || ...     
                   abs(tmpPlan.JDstart(Plan_row)-tmpJDstart)>seconds(Args.timingPrecision)/3600/24  || ...
                   abs(tmpPlan.JDend(Plan_row)-tmpJDend)>seconds(Args.timingPrecision)/3600/24
                    
                    fprintf('Bad timing within row\n');
                    CheckStatus = false;
                    badPlanRow = tmpPlan(Plan_row,:);
                    return               
                end

                currTend = tmpTend;
            end
            
            % Validate that Obj.Start time and the first start time in the plan agree
            if abs(Obj.EndTime-currTend)>Args.timingPrecision
                fprintf('Bad End Time of Entire Object\n');
                CheckStatus = false;
                badPlanRow = tmpPlan(end,:);
                return
            end            
            
            CheckStatus = true;
            badPlanRow = [];
        end
        %
        function adjustGroupStartTime(Obj,Args)
            % Adjust the start time of a group in the plan by 3 options: 
            %       a given NewStartTime, a given ShiftTime, or relative to a target in the OverLap targets list.
            % If no GroupList is provided, will adjust all groups in the plan, one by one.
            arguments
                Obj
                Args.GroupList             = [];
                Args.NewStartTime          = [];
                Args.ShiftTime   duration  = seconds(inf);
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
            
            Obj.StartTime = min(Obj.Plan.Tstart);
            Obj.EndTime = max(Obj.Plan.Tend);
            
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
                Args.ExtSurveyMapsFile = 'ExtSurveyMaps.mat';%'~/matlab/data/ULTRASAT/ExtSurveyMaps.mat';
                Args.FieldObjectsFile  = 'FieldObjects.mat';%'~/matlab/data/ULTRASAT/FieldObjects.mat';
                Args.AveExtincFile      = 'A_USat_aver7deg_hp49152_v2.mat'; % '~/matlab/data/ULTRASAT/A_USat_aver7deg_hp49152_v2.mat'
                Args.HealpixNside = 2^8; % corresponds to R ~ 0.2 deg
                Args.TargList            = []; % List of Targets (index) to update. If empty, update all targets in UniqTarg
            end
            
            % If no list, apply to all targets
            if isempty(Args.TargList)
                Args.TargList = 1:height(Obj.UniqTarg);
            end
                        
            % target coordinates 
            RA  = Obj.UniqTarg.RA(Args.TargList); 
            Dec = Obj.UniqTarg.Dec(Args.TargList); 
            
            % extinction 
            Obj.UniqTarg.A_U(Args.TargList) = ultrasat.tools.extinction(RA, Dec,'AveragedExt',fullfile(Obj.BaseDataDir,Args.AveExtincFile)); 
            
            % load the lists of external important objects and survey maps
            load(fullfile(Obj.BaseDataDir,Args.ExtSurveyMapsFile)); % 'SurveyMaps' table
            load(fullfile(Obj.BaseDataDir,Args.FieldObjectsFile));  % 'Known_Obj_large', 'Known_Obj_small' tables

            for ii = 1:numel(Args.TargList) % loop over targets 
                
                iT = Args.TargList(ii);                
                
                RA0 = Obj.UniqTarg.RA(iT); Dec0 = Obj.UniqTarg.Dec(iT);                
                % make a circular FOV region
                FOV = ultrasat.tools.getFOVcircle(RA0,Dec0,'Radius',Obj.Rfov,'Plot',0);  
                FOVp = polyshape(FOV);  % a polyshape is useful to test intersections
                
                % select calibration objects 
                Ind = celestial.search.isPointInsidePolygon(Obj.CalibObj.RA, Obj.CalibObj.Dec, FOV);
                Obj.UniqTarg.CalObj{iT} = num2cell(find(Ind>0));
                
                % select reference images
%                 Ind = celestial.search.isPointInsidePolygon(Obj.RefIma.RA, Obj.RefIma.Dec,FOV); 
%                 Obj.UniqTarg.RefImageIDs{iT} = num2cell(find(Ind>0));

                % select external surveys 
                Ind = overlaps(SurveyMaps.Shape,FOVp);
                Obj.UniqTarg.ExtSurveys{iT} = num2cell(find(Ind>0));
               
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
                Obj.UniqTarg.FieldObj{iT} = Field;
                
                % calcaulte healpix indices covered by this target
                % Currently only uses a cone and not actual polygon which can be used only in  relevant orientation (i.e. roll)           
                ID = celestial.healpix.coneSearch(Args.HealpixNside,RA0,Dec0,Obj.Rfov,'RadiusUnits','deg','CooUnits','deg'); % (returns Ipix ids)                
                Obj.UniqTarg.HealpixArray{iT} = celestial.healpix.pix2uniqueId(Args.HealpixNside,ID); % can be converted to unique ids                        
            end            
        end
        %
        function updatePlanRowProperties(Obj, Plan_row, Args)
            % Calcaulte and fill for a given plan row the following properties: 
            %       TotalDuration, Tend, JDstart, JDend, ExpectedRoll,  NoComm, HardObs, MoonDist, SunDist, EarthDist,OverlapTargets
            % If asked to CalcStartTimeFromPrevTarget then also calcuates:
            %       SlewTimeBefore, Tstart
            % Return error If there's issue with  Sun/Earth/Moon limits
            arguments
                Obj
                Plan_row                    % Index
                Args.CalcStartTimeFromPrevTarget   = false; % Relevant for targets part of a group (not the first)
            end 
           
            RAD = 180/pi;
            
            Obj.Plan.TotalDuration(Plan_row) = Obj.Plan.Nexposures(Plan_row) * Obj.Plan.ExpTime(Plan_row) + Obj.FullTileReadTime; 

            if Args.CalcStartTimeFromPrevTarget
                [T_sec,~] = ultrasat.tools.calcSlew(Obj.Plan.RA(Plan_row-1),Obj.Plan.Dec(Plan_row-1),Obj.Plan.RA(Plan_row),Obj.Plan.Dec(Plan_row),...
                                                    'Units','deg','CheckTrajectory',true);  
                Obj.Plan.SlewTimeBefore(Plan_row) = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;  
                Obj.Plan.Tstart(Plan_row) = Obj.Plan.Tend(Plan_row-1) + Obj.Plan.SlewTimeBefore(Plan_row);  
            end

            Obj.Plan.Tend(Plan_row) = Obj.Plan.Tstart(Plan_row) + Obj.Plan.TotalDuration(Plan_row); 
            Obj.Plan.JDstart(Plan_row) = juliandate(Obj.Plan.Tstart(Plan_row));  
            Obj.Plan.JDend(Plan_row) = juliandate(Obj.Plan.Tend(Plan_row)); 

            Obj.Plan.ExpectedRoll(Plan_row) = ultrasat.tools.expectedRoll(Obj.Plan.RA(Plan_row),Obj.Plan.Dec(Plan_row),Obj.Plan.JDstart(Plan_row));

            TargetVis = ultrasat.ULTRASAT_restricted_visibility(Obj.Plan.JDstart(Plan_row), [Obj.Plan.RA(Plan_row) Obj.Plan.Dec(Plan_row)],'CooUnits','deg',...
                'MinSunDist',Obj.ObsSunDist,'MinMoonDist',Obj.ObsMoonDist,'MinEarthDist',Obj.ObsEarthDist,'MinDistOffset',0); 

            if ~all([TargetVis.EarthLimits , TargetVis.MoonLimits , TargetVis.SunLimits])
                fprintf('Target %d, JDstart %.2f\n',Obj.Plan.UniqTargInd(Plan_row),Obj.Plan.JDstart(Plan_row))
                
                % @Chen: Temporary for development - removed to allow GUI tests (06/07/2025)
				if ~ispc
                	error('Issue with Sun/Earth/Moon limits');
				end
            end

            Obj.Plan.NoComm(Plan_row) = ~all(TargetVis.CommLimits); 
            Obj.Plan.HardObs(Plan_row) = ~all(TargetVis.PowerLimits);


            Obj.Plan.MoonDist(Plan_row) = TargetVis.MoonAngDist*RAD; 
            Obj.Plan.SunDist(Plan_row) = TargetVis.SunAngDist*RAD;
            Obj.Plan.EarthDist(Plan_row) = TargetVis.EarthAngDist*RAD; 

            % TODO - ADD Calc Zody,LimMag  

            % Search for overlapping targets
            if ~isempty(Obj.MissionApprovedPlan)          
                Obj.Plan.OverlapTargets{Plan_row} = find((Obj.Plan.Tstart(Plan_row) > Obj.MissionApprovedPlan.Tstart & Obj.Plan.Tstart(Plan_row) < Obj.MissionApprovedPlan.Tend) |...
                                                    (Obj.Plan.Tend(Plan_row)   > Obj.MissionApprovedPlan.Tstart & Obj.Plan.Tend(Plan_row)   < Obj.MissionApprovedPlan.Tend));
            end
        end
        %
        function updateTargetVisibility(Obj, Args)
            % Calcuate visibility for all unique targets for a given time window (default window is Obj.CheckTimes)
            arguments
                Obj                     
                Args.TimeBin         = 0.01; % [days] % this is close to 1 visit 
                Args.WindowStartTime = []; 
                Args.WindowEndTime   = []; 
                Args.ObsSunDist      = [];
                Args.ObsMoonDist     = [];
                Args.ObsEarthDist    = [];
            end
            %
            if isempty(Args.WindowStartTime)
                Args.WindowStartTime = Obj.CheckTimes(1);
            end
            
            if isempty(Args.WindowEndTime)
                Args.WindowEndTime   = Obj.CheckTimes(2);
            end
            
            if isempty(Args.ObsSunDist)
                Args.ObsSunDist = Obj.ObsSunDist;
            end
            
            if isempty(Args.ObsMoonDist)
                Args.ObsMoonDist = Obj.ObsMoonDist;
            end
            
            if isempty(Args.ObsEarthDist)
                Args.ObsEarthDist = Obj.ObsEarthDist;
            end
            
            StartJD = juliandate(Args.WindowStartTime);
            EndJD   = juliandate(Args.WindowEndTime);
            VisJD   = StartJD + (0:Args.TimeBin:(EndJD-StartJD))';                         
            Obj.Vis = ultrasat.ULTRASAT_restricted_visibility(VisJD, [Obj.UniqTarg.RA Obj.UniqTarg.Dec],'CooUnits','deg',...
                'MinSunDist',Args.ObsSunDist,'MinMoonDist',Args.ObsMoonDist,'MinEarthDist',Args.ObsEarthDist,'MinDistOffset',0);             
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
            % Set Obj.Status to 'draft' and Obj.ScheduledTime time to 'now'. (called from Obj.scheduleTargets)
            Obj.Status    = 'draft';
            Obj.ScheduledTime = datetime('now','TimeZone', 'UTC');    
        end
        %
        function validate(Obj,Args)
            % TODO - send plan to the validator. In addition, set Obj.Status to 'validated' and Obj.ValidatedTime to 'now'
            arguments
                Obj
                Args.checkSelfConsistency       = true;
            end
            
            if Args.checkSelfConsistency  % Check self consistency of plan before sending to validation
                CheckStatus = Obj.planSelfConsistencyCheck;
                if ~CheckStatus
                    error('Plan is not self-consistent. Validation aborted'); 
                end
            end
            
            if isempty(Obj.Mclient)
                error('Obj.Mclient must be set'); 
            end

            planStruct = Obj.planTable2struct;
            % send struct plan to the validator.
            Obj.ValidationResponse = Obj.Mclient.validatePlan(planStruct);      
            targets = Obj.ValidationResponse.task.targets;

            if numel(targets)~=height(Obj.Plan)
                error('Number of targets in validation response do not match the number of targets in the plan. Validation aborted');
            else
                for i = 1:numel(targets)  % assumes same order of target SHOULD VERIFY!
                    Obj.Plan.ValidationStatus(i) = targets(i).status;
                    Obj.Plan.PowerStatus(i) = targets(i).power_status;
                    Obj.Plan.ObrdStatus(i) = targets(i).obrd_status;
                    Obj.Plan.Tend_ValidationEstimate(i) = Obj.parseIsoDatetime(targets(i).estimated_end_time);
                    Obj.Plan.Roll_ValidationEstimate(i) = targets(i).coord_roll;
                    Obj.Plan.ValidationWarning{i} = targets(i).warning;
                end
            end
                        
            Obj.Status    = 'validated';
            Obj.ValidatedTime = datetime('now','TimeZone', 'UTC');     
        end        
        %
        function clearValidationData(Obj)
            % Clears valiation data from Plan table, delete the ValidationTime and ValidationResponse and change status back to draft
            
            Obj.Plan.ValidationStatus(:) = string(missing);
            Obj.Plan.PowerStatus(:) = string(missing);
            Obj.Plan.ObrdStatus(:) = string(missing);
            Obj.Plan.Tend_ValidationEstimate(:) = NaT;
            Obj.Plan.Roll_ValidationEstimate(:) = 0;
            Obj.Plan.ValidationWarning(:) = cell(size(Obj.Plan.ValidationWarning));

            Obj.ValidationResponse = [];      

            Obj.Status    = 'draft';
            Obj.ValidatedTime = NaT;     
        end        
        %
        function submit(Obj,Args)
            %  TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.SubmittedTime to 'now'
            arguments
                Obj
                Args.checkSelfConsistency       = true;
            end
            
            if Args.checkSelfConsistency  % Check self consistency of plan before sending to validation
                CheckStatus = Obj.planSelfConsistencyCheck;
                if ~CheckStatus
                    error('Plan is not self-consistent. Submition aborted'); 
                end
            end

            if isempty(Obj.Mclient)
                error('Obj.Mclient must be set'); 
            end            

            planStruct = Obj.planTable2struct;
            % send struct plan to the Mission C&C.
            Obj.Mclient.submitPlan(planStruct);
            
            Obj.Status    = 'submitted';
            Obj.SubmittedTime = datetime('now','TimeZone', 'UTC'); 
        end
        %
        function planStruct = planTable2struct(Obj,Args)
            % Return a struct array of a conversion of the Obj.Plan table, in the correct naming and format for validation/submission
            arguments
                Obj
                Args.fields = {};
                Args.DefRoll = 0;
            end        
                      
            if isempty(Args.fields) %use defults fields
                tmpTable = Obj.Plan;
                
                keepVars = false(size(tmpTable.Properties.VariableNames));
                
                %rename Name->name
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Name');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'name'};
                
                %rename RA->ra
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'RA');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'ra'};
                
                %rename Dec->decl
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Dec');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'decl'};
                
                %rename ExpectedRoll->roll
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'ExpectedRoll');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'roll'};
                
                if ~isempty(Args.DefRoll)
                    tmpTable.roll(:) = Args.DefRoll;
                end
                
                %rename Tstart->start_time
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Tstart');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'start_time'};                
                              
                %rename Tend->end_time
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Tend');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'end_time'};                  
               
                %rename ExpTime->exposure
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'ExpTime');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'exposure'};   
                
                %convert to numeric
                tmpTable.exposure = seconds(tmpTable.exposure);
                
                %rename Nexposures->image_count
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Nexposures');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'image_count'}; 
                
                %rename TotalDuration->total_seconds
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'TotalDuration');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'total_seconds'};  
                
                %convert to numeric
                tmpTable.total_seconds = seconds(tmpTable.total_seconds);
                
                %rename Tiles->tiles
                curr_ind = strcmp(tmpTable.Properties.VariableNames,'Tiles');
                keepVars = keepVars | curr_ind;
                tmpTable.Properties.VariableNames(curr_ind) = {'tiles'};   
                
                tmpTable.tiles = regexprep(cellstr(tmpTable.tiles),'(\w)','$1,');
                tmpTable.tiles = regexprep(tmpTable.tiles,',$','');
                
                     
                tmpTable = tmpTable(:,keepVars);
                
                planStruct = table2struct(tmpTable);

                % MATLAB cannot have array with single struct item, the
                % only solution is to convert the array to cellarray
                %if numel(planStruct) == 1
                %    planStruct = {planStruct};
                %end
            else
                error('Currently does not support non-standard fields');
            end
            
        end
        %
        function [Res,h] = showCalibObj(Obj,UniqTargInd,Args)
            % Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
            arguments
                Obj
                UniqTargInd               = [];
                Args.PlotSpectrum = false;
                Args.subInd2plot  = 1;
                Args.WaveRange    = []; % [nm] range for spectrum plotting, e.g. [230 300] 
                Args.AxesHandle       =[]; % appUIAxes
            end
            %
            h = [];
            %
            if isempty(UniqTargInd)
                TabInd = unique(Cell2Vec([Obj.UniqTarg.CalObj{:}]));
                Res = Obj.CalibObj(TabInd,:);
            else
                TabInd = [Obj.UniqTarg.CalObj{UniqTargInd}{:}]; % 
                Res = Obj.CalibObj(TabInd,:); 
            end
            if Args.PlotSpectrum
                Fname = sprintf('%s/%s.fits',Obj.CalibDir,Res.obj{Args.subInd2plot});
                Ftab  = fitsread(Fname,'binarytable');
                Spec  = [Ftab{1} Ftab{6} Ftab{7}];  
                
                if isempty(Args.AxesHandle)
                    h = figure('WindowStyle','docked','Color',[1 1 1]); clf;
                    ax = axes(h);
                else 
                    ax = Args.AxesHandle;
                end
                
                errorbar(ax,Spec(:,1),Spec(:,2),Spec(:,3),'.'); 
                xlabel(ax, '\lambda [A]'); 
                ylabel(ax, 'F [erg/cm(2)/s/A]'); 
                set(ax, 'YScale', 'log');
                if ~isempty(Args.WaveRange)
                    xlim(ax,Args.WaveRange.*10);
                end
                title(ax,sprintf('%s: Teff = %.0f, log(g) = %.1f',Res.obj{Args.subInd2plot},Res.Teff_K_(Args.subInd2plot),Res.logG(Args.subInd2plot))); 
            end            
        end
        %
        function plotVisibility(Obj,UniqTargInd,Args)
            % plot the visibilty of a UniqTarg
            arguments
                Obj
                UniqTargInd
                Args.AxesHandle       =[]; % appUIAxes                
                Args.TimeWindowJD   = []; 
                Args.JD_offset    = 2460000;
                Args.SunColor     = 'k';
                Args.EarthColor     = 'b';
                Args.MoonColor     = 'g';
                Args.TimeColor      = 'r';
            end
            
            RAD = 180/pi;  
            
            if isempty(Args.AxesHandle)
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;  
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end
            hold(ax, 'on');  
            box(ax, 'on');
            
            V = Obj.Vis;
            
            % plot Sun/Earth/Moon distances
            plot(ax,V.JD-Args.JD_offset,V.SunAngDist(:,UniqTargInd)*RAD,Args.SunColor);
            plot(ax,V.JD-Args.JD_offset,V.EarthAngDist(:,UniqTargInd)*RAD,Args.EarthColor);
            plot(ax,V.JD-Args.JD_offset,V.MoonAngDist(:,UniqTargInd)*RAD,Args.MoonColor);
            
            % plot Sun/Earth/Moon limits
            plot(ax,V.JD([1,end])-Args.JD_offset,[Obj.ObsSunDist Obj.ObsSunDist],['--' Args.SunColor],'linewidth',2);
            plot(ax,V.JD([1,end])-Args.JD_offset,[Obj.ObsEarthDist Obj.ObsEarthDist],['--' Args.EarthColor],'linewidth',2);
            plot(ax,V.JD([1,end])-Args.JD_offset,[Obj.ObsMoonDist Obj.ObsMoonDist],['--' Args.MoonColor],'linewidth',2);
            
            yl = ylim(ax); % can be removed when using xregion
                        
            % Check for unobservable times due to Sun %% ERROR if only one JD is not observable
            Fvis = find(~V.SunLimits(:,UniqTargInd));
            if ~isempty(Fvis)
                Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                nonVisWindows(:,1) = V.JD(Fvis(1:2:end));
                nonVisWindows(:,2) = V.JD(Fvis(2:2:end));

                for i = 1:height(nonVisWindows)
                    fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)]-Args.JD_offset,...
                        [0,0,180,180],Args.SunColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                end
            end
            
            % Check for unobservable times due to Earth %% ERROR if only one JD is not observable
            Fvis = find(~V.EarthLimits(:,UniqTargInd));
            if ~isempty(Fvis)            
                Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                clear nonVisWindows;
                nonVisWindows(:,1) = V.JD(Fvis(1:2:end));
                nonVisWindows(:,2) = V.JD(Fvis(2:2:end));

                for i = 1:height(nonVisWindows)
                    fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)]-Args.JD_offset,...
                        [0,0,180,180],Args.EarthColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                end
            end
            
            % Check for unobservable times due to Moon %% ERROR if only one JD is not observable
            Fvis = find(~V.MoonLimits(:,UniqTargInd));
            if ~isempty(Fvis)            
                Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                clear nonVisWindows;
                nonVisWindows(:,1) = V.JD(Fvis(1:2:end));
                nonVisWindows(:,2) = V.JD(Fvis(2:2:end));

                for i = 1:height(nonVisWindows)
                    fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)]-Args.JD_offset,...
                        [0,0,180,180],Args.MoonColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                end            
            end
            
            % set plot limits
            ylim(ax,yl); % can be removed when using xregion
            
            xlim(ax,V.JD([1,end])-Args.JD_offset);
            if ~isempty(Args.TimeWindowJD)
                xlim(ax,Args.TimeWindow-Args.JD_offset)
            end
            
            
            % plot StartTime and EndTime            
            xline(ax,juliandate(Obj.StartTime)-Args.JD_offset,['-' Args.TimeColor],'Start Time');
            xline(ax,juliandate(Obj.EndTime)-Args.JD_offset,['-' Args.TimeColor],'End Time');
            
            xlabel(ax,sprintf('JD-%.1f',Args.JD_offset)); 
            ylabel(ax,'Angular distance [deg]');
            title(ax,sprintf('Visibility of UniqTarget #%d',UniqTargInd)); 
            legend(ax, 'Sun','Earth','Moon','Location','best');
            hold(ax, 'off');  
        end
        %
        function plotMapPlan(Obj,Args)
            % plotting on a map relevant properties and info from the plan
            % TODO - Change to map projection later
            %
            %
            arguments
                Obj
                Args.AxesHandle         =  [];
                Args.cooSys             =  'j2000.0';
                Args.plotTstart         = []; % datetime or JD
                Args.plotTend           = []; % datetime or JD
                Args.disp_uniqTarg      = false;
                Args.UniqTargInds       = [];
                Args.disp_plan          = true;
                Args.plan_rows          = [];
                Args.ExtinctionMap      = false;
                Args.CalObjMap          = false;
                Args.disp_MissAprvPlan  = false;
                Args.MissAprvPlan_rows  = [];                
                Args.vis_at_time_map    = false;
                Args.AveExtincFile      = 'A_USat_aver7deg_hp49152_v2.mat'; % '~/matlab/data/ULTRASAT/A_USat_aver7deg_hp49152_v2.mat'
            end
            RAD = 180/pi;  
            
            if isempty(Args.AxesHandle)
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;  
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end
            hold(ax, 'on');  
            box(ax, 'on'); 
            
            if Args.ExtinctionMap
                RA_vec = (0:360); Dec_vec = (-90:90);
                [RA_grid,Dec_grid] = meshgrid(RA_vec,Dec_vec);
                A_u = ultrasat.tools.extinction(RA_grid,Dec_grid,'AveragedExt',fullfile(Obj.BaseDataDir,Args.AveExtincFile)); 
                imagesc(ax,RA_vec, Dec_vec, A_u);
                c = colorbar(ax);
                c.Label.String = 'A_{ULTRASAT}';
                clim(ax, [0,1.1]);
                set(ax,'YDir','normal');
            end
            
            if Args.vis_at_time_map
                disp('TBD');
            end            
            
            if Args.disp_uniqTarg
                UniqTargInds = Args.plan_rows;
                if isempty(UniqTargInds)
                    UniqTargInds = 1:height(Obj.UniqTarg);
                end
                
                for ii = 1:numel(UniqTargInds)
                    CircFOV = ultrasat.tools.getFOVcircle(Obj.UniqTarg.RA(UniqTargInds(ii)),Obj.UniqTarg.Dec(UniqTargInds(ii)),'Radius',Obj.Rfov);
                    CircFOV(CircFOV(:,1)<0,1) = CircFOV(CircFOV(:,1)<0,1)+360;
                    CircFOV(CircFOV(:,1)>360,1) = CircFOV(CircFOV(:,1)>360,1)-360;
                    
                    plot(ax,CircFOV(:,1),CircFOV(:,2),'.b');
                end
            end
            
            if Args.CalObjMap
                if ~isempty(Obj.CalibObj)
                    plot(ax,Obj.CalibObj.RA,Obj.CalibObj.Dec,'*m');
                end
            end
            
            if Args.disp_MissAprvPlan
                MissAprvPlan_rows = Args.MissAprvPlan_rows;
                if isempty(MissAprvPlan_rows)
                    MissAprvPlan_rows = 1:height(Obj.MissionApprovedPlan);
                end
                
                for ii = 1:numel(MissAprvPlan_rows)
                    currFoV = ultrasat.tools.getFOVcorners(Obj.MissionApprovedPlan.RA(MissAprvPlan_rows(ii)),Obj.MissionApprovedPlan.Dec(MissAprvPlan_rows(ii)),...
                        'Roll',Obj.MissionApprovedPlan.Roll(MissAprvPlan_rows(ii)));
                    currFoV.RA(currFoV.RA<0) = currFoV.RA(currFoV.RA<0)+360;
                    currFoV.RA(currFoV.RA>360) = currFoV.RA(currFoV.RA>360)-360;
                    
                    plot(ax,polyshape(currFoV.RA,currFoV.Dec),'EdgeColor','r','FaceColor','none','linewidth',2);
                end
            end
            
            if Args.disp_plan
                plan_rows = Args.plan_rows;
                if isempty(plan_rows)
                    plan_rows = 1:height(Obj.Plan);
                end
                
                for ii = 1:numel(plan_rows)
                    currFoV = ultrasat.tools.getFOVcorners(Obj.Plan.RA(plan_rows(ii)),Obj.Plan.Dec(plan_rows(ii)),'Roll',Obj.Plan.ExpectedRoll(plan_rows(ii)));
                    currFoV.RA(currFoV.RA<0) = currFoV.RA(currFoV.RA<0)+360;
                    currFoV.RA(currFoV.RA>360) = currFoV.RA(currFoV.RA>360)-360;
                    
                    plot(ax,polyshape(currFoV.RA,currFoV.Dec),'EdgeColor','k','FaceColor','none','linewidth',2);
                end
            end
            
            xlim(ax, [0,360]);
            ylim(ax, [-90,90]);
            xlabel(ax, 'RA [deg]');
            ylabel(ax, 'Dec [deg]');
            
            hold(ax, 'off'); 
        end


        function dt = parseIsoDatetime(Obj, str)
            % Convert JSON date/time string to datetime object
            if endsWith(str, 'Z')
                fmt = 'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z''';
            else
                fmt = 'yyyy-MM-dd''T''HH:mm:ss.SSSSSSXXX';
            end
            dt = datetime(str, 'InputFormat', fmt, 'TimeZone', Obj.SysTimeZone);
        end
        
    end
    % 
    methods (Static)  % static methods
        %
        function CheckTimes = getDefaultCheckTimes()
           % Get the default Check times. TODO - update if needed
           
           % CheckTimes =datetime({'2028-01-01 00:00:00','2028-07-01 00:00:00'});
           
           T1 = dateshift(datetime('now'),'start','month'); 
           T2 = T1+calmonths(7); 
           CheckTimes = [T1,T2];
        end
    end
    %
    methods(Static) % unitTest, Debug
        Result = debug()
            % unitTest
            function Result = unitTest(Args)
                arguments
                    Args.Verbose   = true;
                    Args.Parts     = {'HCS','LCS','TOO','DDT'}; % {'HCS','LCS','AllSS','TOO','DDT'}; % currently we need HCS to test LCS 
                end
                % unitTest
                Result=false;
                %
                if Args.Verbose
                    fprintf('Start uplanner unit Test\n');
                    fprintf('---------------------------------\n');
                end
                %
                if ismember('hcs',lower(Args.Parts))                    
                    if Args.Verbose
                        fprintf('Start testing HCS plan...');
                    end
                    
                    % Example for creating HCS survey:
                    HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');
                    upHCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','HCS');
                    upHCS.StartTime = 'now';
                    upHCS.EndTime = upHCS.StartTime+calmonths(6)-days(1);
                    upHCS.addUniqTargets(HCS_fields.RA('S1'),HCS_fields.Dec('S1'),'Name',HCS_fields.Name('S1'));
                    upHCS.buildHCS;
                    
                    if Args.Verbose
                        fprintf('completed\n');
                    end                    
                end
                %
                if ismember('lcs',lower(Args.Parts))
                    if Args.Verbose
                        fprintf('Start testing LCS plan...');
                    end
                    
                    % upHCS.plotMapPlan('disp_uniqTarg',true,'disp_plan',true,'ExtinctionMap',true,'CalObjMap',true,'disp_MissAprvPlan',true)
                    
                    % Example for creating LCS survey:
                    upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');
                    upLCS.StartTime = 'now';
                    upLCS.EndTime = upLCS.StartTime+caldays(45);
                    upLCS.DailyWindowStartTime = duration('03:00:00');
                    
                    LCS_grid = readtable(fullfile(upLCS.BaseDataDir,'LCS_nonoverlapping_grid.csv'));
                    F = LCS_grid.V45==1 & LCS_grid.A_U_1==1;
                    upLCS.addUniqTargets(LCS_grid.RA(F),LCS_grid.Dec(F),'Name',num2cell(LCS_grid.Field(F)));
                    
                    upLCS.updateTargetVisibility('WindowStartTime',upLCS.StartTime,'WindowEndTime',upLCS.EndTime);
                    F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits ,1));
                    
                    % Fakely retrive upHCS ar approved target list
                    upLCS.retrieveMissionApprovedPlan('inputPlan',upHCS.Plan);
                    
                    % check with struct
                    load(fullfile(upLCS.BaseDataDir,'api_response.mat')');
                    
                    upLCS.retrieveMissionApprovedPlan('inputPlan',response);
                    
                    upLCS.buildLCS('TargetList',F2);
                    
                    if Args.Verbose
                        fprintf('completed\n');
                    end
                    
                    if Args.Verbose
                        fprintf('Start testing adjustGroupStartTime, edit/del UniqTarg/PlanRow...');
                    end
                    
                    upLCS.adjustGroupStartTime;  % Check adjustments relative to Approved List
                    
                    CheckStatus = upLCS.planSelfConsistencyCheck;
                    if ~CheckStatus
                        return
                    end
                    
                    upLCS.editUniqTarg(4,'Name',"bla");
                    upLCS.editUniqTarg(4,'RA',100);
                    
                    upLCS.editPlanRow(1);
                    upLCS.editPlanRow(1,'Tiles',"124");
                    upLCS.editPlanRow(1,'updateRowsProp',true);
                    upLCS.editPlanRow(1,'Nexposures',2);
                    upLCS.editPlanRow(1,'ExpTime',seconds(250));
                    upLCS.editPlanRow(10,'ExpTime',seconds(250));
                    upLCS.editPlanRow(5,'ExpTime',seconds(250));
                    
                    upLCS.delPlanRow(10);
                    upLCS.delPlanRow(3);
                    upLCS.delPlanRow(1)
                    
                    %upLCS.delUniqTarg(1);
                    upLCS.delUniqTarg(5,'abort_if_in_plan',false);
                    
                    if Args.Verbose
                        fprintf('completed\n');
                    end                    
                end
                %
                if ismember('too',lower(Args.Parts))                    
                    if Args.Verbose
                        fprintf('Start ToO plan...\n');
                    end                    
                    % a simple example for ToO plan:
                    if Args.Verbose
                        fprintf('a minimal example: ');
                    end  
                    HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');                   
                    upTOO = ultrasat.planner.uplanner('AstPlanner','YS','Type','TOO');
                    upTOO.buildTOO('RA',HCS_fields.RA,'Dec',HCS_fields.Dec,'Name',HCS_fields.Name);
                    if Args.Verbose
                        fprintf('%d exposures scheduled\n',height(upTOO.Plan));
                        fprintf('completed\n');
                        fprintf('-------------------------\n');
                    end                    
                    
                    % a ToO plan from an input probability map:                                        
                    upTOO1 = ultrasat.planner.uplanner('AstPlanner','AK','Type','TOO');
                    upTOO1.TOOMaxTargets     = 4;
                    upTOO1.TOOMinCoveredProb = 0.3;
                    upTOO1.TOOWindowDuration = hours(3);  
                    upTOO1.TOOAlertProbMap   = readtable('~/matlab/data/ULTRASAT/lvc_2024_04_01_00_40_58_000000.csv');
                    if Args.Verbose
                        fprintf('a ToO plan from an external probability map:\n');
                        fprintf('Maximal number of exposures: %d\n',upTOO1.TOOMaxTargets);
                        fprintf('Minimal probability to be covered: %.2f\n',upTOO1.TOOMinCoveredProb);
                    end  
                    upTOO1.buildTOO('Verbosity',0,'DrawMaps',0);            
                                fprintf('%d exposures scheduled\n',height(upTOO1.Plan));
                                fprintf('-------------------------\n');

                    upTOO2 = ultrasat.planner.uplanner('AstPlanner','AK','Type','TOO');
                    upTOO2.TOOMaxTargets     = 100;
                    upTOO2.TOOMinCoveredProb = 0.9;
                    upTOO2.TOOWindowDuration = hours(5);
                    upTOO2.TOOAlertProbMap   = readtable('~/matlab/data/ULTRASAT/lvc_2024_04_01_00_40_58_000000.csv');                  
                    if Args.Verbose
                        fprintf('a ToO plan from an external probability map:\n');
                        fprintf('Maximal number of exposures: %d\n',upTOO2.TOOMaxTargets);
                        fprintf('Minimal probability to be covered: %.2f\n',upTOO2.TOOMinCoveredProb);
                    end                      
                    upTOO2.buildTOO('Verbosity',0,'DrawMaps',0);    
                                fprintf('%d exposures scheduled\n',height(upTOO2.Plan));
                                fprintf('-------------------------\n');
                end
                %
                if ismember('ddt',lower(Args.Parts))
                    if Args.Verbose
                        fprintf('Start DDT plan...');
                    end                    
                    % Example DDT plan (very basic):
                    HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');                   
                    upDDT = ultrasat.planner.uplanner('AstPlanner','YS','Type','DDT');
                    upDDT.addUniqTargets(HCS_fields.RA,HCS_fields.Dec,'Name',num2cell(HCS_fields.Name));
                    upDDT.addDDT2Plan([1,2],datetime('now','TimeZone','UTC'));
                    upDDT.addDDT2Plan([3,2],datetime('tomorrow','TimeZone','UTC'));
                    
                    if Args.Verbose
                        fprintf('completed\n');
                    end                    
                end
                %
                if ismember('allss',lower(Args.Parts))
                    if Args.Verbose
                        fprintf('Start AllSS plan...\n');
                    end
                    
                    % Example for AllSS plan:
                    DitherLeg = 3.0;
%                     upAllSS = ultrasat.planner.uplanner('AstPlanner','YS','Type','AllSS','ExtragalDitherLeg',DitherLeg,...
%                         'Save','~/alss_uniq_targ.mat'); % first time we need to build the AllSS target list and save it
                    upAllSS = ultrasat.planner.uplanner('AstPlanner','YS','Type','AllSS','ExtragalDitherLeg',DitherLeg,...
                        'Load','~/matlab/data/ULTRASAT/alss_uniq_targ.mat');
                    
                    upAllSS.StartTime = '2028-07-01'; 
                    upAllSS.StartTime = upAllSS.StartTime + hours(12);  % 12 hr are added in order to alleviate visibility constraints 
                    upAllSS.EndTime   = upAllSS.StartTime + calmonths(6) - days(1);        
                    
                    upAllSS.ExtragalMinIntervals   = [1 3 9]; % [1 2 4] [1 3 9]
                    upAllSS.BufferEarthDist        = 0.5;
                    upAllSS.BufferSunDist          = 0.5;
                    upAllSS.BufferMoonDist         = 0.5;
                    upAllSS.DailyWindowMaxDuration = hours(5.5);
                    
                    upAllSS.EmptyDay               = false;
                    
                    %%%%
                    
                    upAllSS.EndTime                = upAllSS.StartTime + days(7);
                    upAllSS.DailyWindowMaxDuration = hours(24);
                    upAllSS.BufferEarthDist        = 3.0;
                    upAllSS.ExtragalMinIntervals   = [0 0 0];
                    upAllSS.BufferEarthDist        = 8.0; % 6.0;
%                     % currently distributeAllSS cannot work with reduced
%                     % number of extragalactic visits, need to be improved 
% %                     upAllSS.HighLatVisits  = 1;    % only 1 (or 2?) extragal points for the first week?             
                    
                    upAllSS.buildAllSS('AllowPartial',true,'Verbose',true,...                                                                              
                                       'MergeSameTargets',false,'AverageSlew',60);
                    % TODO: make a 2-stage plan: 1 dedicated week + all the
                    % rest in the rest 180-7 days in 5.5 hr windows (along with the HCS) 
                    % note the "Incomplete" variable in buildAllSS
                    
                    upAllSS.StartTime = upAllSS.EndTime;
                    upAllSS.EndTime   = upAllSS.StartTime + calmonths(6) - days(8);
                    upAllSS.DailyWindowMaxDuration = hours(5.5);
                    upAllSS.ExtragalMinIntervals   = [1 3 9];
                    upAllSS.buildAllSS('AllowPartial',true,'Verbose',true,...                                                                              
                                       'MergeSameTargets',false,'AverageSlew',60);

                    if Args.Verbose
                        fprintf('completed\n');
                        fprintf('-------------------------\n');
                    end
                end
                %
                Result=true;
                if Args.Verbose
                    fprintf('Unit Test completed succefully\n');
                end
            end
    end
end
