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
%                                    Allows to select a target from UniqTarg 
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
% - Obj.validate(Args)                                      : TODO - send plan to the validator. In addition, set Obj.Validated true/false, and Obj.ValidatedTime to 'now'
% - Obj.clearValidationData                                 : Clears valiation data from Plan table, delete the ValidationTime and ValidationResponse and change status back to draft
% - Obj.submit(Args)                                        : TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.SubmittedTime to 'now'
%
% - planStruct = planTable2struct(Obj,Args)                 : Return a struct array of a conversion of the Obj.Plan table, in the correct naming and format for validation/submission
%
% - Res = Obj.getCalibObj(UniqTargInd)                      : Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
%
% - h = Obj.plotCalibSpectrum(Res,Args)                     : Plot the spectra returned by getCalibObj()
%
% - Obj.plotVisibility(UniqTargInd,Args)                    : Plot the visibilty of a UniqTarg
%
% - Obj.plotMapPlan(Args)                                   : plotting on a map relevant properties and info from the plan
%                                                                     TODO - Change to map projection later
% - CheckTimes = getDefaultCheckTimes()                     : Get the default Check times.  TODO - Need to update
%
% - Result = Obj.isEditable()                               : Return true if the plan is editable (Status is 'draft' and Editable is true)
% - Res = Obj.getExtSurveysForTarget(UniqTargInd)           : Return external surveys table for a given unique target index
% - Res = Obj.getFieldObjForTarget(UniqTargInd, FieldName)  : Return table of field objects for a given unique target and field name. FieldName: char or string, e.g. 'Blazars', 'Clusters', 'Small', ...
% - Obj.enforceUniqueNames()                                : Enforce unique names in the UniqTarg table, add _n suffix to duplicate names
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

    % ========================== PUBLIC PROPERTIES ==========================
    properties(Access = public)

        % ------------ General / Common Properties ------------
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
        LastApprovedTargetsWindowStart          % Start time of the last approved targets window
        LastApprovedTargetsWindowEnd            % End time of the last approved targets window

        DefEpochsPerVisit               = 3; 
        Exptime             duration    = seconds(300);      %[s]
        Tiles               string      = ['1','2','3','4']; %
        DefSlewBuffer       duration    = seconds(5);
        FullTileReadTime    duration    = seconds(15); % Time from start read of first row to last. This time will be added to each row in plan (before slew to next target..
        
        % ------------ LCS / AllSS Properties ------------
        DailyWindowStartTime    duration =  duration(00,00,00); % [hrs]   
        DailyWindowMaxDuration  duration =  hours(3);           % [hrs]

        LCS_obj         ultrasat.planner.LcsHelper       % Object of class LCSHelper
        
        % ------------ AllSS Properties ------------
        AllSSgridFile                   = 'AllSS_grid_361.txt'; % the default AllSS grid
        PointTypeCriterion              = 'b';              % 'b' -- by the Galactic latitute or 'a_u' -- by the A_U (ULTRASAT band extinction) 
        AllSSHighLatThresh              = 30;               % |b| [deg]
        HighLatVisits                   =  4;               % 1 visit = 3 x 300 s 
        LowLatVisits                    =  2;               %
        DitherPattern                   = '2x2';            % not used as of yet
        DitherLeg                       = 0;                % [deg] dither leg size
        ExtragalMinIntervals            = [0 0 0];          % minimal intervals in days between extragalactic visits
        DailySlots                                          % number of slots in a day
        MaxDailyVisits                                      % maximal allowed number of daily visits (determined from DailyWindowMaxDuration) 
        EmptyDay                        = false;            % 1 empty day in a week (visibility set to 0 for all slots)
        BufferEarthDist                 = 0;                % buffer distances for visibility predictions
        BufferSunDist                   = 0;
        BufferMoonDist                  = 0;
        SchedStatus                                         % a table of AllSS points with the scheduling status marked 
        
        % ------------ TOO Properties ------------
        TOOStartTime       datetime     =  datetime('now'); % [hrs]   
        TOOWindowDuration  duration     =  hours(3);        % [hrs]
        TOOMaxTargets                   =  4;               % maximal number of target fields
        TOOMinAddedProb                 =  0.05;            % minimal covered probability difference between N and N+1 targets employed
        TOOMinCoveredProb               =  0.9;             % minimal covered probability
        TOOAlertProbMap                                     % input probability map        
        TOOUsedTargets                                      % the number of actually employed targets
        TOOCoveredProb                                      % actually covered probability (all targets)
        TOOCoveredByTarget                                  % actually covered probability (vector: per target)
        
        % ------------ General / Common Properties ------------
        N_uniqueTargets                 =  0;               % number of unique targets
        N_planTargets                   =  0;               % number of targets in the plan
        
        Rfov                            =  10;              % [deg] FOV radius conservative, w/o roll information
        
        BaseDataDir                                         % Base directory for data needed for uplanner
        
        CalibObj                        = [];               % table of calibration objects 
        CalibDir                                            % the catibration objects' spectra directory 

        ExtSurveysTable                 = [];               % table
        FieldObjects                    = [];               % struct

        RetrivedMissionTime     datetime                    % date or empty        
        ScheduledTime           datetime                    % date or empty
        ValidatedTime           datetime                    % date or empty
        SubmittedTime           datetime                    % date or empty
        Status                  char        = 'draft';      % 'draft', 'submitted'
        Editable                logical     = true;         %
        Validated               logical     = false;        % Validation result
        ValidationResponse      struct      % sturct containing the latest response from validator (corresponding to  ValidatedTime)
        
        AstPlanner              char                        % name of the Astronomer-Planner
        Mclient                                             % API client - MissionClient / MissionClientSim
    end

    % ========================== PRIVATE PROPERTIES ==========================
    properties(Hidden, Constant)
        Plan_AllowedTypes  = {'HCS', 'LCS', 'AllSS', 'DDT', 'TOO'};
        
        SysTimeZone        = 'UTC';

        % ------------ Plan Targets Table ------------
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
                                               
        % ------------ Unique Targets Table ------------
        Target_DefVarNames = {'Name', 'RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj', 'HealpixArray','DitherGroup'};
        Target_DefVarTypes = {'string', 'double', 'double', 'double', 'cell', 'cell', 'cell', 'cell', 'cell', 'double'};  
        
        % ------------ Approved Targets Table ------------
        MissionApprovedPlan_VarNames   = {'Name','pk','TargetID','RA', 'Dec','Roll',...
                              'Tstart','Tend','ExpTime','Nexposures','TotalDuration'};
        MissionApprovedPlan_VarTypes   = {'string','uint64','char','double','double','double',...
                              'datetime','datetime','duration','double','duration'};        
        
        % ------------ Observation ------------
        ObsSunDist           = 70;      % [deg]
        ObsMoonDist          = 34;      % [deg]
        ObsEarthDist         = 56;      % [deg]        

        % ------------ Slew ------------
        SlewSunDist          = 70;      % [deg]
        SlewMoonDist         = 19.5;    % [deg]
        SlewEarthDist        = 19.5;    % [deg]                
    end 

    % ========================== CONSTRUCTOR ==========================
    methods  % Constructor
        function Obj = uplanner(Args)
            % object constructor
            % example: up = ultrasat.planner.uplanner('AstPlanner','YS');
            arguments                
                Args.Type        = '';   % plan type: HCS, LCS, AllSS, DDT, TOO  
                Args.AstPlanner  = '';
                Args.StartTime   datetime   = NaT;   % start of the whole plan
                Args.EndTime     datetime   = NaT;   %   end of the whole plan
               
                Args.BaseDataDir = '~/matlab/data/ULTRASAT/'; % Base directory for data needed for uplanner
                Args.CalObjFile  = 'starlib23_table.mat';     % the calibration objects' list (within  BaseDataDir)
                Args.CalSubDir   = 'Calib/';                  % the catibration objects' spectra directory (within  BaseDataDir)
                
                Args.ExtSurveyMapsFile = 'ExtSurveyMaps.mat'; %'~/matlab/data/ULTRASAT/ExtSurveyMaps.mat';
                Args.FieldObjectsFile  = 'FieldObjects.mat';  %'~/matlab/data/ULTRASAT/FieldObjects.mat';

                Args.AllSSgridFile = [];                      % an alternative AllSS grid (the default is in the properties)
                Args.ExtragalDitherLeg = [];                  % an alternative dither leg size for the AllSS grid
                Args.Save          = [];
                Args.Load          = [];            
            end

            % Windows
            if ispc
                Args.BaseDataDir = fullfile(getenv('ASTROPACK_DATA_PATH'), 'ULTRASAT');
            end

            % If the AstPlanner is not set, error
            if isempty(Args.AstPlanner) 
                error('Planner Name is missing');
            else
                Obj.AstPlanner = Args.AstPlanner;  
            end

            % Set plan type if provided
            if ~isempty(Args.Type)
                Obj.Type = Args.Type;               
            end

            % Set start and end times if provided
            if ~isnat(Args.StartTime)
                Obj.StartTime =Args.StartTime;
            end            
            if ~isnat(Args.EndTime)
                Obj.EndTime =Args.EndTime;
            end

            % Set check times if not provided
            Obj.CheckTimes = Obj.getDefaultCheckTimes();
            Obj.CheckTimes.TimeZone = Obj.SysTimeZone;

            % Create the Plan table with the default variables
            Obj.Plan = table('Size',[Obj.N_planTargets,numel(Obj.Plan_DefVarNames)],'VariableNames', Obj.Plan_DefVarNames,...
                                'VariableTypes',Obj.Plan_DefVarTypes);
                            
            % Set the timezone
            Obj.Plan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.Plan.Tend.TimeZone = Obj.SysTimeZone;
            Obj.Plan.Tend_ValidationEstimate.TimeZone = Obj.SysTimeZone;

            % Create the Unique Targets table with the default variables
            Obj.UniqTarg = table('Size',[Obj.N_uniqueTargets,numel(Obj.Target_DefVarNames)],'VariableNames', Obj.Target_DefVarNames,...
                                'VariableTypes',Obj.Target_DefVarTypes); 

            % Create the Mission Approved Plan table with the default variables
            Obj.MissionApprovedPlan = table('Size',[0,numel(Obj.MissionApprovedPlan_VarNames)],'VariableNames', Obj.MissionApprovedPlan_VarNames,...
                                'VariableTypes',Obj.MissionApprovedPlan_VarTypes);           
                            
            % Set the timezone
            Obj.MissionApprovedPlan.Tstart.TimeZone = Obj.SysTimeZone;
            Obj.MissionApprovedPlan.Tend.TimeZone = Obj.SysTimeZone;                            

            % Set folder paths            
            Obj.BaseDataDir = Args.BaseDataDir;
            Obj.CalibDir = fullfile(Obj.BaseDataDir, Args.CalSubDir);
            
            % ---------- Load ----------
            % Load the calibration objects' table
            load(fullfile(Obj.BaseDataDir, Args.CalObjFile)); 
            Obj.CalibObj = CalibObj;
            
            % Load the lists of external important objects and survey maps
            load(fullfile(Obj.BaseDataDir, Args.ExtSurveyMapsFile)); % 'SurveyMaps' table
            Obj.ExtSurveysTable = SurveyMaps;

            % Load the lists of field objects and store them in a struct Obj.FieldObjects
            load(fullfile(Obj.BaseDataDir, Args.FieldObjectsFile));  % 'Known_Obj_large', 'Known_Obj_small' tables
            Obj.FieldObjects.TransPlanets = Known_Obj_large.WG3_det_trans_planets;
            Obj.FieldObjects.MassiveStars = Known_Obj_large.WG5_Massive_Stars;
            Obj.FieldObjects.Clusters = Known_Obj_large.WG5_AllClusters;
            Obj.FieldObjects.Blazars = Known_Obj_large.WG7_Blazars;
            Obj.FieldObjects.Small = Known_Obj_small;
            
            % ---------- AllSS ----------
            % If the Type is AllSS, construct the AllSS grid and set DitherLeg if provided
            if strcmpi(Obj.Type,'AllSS') 
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
            % Construction of the AllSS grid 
            arguments
                Obj
                Args.Verbosity = 1;     
                Args.Save      = [];
                Args.Load      = [];
            end

            % Load the grid file
            if isempty(Args.Load)
                % read the main grid file
                Grid = readtable(fullfile(Obj.BaseDataDir,Obj.AllSSgridFile));
                
                % Determine the two types of sky points (extragalactic and galactic)
                RAD = 180/pi;
                if strcmpi(Obj.PointTypeCriterion,'b')       % Distinction according to the Galactic latitude
                    [~, Grid.b] = celestial.coo.convert_coo(Grid.RA./RAD,Grid.Dec./RAD,'j2000.0','g');
                    Extragal = abs(Grid.b.*RAD) > Obj.AllSSHighLatThresh;
                elseif strcmpi(Obj.PointTypeCriterion,'a_u') % Distinction according to the averaged A_U
                    Grid.A_U = ultrasat.tools.extinction(Grid.RA,Grid.Dec);
                    Extragal = Grid.A_U < 1;
                else
                    error('Unknown point type criterion');
                end

                % Print the progress if verbosity is set
                if Args.Verbosity > 0
                    fprintf('Adding unique targets...\n'); tic
                end

                % Dither the extragalactic points
                [DitheredGrid, DitherGroup] = ultrasat.tools.ditherGrid(Grid(Extragal,:),'Leg',Obj.DitherLeg,...
                    'Ngrid',4,'Pattern',Obj.DitherPattern);
                
                % Add the galactic points to the unique targets list
                Obj.addUniqTargets(Grid.RA(~Extragal),Grid.Dec(~Extragal),'Name',num2cell(Grid.id(~Extragal)),...
                    'UpdateVisibility',false);

                % Add the extragalactic points to the unique targets list
                Obj.addUniqTargets(DitheredGrid.RA,DitheredGrid.Dec,'Name',num2cell(DitheredGrid.id),...
                    'DitherGroup',DitherGroup,'UpdateVisibility',false);
                
                if Args.Verbosity > 0
                    fprintf('%d unique targets added in %.0f s \n',height(Obj.UniqTarg),toc);
                end

                % Fill the scheduled status table
                Obj.SchedStatus = table(Obj.UniqTarg.Name,Obj.UniqTarg.RA,Obj.UniqTarg.Dec,Obj.UniqTarg.DitherGroup,...
                    repmat(0,1,Obj.N_uniqueTargets)','VariableNames',{'Name','RA','Dec','DithGroup','Status'});

            % If a file is provided, load the unique targets and scheduled status from the file
            else
                load(Args.Load);
                Obj.UniqTarg    = UniqTarg;        
                Obj.SchedStatus = SchedStatus;     
                Obj.N_uniqueTargets = height(Obj.UniqTarg);
            end

            % Save the unique target list grid in the file named Args.Save
            if ~isempty(Args.Save)
                UniqTarg    = Obj.UniqTarg;
                SchedStatus = Obj.SchedStatus;
                save(Args.Save,'UniqTarg', 'SchedStatus');
            end
        end
    end 

    % ========================== SETTERS/GETTERS ==========================
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

    % ========================== BUILD PLANS ==========================
    methods % Building the plans          
        %
        function buildHCS(Obj,Args)
            % Build a plan for a HCS field, using a single selected UniqTarget 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/ height(Obj.UniqTarg) >=1)
            arguments
                Obj
                Args.HCS_UniqTarg = 1; % Default is the first line if not selected
                Args.ClearPlanIfExist = true;
            end               
            
            % Verify that all relevant parameters are set and valid
            
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
                error('HCS requires a unique target');
            end            
            if numel(Args.HCS_UniqTarg) ~=1
                error('HCS requires one single target');
            end

            % Clean Plan if exists and requested
            if Args.ClearPlanIfExist && ~isempty(Obj.Plan)
                Obj.clearPlan;
            end

            % Calc number of exposures within the plan time 
            Nexposures = floor((Obj.EndTime-Obj.StartTime)/Obj.Exptime);
            
            % Schedule HCS field
            Obj.scheduleTargets(Args.HCS_UniqTarg,Obj.StartTime,'Nexp',Nexposures);
        end

        %
        function buildLCS1(Obj,Args)
            % Build a plan for a Targetlist of LCS fields. If a list is not provided, uses all targets in the unique target list.
            % Fill in a daily window of observations and move to the next day. 
            % All relevant parameters should be set before calling this function
            % (StartTime/EndTime/Exptime/Tiles/DefEpochsPerVisit/DailyWindowStartTime/DailyWindowMaxDuration/ height(Obj.UniqTarg)>0)
            arguments
                Obj
                Args.TargetList = [];
            end

            % Verify that all the relevant parameters are set and valid
            
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

             % Calculate the current start time
            CurrStartTime = dateshift(Obj.StartTime,'start','day');
            if CurrStartTime < Obj.StartTime
                CurrStartTime = CurrStartTime+1;
            end
            Obj.StartTime = CurrStartTime;
                
            Obj.LCS_obj = ultrasat.planner.LcsHelper('AllSkyTable',Obj.UniqTarg(Args.TargetList,:),...
                                                                              'StartDate',Obj.StartTime,'EndDate',Obj.EndTime,...
                                                                              'DailyWindowStartTime',Obj.DailyWindowStartTime,...
                                                                             'prep_before_schedule',true,'build_the_schedule',true);

            
            DailySchedule = Obj.LCS_obj.Daily_schedule;

            Days = find(~all(isnan(DailySchedule),2))-1;

            for CurrGroup = 1:numel(Days)
                CurrStartTime = Obj.LCS_obj.StartDate + Obj.LCS_obj.DailyWindowStartTime + Days(CurrGroup);
                DailyTargets = DailySchedule(CurrGroup,~isnan(DailySchedule(CurrGroup,:)));

                % TODO - currently naive ordering, should refine
                Dec = Obj.UniqTarg.Dec(DailyTargets);
                [~,I] = sort(Dec,'ascend');
                DailyTargets = DailyTargets(I);
                
                Obj.scheduleTargets(Args.TargetList(DailyTargets),CurrStartTime,'Group',CurrGroup);
            end
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
           
            %------------------------------------------------------
            % @Chen for Yossi: EXAMPLE for using the helper for LCS modifications
            % This code just print one line - 
            %Helper = ultrasat.planner.LcsHelper(Obj);
            %Helper.buildLcs();
            %------------------------------------------------------

            % Verify that all the relevant parameters are set and valid
            
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
                
            % Calculate the expected number of targets fit in a single window
            NUtarg = numel(Args.TargetList);

            % Calculate the maximum number of targets per window
            MaxTargPerWindow = floor(Obj.DailyWindowMaxDuration / (double(Obj.DefEpochsPerVisit) * Obj.Exptime + Obj.DefSlewBuffer + Obj.FullTileReadTime + seconds(100))); % last argument is conservative slew time
             
            % Calculate the current start time
            CurrStartTime = dateshift(Obj.StartTime,'start','day') + Obj.DailyWindowStartTime;
            if CurrStartTime < Obj.StartTime
                CurrStartTime = CurrStartTime+1;
            end
            Obj.StartTime = CurrStartTime;
            
            % Use the end time of the plan
            MaxEndTime = Obj.EndTime;
            
            % Initialize the current group and first target index
            CurrGroup = 1;
            CurrFirstTargetInd = 1;
            
            % Loop over the targets within the window
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

                % @Chen - for TooPlannerRunner (28/01/2026)
                Args.SaveMaps      = false;     % save plots to files
                Args.MapOutputDir  = '';        % folder to save plots
                Args.MapBaseName   = 'too';     % prefix for files
                Args.MapFormats    = {'png','fig'}; % {'png','jpg','fig'}
                Args.CloseFigures  = true;      % close figures after saving                        
            end
            
            % Verify that all relevant parameters are set and valid

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
                        
            % Set the times
            Obj.StartTime  = Obj.TOOStartTime;
            Obj.EndTime    = Obj.TOOStartTime + Obj.TOOWindowDuration;
            Obj.CheckTimes = [Obj.StartTime, Obj.EndTime];
            
            % If a map is provided, cover the probability map
            if ~isempty(Args.Map)

                [RA, Dec, Stat] = ultrasat.tools.coverProbMap(Args.Map,...
                    'MaxTarg',Obj.TOOMaxTargets,'MinProb',Obj.TOOMinCoveredProb,'MinAddedProb',Obj.TOOMinAddedProb,...
                    'Verbosity',Args.Verbosity,'DrawMaps',Args.DrawMaps, ...
                    'SaveMaps',  Args.SaveMaps, ...
                    'MapOutputDir', Args.MapOutputDir, ...
                    'MapBaseName', Args.MapBaseName, ...
                    'MapFormats', Args.MapFormats, ...
                    'CloseFigures', Args.CloseFigures);        

                Names = num2cell(1:numel(RA)); % may add "TOOfield.." to the name? 
                Obj.addUniqTargets(RA, Dec,'Name',Names); 
                
                Obj.TOOUsedTargets = Stat.Ntarg; 
                Obj.TOOCoveredProb = Stat.CoveredProb;
                Obj.TOOCoveredByTarget = Stat.IndividualCoveredProb;

            % If RA and Dec are provided, add the targets to the unique target list
            elseif ~isempty(Args.RA) && ~isempty(Args.Dec) && numel(Args.RA)==numel(Args.Dec)
                [RA, Dec] = deal(Args.RA, Args.Dec);
                Obj.addUniqTargets(RA, Dec,'Name',Args.Name);                
            else
                error('No TOO targets/map');  % No targets or map provided
            end
            
            % -------- Check visibility and shift the window if needed --------

            %if ~all(Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits ,1)
            if ~all(Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits,'all')
                fprintf('Visibility issue: immediate observation is not possible\n');     

                % Scan 6 months ahead and find the first occurence of an Obj.TOOWindowDuration window:
                Obj.CheckTimes = [Obj.StartTime, Obj.StartTime + calmonths(6)]; 
                Obj.updateTargetVisibility('TimeBin',Args.TimeBin);
                Nbins  = ceil(Obj.TOOWindowDuration/days(Args.TimeBin)); 
                Limits = Obj.Vis.SunLimits & Obj.Vis.EarthLimits & Obj.Vis.MoonLimits;

                %CombinedLimits = prod(Limits,2);
                % find a period of Obj.TOOWindowDuration length where CombinedLimits is 1:                
                % Ind   = tools.find.findGroupOfConsecutiveVals(CombinedLimits, 1, Nbins, 1);

                % Find the first occurence of an Obj.TOOWindowDuration window for each target
                for i=1:Obj.TOOUsedTargets
                    Ind(i,:)   = tools.find.findGroupOfConsecutiveVals(Limits(:,i), 1, Nbins, 1);
                end

                % If there is a valid window, set the start and end times
                if ~isempty(Ind)                    
                    % Obj.StartTime  = datetime(Obj.Vis.JD(Ind(1)),'ConvertFrom','juliandate','TimeZone','UTC');
                    % Obj.EndTime    = datetime(Obj.Vis.JD(Ind(end)),'ConvertFrom','juliandate','TimeZone','UTC');                    

                    StartSlot = min(Ind,[],'all');    % find the earliest slot for 1 target
                    FirstTarg = find(Ind==StartSlot); % and the target number
                    Obj.StartTime = datetime(Obj.Vis.JD(StartSlot),'ConvertFrom','juliandate','TimeZone','UTC');
                    Obj.EndTime   = datetime(Obj.Vis.JD(StartSlot+Nbins-1),'ConvertFrom','juliandate','TimeZone','UTC');

                    % Remove all the targets and add the nearest one only
                    Obj.delUniqTarg(1:Obj.TOOUsedTargets);
                    Obj.addUniqTargets(RA(FirstTarg), Dec(FirstTarg),'Name',Names(FirstTarg));  

                    fprintf('The nearest visibility window is found at %s\n',Obj.StartTime);                    
                    fprintf('for 1 target covering %.2f probability\n',Obj.TOOCoveredByTarget(FirstTarg));        
                else
                    error('No visibility window for the TOO can be found within the next 6 months');
                end
            end
            
            % -------- Schedule the targets --------
            % Calculate the number of targets
            NTargets = height(Obj.UniqTarg);

            % Calculate the maximum number of targets per window
            MaxTargInWindow = floor(Obj.TOOWindowDuration / (double(Obj.DefEpochsPerVisit) * Obj.Exptime + Obj.DefSlewBuffer + Obj.FullTileReadTime + seconds(100))); % last argument is conservative slew time
            
            % Schedule the targets
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
            
            % If no group is provided, set the group to the next available group
            if isempty(Args.Group)
                if isempty(Obj.Plan)
                    Args.Group = 1;
                else
                    Args.Group = max(Obj.Plan.Group)+1;
                end
            end
            
            % Schedule the targets
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

            %------------------------------------------------------
            % @Chen for Yossi: EXAMPLE for using the helper for AllSS modifications
            % This code just print one line - 
            Helper = ultrasat.planner.AllSSHelper(Obj);
            Helper.buildAllSS();
            %------------------------------------------------------

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

    % ---------------------- Auxiliary Functions -----------------------
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
            
            % If a file is provided, read the coordinates from the file
            if ~isempty(Args.File)

                % Load the file coordinates into a table
                cooFile = readtable(Args.File);

                % Find the columns for RA, Dec, and Name
                colRA = find(strcmp(cooFile.Properties.VariableNames,'RA'));
                colDec = find(strcmp(cooFile.Properties.VariableNames,'Dec'));
                colName = find(strcmp(cooFile.Properties.VariableNames,'Name'));

                % Get the number of columns in the file
                Ncol = numel(cooFile.Properties.VariableNames);
                
                % If RA/Dec headers not found, use default file structure: "Name, RA, Dec" or "RA, Dec"
                if isempty(colRA) || isempty(colDec)
                    if Ncol == 3       % Name, RA, Dec
                        colName = 1;
                        colRA = 2;
                        colDec =3;
                    else               % RA, Dec
                        colRA = 1;
                        colDec =2;
                    end                    
                end

                % Extract the RA and Dec from the file data, Name is optional
                RA  = table2array(cooFile(:,colRA)); 
                Dec = table2array(cooFile(:,colDec));
                if ~isempty(colName)
                    Args.Name = string(table2array(cooFile(:,colName)));
                end
            end

            % Calculate the number of unique targets to be added
            NUtarg = numel(RA);
            NU0    = height(Obj.UniqTarg);

            % Set the RA and Dec of the unique target
            Obj.UniqTarg.RA( NU0+1:NU0+NUtarg) =  RA; 
            Obj.UniqTarg.Dec(NU0+1:NU0+NUtarg) = Dec;

            % Set Name if provided (for single target addition)
            if ~isempty(Args.Name)
                Obj.UniqTarg.Name(NU0+1:NU0+NUtarg) = Args.Name;
            end

            % Enforce unique names in the UniqTarg table, renaming duplicates with suffix _n
            Obj.enforceUniqueNames();

            % Set DitherGroup
            if ~isempty(Args.DitherGroup)
                Obj.UniqTarg.DitherGroup(NU0+1:NU0+NUtarg) = Args.DitherGroup;
            end

            % Update the number of unique targets
            Obj.N_uniqueTargets = height(Obj.UniqTarg);

            % Update the target properties
            Obj.updateTargetProperties('TargList',NU0+1:NU0+NUtarg);

            % Update the visibility
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
            
            % If the coordinates changed, update the target properties and visibility
            if CooChanged
                Obj.updateTargetProperties('TargList',UniqTargInd);
                Obj.updateTargetVisibility();
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
            
            % If the target is in the plan, abort the deletion unless explicitly asked to delete anyway
            if Args.abort_if_in_plan && ~isempty(Plan_rows) 
                error('UniqTargInd is in Plan - aborting deletion');
            end

            % Delete the target from the unique target list
            Obj.UniqTarg(UniqTargInd,:) = [];

            % Get the list of groups that the target is part of
            Glist = unique(Obj.Plan.Group(Plan_rows));
            
            % Delete the target from the plan
            Obj.Plan(Plan_rows,:)=[];

            % Edit the groups that the target is part of
            for ii = 1:numel(Glist)
                % edit the group
                G = find(Obj.Plan.Group==Glist(ii),1); % find first group member, if any...
                if ~isempty(G)
                    Obj.editPlanRow(G,'updateRowsProp',true);
                end
            end               
            
            % Update the UniqTargInd of the remaining targets
            Obj.Plan.UniqTargInd(Obj.Plan.UniqTargInd>UniqTargInd) = Obj.Plan.UniqTargInd(Obj.Plan.UniqTargInd>UniqTargInd)-1;
            
            % Update the number of unique targets
            Obj.N_uniqueTargets = height(Obj.UniqTarg);
            
            % Update the visibility of the remaining targets
            % TODO - consider removing specific UniqTarg                
            Obj.updateTargetVisibility();                
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
        function scheduleTargets(Obj, UniqTargetIndexes, StartTime,Args)
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

            % Set default values for Nexp, Exptime, and Tiles if not provided
            if isempty(Args.Nexp)
                Args.Nexp = Obj.DefEpochsPerVisit;
            end
            if isempty(Args.Exptime)
                Args.Exptime = Obj.Exptime;
            end
            if isempty(Args.Tiles)
                Args.Tiles = Obj.Tiles;
            end
            
            % Calculate the number of targets to schedule
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
            
            % ------------------------------------------
            % @Chen (05/10/2025)
            % Avoid Warning: "The assignment added rows to the table, but did not assign values to all of the table's existing variables. Those variables are extended with rows containing default values".
            % The fix is to first extend Obj.Plan to the required height, then fill it
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
            % ------------------------------------------

            % Add plan rows one by one
            for ii = 1:NUtarg
            
                Plan_row = NProws+ii;

                % Get the unique target index for the current target
                curr_UniqTargInd = UniqTargetIndexes(ii);
                
                % Set the plan row properties
                Obj.Plan.Name(Plan_row) = Obj.UniqTarg.Name(curr_UniqTargInd);
                Obj.Plan.UniqTargInd(Plan_row) = curr_UniqTargInd;
                Obj.Plan.RA(Plan_row)  = Obj.UniqTarg.RA(curr_UniqTargInd); 
                Obj.Plan.Dec(Plan_row) = Obj.UniqTarg.Dec(curr_UniqTargInd); 
                Obj.Plan.ExpTime(Plan_row) = Args.Exptime(ii);
                Obj.Plan.Tiles(Plan_row)   = Args.Tiles(ii);
                Obj.Plan.Nexposures(Plan_row) = Args.Nexp(ii);

                % Set the start time for the first target
                if ii == 1
                    Obj.Plan.Tstart(Plan_row) = StartTime;
                    Obj.updatePlanRowProperties(Plan_row);
                else % for subsequent targets, calculate the start time from the previous target
                    Obj.updatePlanRowProperties(Plan_row,'CalcStartTimeFromPrevTarget',true);
                end 
            end
            
            % Set the group for the new targets
            Obj.Plan.Group((NProws+1):(NProws+NUtarg)) = Args.Group;
            
            % Update the number of targets in the plan
            Obj.N_planTargets = height(Obj.Plan);
            
            % Update the start and end time of the plan
            Obj.StartTime = min(Obj.Plan.Tstart);
            Obj.EndTime = max(Obj.Plan.Tend);
            
            % Change status to scheduled and timestamp the schedule
            Obj.setScheduledStatus();
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
            
            % Use the provided arguments if provided
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
            
            % Update the properties of the row and the group if needed
            if updateRowsProp
                % Extract the group of the row
                G = Obj.Plan.Group(Plan_row);
                
                % If the row is not part of a group, update the properties of the row
                if G == -1
                    Obj.updatePlanRowProperties(Plan_row);
                else % If the row is part of a group, update the properties of the group

                    % Extract the list of rows in the group
                    Glist = find(Obj.Plan.Group==G);
                    if Plan_row == Glist(1) % first in the group
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
            
            % If the next plan row is part of the same group, update the properties of the group
            if Obj.Plan.Group(Plan_row)==G && G~=-1 
                Obj.editPlanRow(Plan_row,'updateRowsProp',true);
            end
             
        end
        %
        function clearPlan(Obj)
            % Clear the plan
            
            % Remove the plan
            Obj.Plan(:,:) = [];
            % Clean the number of unique targets
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
            
            % For now, allow to get a uPlan and use it as reference
            if isa(Args.inputPlan,'table')
                Obj.clearMissionApprovedPlan();
                
                % Set the mission approved plan properties
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

                % Set the last approved targets window start and end times
                Obj.LastApprovedTargetsWindowStart = Args.WindowStartTime;
                Obj.LastApprovedTargetsWindowEnd   = Args.WindowEndTime;

                % Get the approved targets from the backend
                structPlan = Obj.Mclient.getApprovedTargets(Args.WindowStartTime, Args.WindowEndTime);
            end
                       
            Obj.clearMissionApprovedPlan;
            Obj.RetrivedMissionTime = datetime('now','TimeZone', Obj.SysTimeZone);            

            if isempty(structPlan.targets)
                return;
            end

            % Convert the struct to a table
            TargetsTable = struct2table(structPlan.targets, 'AsArray', true);            
            
            % Set the mission approved plan properties
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
        function [CheckStatus,badPlanRow,badPlanRowIndex,Message] = planSelfConsistencyCheck(Obj,Args)
            % Verify that the plan schedule is self consistent
            arguments
                Obj
                Args.timingPrecision = seconds(0.01);
            end
            
            % Initialize outputs
            CheckStatus = false;
            badPlanRow = [];
            badPlanRowIndex = 0;
            Message = '';

            % Plan is empty, nothing to check
            if isempty(Obj.Plan)                
                return
            end

            tmpPlan = Obj.Plan;            
            tmpPlan = sortrows(tmpPlan,'Tstart');
            
            % Validate that Obj.Start time and the first start time in the plan agree
            if abs(Obj.StartTime-tmpPlan.Tstart(1))>Args.timingPrecision
                Message = 'Bad Start Time of Entire Object';
                fprintf('%s\n', Message);
                CheckStatus = false;
                badPlanRow = tmpPlan(1,:);
                badPlanRowIndex = 1;
                return
            end
            
            % Loop over the plan rows and validate the timing
            for Plan_row = 1:height(tmpPlan)
                % Calculate and validate times between targets
                if Plan_row > 1
                    
                    [T_sec,~] = ultrasat.tools.calcSlew(tmpPlan.RA(Plan_row-1),tmpPlan.Dec(Plan_row-1),tmpPlan.RA(Plan_row),tmpPlan.Dec(Plan_row),...
                                                        'Units','deg','CheckTrajectory',true);
                    tmpSlewTimeBefore = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;
                    tmpTstart = currTend + tmpSlewTimeBefore;                    
 
                    if (tmpPlan.Tstart(Plan_row)-tmpTstart)<-Args.timingPrecision
                        Message = 'Bad timing between rows';
                        fprintf('%s\n', Message);
                        CheckStatus = false;
                        badPlanRow = tmpPlan(Plan_row,:);
                        badPlanRowIndex = Plan_row;
                        return               
                    end                    
                end

                % Calculate and validate relative time within the plan row 
                tmpTotalDuration = tmpPlan.Nexposures(Plan_row) * tmpPlan.ExpTime(Plan_row) + Obj.FullTileReadTime;
                tmpTend = tmpPlan.Tstart(Plan_row) + tmpTotalDuration;
                
                % Calculate the Julian dates
                tmpJDstart = juliandate(tmpPlan.Tstart(Plan_row));
                tmpJDend = juliandate(tmpTend);
                
                % Validate the timing within the plan row
                if abs(tmpPlan.TotalDuration(Plan_row)-tmpTotalDuration)>Args.timingPrecision || ...
                   abs(tmpPlan.Tend(Plan_row)-tmpTend)>Args.timingPrecision || ...     
                   abs(tmpPlan.JDstart(Plan_row)-tmpJDstart)>seconds(Args.timingPrecision)/3600/24  || ...
                   abs(tmpPlan.JDend(Plan_row)-tmpJDend)>seconds(Args.timingPrecision)/3600/24

                    % If the timing is bad, set the bad plan row and return
                    Message = 'Bad timing within row';
                    fprintf('%s\n', Message);
                    CheckStatus = false;
                    badPlanRow = tmpPlan(Plan_row,:);
                    badPlanRowIndex = Plan_row;
                    return               
                end

                currTend = tmpTend;
            end
            
            % Validate that Obj.Start time and the first start time in the plan agree
            if abs(Obj.EndTime-currTend)>Args.timingPrecision
                Message = 'Bad End Time of Entire Object';
                fprintf('%s\n', Message);
                CheckStatus = false;
                badPlanRow = tmpPlan(end,:);
                badPlanRowIndex = height(tmpPlan);
                return
            end            
            
            CheckStatus = true;
            badPlanRow = [];
            badPlanRowIndex = 0;
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
            
            % If no group list is provided, apply to all groups
            if isempty(Args.GroupList)
                Args.GroupList = unique(Obj.Plan.Group);
            end
            
            % Loop over the groups
            for Gind= 1:numel(Args.GroupList)
                ShiftTime = Args.ShiftTime;
                
                % Get the plan rows for the current group
                Plan_rows = find(Obj.Plan.Group==Args.GroupList(Gind));

                % If a new start time is provided, calculate the shift time
                if ~isempty(Args.NewStartTime)
                    ShiftTime = Args.NewStartTime - Obj.Plan.Tstart(Plan_rows(1));
                end
                
                if isinf(ShiftTime)
                    % Calculate the shift based on the overlaptargets
                    ShiftTime = seconds(0); % in case it doesn't find any match - does not shift the time
                    
                    % Extract the list of overlap targets
                    OTlist = Obj.Plan.OverlapTargets{Plan_rows(1)};

                    % Loop over the overlap targets
                    for ii = 1:numel(OTlist)
                        CurrOTind = OTlist(ii);

                        % Check if starttime of entire group within curr overlap target window
                        if (Obj.Plan.Tstart(Plan_rows(1)) > Obj.MissionApprovedPlan.Tstart(CurrOTind) && Obj.Plan.Tstart(Plan_rows(1)) < Obj.MissionApprovedPlan.Tend(CurrOTind))
                            
                            % Calculate the slew time
                            [T_sec,~] = ultrasat.tools.calcSlew(Obj.MissionApprovedPlan.RA(CurrOTind),Obj.MissionApprovedPlan.Dec(CurrOTind),Obj.Plan.RA(Plan_rows(1)),Obj.Plan.Dec(Plan_rows(1)),...
                                                        'Units','deg','CheckTrajectory',true);
                            
                            SlewTime = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;
                            
                            % Extract the end time and exposure time of the overlap target
                            OT_Tend = Obj.MissionApprovedPlan.Tend(CurrOTind);
                            OT_ExpTime = Obj.MissionApprovedPlan.ExpTime(CurrOTind);
                            
                            % Calculate the close end time of the overlap target
                            OT_Tend_close = OT_Tend + round((Obj.Plan.Tstart(Plan_rows(1))-OT_Tend-SlewTime)./OT_ExpTime)*OT_ExpTime;
                            
                            % Calculate the shift time
                            ShiftTime = OT_Tend_close + SlewTime - Obj.Plan.Tstart(Plan_rows(1));
                            
                            % Set the slew time before the first row of the group
                            Obj.Plan.SlewTimeBefore(Plan_rows(1)) = SlewTime;
                        end
                    end
                end
                                
                % Apply the shift to the start and end times of the group
                Obj.Plan.Tstart(Plan_rows) = Obj.Plan.Tstart(Plan_rows) + ShiftTime;
                Obj.Plan.Tend(Plan_rows) = Obj.Plan.Tend(Plan_rows) + ShiftTime;
                
                Obj.Plan.JDstart(Plan_rows) = juliandate(Obj.Plan.Tstart(Plan_rows));
                Obj.Plan.JDend(Plan_rows) = juliandate(Obj.Plan.Tend(Plan_rows));
            end
            
            % Update the start and end times of the plan
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
            
            for ii = 1:numel(Args.TargList) % loop over targets 
                
                iT = Args.TargList(ii);                
                
                RA0 = Obj.UniqTarg.RA(iT); Dec0 = Obj.UniqTarg.Dec(iT);

                % Make a circular FOV region
                FOV = ultrasat.tools.getFOVcircle(RA0,Dec0,'Radius',Obj.Rfov,'Plot',0);  
                FOVp = polyshape(FOV);  % a polyshape is useful to test intersections
                
                % Select calibration objects 
                Ind = celestial.search.isPointInsidePolygon(Obj.CalibObj.RA, Obj.CalibObj.Dec, FOV);
                Obj.UniqTarg.CalObj{iT} = num2cell(find(Ind>0));
                
                % Select reference images
                %Ind = celestial.search.isPointInsidePolygon(Obj.RefIma.RA, Obj.RefIma.Dec,FOV); 
                %Obj.UniqTarg.RefImageIDs{iT} = num2cell(find(Ind>0));

                % select external surveys 
                Ind = overlaps(Obj.ExtSurveysTable.Shape, FOVp);
                Obj.UniqTarg.ExtSurveys{iT} = num2cell(find(Ind>0));
               
                % select specific objects falling into the FOV
                Ind = celestial.search.isPointInsidePolygon(Obj.FieldObjects.Small.RA, Obj.FieldObjects.Small.Dec, FOV);
                Field.Small = num2cell(find(Ind>0));

                % FieldObjects.TransPlanets
                Ind = celestial.search.isPointInsidePolygon(Obj.FieldObjects.TransPlanets.ra, Obj.FieldObjects.TransPlanets.dec,FOV);
                Field.TransPlanets = num2cell(find(Ind>0));

                % FieldObjects.MassiveStars
                Ind = celestial.search.isPointInsidePolygon(Obj.FieldObjects.MassiveStars.RA, Obj.FieldObjects.MassiveStars.Dec,FOV);
                Field.MassiveStars = num2cell(find(Ind>0));

                % FieldObjects.Clusters
                Ind = celestial.search.isPointInsidePolygon(Obj.FieldObjects.Clusters.RA, Obj.FieldObjects.Clusters.DEC,FOV);
                Field.Clusters = num2cell(find(Ind>0));                

                % FieldObjects.Blazars
                Ind = celestial.search.isPointInsidePolygon(Obj.FieldObjects.Blazars.RA, Obj.FieldObjects.Blazars.Dec,FOV);
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
            
            % Calculate the total duration of the plan row
            Obj.Plan.TotalDuration(Plan_row) = Obj.Plan.Nexposures(Plan_row) * Obj.Plan.ExpTime(Plan_row) + Obj.FullTileReadTime; 

            % Optionally calculate the slew time before, and the start time from the previous target
            if Args.CalcStartTimeFromPrevTarget
                [T_sec,~] = ultrasat.tools.calcSlew(Obj.Plan.RA(Plan_row-1),Obj.Plan.Dec(Plan_row-1),Obj.Plan.RA(Plan_row),Obj.Plan.Dec(Plan_row),...
                                                    'Units','deg','CheckTrajectory',true);  
                Obj.Plan.SlewTimeBefore(Plan_row) = seconds(ceil(T_sec)) + Obj.DefSlewBuffer;  
                Obj.Plan.Tstart(Plan_row) = Obj.Plan.Tend(Plan_row-1) + Obj.Plan.SlewTimeBefore(Plan_row);  
            end

            % Calculate the end time of the plan row
            Obj.Plan.Tend(Plan_row) = Obj.Plan.Tstart(Plan_row) + Obj.Plan.TotalDuration(Plan_row); 
            Obj.Plan.JDstart(Plan_row) = juliandate(Obj.Plan.Tstart(Plan_row));  
            Obj.Plan.JDend(Plan_row) = juliandate(Obj.Plan.Tend(Plan_row)); 

            % Calculate the expected roll of the plan row
            Obj.Plan.ExpectedRoll(Plan_row) = ultrasat.tools.expectedRoll(Obj.Plan.RA(Plan_row),Obj.Plan.Dec(Plan_row),Obj.Plan.JDstart(Plan_row));

            % Calculate the visibility of the plan row
            VisJD = Obj.Plan.JDstart(Plan_row) + (0:days(Obj.Plan.ExpTime(Plan_row)):(Obj.Plan.JDend(Plan_row)-Obj.Plan.JDstart(Plan_row)))'; 
            TargetVis = ultrasat.ULTRASAT_restricted_visibility(VisJD, [Obj.Plan.RA(Plan_row) Obj.Plan.Dec(Plan_row)],'CooUnits','deg',...
                'MinSunDist',Obj.ObsSunDist,'MinMoonDist',Obj.ObsMoonDist,'MinEarthDist',Obj.ObsEarthDist,'MinDistOffset',0); 

            if ~all([TargetVis.EarthLimits ; TargetVis.MoonLimits ; TargetVis.SunLimits])
                fprintf('Target %d, JDstart %.2f\n',Obj.Plan.UniqTargInd(Plan_row),Obj.Plan.JDstart(Plan_row))
                
                % @Chen: Temporary for development - removed to allow GUI tests (06/07/2025)
				if ~ispc
                	error('Issue with Sun/Earth/Moon limits');
				end
            end

            % Update no communication and hard observation flags
            Obj.Plan.NoComm(Plan_row) = ~all(TargetVis.CommLimits); 
            Obj.Plan.HardObs(Plan_row) = ~all(TargetVis.PowerLimits);

            % Update the moon, sun, and earth distances
            Obj.Plan.MoonDist(Plan_row) = TargetVis.MoonAngDist(1)*RAD; 
            Obj.Plan.SunDist(Plan_row) = TargetVis.SunAngDist(1)*RAD;
            Obj.Plan.EarthDist(Plan_row) = TargetVis.EarthAngDist(1)*RAD; 

            % TODO @Yossi - ADD Calc Zody,LimMag  

            % Search for overlapping targets in the mission approved plan
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
                Args.TimeBin         = 0.01;        % [days] % this is close to 1 visit 
                Args.WindowStartTime = [];          % Start time of the time window
                Args.WindowEndTime   = [];          % End time of the time window
                Args.ObsSunDist      = [];          % Minimum sun distance
                Args.ObsMoonDist     = [];          % Minimum moon distance
                Args.ObsEarthDist    = [];          % Minimum earth distance
            end

            % Use the default values if not provided

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
            
            % Calculate the start and end Julian dates
            StartJD = juliandate(Args.WindowStartTime);
            EndJD   = juliandate(Args.WindowEndTime);

            % Calculate the Julian dates of the visibility window
            VisJD   = StartJD + (0:Args.TimeBin:(EndJD-StartJD))';          
            
            % Calculate the visibility of the targets
            Obj.Vis = ultrasat.ULTRASAT_restricted_visibility(VisJD, [Obj.UniqTarg.RA Obj.UniqTarg.Dec],'CooUnits','deg',...
                'MinSunDist',Args.ObsSunDist,'MinMoonDist',Args.ObsMoonDist,'MinEarthDist',Args.ObsEarthDist,'MinDistOffset',0);             
            %Obj.CombVis      = Obj.Vis.SunLimits .* Obj.Vis.MoonLimits .* Obj.Vis.EarthLimits;  
            %Obj.CombVisPower = Obj.CombVis .* Obj.Vis.PowerLimits; 
        end
        %
        function adjustCheckTimes(Obj,CheckStartTime,CheckEndTime)
            % Set Obj.CheckTimes and then calls Obj.updateTargetVisibility and Obj.retrieveMissionApprovedPlan
            Obj.CheckTimes = [CheckStartTime;CheckEndTime];
            Obj.updateTargetVisibility;
            Obj.retrieveMissionApprovedPlan;
        end
        %
        function setScheduledStatus(Obj)
            % Set Obj.Status to 'draft' and Obj.ScheduledTime time to 'now'. (called from Obj.scheduleTargets)
            Obj.Status    = 'draft';
            Obj.ScheduledTime = datetime('now','TimeZone', 'UTC');    
        end
        %
        function validate(Obj,Args)
            % TODO - send plan to the validator. In addition, set Obj.Validated and Obj.ValidatedTime to 'now'
            arguments
                Obj
                Args.checkSelfConsistency       = true;
            end
            
            % Clear validation status
            Obj.Validated = false;

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
                                    
            % Done
            Obj.Validated = true;
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
            Obj.Validated = false;
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
            % Must set status to 'submitted' before the call because submitPlan() 
            % writes this status to the database/json
            Obj.Status = 'submitted';
            Obj.SubmittedTime = datetime('now','TimeZone', 'UTC'); 
            try
                Obj.Mclient.submitPlan(planStruct);
            catch ME
                Obj.Status = 'draft';
                error('Mclient.submitPlan failed: %s', ME.message);
            end
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
        function [Res] = getCalibObj(Obj,UniqTargInd)
            % Return the table data of calibration objects
            arguments
                Obj
                UniqTargInd = [];
            end

            % If UniqTargInd is not provided, return all calibration objects
            % Otherwise, return the calibration objects for the given UniqTargInd
            % TabInd is the index of the calibration objects in the CalibObj table
            if isempty(UniqTargInd)
                TabInd = unique(Cell2Vec([Obj.UniqTarg.CalObj{:}]));
                Res = Obj.CalibObj(TabInd,:);
            else
                TabInd = [Obj.UniqTarg.CalObj{UniqTargInd}{:}]; % 
                Res = Obj.CalibObj(TabInd,:); 
            end
        end

        % -------------------------- Plotting --------------------------

        function h = plotCalibSpectrum(Obj,Res,Args)
            % Plot the spectra of CalibObj, returned by getCalibObj()
            arguments
                Obj
                Res
                Args.subInd2plot  = 1;
                Args.WaveRange    = []; % [nm] range for spectrum plotting, e.g. [230 300] 
                Args.AxesHandle       =[]; % appUIAxes
            end
            
            h = [];

            % Get the filename of the calibration object
            Fname = sprintf('%s/%s.fits',Obj.CalibDir,Res.obj{Args.subInd2plot});
            if ~isfile(Fname)
                error('plotCalibSpectrum: file not found: %s', Fname)                    
            end
            try
                Ftab  = fitsread(Fname,'binarytable');                        
            catch ME
                error('plotCalibSpectum: failed to read: %s - %s', Fname, ME.message)
            end
         
            % Read the spectrum from the FITS file
            Spec  = [Ftab{1} Ftab{6} Ftab{7}];  
            
            if isempty(Args.AxesHandle)
                % Create a new figure if no axes handle is provided
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end
            
            % Plot the spectrum
            errorbar(ax,Spec(:,1),Spec(:,2),Spec(:,3),'.'); 

            % Set the x and y labels
            xlabel(ax, '$\lambda\ [\mathrm{\AA}]$', 'Interpreter', 'latex');
            ylabel(ax, 'F [erg cm$^{-2}$ s$^{-1}$ \AA$^{-1}$]', 'Interpreter','latex');

            % Set the y scale to log
            set(ax, 'YScale', 'log');
            if ~isempty(Args.WaveRange)
                xlim(ax,Args.WaveRange.*10);
            end

            % Set the title
            title(ax,sprintf('%s: Teff = %.0f [K], log(g) = %.1f',Res.obj{Args.subInd2plot},Res.Teff_K_(Args.subInd2plot),Res.logG(Args.subInd2plot))); 
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
                Args.TimeUTC      = false; % false=Time JD
                Args.plotSun        = true;
                Args.SunColor     = 'k';
                Args.plotEarth        = true;
                Args.EarthColor     = 'b';
                Args.plotMoon        = true;                
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
            l = {};

            V = Obj.Vis;

            % Convert the time to UTC or JD
            if Args.TimeUTC
                t = datetime(V.JD,'ConvertFrom','juliandate');
                timeWindow = datetime(Args.TimeWindowJD,'ConvertFrom','juliandate');
                startTime = Obj.StartTime;
                endTime = Obj.EndTime;
                xlabeltext='UTC';
            else
                t = V.JD-Args.JD_offset;
                timeWindow = Args.TimeWindowJD-Args.JD_offset;
                startTime = juliandate(Obj.StartTime)-Args.JD_offset;
                endTime = juliandate(Obj.EndTime)-Args.JD_offset;
                xlabeltext=sprintf('JD-%.1f',Args.JD_offset);
            end

            % Plot the Sun/Earth/Moon distances
            if Args.plotEarth
                plot(ax,t,V.EarthAngDist(:,UniqTargInd)*RAD,Args.EarthColor);
                l = [l,{'Earth'}];
            end
            if Args.plotMoon
                plot(ax,t,V.MoonAngDist(:,UniqTargInd)*RAD,Args.MoonColor);
                l = [l,{'Moon'}];
            end
            if Args.plotSun
                plot(ax,t,V.SunAngDist(:,UniqTargInd)*RAD,Args.SunColor);
                l = [l,{'Sun'}];
            end
            
            % Plot Sun/Earth/Moon limits
            if Args.plotEarth
                plot(ax,t([1,end]),[Obj.ObsEarthDist Obj.ObsEarthDist],['--' Args.EarthColor],'linewidth',2);
            end
            if Args.plotMoon
               plot(ax,t([1,end]),[Obj.ObsMoonDist Obj.ObsMoonDist],['--' Args.MoonColor],'linewidth',2);
            end
            if Args.plotSun
                plot(ax,t([1,end]),[Obj.ObsSunDist Obj.ObsSunDist],['--' Args.SunColor],'linewidth',2);   
            end
            
            yl = ylim(ax); % can be removed when using xregion
                        
            % Check for unobservable times due to Earth %% ERROR if only one JD is not observable
            if Args.plotEarth
                Fvis = find(~V.EarthLimits(:,UniqTargInd));
                if ~isempty(Fvis)            
                    Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                    Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                    clear nonVisWindows;
                     if Args.TimeUTC
                         nonVisWindows(:,1) = datetime(V.JD(Fvis(1:2:end)),'ConvertFrom','juliandate');
                         nonVisWindows(:,2) = datetime(V.JD(Fvis(2:2:end)),'ConvertFrom','juliandate');
                     else
                         nonVisWindows(:,1) = V.JD(Fvis(1:2:end))-Args.JD_offset;
                         nonVisWindows(:,2) = V.JD(Fvis(2:2:end))-Args.JD_offset;
                     end
    
                    for i = 1:height(nonVisWindows)
                        fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)],...
                            [0,0,180,180],Args.EarthColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                    end
                end
            end

            % Check for unobservable times due to Moon %% ERROR if only one JD is not observable
            if Args.plotMoon            
                Fvis = find(~V.MoonLimits(:,UniqTargInd));
                if ~isempty(Fvis)            
                    Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                    Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                    clear nonVisWindows;
                     if Args.TimeUTC
                         nonVisWindows(:,1) = datetime(V.JD(Fvis(1:2:end)),'ConvertFrom','juliandate');
                         nonVisWindows(:,2) = datetime(V.JD(Fvis(2:2:end)),'ConvertFrom','juliandate');
                     else
                         nonVisWindows(:,1) = V.JD(Fvis(1:2:end))-Args.JD_offset;
                         nonVisWindows(:,2) = V.JD(Fvis(2:2:end))-Args.JD_offset;
                     end
    
                    for i = 1:height(nonVisWindows)
                        fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)],...
                            [0,0,180,180],Args.MoonColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                    end            
                end
            end

            % Check for unobservable times due to Sun %% ERROR if only one JD is not observable
            if Args.plotSun            
                Fvis = find(~V.SunLimits(:,UniqTargInd));
                if ~isempty(Fvis)
                    Fedges = find(diff(Fvis(1:(end-1)))>1 | diff(Fvis(2:(end)))>1)+1;
                    Fvis = [Fvis(1);Fvis(Fedges);Fvis(end)];
                    clear nonVisWindows;
                     if Args.TimeUTC
                         nonVisWindows(:,1) = datetime(V.JD(Fvis(1:2:end)),'ConvertFrom','juliandate');
                         nonVisWindows(:,2) = datetime(V.JD(Fvis(2:2:end)),'ConvertFrom','juliandate');
                     else
                         nonVisWindows(:,1) = V.JD(Fvis(1:2:end))-Args.JD_offset;
                         nonVisWindows(:,2) = V.JD(Fvis(2:2:end))-Args.JD_offset;
                     end
    
                    for i = 1:height(nonVisWindows)
                        fill(ax, [nonVisWindows(i,1) nonVisWindows(i,2) nonVisWindows(i,2) nonVisWindows(i,1)],...
                            [0,0,180,180],Args.SunColor,'FaceAlpha',0.3,'EdgeColor','none'); % change later to xregion
                    end
                end      
            end

            % Set the plot limits if any of the plots are requested
            if Args.plotSun || Args.plotMoon || Args.plotEarth
                % Set the y limits
                ylim(ax,yl); % can be removed when using xregion
                
                % Set the x limits, if a time window is provided, use the time window limits
                xlim(ax,t([1,end]));
                if ~isempty(Args.TimeWindowJD)
                    xlim(ax,timeWindow)
                end           
            end
            
            % Display vertical lines at the start and end times
            xline(ax,startTime,['-' Args.TimeColor],'Start Time');
            xline(ax,endTime,['-' Args.TimeColor],'End Time');
            
            xlabel(ax,xlabeltext); 
            ylabel(ax,'Angular distance [deg]');

            % Display a title with the target name and index
            TargetName = Obj.UniqTarg.Name(UniqTargInd);
            if isempty(TargetName)
                TargetName = 'UnnamedTarget';
            end
            title(ax, sprintf('Visibility of %s (UniqTarget #%d)', TargetName, UniqTargInd));            
            
            % Display a legend with the plot lines
            legend(ax, l,'Location','best');
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
                      
            % If no axes handle is provided, create a new figure and axes
            if isempty(Args.AxesHandle)
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;  
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end

            % Allow multiple plots to be drawn on the same axes without clearing previous plots
            hold(ax, 'on');  

            % Display a box outline around the axes to make plot boundaries visible
            box(ax, 'on'); 
            
            % Plot the extinction map
            if Args.ExtinctionMap
                RA_vec = (0:360); Dec_vec = (-90:90);
                [RA_grid,Dec_grid] = meshgrid(RA_vec,Dec_vec);
                A_u = ultrasat.tools.extinction(RA_grid,Dec_grid,'AveragedExt',fullfile(Obj.BaseDataDir,Args.AveExtincFile)); 
                imagesc(ax,RA_vec, Dec_vec, A_u);
                colormap(ax,'turbo');
                c = colorbar(ax);
                c.Label.String = 'A_{ULTRASAT}';
                clim(ax, [0,1.1]);
                set(ax,'YDir','normal');
            end
            
            % Plot the visibility at the time of the map
            if Args.vis_at_time_map
                disp('TBD');
            end   
            
            % Plot the calibration objects
            if Args.CalObjMap
                if ~isempty(Obj.CalibObj)
                    plot(ax,Obj.CalibObj.RA,Obj.CalibObj.Dec,'+w');
                end
            end
            
            % Plot the unique targets
            if Args.disp_uniqTarg
                UniqTargInds = Args.plan_rows;
                if isempty(UniqTargInds)
                    UniqTargInds = 1:height(Obj.UniqTarg);
                end
                
                % Loop over the unique target indices
                for ii = 1:numel(UniqTargInds)
                    % Get the FOV circle for the current unique target
                    CircFOV = ultrasat.tools.getFOVcircle(Obj.UniqTarg.RA(UniqTargInds(ii)),Obj.UniqTarg.Dec(UniqTargInds(ii)),'Radius',Obj.Rfov);

                    % Adjust the RA values to be between 0 and 360 degrees
                    CircFOV(CircFOV(:,1)<0,1) = CircFOV(CircFOV(:,1)<0,1)+360;
                    CircFOV(CircFOV(:,1)>360,1) = CircFOV(CircFOV(:,1)>360,1)-360;
                    
                    % Plot the FOV circle
                    plot(ax,CircFOV(:,1),CircFOV(:,2),':k','linewidth',2);
                end
            end
            
            % Plot the mission approved plan
            if Args.disp_MissAprvPlan
                MissAprvPlan_rows = Args.MissAprvPlan_rows;
                if isempty(MissAprvPlan_rows)
                    MissAprvPlan_rows = 1:height(Obj.MissionApprovedPlan);
                end
                
                % Loop over the mission approved plan rows
                for ii = 1:numel(MissAprvPlan_rows)
                    % Get the FOV corners for the current mission approved plan row
                    currFoV = ultrasat.tools.getFOVcorners(Obj.MissionApprovedPlan.RA(MissAprvPlan_rows(ii)),Obj.MissionApprovedPlan.Dec(MissAprvPlan_rows(ii)),...
                        'Roll',Obj.MissionApprovedPlan.Roll(MissAprvPlan_rows(ii)));

                    % Adjust the RA values to be between 0 and 360 degrees
                    currFoV.RA(currFoV.RA<0) = currFoV.RA(currFoV.RA<0)+360;
                    currFoV.RA(currFoV.RA>360) = currFoV.RA(currFoV.RA>360)-360;
                    
                    % Plot the FOV corners
                    plot(ax,polyshape(currFoV.RA,currFoV.Dec),'EdgeColor','r','FaceColor','none','linewidth',2);
                end
            end
            
            % Plot the plan
            if Args.disp_plan
                plan_rows = Args.plan_rows;
                if isempty(plan_rows)
                    plan_rows = 1:height(Obj.Plan);
                end
                
                % Loop over the plan rows
                for ii = 1:numel(plan_rows)
                    % Get the FOV corners for the current plan row
                    currFoV = ultrasat.tools.getFOVcorners(Obj.Plan.RA(plan_rows(ii)),Obj.Plan.Dec(plan_rows(ii)),'Roll',Obj.Plan.ExpectedRoll(plan_rows(ii)));

                    % Adjust the RA values to be between 0 and 360 degrees
                    currFoV.RA(currFoV.RA<0) = currFoV.RA(currFoV.RA<0)+360;
                    currFoV.RA(currFoV.RA>360) = currFoV.RA(currFoV.RA>360)-360;
                    
                    % Plot the FOV corners
                    plot(ax,polyshape(currFoV.RA,currFoV.Dec),'EdgeColor','k','FaceColor','none','linewidth',2);
                end
            end
            
            % Set the x and y limits
            xlim(ax, [0,360]);
            ylim(ax, [-90,90]);

            % Set the x and y labels
            xlabel(ax, 'RA [deg]');
            ylabel(ax, 'Dec [deg]');
            
            % Release the hold state on the axes to allow new plots to overwrite old ones
            hold(ax, 'off'); 
        end

        % ---------------------- Helper Functions ----------------------
        function dt = parseIsoDatetime(Obj, str)
            % Parse ISO 8601 datetime strings with 'Z' or timezone offsets
            dt = ultrasat.api.parseIsoDateTime(str);
        end
        
        %
        function CheckTimes = getDefaultCheckTimes(Obj,Args)
           % Get the default Check Times from the StartTime and EndTime, or from the default check times
           arguments
               Obj
               Args.BufferRelStartEnd  =  days(7);
               Args.DefCheckTimes = datetime({'2028-01-01 00:00:00', '2031-01-01 00:00:00'});
           end
           
           % If StartTime and EndTime are set, use them to calculate the check times
           if ~isempty(Obj.StartTime) && ~isempty(Obj.EndTime)
               T1 = Obj.StartTime - Args.BufferRelStartEnd;
               T2 = Obj.EndTime + Args.BufferRelStartEnd;
               CheckTimes = [T1,T2];

           % If the default check times are set, use them
           elseif numel(Args.DefCheckTimes)==2
               CheckTimes = Args.DefCheckTimes;

           % If no check times are set, use the current date and time to calculate the check times
           else
               T1 = datetime('now')-Args.BufferRelStartEnd; 
               T2 = T1+calmonths(12); 
               CheckTimes = [T1,T2];
           end
        end

        %
        function Result = isEditable(Obj)
            % Allow editing the plan only while still draft, after submit no further modifications are allowed.
            Result = strcmp(Obj.Status, 'draft') && Obj.Editable;
        end

        % ---------------------- New Functions ----------------------
        
        function Res = getExtSurveysForTarget(Obj, UniqTargInd)
            % Return external surveys table for a given unique target index
        
            % Defensive checks
            if isempty(UniqTargInd) || UniqTargInd < 1 || UniqTargInd > height(Obj.UniqTarg)
                Res = Obj.ExtSurveysTable([],:);
                return;
            end
        
            % Extract survey indices (cell content!)
            IndCell = Obj.UniqTarg.ExtSurveys{UniqTargInd};
        
            if isempty(IndCell)
                Res = Obj.ExtSurveysTable([],:);
                return;
            end
        
            % Convert cell array of indices to numeric vector
            Ind = [IndCell{:}];
        
            % Return subset table
            Res = Obj.ExtSurveysTable(Ind, :);
        end


        function Res = getFieldObjForTarget(Obj, UniqTargInd, FieldName)
            % Return table of field objects for a given unique target and field name
            %
            % FieldName: char or string, e.g. 'Blazars', 'Clusters', 'Small', ...
        
            % Normalize field name
            FieldName = char(FieldName);
        
            % Defensive defaults
            Res = table();
        
            % Basic validation
            if isempty(UniqTargInd) || ...
               UniqTargInd < 1 || UniqTargInd > height(Obj.UniqTarg)
                return;
            end
        
            % Check if the field name is a valid field in the FieldObj struct
            if ~isfield(Obj.FieldObjects, FieldName)
                return;
            end
        
            % Extract indices (cell content!)
            IndCell = Obj.UniqTarg.FieldObj{UniqTargInd}.(FieldName);
        
            if isempty(IndCell)
                % Return empty table with correct variables
                Res = Obj.FieldObjects.(FieldName)([],:);
                return;
            end
        
            % Convert cell array of indices to numeric vector
            Ind = [IndCell{:}];
        
            % Slice the corresponding table
            Res = Obj.FieldObjects.(FieldName)(Ind, :);
        end


        function enforceUniqueNames(Obj)
            % Enforce unique names in the UniqTarg table
            Names = Obj.UniqTarg.Name;
        
            % Convert to string to avoid char/cell inconsistencies
            Names = string(Names);
        
            % Count occurrences
            [uNames, ~, ic] = unique(Names, 'stable');
            counts = accumarray(ic, 1);
        
            % Find names that appear >1 time
            dupIdx = find(counts > 1);
        
            if ~isempty(dupIdx)
                warning("addUniqTargets:DuplicateNames", "Duplicate unique target names found. Renaming using suffix _n");
        
                for k = dupIdx'
                    name = uNames(k);
                    rows = find(Names == name);
        
                    % Skip first occurrence, rename second onward
                    for n = 2:numel(rows)
                        newName = sprintf("%s_%d", name, n);
                        Names(rows(n)) = newName;
        
                        fprintf("  Renamed '%s' -> '%s'\n", name, newName);
                    end
                end
        
                % Write updated names back
                Obj.UniqTarg.Name = Names;
            end
        end
        
    end


    methods(Static) % unitTest, Debug      
        Result = unitTest(Args)
            % Function body is in file unitTest.m

    end
end
