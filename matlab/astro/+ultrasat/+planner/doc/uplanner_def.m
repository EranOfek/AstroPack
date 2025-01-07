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
        DailyWindowStartTime    duration    =  duration(23,00,00); % [hrs]   
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
        
        Plan_DefVarNames   = {'Name','UniqTargInd','Group','RA', 'Dec','ExpectedRoll','Tiles',...
                              'Tstart','Tend','JDstart','JDend','ExpTime','Nexposures','TotalDuration','SlewTimeBefore',...
                              'NoComm','HardObs','MoonDist','SunDist','EarthDist','Zody','LimMag','OverlapTargets'};
        Plan_DefVarTypes   = {'string','uint8','uint8','double','double','double','string',...
                              'datetime','datetime','double','double','duration','double','duration','duration',...
                              'logical','logical','double','double','double','double','double','cell'};
                                                                
        Target_DefVarNames = {'Name','RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj','HealpixArray'};
        Target_DefVarTypes = {'string','double','double', 'double', 'cell', 'cell', 'cell', 'cell','cell'};  
        
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


	
    methods % Building the plans          
        %
		function buildHCS % Build a plan for a HCS field. 

        function buildLCS % Build a plan for a Targetlist of LCS fields
		
        function buildTOO % Build a plan for a TOO list
		
		function addDDT2Plan  % Add to the plan a list of DDT targets
		
        function buildAllSS % TODO - write build All Sky-Survey function


    methods % Auxiliary functions
        %
        function addUniqTargets            % Add a list of [RA,Dec] coordinates (in degrees) to the unique targetList, 
         	 
        function scheduleTargets            % Schedule a group of targets, starting at StartTime
         
		function retrieveMissionApprovedPlan(Obj,Args) % Retrive the mission approved plan in a given time window
		
        function clearUniqueTargets            % Clear the unique target list, as well as the plan and visibility object 

        function clearPlan(Obj) % Clear the plan

		function clearMissionApprovedPlan(Obj)  % Clear the Mission Approved Plan table

		function planSelfConsistencyCheck(Obj,Args)  % Verify that the plan schedule is self consistent

		function adjustGroupStartTime(Obj,Args)  % Adjust the start time of a group in the plan by 3 options: 

		function updateTargetProperties(Obj, Args)  % updateTargetProperties(Obj, Args)

        function updateTargetVisibility(Obj, Args)  % Calcuate visibility for all unique targets for a given time window (default window is Obj.CheckTimes)
     
        function adjustCheckTimes           % Set Obj.CheckTimes and then calls Obj.updateTargetVisibility and Obj.retrieveMissionApprovedPlan

        function schedule(Obj)            % Set Obj.Status to 'draft' and Obj.Scheduled time to 'now'. (called from Obj.scheduleTargets)

        function validate(Obj,Mclient)            % TODO - send plan to the validator. In addition, set Obj.Status to 'validated' and Obj.Validated time to 'now'

        function submit(Obj,Mclient)            %  TODO - submit plan to the Mission C&C. In addition, set Obj.Status to 'submitted' and Obj.Submitted time to 'now'

        %
        function planTable2struct(Obj,Args)            % Return a struct array of a conversion of the Obj.Plan table, in the correct naming and format for validation/submission
      
        %
        function showCalibObj(Obj,TargInd,Args)            % Return the table data of calibration objects and (optionally) plot the spectra (of selected one)
         
    end
