% Example for creating HCS survey:
%   S1 = [67,-59]; N2 = [215,60]; N3 = [254,64];
%   up = ultrasat.planner.uplanner('AstPlanner','YS','Type','HCS');
%   up.addTargets(S1(1),S1(2),'Name',{'S1'});
%   up.StartTime = '2028-01-01 00:12:00';
%   up.EndTime = '2028-07-01 00:12:00';
%   up.buildHCS;

classdef uplanner < Component 
    % 
    properties(Access = public)
        Type                char        % HCS, LCS, AllSS, DDT, TOO 
        StartTime           datetime    % start of the whole plan
        EndTime             datetime    %   end of the whole plan
        Plan                            % target list 
        UniqTargList                    % unique target list
        Vis                             % visibility matrix 

        DefEpochsPerVisit   uint8       =  3; 
        Exptime             duration    = seconds(300); %[s]
        Tiles(1,:)          cell        = {'1','2','3','4'}; %

        
        % LCS / AllSS
        DailyWindowStartTime    duration    =  duration(10,00,00); % [hrs]   
        DailyWindowMaxDuration  duration    =  hours(3);       % [hrs]        
        %Cadence                         % [days]  NOT SURE if REQUIRED
        
        
        % AllSS
        AllSSHighLatThresh  double      = 30; % |RA| [deg]
        HighLatVisits       uint8       = 16; % 1 visit = 3 x 300 s 
        LowLatVisits        uint8       =  2;      
        DitherPattern                   = '2x2';
        
        % TOO
        TOOMaxTargets       uint8       =  4;
        TOOProbMap      
        
        N_uniqueTargets     uint8       =  0; % number of unique targets
        N_planTargets       uint8       =  0; % number of targets in the plan
        
        Rfov                            =  7.19; % [deg] FOV radius w/account of the gap
        
        CalibObj                        = []; % table of calibration objects 
        CalibDir           
        
        Scheduled                       % date or empty
        Validated                       % date or empty
        Status              char        = 'draft';
        
        AstPlanner          char        % name of the Astronomer-Planner
    end
    % 
    properties(Hidden, Constant)
        Plan_AllowedTypes  = {'HCS', 'LCS', 'AllSS', 'DDT', 'TOO'};
        
        SysTimeZone        = 'UTC';
        
        Plan_DefVarNames   = {'Name','UniqTargInd','Group','RA', 'Dec','Roll','Tiles',...
                              'Tstart','Tend','JDstart','JDend','ExpTime','Nexposures','TotalDuration','SlewTimeBefore',...
                              'MoonDist','SunDist','EarthDist','Zody','LimMag','OverlapTargets'};
        Plan_DefVarTypes   = {'char','uint8','uint8','double','double','double','cell',...
                              'datetime','datetime','double','double','duration','double','duration','duration',...
                              'double','double','double','double','double','cell'};
                                                                
        Target_DefVarNames = {'Name','RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj'};
        Target_DefVarTypes = {'char','double','double', 'double', 'cell', 'cell', 'cell', 'cell'};  
        
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
            load(Args.CalObj); % load the calibration objects' table
            Obj.CalibObj = CalibObj;
            Obj.CalibDir = Args.CalDir;
        end
    end 
    %
    methods % Setters/Getters
        function set.Type(Obj, Type)
            % setter for Plan Type - verify from allowed list
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
            % build a plan for a list of DDT targets
%            arguments
%                Obj
%                RA               = 0; 
%                Dec              = 0;  
%                Args.CooFile     = '';   % coordinate file name % ~/test.coo 
%                Args.DailyWindow = 24.0; % the actual space for the HCS will be different for HCS + LCS and HCS + AllSS cases!  
%                Args.StartTime   = [];
%                Args.EndTime     = [];                
%            end  
            
            RAD = 180/pi;
            
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
            if size(Obj.UniqTargList,1) ~=1
                error('HCS reuire one single target');
            end
                
            % Add plan row (should later move to a dedicated function)
            Plan_row=1;
            TardetInd = 1;
            Obj.Plan.Name(Plan_row) = Obj.UniqTargList.Name(TardetInd);
            Obj.Plan.UniqTargInd(Plan_row) = TardetInd;
            Obj.Plan.RA(Plan_row) = Obj.UniqTargList.RA(TardetInd);
            Obj.Plan.Dec(Plan_row) = Obj.UniqTargList.Dec(TardetInd);
            Obj.Plan.Tstart(Plan_row) = Obj.StartTime;
            Obj.Plan.ExpTime(Plan_row) = Obj.Exptime;
            Obj.Plan.Tiles = Obj.Tiles;
            
            % Calc number of exposures within the plan time 
            Obj.Plan.Nexposures(Plan_row) = floor((Obj.EndTime-Obj.StartTime)/Obj.Exptime);
            
            % Calc several times: TotalDuration, end time, JDstart, JDend
            Obj.Plan.TotalDuration(Plan_row) = Obj.Plan.Nexposures(Plan_row) * Obj.Plan.ExpTime(Plan_row);
            Obj.Plan.Tend(Plan_row) = Obj.Plan.Tstart(Plan_row) + Obj.Plan.TotalDuration(Plan_row);
            Obj.Plan.JDstart(Plan_row) = juliandate(Obj.Plan.Tstart(Plan_row));
            Obj.Plan.JDend(Plan_row) = juliandate(Obj.Plan.Tend(Plan_row));
            
            TargetVis = ultrasat.ULTRASAT_restricted_visibility(Obj.Plan.JDstart(Plan_row), [Obj.Plan.RA(Plan_row) Obj.Plan.Dec(Plan_row)]./RAD,...
                'MinSunDist',Obj.ObsSunDist/RAD,'MinMoonDist',Obj.ObsMoonDist/RAD,'MinEarthDist',Obj.ObsEarthDist/RAD);
            
            Obj.Plan.MoonDist(Plan_row) = TargetVis.MoonAngDist*RAD;
            Obj.Plan.SunDist(Plan_row) = TargetVis.SunAngDist*RAD;
            Obj.Plan.EarthDist(Plan_row) = TargetVis.EarthAngDist*RAD;

            % ADD Calc OverlapTargets,Zody,LimMag            
            
            % update Number of target in the plan;
            Obj.N_planTargets = height(Obj.Plan);
            
            % update End time of the plan;
            Obj.EndTime = Obj.Plan.Tend(Plan_row);
            
            % schedule targets and fill the plan
%            Obj.schedule
            % make a schedule 
            % show which observations in the existing plan are to be replaced 
                % this is not needed for the HCS?
            % validate the plan
%            Obj.validate
            % submit the plan as JSON and save the plan in a .mat object
%            Obj.submit
        end
        %
        function buildLCS(Obj, Args)
            % build a plan for a list of DDT targets
            arguments
                Obj
                Args.Coo
            end
            % check visibility within the given time interval for each of the targets
            % 
            % fill in the target list 
            %                       
        end
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
            % read the map, prepare targets
            %
            % check visibility for each of the targets and prepare the plan 
            %
            % 
        end
        %
    end
    %
    methods % Auxiliary functions
        %
        function addTargets(Obj, RA, Dec, Args)
            % read unique target coordinates
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
        function clearUniqueTargets(Obj)
            % remove all unique targets
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
            % remove the plan
            Obj.Plan(:,:) = [];
            % clean the number of unique targets
            Obj.N_planTargets = 0;
        end        
        %
        function updateTargetProperties(Obj, Args)
            % fill the properties lines for each of the unique targets 
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

            for iT = Obj.N_uniqueTargets % loop over targets 
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
        function Res = showCalibObj(Obj,Ind,Args)
            % show the table data and spectra of calibration objects
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
                Ind               = [];
                Args.PlotSpectrum = false;
                Args.WaveRange    = []; % [nm] range for spectrum plotting, e.g. [230 300] 
            end
            %
            if isempty(Ind)
                TabInd = Cell2Vec(Obj.UniqTargList.CalObj{1});
                Res = Obj.CalibObj(TabInd,:);
            else
                TabInd = Obj.UniqTargList.CalObj{1}{Ind}; % what to do if Ind is an array?? 
                Res = Obj.CalibObj(TabInd,:); 
            end
            if Args.PlotSpectrum
                Fname = sprintf('%s/%s.fits',Obj.CalibDir,Res.obj{1});
                Ftab  = fitsread(Fname,'binarytable');
                Spec  = [Ftab{1} Ftab{6} Ftab{7}];                
                figure; clf                                
                errorbar(Spec(:,1),Spec(:,2),Spec(:,3),'.'); xlabel '\lambda, A'; ylabel 'F, erg/cm(2)/s/A'; set(gca, 'YScale', 'log');
                if ~isempty(Args.WaveRange)
                    xlim(Args.WaveRange.*10);
                end
                Title = sprintf('%s: Teff = %.0f, log(g) = %.1f',Res.obj{1},Res.Teff_K_,Res.logG); title(Title)        
            end            
        end
        %
        function updateTargetVisibility(Obj, Args)
            % calculate visibility for all the unique targets for the given period
            arguments
                Obj                     
                Args.TimeBin           = 0.01; % [days] 
                Args.SunDist           = 70;   % [deg]
                Args.MoonDist          = 34;   % [deg]
                Args.EarthDist         = 56;   % [deg]
            end
            %
            RAD = 180/pi;          
            %
            StartJD = celestial.time.julday(datestr(Obj.StartTime,'yyyy-mm-ddTHH:MM:SS'));
            EndJD   = celestial.time.julday(datestr(Obj.EndTime,'yyyy-mm-ddTHH:MM:SS'));
            VisJD  = StartJD + (0:Args.TimeBin:(EndJD-StartJD))';                         
            Obj.Vis    = ultrasat.ULTRASAT_restricted_visibility(VisJD, [Obj.UniqTargList.RA Obj.UniqTargList.Dec]./RAD,...
                'MinSunDist',Args.SunDist/RAD,'MinMoonDist',Args.MoonDist/RAD,'MinEarthDist',Args.EarthDist/RAD);             
%             Obj.CombVis      = Obj.Vis.SunLimits .* Obj.Vis.MoonLimits .* Obj.Vis.EarthLimits;  
%             Obj.CombVisPower = Obj.CombVis .* Obj.Vis.PowerLimits; 
        end
        %
        function schedule(Obj,Args)
            %
            arguments
                Obj
                Args.A
            end
            %
            Obj.Scheduled = datetime('now','TimeZone', 'UTC');    
        end
        %
        function validate(Obj,Args)
            %
            arguments
                Obj
                Args.A
            end
            %
            Obj.Status    = 'validated';
            Obj.Validated = datetime('now','TimeZone', 'UTC');     
        end        
        %
        function submit(Obj,Args)
            %
            arguments
                Obj
                Args.A
            end
            %
            Obj.Status    = 'submitted';            
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
