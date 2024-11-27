classdef uplanner < Component 
    % 
    properties(Access = public)
        Type                     % HCS, LCS, AllSS, DDT, TOO 
        StartTime                % start of the whole plan
        EndTime                  %   end of the whole plan
        Plan                     % target list 
        UniqTargList             % unique target list
        Vis                      % visibility matrix 
        
        % HCS, LCS 
        DailyWindow              % [hrs]    
        Cadence                  % [hrs] 
        
        % AllSS
        AllSSHighLatThresh = 30; % |RA| [deg]
        HighLatVisits      = 16; % 1 visit = 3 x 300 s 
        LowLatVisits       =  2;
        EpochPerVisit      =  3;       
        DitherPattern      = '2x2';
        
        % TOO
        TOOMaxTargets      =  4;
        TOOProbMap      
        
        N0                 =  0; % number of unique targets
        Ntarg              =  0; % number of targets in the plan
        
        Rfov               =  7.19; % [deg] FOV radius w/account of the gap
        
        CalibObj           = []; % table of calibration objects 
        CalibDir           
        
        Scheduled                % date or empty
        Validated                % date or empty
        Status             = 'draft';
        
        AstPlanner         = 'YS'; % name of the Astronomer-Planner
    end
    % 
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
                                    'MoonDist','SunDist','EarthDist','SlewDist','OverlapTargets','Zody','Limmag'};
                Args.TargColumns = {'RA', 'Dec', 'A_U', 'CalObj', 'RefImageIDs', 'ExtSurveys', 'FieldObj'};
                
                Args.DailyWindow = [];   % length in hours
                Args.Cadence     = [];   % cadence in days                
                
                Args.TOOMaxTargets = 4; 
                
                Args.AstPlanner  = [];
                
                Args.CalObj      = '~/matlab/data/ULTRASAT/starlib23_table.mat';  % the calibration objects' list 
                Args.CalDir      = '~/matlab/data/ULTRASAT/Calib/';               % the catibration objects' spectra    
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
            if ~isempty(Args.AstPlanner)             
                Obj.AstPlanner= Args.AstPlanner; 
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
            Obj.Plan = table([],[],[],[],[],[],[],[],[],[],[],[],'VariableNames', Args.PlanColumns); 
            %
            Obj.UniqTargList = table([],[],[],[],[],[],[],'VariableNames', Args.TargColumns); 
            %
            load(Args.CalObj); % load the calibration objects' table
            Obj.CalibObj = CalibObj;
            Obj.CalibDir = Args.CalDir;
        end
    end 
    %
    methods % Building the plans          
        %
        function buildHCS(Obj, RA, Dec, Args)
            % build a plan for a list of DDT targets
            arguments
                Obj
                RA               = 0; 
                Dec              = 0;  
                Args.CooFile     = '';   % coordinate file name % ~/test.coo 
                Args.DailyWindow = 24.0; % the actual space for the HCS will be different for HCS + LCS and HCS + AllSS cases!  
                Args.StartTime   = [];
                Args.EndTime     = [];                
            end  
            % set survey properties  
            Obj.Type        = 'HCS';            
            Obj.Cadence     = 300 / 3600;             
            Obj.DailyWindow = Args.DailyWindow;            
            % change start and stop time, if requested
            if ~isempty(Args.StartTime) 
                Obj.StartTime = Args.StartTime; 
            end            
            if ~isempty(Args.EndTime)             
                Obj.EndTime   = Args.EndTime; 
            end            
            % load unique targets 
            Obj.loadUniqTargCoo(RA, Dec, 'File', Args.CooFile)                                   
            % fill properties of unique target fields
            Obj.fillUniqTargProp                        
            % schedule targets and fill the plan
            Obj.schedule
            % make a schedule 
            % show which observations in the existing plan are to be replaced 
                % this is not needed for the HCS?
            % validate the plan
            Obj.validate
            % submit the plan as JSON and save the plan in a .mat object
            Obj.submit
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
        function loadUniqTargCoo(Obj, RA, Dec, Args)
            % read unique target coordinates
            arguments
                Obj
                RA        = 0; % [deg]
                Dec       = 0; % [deg]
                Args.File = ''; % coordinate file name % ~/test.coo
            end
            %
            if ~isempty(Args.File)
                Coo = readmatrix(Args.File,'FileType','text');
                RA  = Coo(:,1); Dec = Coo(:,2);
            end
            %
            Obj.N0 = numel(RA); % the number of unique targets
            %
            Obj.UniqTargList.RA(1:Obj.N0) = RA(1:Obj.N0); Obj.UniqTargList.Dec(1:Obj.N0) = Dec(1:Obj.N0);
        end
        %
        function fillUniqTargProp(Obj, Args)
            % fill the properties lines for each of the unique targets 
            arguments
                Obj    
                Args.A
            end              
            % target coordinates 
            RA = Obj.UniqTargList.RA; Dec = Obj.UniqTargList.Dec; 
            
            % extinction 
            Obj.UniqTargList.A_U = ultrasat.tools.extinction(RA, Dec); 
                            
            for iT = Obj.N0 % loop over targets 
                RA0 = Obj.UniqTargList.RA(iT); Dec0 = Obj.UniqTargList.Dec(iT);                
                % make a circular FOV region
                FOV = ultrasat.tools.getFOVcircle(RA0,Dec0,'Radius',Obj.Rfov,'Plot',0);  
                FOVp = polyshape(FOV);  % a polyshape is useful to test intersections
                
                % select calibration objects 
                Ind = celestial.search.isPointInsidePolygon(Obj.CalibObj.RA, Obj.CalibObj.Dec, FOV);
                Obj.UniqTargList.CalObj = num2cell(find(Ind>0));

                % select reference images
%                 Ind = celestial.search.isPointInsidePolygon(Obj.RefIma.RA, Obj.RefIma.Dec,FOV); 
%                 Obj.UniqTargList.RefImageIDs = num2cell(find(Ind>0));

                % select external surveys 
%                 load(extsurveymaps);
%                 Ind = overlaps(ExtSurv,FOVp));
%                 Obj.UniqTargList.ExtSurveys =                

                % select specific objects falling into the FOV
%                 Obj.UniqTargList.FieldObj =
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
            %          Tab = P.showCalibObj(P.UniqTargList.CalObj) 
            % or
            %          P.showCalibObj(P.UniqTargList.CalObj{1}, 'PlotSpectrum',true); 
            arguments
                Obj
                Ind
                Args.PlotSpectrum = false;
                Args.Band         = [];     % [nm] band for spectrum plotting, e.g. [230 300] 
            end
            %
            if iscell(Ind)
                Ind = Cell2Vec(Ind);
            end
            Res = Obj.CalibObj(Ind,:);
            if Args.PlotSpectrum
                Fname = sprintf('%s/%s.fits',Obj.CalibDir,Res.obj{1});
                Ftab  = fitsread(Fname,'binarytable');
                Spec  = [Ftab{1} Ftab{6} Ftab{7}];                
                figure; clf                                
                errorbar(Spec(:,1),Spec(:,2),Spec(:,3),'.'); xlabel '\lambda, A'; ylabel 'F, erg/cm(2)/s/A'; set(gca, 'YScale', 'log');
                if ~isempty(Args.Band)
                    xlim(Args.Band.*10);
                end
                Title = sprintf('%s: Teff = %.0f, log(g) = %.1f',Res.obj{1},Res.Teff_K_,Res.logG); title(Title)        
            end            
        end
        %
        %
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
