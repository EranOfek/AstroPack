% uplanner helper class for LCS - create it from uplanner buildLCS etc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of functions:
% - ultrasat.LcsHelper.LcsHelper(Args): Constructor
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Additional functions to be considered:
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classdef LcsHelper < Component 
    % 
    properties(Access = public)
        %Planner                                 % uplanner

        % Configurations
        Whole_daily_window = false;%true;% 
        Allow1dgap         = false;

        % Definitions
        AllSky      table     %

        StartDate datetime  = '2029-02-01 00:00:00';
        EndDate datetime  ;
        First_day = 1; % Currently must be 1
        Last_day = 420;
        
        DailyWindowStartTime duration =  duration(00,00,00); % [HH,MM,SS]

        Daily_LCS_slots =   11;    
        SlotTime            =   3*300/60/60/24;% [days] = 3 x 300 s / 86400;
        
        Min_window = 45;
        Max_window_cut = 135;
        max_ext = 1;

        SetBnumel = 16;
        SetCnumel = 16;

        SetA_Nwindows = 6;
        
        % Intermediate Results
        Nominal_windows table
        Full_windows    table

        vis_day_field   logical
        
        vis3d_slot_day_field logical
        vis2d_day_field_ALL logical
        vis2d_day_field_ANY logical        

        All_fields_windows          table
        All_fields_windows_1dgap    table
        Longest_window_per_field    table

        Good_fields_windows             table
        Good_longest_window_per_field   table

        SetA_fields             table
        SetB_fields             table
        SetC_fields             table
        SetD_possible_fields    table
        SetD_ranked_fields      table

        inds_2move      =   [];
        inds_open       =   [];

        % Final schedules
        Schedule                table
        Daily_schedule          
    end

    % 
    properties (Hidden, Constant)

    end 

    % 
    methods  % Constructor
        function Obj = LcsHelper(Args)
            % object constructor
            % example: LCS = ultrasat.planner.LcsHelper();
            %          LCS = ultrasat.planner.LcsHelper('prep_before_schedule',true,'build_the_schedule',true);
            arguments                
                %UPlanner               		%
                Args.StartDate = [];             % 
                Args.EndDate = [];             % 
                Args.AllSkyTable = '~/matlab/data/ULTRASAT/LCS_nonoverlapping_grid_surveys.csv';

                Args.DailyWindowStartTime duration = duration.empty;

                Args.prep_before_schedule = false;
                Args.build_the_schedule = false;
            
            end

            % Set StartDate
            if ~isempty(Args.StartDate)
                Obj.StartDate = dateshift(Args.StartDate,'start','day');
            end

            % Set EndDate if provided, otherwise set it to the start date plus the last day
            if isempty(Args.EndDate)
                Obj.EndDate = Obj.StartDate+Obj.Last_day;
            else
                Obj.EndDate = dateshift(Args.EndDate,'start','day');
                Obj.Last_day = days(Obj.EndDate-Obj.StartDate);
            end

            % Set DailyWindowStartTime if provided
            if ~isempty(Args.DailyWindowStartTime)
                Obj.DailyWindowStartTime = Args.DailyWindowStartTime;
            end
            
            % Read fields from the AllSkyTable if provided (as filename or table)
            if ischar(Args.AllSkyTable)
                Obj.AllSky = readtable(Args.AllSkyTable);
                AU_ind = find(Obj.AllSky.Properties.VariableNames=="AU");
                if ~isempty(AU_ind)
                    Obj.AllSky.Properties.VariableNames(AU_ind) = "A_U";
                end
            elseif istable(Args.AllSkyTable)
                Obj.AllSky = table('Size',[height(Args.AllSkyTable),4],'VariableNames',{'Field','RA','Dec','AU'},'VariableTypes',{'double','double','double','double'});
                Obj.AllSky.Field = (1:height(Args.AllSkyTable))';
                Obj.AllSky.RA = Args.AllSkyTable.RA;
                Obj.AllSky.Dec = Args.AllSkyTable.Dec;
                Obj.AllSky.A_U = Args.AllSkyTable.A_U; % as given by UniqTargetTable
            end


            % Run prepTablesBeforeSchedule if requested
            if Args.prep_before_schedule
                Obj.prepTablesBeforeSchedule;
                if Args.build_the_schedule
                    Obj.buildSchedule;
                end
            end
            
        end

    end 


    %
    methods

        % === Main functions ===
        function prepTablesBeforeSchedule (Obj)
            % Description: prepTablesBeforeSchedule           
            arguments
                Obj
            end         

            % Step1: Build_daily_visibility_for_all_LCS_fields
            Obj.calc_vis_matrix();

            % Step2: Calculate continous visibilty windows (with option to skip 1d)
            [Obj.All_fields_windows,Obj.All_fields_windows_1dgap,Obj.Longest_window_per_field] ...
                                = Obj.calc_cont_vis_windows;

            % Step3: Define the list of good fields (can choose if to allow 1d gap), and then divide them into 3 groups:
            %        Consider replacing with upload
            [Obj.SetA_fields,Obj.SetB_fields,Obj.SetC_fields,Obj.SetD_possible_fields,...
             Obj.Good_fields_windows,Obj.Good_longest_window_per_field] = categorizeFields(Obj);

            % set Nominal_windows
            Obj.Nominal_windows.start = Obj.First_day+Obj.Min_window*(0:7)';
            Obj.Nominal_windows.end = Obj.Nominal_windows.start+Obj.Min_window-1;

        end

        function buildSchedule (Obj,Args)
            % Description: buildSchedule           
            arguments
                Obj
                Args.Startind_Full_windows = 1; % or 9
            end   


            % Step4:  Schedule set A - 6 windows of 45d, each with different 8 SetA fields
            %         (total of 48 fields)
            Obj.schedule_SetA;


            % set Full_windows
            Obj.Full_windows.start = Obj.Schedule.start(Args.Startind_Full_windows:(Args.Startind_Full_windows+7));
            Obj.Full_windows.end = Obj.Schedule.end(Args.Startind_Full_windows:(Args.Startind_Full_windows+7));

            % Step5:  Schedule set C - Schedule 2 windows of 135d, each with 8 different setC fields  
            %         (total of 16 fields)
            Obj.schedule_SetC;

            % Step6:  Schedule set B - each of the 16 fields is scheduled for 135d window (45d at 1d cadence and 90d at 4d cadence)
            %         (total of 16 fields)
            Obj.schedule_SetB;

            % Step7:  Schedule set D - Schedulde four Category D fields (45d@1d cadence)
            %         (total of 4 fields)
            Obj.schedule_SetD;

            % Step8:  Correct inds_2move to inds_open (NOT general enough yet - to supprt mutiple inds_2move of the same ind)
            Obj.correct_Inds;

            % calc daily schedule
            Obj.calcDailySchedule;
        end

        % === Step functions ===
        function calc_vis_matrix(Obj)
            % Description: step 1 function
            %              Build_daily_visibility_for_all_LCS_fields
            arguments
                Obj
            end

            RAD  = 180/pi;

            % Calculate the number of days
            NumDays = Obj.Last_day - Obj.First_day+1;

            % Calculate the number of visibility slots
            N_vis_slots = Obj.Daily_LCS_slots+1; % To account for slew time

            % Calculate the Julian dates
            l = zeros(1,N_vis_slots*NumDays);
            for i=1:NumDays
                for j=1:N_vis_slots
                    k = (i-1)*N_vis_slots+j;
                    l(k) = (i-1)+(j-1).*Obj.SlotTime;
                end
            end
            
            % Calculate the grid of RA and Dec
            Grid = [Obj.AllSky.RA,Obj.AllSky.Dec];
            
            % Calculate the Julian dates
            JD = juliandate(Obj.StartDate+Obj.DailyWindowStartTime) + l;
            
            % Calculate the visibility matrix
            Vis = ultrasat.ULTRASAT_restricted_visibility(JD',Grid./RAD,'MinSunDist',70,'MinMoonDist',34,'MinEarthDist',56);

            % Calculate the visibility limits
            Lim = Vis.PowerLimits & Vis.SunLimits & Vis.MoonLimits & Vis.EarthLimits;
            
            % Reshape the visibility matrix
            Obj.vis3d_slot_day_field = reshape(Lim,[N_vis_slots,NumDays,length(Grid)]); 

            % Calculate the visibility matrix for all fields
            Obj.vis2d_day_field_ALL = squeeze(all(Obj.vis3d_slot_day_field,1));

            % Calculate the visibility matrix for any field
            Obj.vis2d_day_field_ANY = squeeze(any(Obj.vis3d_slot_day_field,1));
            if Obj.Whole_daily_window
                Obj.vis_day_field = Obj.vis2d_day_field_ALL;                                % Visible in whole 3 hour window
            else
                Obj.vis_day_field = Obj.vis2d_day_field_ANY;                                % Visible in at least 15min(1bin) in the 3 hour window
            end
        end

        function [All_fields_windows,All_fields_windows_1dgap,Longest_window_per_field] = calc_cont_vis_windows(Obj)
            % Description: step 2 function
            %              Calculate continous visibilty windows (with option to skip 1d)
            arguments
                Obj
            end
            
            Nfields = width(Obj.vis_day_field);
            
            if Nfields==0   % consider adding error
                return;
            end
            
            d = diff(Obj.vis_day_field(Obj.First_day:Obj.Last_day,:));
            
            All_fields_windows = [];
            All_fields_windows_1dgap = [];
            
            Longest_window_per_field = [];
            Longest_window_per_field(:,1) = Obj.AllSky.Field;%1:Nfields;
            Longest_window_per_field(:,2) = Obj.AllSky.A_U;
            
            for i = 1:Nfields
                vis_start = find(d(:,i)==1)+Obj.First_day;
                vis_end = find(d(:,i)==-1)+Obj.First_day-1;
            
                curr_field_windows = [];
            
                if Obj.vis_day_field(Obj.First_day,i)==1
                    vis_start = [Obj.First_day;vis_start];
                end
                curr_field_windows(:,3) = vis_start;
            
                if Obj.vis_day_field(Obj.Last_day,i)==1
                    vis_end = [vis_end;Obj.Last_day];
                end
                curr_field_windows(:,4) = vis_end;
            
                curr_field_windows(:,5) = curr_field_windows(:,4)-curr_field_windows(:,3)+1;    
            
                curr_field_windows(:,1) = Obj.AllSky.Field(i);
                curr_field_windows(:,2) = Obj.AllSky.A_U(i);
            
                curr_field_windows_1dgap = curr_field_windows;
            
                F_1dgap = true(height(curr_field_windows_1dgap),1);
                for j = height(curr_field_windows_1dgap):-1:2
                    if (curr_field_windows_1dgap(j,3)-1) == curr_field_windows_1dgap(j-1,4)
                        curr_field_windows_1dgap(j-1,4) = curr_field_windows_1dgap(j,4);
                        F_1dgap(j)=false;
                    end
                end
                curr_field_windows_1dgap = curr_field_windows_1dgap(F_1dgap,:);
                curr_field_windows_1dgap(:,5) = curr_field_windows_1dgap(:,4)-curr_field_windows_1dgap(:,3)+1;
                
                All_fields_windows = [All_fields_windows;curr_field_windows];
                All_fields_windows_1dgap = [All_fields_windows_1dgap;curr_field_windows_1dgap];
                Longest_window_per_field(i,3) = max(curr_field_windows(:,5));
                Longest_window_per_field(i,4) = max(curr_field_windows_1dgap(:,5));
            
            end
            
            All_fields_windows = array2table(All_fields_windows,'VariableNames',{'Field','Av_ext','vis_start','vis_end','window'});
            All_fields_windows_1dgap = array2table(All_fields_windows_1dgap,'VariableNames',{'Field','Av_ext','vis_start','vis_end','window'});
            Longest_window_per_field = array2table(Longest_window_per_field,'VariableNames',{'Field','Av_ext','max_window','max_window_1dgap'});
        end

        function [SetA_fields,SetB_fields,SetC_fields,SetD_possible_fields,...
                  Good_fields_windows,Good_longest_window_per_field] = categorizeFields(Obj)
            % Description: step 3 function
            %              Define the list of good fields (can choose if to allow 1d gap), and then divide them into 3 groups
            % 
            %              A. At least 48 fields with A<1 AND (All fields with  45d < visiblilty < 135d) + (6 fields with visiblilty>135d and worst extinction, ranked >32) 
            %              B. 16 fields with visiblilty>135d and best extinction (all A<1, of course)
            %              C. 16 remaining fields with visiblilty>135d and A<1 (ranked 17-32)
            %              D_possilbe. fields with visiblilty>135d and A>1
            arguments
                Obj
            end

            if Obj.Allow1dgap
                Good_fields_windows = Obj.All_fields_windows_1dgap(Obj.All_fields_windows_1dgap.window>=Obj.Min_window & Obj.All_fields_windows_1dgap.Av_ext<=Obj.max_ext,:);
                SetD_possible_fields = Obj.All_fields_windows(Obj.All_fields_windows_1dgap.window>=Obj.Min_window & Obj.All_fields_windows_1dgap.Av_ext>Obj.max_ext,:);
            
                Good_longest_window_per_field = Obj.Longest_window_per_field(Obj.Longest_window_per_field.Av_ext<=Obj.max_ext & Obj.Longest_window_per_field.max_window_1dgap>=Obj.Min_window,:);
            
            else
                Good_fields_windows = Obj.All_fields_windows(Obj.All_fields_windows.window>=Obj.Min_window & Obj.All_fields_windows.Av_ext<=Obj.max_ext,:);
                SetD_possible_fields = Obj.All_fields_windows(Obj.All_fields_windows.window>=Obj.Min_window & Obj.All_fields_windows.Av_ext>Obj.max_ext,:);
            
                Good_longest_window_per_field = Obj.Longest_window_per_field(Obj.Longest_window_per_field.Av_ext<=Obj.max_ext & Obj.Longest_window_per_field.max_window>=Obj.Min_window,:);
            end
            
            Good_fields_windows.scheudled(:) = false;
            Good_longest_window_per_field.scheudled(:) = false;
            for i =1:height(Good_longest_window_per_field)
                Good_longest_window_per_field.possible_windows(i) = sum(Good_fields_windows.Field==Good_longest_window_per_field.Field(i));
            end
            
            Long_fields = Good_longest_window_per_field(Good_longest_window_per_field.max_window>=Obj.Max_window_cut,:);
            Long_fields = sortrows(Long_fields,'Av_ext');
            
            SetB_fields = Long_fields(1:Obj.SetBnumel,:);
            SetC_fields = Long_fields((Obj.SetBnumel+1):(Obj.SetBnumel+Obj.SetCnumel),:);
            SetA_fields = [Good_longest_window_per_field(Good_longest_window_per_field.max_window<Obj.Max_window_cut,:); Long_fields((Obj.SetBnumel+Obj.SetCnumel+1):end,:)];
            
            if Obj.Allow1dgap
                SetA_fields = sortrows(SetA_fields,'max_window_1dgap');
                SetB_fields = sortrows(SetB_fields,'max_window_1dgap');
                SetC_fields = sortrows(SetC_fields,'max_window_1dgap');
            else
                SetA_fields = sortrows(SetA_fields,'max_window');
                SetB_fields = sortrows(SetB_fields,'max_window');
                SetC_fields = sortrows(SetC_fields,'max_window');
            end

        end

        function schedule_SetA(Obj)
            % Description: step 4 function
            %               Schedule 6 windows of 45d, each with different 8 fields from SetA (total of 48 fields)
            arguments
                Obj
            end

            if ~isempty(Obj.Schedule)
                fprintf('Schedule must be empty\n')
                return
            end

            SetA_Schedule = table();

            for Group = 1:Obj.SetA_Nwindows
            
                % Group1_45_scheudle
                curr_Schedule = table();
                curr_Schedule.category(1:8) = {'A'};
                curr_Schedule.group(1:8) = Group;
                curr_Schedule.ind(:) = 1:8;
                
                Field1 = Obj.SetA_fields.Field(~Obj.SetA_fields.scheudled);% & Good_longest_window_per_field.possible_windows==1);
                Field1 = Field1(1);
                start1 = Obj.Good_fields_windows.vis_start(Obj.Good_fields_windows.Field==Field1);
                start1 = start1(1);
                end1  = Obj.Good_fields_windows.vis_end(Obj.Good_fields_windows.Field==Field1);
                end1 = end1(1);
                
                ind1 = 0;
                if Group>1 % try matching previous windows
                   found_window = find(start1<=SetA_Schedule.start & end1>=SetA_Schedule.end,1);
                   
                  if ~isempty(found_window)
                      
                      ind1 = SetA_Schedule.ind(found_window);
                      match_group = SetA_Schedule.group(found_window);
                      curr_Schedule.start = SetA_Schedule.start(found_window) + ((1:8)'-ind1)*Obj.Min_window;
                      curr_Schedule.end= curr_Schedule.start+Obj.Min_window-1;  
                  end
                end
            
                if ind1==0 %try nominal windows
                   found_ind = find(start1<=Obj.Nominal_windows.start & end1>=Obj.Nominal_windows.end,1);
                   
                  if ~isempty(found_ind)
                      
                      ind1 = found_ind;
                      curr_Schedule.start = Obj.Nominal_windows.start;
                      curr_Schedule.end= Obj.Nominal_windows.end;  
                  end
                end
            
                if ind1==0 %find best optimal window
                    if start1<(Obj.Last_day/2)
                      ind1 = floor((start1-Obj.First_day)/Obj.Min_window)+1;
                    
                      curr_Schedule.start = start1 + ((1:8)'-ind1)*Obj.Min_window;
                      curr_Schedule.end= curr_Schedule.start+Obj.Min_window-1;
                    else
                      ind1 = 8-floor((Obj.Last_day-end1)/Obj.Min_window);
                    
                      curr_Schedule.end = end1 + ((1:8)'-ind1)*Obj.Min_window;
                      curr_Schedule.start= curr_Schedule.end-Obj.Min_window+1;
                    end
                end
                
                curr_Schedule.Field(ind1)=Field1;
                
                Obj.SetA_fields.scheudled(Obj.SetA_fields.Field==Field1) = true;
                Obj.Good_fields_windows.scheudled(Obj.Good_fields_windows.Field==Field1) = true;
                
                for ind=1:8
                    if ind~=ind1
                        
                        curr_start = curr_Schedule.start(ind);
                        curr_end = curr_Schedule.end(ind);
                    
                        found_field=false;
                        available_fields = Obj.SetA_fields.Field(~Obj.SetA_fields.scheudled);
                        av_ind=1;
                        while ~found_field
                           curr_field = available_fields(av_ind);
                           F = Obj.Good_fields_windows.Field==curr_field & ...
                               Obj.Good_fields_windows.vis_start<= curr_start & ...
                               Obj.Good_fields_windows.vis_end>= curr_end;
                           window = Obj.Good_fields_windows(F,:);
                    
                           if isempty(window)
                               av_ind = av_ind+1;
                           else
                               curr_Schedule.Field(ind)=curr_field;
                               Obj.SetA_fields.scheudled(Obj.SetA_fields.Field==curr_field) = true;
                               Obj.Good_fields_windows.scheudled(Obj.Good_fields_windows.Field==curr_field) = true;
                               found_field = true;
                           end
                        end
                    end
                end
                
                SetA_Schedule = [SetA_Schedule;curr_Schedule];
            end
            
            Obj.Schedule = [Obj.Schedule;SetA_Schedule];
        end

        function schedule_SetC(Obj,Args)
            % Description: step 5 function
            %               Schedule 2 windows of 135d, each with 8 different fields from setC (total of16 fields) 
            arguments
                Obj
                Args.SetC_start_ind = 3; % tailored with SetB windows
                Args.SetC_nWindows  = 3;
            end 

            Start1 = unique(Obj.Schedule.start(Obj.Schedule.ind==Args.SetC_start_ind));
            Start1 = Start1(1); %(1)
            Base_windows.start = Start1+Args.SetC_nWindows*Obj.Min_window*(0:1)';
            Base_windows.end = Base_windows.start+Args.SetC_nWindows*Obj.Min_window-1;
               
            Obj.SetC_fields.vis_windows = false(numel(Obj.SetC_fields.Field),numel(Base_windows.start));
            
            for i = 1:numel(Obj.SetC_fields.Field)
                for j = 1:numel(Base_windows.start)
                   F = Obj.Good_fields_windows.Field==Obj.SetC_fields.Field(i) & ...
                       Obj.Good_fields_windows.vis_start<= Base_windows.start(j) & ...
                       Obj.Good_fields_windows.vis_end>= Base_windows.end(j);
                   Obj.SetC_fields.vis_windows(i,j) = ~isempty(find(F,1));
                end
            end
            
            Obj.SetC_fields.one_window = sum(Obj.SetC_fields.vis_windows,2);
            Obj.SetC_fields = sortrows(Obj.SetC_fields,'one_window');
            
            curr_Schedule = table();
            curr_Schedule.category(1:16) = {'C'};
            
            curr_Schedule.group(1:8) = 11;
            curr_Schedule.ind(1:8) = 1:8;
            curr_Schedule.start(1:8) = Base_windows.start(1);
            curr_Schedule.end(1:8) = Base_windows.end(1);
            
            curr_Schedule.group(9:16) = 12;
            curr_Schedule.ind(9:16) = 1:8;
            curr_Schedule.start(9:16) = Base_windows.start(2);
            curr_Schedule.end(9:16) = Base_windows.end(2);
            
            Indwindow1 = 1;
            Indwindow2 = 9;
            
            for i = 1:numel(Obj.SetC_fields.Field)
                if Obj.SetC_fields.vis_windows(i,1) && Indwindow1<9
                    curr_Schedule.Field(Indwindow1) = Obj.SetC_fields.Field(i);
                    Obj.SetC_fields.scheudled(Obj.SetC_fields.Field==Obj.SetC_fields.Field(i)) = true;
                    Obj.Good_fields_windows.scheudled(Obj.Good_fields_windows.Field==Obj.SetC_fields.Field(i)) = true;
                    Indwindow1 = Indwindow1+1;
                elseif Obj.SetC_fields.vis_windows(i,2) && Indwindow2<17
                    curr_Schedule.Field(Indwindow2) = Obj.SetC_fields.Field(i);
                    Obj.SetC_fields.scheudled(Obj.SetC_fields.Field==Obj.SetC_fields.Field(i)) = true;
                    Obj.Good_fields_windows.scheudled(Obj.Good_fields_windows.Field==Obj.SetC_fields.Field(i)) = true;
                    Indwindow2 = Indwindow2+1;        
                end
            end
            
            f = curr_Schedule.Field>0;
            
            Obj.Schedule = [Obj.Schedule;curr_Schedule(f,:)];            

        end

        function schedule_SetB(Obj,Args)
            % Description: step 6 function
            %              Schedule SetB: each of the 16 fields is scheduled for 135d window (45d at 1d cadence and 90d at 4d cadence)
            %       Try 4 2 4 3 2 3 3 3
            arguments
                Obj
                Args.W45   = [8     8     7     7     6     6     4     5     3     4     5     3     1     1     1     1];
                Args.W90_1 = [6     6     6     6     7     7     3     3     2     3     3     2     2     2     2     2];
                Args.W90_2 = [7     7     8     8     8     8     2     4     4     2     4     4     3     3     3     3];
            end
            
            % SetB division plan
            SetB_division = table();
            SetB_division.W45   = [ 1     1     1     1     3     3     4     4     5     5     6     6     7     7     8     8]';
            SetB_division.W90_1 = [ 2     2     2     2     2     2     3     3     3     3     7     7     6     6     6     6]';
            SetB_division.W90_2 = [ 3     3     3     3     4     4     2     2     4     4     8     8     8     8     7     7]';
            SetB_division.firstInd = min(table2array(SetB_division(:,1:3)),[],2);
            SetB_division = sortrows(SetB_division,'firstInd');

            % Updtae Inds open/2move base on the above plan and SetC assumption
            Obj.inds_2move=3;
            Obj.inds_open = [5 2 2 1 2];            

            % Calc how many fields start in each First Ind based on the above division plan
            N_firstInd = (1:6)';
            for i = 1:numel(N_firstInd)
                N_firstInd(i,2) = sum(SetB_division.firstInd==i);
            end
            N_firstInd = sortrows(N_firstInd,2,'descend');

            % Calc Vis Windows
            Obj.SetB_fields.vis_windows = false(Obj.SetBnumel,numel(Obj.Full_windows.start));
            
            for i = 1:Obj.SetBnumel
                for j = 1:numel(Obj.Full_windows.start)
                   F = Obj.Good_fields_windows.Field==Obj.SetB_fields.Field(i) & ...  
                       Obj.Good_fields_windows.vis_start<= Obj.Full_windows.start(j) & ...
                       Obj.Good_fields_windows.vis_end>= Obj.Full_windows.end(j);
                   Obj.SetB_fields.vis_windows(i,j) = ~isempty(find(F,1));
                end
            end

            % Initiate the cont3_window and update the N_firstInd table with only relevant Inds
            cont3_vis_windows = Obj.SetB_fields.vis_windows(:,1:6) & Obj.SetB_fields.vis_windows(:,2:7) & Obj.SetB_fields.vis_windows(:,3:8);            

            cont3_vis_windows(:,N_firstInd(N_firstInd(:,2)==0,1))=false;
            N_firstInd = N_firstInd(N_firstInd(:,2)>0,:);

            % Set each field in Obj.SetB_fields to the relevant row in SetB_division
            for i = 1:height(N_firstInd)
                CurrFirstInd = N_firstInd(i,:);
                CurrVisFields = find(cont3_vis_windows(:,CurrFirstInd(1)));
                if numel(CurrVisFields)<CurrFirstInd(2) % not enough fields found
                    error('Not enough fields in SetB for ind %d',CurrInd(1));
                elseif numel(CurrVisFields)>CurrFirstInd(2) % don't need all fields
                    N2remove = numel(CurrVisFields)-CurrFirstInd(2);
                    
                    F_all = find(all(cont3_vis_windows(CurrVisFields,N_firstInd(i+1:end,1)),2));
                    if numel(F_all)>N2remove
                        F_all = F_all(1:N2remove);
                    end
                    if numel(F_all)>0
                        CurrVisFields(F_all) =[];
                        N2remove = N2remove-numel(F_all);
                    end

                    if N2remove>0
                        F_any = find(any(cont3_vis_windows(CurrVisFields,N_firstInd(i+1:end,1)),2));
                        if numel(F_any)>N2remove
                            F_any = F_any(1:N2remove);
                        end
                        if numel(F_any)>0
                            CurrVisFields(F_any) =[];
                            N2remove = N2remove-numel(F_any);
                        end
                    end

                    if N2remove>0
                        error('Problem with SetB scheudle');
                    end
                end
                Obj.SetB_fields.firstInd(CurrVisFields) = CurrFirstInd(1);
                Obj.SetB_fields.SetB_division_Ind(CurrVisFields) = find(SetB_division.firstInd==CurrFirstInd(1));
                cont3_vis_windows(CurrVisFields,:) = false;
                cont3_vis_windows(:,CurrFirstInd(1)) = false;
            end

            Obj.SetB_fields.W45 = SetB_division.W45(Obj.SetB_fields.SetB_division_Ind);
            Obj.SetB_fields.W90_1 = SetB_division.W90_1(Obj.SetB_fields.SetB_division_Ind);
            Obj.SetB_fields.W90_2 = SetB_division.W90_2(Obj.SetB_fields.SetB_division_Ind);

            
            % Schedule set B - for each window (ind) set both 1d and 4d cadence fields

            SetB_Schedule = table();
            for i = 1:8
                % W45
                %IndsW45 = find(Args.W45==i);
                IndsW45 = find(Obj.SetB_fields.W45==i);
                Nw45 = numel(IndsW45);
            
                curr_Schedule = table();
                curr_Schedule.category(1:Nw45) = {'B_45'};
                curr_Schedule.group(1:Nw45) = 100+i;
                curr_Schedule.ind(:) = 1:Nw45;
                curr_Schedule.start(1:Nw45) = Obj.Full_windows.start(i);
                curr_Schedule.end(1:Nw45) = Obj.Full_windows.end(i);
                curr_Schedule.Field(1:Nw45) = Obj.SetB_fields.Field(IndsW45); 
                Obj.SetB_fields.scheudled(IndsW45) = Obj.SetB_fields.scheudled(IndsW45)+1;
            
                SetB_Schedule = [SetB_Schedule;curr_Schedule];
            
                %IndsW90 = find(Args.W90_1==i | Args.W90_2==i);
                IndsW90 = find(Obj.SetB_fields.W90_1==i | Obj.SetB_fields.W90_2==i);
                Nw90 = numel(IndsW90);
            
                curr_Schedule = table();
                curr_Schedule.category(1:Nw90) = {'B_90'};
                curr_Schedule.group(1:Nw90) = 200+i;
                curr_Schedule.ind(:) = 1:Nw90;
                curr_Schedule.start(1:Nw90) = Obj.Full_windows.start(i);
                curr_Schedule.end(1:Nw90) = Obj.Full_windows.end(i);
                curr_Schedule.Field(1:Nw90) = Obj.SetB_fields.Field(IndsW90);  
                Obj.SetB_fields.scheudled(IndsW90) = Obj.SetB_fields.scheudled(IndsW90)+1;
            
                SetB_Schedule = [SetB_Schedule;curr_Schedule];
            
            end            
            Obj.Schedule = [Obj.Schedule;SetB_Schedule];

        end

        function schedule_SetD(Obj,Args)
            % Description: step 7 function
            %              Schedulde four Category D fields (45d@1d cadence)
            %       Rank by WG5
            arguments
                Obj
                Args.Rank = [79 12 48 28 16 88 55 32 213 26];
            end

            % Initialize the SetD_ranked_fields table
            Obj.SetD_ranked_fields = table();
            Obj.SetD_ranked_fields.Field(1:10) = Args.Rank;
            Obj.SetD_ranked_fields.vis_windows = false(numel(Obj.SetD_ranked_fields.Field),numel(Obj.Full_windows.start));

            % Calculate the visibility windows for the SetD_ranked_fields
            for i = 1:numel(Obj.SetD_ranked_fields.Field)
                for j = 1:numel(Obj.Full_windows.start)
                   F = Obj.SetD_possible_fields.Field==Obj.SetD_ranked_fields.Field(i) & ...
                       Obj.SetD_possible_fields.vis_start<= Obj.Full_windows.start(j) & ...
                       Obj.SetD_possible_fields.vis_end>= Obj.Full_windows.end(j);
                   Obj.SetD_ranked_fields.vis_windows(i,j) = ~isempty(find(F,1));
                end
            end

            % Initialize the SetD_Schedule table
            SetD_Schedule = table();            
            Rank_ind=1;
            for i = 1:4
                Vis_inds = [];
                while isempty(Vis_inds)
                    Vis_inds = find(Obj.SetD_ranked_fields.vis_windows(Rank_ind,:));
                    Rank_ind = Rank_ind+1;
                end
            
                Ind = [];
                Oind = 1;
                while isempty(Ind) && Oind<=numel(Obj.inds_open)
                   if  find(Vis_inds==Obj.inds_open(Oind),1)
                      Ind = Obj.inds_open(Oind);
                      Obj.inds_open(Oind) = [];
                   else
                      Oind = Oind+1;
                   end
                end
                if isempty(Ind)
                    Ind = Vis_inds(1);
                    Obj.inds_2move = [Obj.inds_2move,Ind];
                end
            
                % Initialize the current schedule table
                curr_Schedule = table();
                curr_Schedule.category = {'D'};
                curr_Schedule.group = 300+i;
                curr_Schedule.ind = Ind;
                curr_Schedule.start = Obj.Full_windows.start(Ind);
                curr_Schedule.end = Obj.Full_windows.end(Ind);
                curr_Schedule.Field = Obj.SetD_ranked_fields.Field(Rank_ind-1);
                Obj.SetD_ranked_fields.scheudled(Rank_ind-1) = 1;
            
                SetD_Schedule = [SetD_Schedule;curr_Schedule];
            end            

            Obj.Schedule = [Obj.Schedule;SetD_Schedule];
        end

        function correct_Inds(Obj)
            % Description: step 8 function
            %              Correct inds_2move to inds_open (NOT general enough yet - to supprt mutiple inds_2move of the same ind)
            arguments
                Obj    
            end

            Correct_fields = table('Size',[0,3],'VariableNames',{'Field','ind_2_move','ind_open'},'VariableTypes',{'double','double','double'});
            
            % Find possible fields to move from 'inds2move' to 'open_inds'
            % Only SetA fields are considered
            for i = 1:numel(Obj.inds_2move)
                curr_ind_2_move = Obj.inds_2move(i);
                Possible_fields_2_move = Obj.Schedule.Field(Obj.Schedule.start==Obj.Full_windows.start(curr_ind_2_move) & strcmp(Obj.Schedule.category,{'A'}));
                
                for j = 1:numel(Possible_fields_2_move)
                    for k = 1:numel(Obj.inds_open)
                       F = Obj.All_fields_windows.Field==Possible_fields_2_move(j) & ...
                           Obj.All_fields_windows.vis_start<= Obj.Full_windows.start(Obj.inds_open(k)) & ...
                           Obj.All_fields_windows.vis_end>= Obj.Full_windows.end(Obj.inds_open(k));  
                       if ~isempty(find(F,1))
                           Correct_fields.Field(end+1) =  Possible_fields_2_move(j);
                           Correct_fields.ind_2_move(end) =  Obj.inds_2move(i);
                           Correct_fields.ind_open(end) =  Obj.inds_open(k);
                       end
                    end
                end
            end
            
            Correct_ind = zeros(size(Obj.inds_2move));
            First_correct_ind = 1;

            while any(Correct_ind==0) && First_correct_ind<=numel(Correct_fields.Field)
                Correct_ind(:) = 0;
                for i = 1:numel(Correct_ind)
                    if i ==1
                        Correct_ind(i) = First_correct_ind;
                    else
                        taken_inds = Correct_ind(Correct_ind~=0);
                        tmp = find(all(Correct_fields.Field~=Correct_fields.Field(taken_inds)',2) & all(Correct_fields.ind_2_move~=Correct_fields.ind_2_move(taken_inds)',2) & all(Correct_fields.ind_open~=Correct_fields.ind_open(taken_inds)',2));
                        if ~isempty(tmp)
                           Correct_ind(i) =  tmp(1);
                        end
                    end
                end
                First_correct_ind = First_correct_ind+1;
            end

            Correct_fields = Correct_fields(Correct_ind,:);

            for i = 1:numel(Correct_fields.Field)
                Schedule_ind = find(Obj.Schedule.Field==Correct_fields.Field(i) & Obj.Schedule.ind==Correct_fields.ind_2_move(i));
                Obj.Schedule.group(Schedule_ind) = 7;
                Obj.Schedule.ind(Schedule_ind) = Correct_fields.ind_open(i);
                Obj.Schedule.start(Schedule_ind) = Obj.Full_windows.start(Obj.Schedule.ind(Schedule_ind));
                Obj.Schedule.end(Schedule_ind) = Obj.Full_windows.end(Obj.Schedule.ind(Schedule_ind));
            end

            Obj.inds_2move = [];
            Obj.inds_open = [];
        end

        function calcDailySchedule(Obj)
            % Description: calcaulte Daily Schedule
            arguments
                Obj    
            end

            % Base daily schedule
            Obj.Daily_schedule = nan(Obj.Last_day-Obj.First_day+1,Obj.Daily_LCS_slots);

            for i = 1:height(Obj.Schedule)
                for curr_d = Obj.Schedule.start(i):Obj.Schedule.end(i)
                    if ~(any(strcmp(Obj.Schedule.category{i},{'C','B_90'})) && mod((curr_d-Obj.Schedule.start(i)+1),4)~=mod(Obj.Schedule.ind(i),4))
                        open_slot = find(isnan(Obj.Daily_schedule(curr_d,:)),1);
                        Obj.Daily_schedule(curr_d,open_slot) = Obj.Schedule.Field(i);
                    end
                end
            end

            % adjust daily order according visibilty
            for d = 1:height(Obj.Daily_schedule)
                if any(~isnan(Obj.Daily_schedule(d,:)))
                    currFields = Obj.Daily_schedule(d,~isnan(Obj.Daily_schedule(d,:)));
                    fieldInds2move = find(~Obj.vis2d_day_field_ALL(d,currFields));
                    if ~isempty(fieldInds2move)
                        fields2move = currFields(fieldInds2move);
                        possible_newInds = squeeze(Obj.vis3d_slot_day_field(:,d,currFields(fieldInds2move)));
                        possible_newInds((numel(currFields)+1):end,:) = false; % currently do not exceed numel(currFields)
                        for ii = 1:numel(fields2move)
                            if find(possible_newInds(:,ii),1)<(Obj.Daily_LCS_slots/2)
                                newInd = find(possible_newInds(:,ii),1);
                            else
                                newInd = find(possible_newInds(:,ii),1,'last');
                            end
                            field2move = fields2move(ii);
                            currFields(currFields==field2move) = [];
                            currFields = [currFields(1:(newInd-1)) field2move currFields((newInd):end)];
                            possible_newInds(newInd,(ii+1):end) = false;
                        end
                        Obj.Daily_schedule(d,~isnan(Obj.Daily_schedule(d,:))) = currFields;
                    end
                end
            end
        end

        % === Plotting and save products functions ===
        function plotSchedule(Obj,Args)
            % plot the Schedule
            arguments
                Obj
                Args.AxesHandle       =[]; % appUIAxes

                Args.SeperateCatColor      = 'r';
                Args.SavePlot = false;
                Args.FN2SavePlot = 'LCS_Schedule';
                Args.FormatSavePlot = 'png';
                Args.PlotTitle = 'LCS schedule';
            end
            
            if isempty(Args.AxesHandle)
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;  
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end

            % Hold the axes and set the box to on
            hold(ax, 'on');  
            box(ax, 'on');
            l = {};

            % Sort the schedule by category
            Schedule = sortrows(Obj.Schedule,'category');
            for i = 1:height(Schedule)
                % Plot the schedule for each category
                switch Schedule.category{i}
                    case 'A'
                        plot([Schedule.start(i),Schedule.end(i)],ones(2,1)*Schedule.group(i),'-k');
                    case 'C'
                        plot([Schedule.start(i),Schedule.end(i)],ones(2,1)*(mod((Schedule.ind(i)-1),4)/8+floor((Schedule.ind(i)-1)/4)+17),'--k');
                    case 'B_45'
                        plot([Schedule.start(i),Schedule.end(i)],ones(2,1)*(mod((Schedule.ind(i)-1),4)+floor((Schedule.ind(i)-1)/4)+9),'-k');
                    case 'B_90'
                        plot([Schedule.start(i),Schedule.end(i)],ones(2,1)*(mod((Schedule.ind(i)-1),4)/8+floor((Schedule.ind(i)-1)/4)+13),'--k');
                    case 'D'
                        plot([Schedule.start(i),Schedule.end(i)],ones(2,1)*Schedule.group(i)-300+20,'-k');
                        
                end
            end

            ylim([0,25]);            

            set(ax,'Ydir','reverse');
            set(gca,'YTickLabels',[])

                        
            % Display vertical lines at the start and end times
            yline(ax,8,['-' Args.SeperateCatColor],'Category A (48 fields, 45d window @ 1d cadance)');
            yline(ax,16,['-' Args.SeperateCatColor],'Category B (16 fields, 45d window @ 1d cadance + 90d window @ 4d cadance)');
            yline(ax,20,['-' Args.SeperateCatColor],'Category C  (16 fields, 135d window @ 4d cadance)');
            yline(ax,25,['-' Args.SeperateCatColor],'Category D (4 fields, 45d window @ 1d cadance)');
            
            xlabel(ax,sprintf('Time since %s [days]',Obj.StartDate)); 

            % Display a title 
            title(ax, Args.PlotTitle);            
            
            % Display a legend with the plot lines
%            legend(ax, l,'Location','best');
            hold(ax, 'off');

            if Args.SavePlot
               saveas(ax,Args.FN2SavePlot,Args.FormatSavePlot);
            end
        end

        function plotCatB(Obj,Args)
            % plot the Schedule
            arguments
                Obj
                Args.AxesHandle       =[]; % appUIAxes

                Args.SeperateCatColor      = 'k';
                Args.SavePlot = false;
                Args.FN2SavePlot = 'CatB_Schedule';
                Args.FormatSavePlot = 'png';
                Args.PlotTitle = 'Category B Schedule per Field';
            end

            % Create the figure and axes if not provided
            if isempty(Args.AxesHandle)
                h = figure('WindowStyle','docked','Color',[1 1 1]); clf;  
                ax = axes(h);
            else 
                ax = Args.AxesHandle;
            end
            hold(ax, 'on');  
            box(ax, 'on');
            l = {};

            % Plot the Schedule for the SetB fields
            for i = 1:Obj.SetBnumel
                Ind = find(Obj.Schedule.Field==Obj.SetB_fields.Field(i));
                for j = 1:numel(Ind)
                    if strcmp(Obj.Schedule.category(Ind(j)),'B_45')
                       plot(ax,[Obj.Schedule.start(Ind(j)),Obj.Schedule.end(Ind(j))],ones(2,1)*i,'-k'); 
                    else
                       plot(ax,[Obj.Schedule.start(Ind(j)),Obj.Schedule.end(Ind(j))],ones(2,1)*i,'--k');
                    end
                end
            end

            % Set the y-axis limits
            ylim([0.5,16.5]);            

            % Set the x-axis label
            xlabel(ax,sprintf('Time since %s [days]',Obj.StartDate)); 

            % Display a title 
            title(ax, Args.PlotTitle);            
            
            % Display a legend with the plot lines
%            legend(ax, l,'Location','best');
            hold(ax, 'off');

            % Save the plot if requested
            if Args.SavePlot
               saveas(ax,Args.FN2SavePlot,Args.FormatSavePlot);
            end
        end        

    end


    methods (Static) % unitTest, Debug

        % === Write here unit test of the class ===
        function Result = unitTest()

            % From uplanner.unitTest() etc
            upLCS = ultrasat.planner.uplanner('AstPlanner','YS','Type','LCS');

            % ...

            Result = true;
        end

    end
end
