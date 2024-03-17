function scheduleAllSkySurvey(Args)
    % distribute AllSky observations for a given start time, grid of pointings, and a given set of constraints
    % Requirements: 2 Gal src visits in one day, 
    %               4 ExtraGal src visits in one day, 4 days separated on a log scale (e.g., day 1, 2, 12, 112)
    % Input: -
    %        * ...,key,val,...
    %        'AllSkyGrid' - a grid of pointings (RA, Dec) in deg
    %        'StartDate'  - the starting date and time
    %        'NumDays'    - length of the survey in days   
    %        'VisitsGal'  - number of visits for galactic pointings
    %        'VisitsExtraGal' - number of visits for extragalactic pointings 
    %        'MinIntervals'- 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point 
    %        'SlotLength' - visit length + daily average retargeting margin (in seconds)
    %        'BlockLength'- maximal length of the daily AllSkySurvey block (in hours)
    %        'PlotSchedule' - whether to plot the survey schdule and distance histogram
    % Output: a schedule text file as JD, RA, Dec for each of the visits
    % Author: A.M. Krassilchtchikov (Feb 2024)
    % Example: ultrasat.scheduleAllSkySurvey('StartDate','2027-01-01 00:00:00');
    arguments
        Args.AllSkyGrid = '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_350_rep1.txt';
        Args.StartDate  = '2027-06-25 00:00:00'; % '2027-01-01 00:00:00';
        Args.NumDays        = 180;   % [days]
        Args.VisitsGal      =   2;   % number of visits for a Galactic field
        Args.VisitsExtraGal =  16;   % number of visits for an extragalactic field      
        Args.MinIntervals   = [1 4 16];  % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point
        Args.SlotLength     = 960;   % [s] 960 s = 3 x 300 s + 60 s
        Args.BlockLength    = 5.4;   % [hrs] % for a 960 s slot this would give 20 slots/day       
        Args.PlotSchedule   = false; % whether to plot the survey schdule and distance histogram
    end
    
    RAD = 180/pi;
    
    SlotsPerDay = floor(24*3600/Args.SlotLength);
    DailyVisits = floor(Args.BlockLength*3600/Args.SlotLength);
    
    % load the the sky map (equatorial coordinates)
    Grid = readmatrix(Args.AllSkyGrid); 
    Np = length(Grid);
      
    % probe visibility during Args.NumDays:
    JD = celestial.time.julday(Args.StartDate) + (0:1/SlotsPerDay:Args.NumDays)';
    Nt = length(JD);
    
    Vis = ultrasat.ULTRASAT_restricted_visibility(JD,Grid./RAD);
    Limits = Vis.SunLimits .* Vis.EarthLimits .* Vis.MoonLimits .* Vis.PowerLimits;     
    
    % for each of the grid points determine if it is galactic or extragalactic either by |b| or by A_lam        
    [Lgal, Bgal] = celestial.coo.convert_coo(Grid(:,1)./RAD,Grid(:,2)./RAD,'j2000.0','g'); 
    GalacticFlag = zeros(Np,1); GalacticFlag( abs( Bgal .* RAD ) < 30 ) = 1;      
        
    Alam = astro.extinction.extinctionGrid(Args.AllSkyGrid,'CooType','j2000.0','Filter','ultrasat');
    ExtinctionFlag = zeros(Np,1); ExtinctionFlag( Alam > 1 ) = 1;
    
%     [lambda, beta] = celestial.coo.convert_coo(Grid(:,1)./RAD,Grid(:,2)./RAD,'j2000.0','e'); 
%     Alam_mean7 = celestial.grid.statSkyGrid('SkyPos',[lambda.*RAD beta.*RAD]);
%     ExtinctionFlagAver = zeros(Np,1); ExtinctionFlagAver( Alam_mean7 > 1 ) = 1; 

%     figure(1); Grid1 = Grid(GalacticFlag==1,:); Grid2 = Grid(GalacticFlag==0,:);    
%     plot(Grid1(:,1),Grid1(:,2),'*'); hold on; plot(Grid2(:,1),Grid2(:,2),'o','Color','red');    
    
    % make a vector of performed visits
    SrcVisits = zeros(Np,1); Visits = zeros(Nt,1); FilledVisits = zeros(Np,1);
    SrcVisits(GalacticFlag == 1) = Args.VisitsGal;
    SrcVisits(GalacticFlag == 0) = Args.VisitsExtraGal;     
    
    % source availability in regards to their visibility
    FreeSlots = sum(Limits,1);       
%     FreeSlots1 = FreeSlots(GalacticFlag==1); FreeSlots2 = FreeSlots(GalacticFlag==0);    
% %     FreeSlots2 = ;
% %     FreeSlots4 = ; 
%     figure(1); subplot(2,1,1)
%     histogram(FreeSlots1/Nt); hold on
%     histogram(FreeSlots2/Nt)    
%     title 'Galactic'
%     subplot(2,1,2)
%     histogram(FreeSlots2/Nt)
%     title 'Extragalactic'
    
    SrcTab = table(Grid(:,1),Grid(:,2),GalacticFlag,ExtinctionFlag,Lgal*RAD,Bgal*RAD,...
                SrcVisits, FilledVisits, FreeSlots',...
        'VariableNames', {'RA', 'Dec', 'Gal','Ext','l','b','Visits','Filled','FreeSlots'});     
    
    % sort the table: first extragalactic objects, first with less visibility (for Algorithm 2) 
%     [SrcTab2, SrcInd] =  sortrows(SrcTab,{'Gal','FreeSlots'}); % this appeared not favourable -- better sort just by visibility
    [SrcTab2, SrcInd] =  sortrows(SrcTab,{'FreeSlots'}); % sort only by visibility

    % 1. make a schedule with a greedy algorithm with recursion (branch cutting):
    [Schedule, SrcTab2] = greedyRec(Limits,SrcTab2,SrcInd,DailyVisits,'MinIntervals',Args.MinIntervals);  
    % save the schedule to a text file
    Sout = Schedule(Schedule>0);
    JDout = JD(Schedule>0);
    writematrix([JDout, SrcTab.RA(Sout), SrcTab.Dec(Sout)],'AllSkySurveySchedule.txt')
    % plot the schedule
    if Args.PlotSchedule
        plotSchedule(Schedule, SrcTab);
    end 
    
    fprintf('\n Schedule successfully built \n');
    
    % 2. make a schedule with a direct method (day by day filling, no log
    % spreading of the extragalatic points)
    
    % actually, we don't know with which source to start, let's start with 1
    % alternatively, we could fix a starting slot for the first day and
    % find the first source appropriate for it (this should be tried as well)    
    
    SrcNum = 1; 
    
    EmptySlots = 0; EmptyDays = 0;
    
    %%% Algorithm 1: 
    % scan all the days and fill the DailyVisits number of slots 
    
    for Iday = 1:Args.NumDays   
        
        Start = (Iday-1)*SlotsPerDay+1; % start slot of the day
        End   =     Iday*SlotsPerDay;   % end slot of the day    
        SkipDay = false;
                     
        % first try to start filling the last source from the previous day:               
        Ind1 = find( Limits(Start:End-DailyVisits+1,SrcNum) == 1, 1, 'first') + Start-1;             
        
        % if no slot is found for the current source or the current source is no more available
        if isempty(Ind1) || SrcTab.Filled(SrcNum) == SrcTab.Visits(SrcNum)
            % search for another source to start the day
            Available = ones(Np,1);                         % make all the objects available
            Available(SrcTab.Filled == SrcTab.Visits) = 0;  % exclude already filled points
            Av = find(Available > 0);  
            if sum(Limits(Start:End-DailyVisits+1, Av )) == 0
                fprintf('\nNo slots are available for day %d, skipping\n',Iday);
                EmptyDays = EmptyDays + 1;
                SkipDay = true;                
            else
                [Ind1, SrcInd] = find( Limits(Start:End-DailyVisits+1, Av ) == 1, 1, 'first');
                SrcNum = Av(SrcInd);
                Ind1 = Ind1 + Start-1;
            end
        end
        
        if ~SkipDay
            
            fprintf('Day %3d: ',Iday);
            
            % mark the visit in the source table and in the visits table
            SrcTab.Filled(SrcNum) = SrcTab.Filled(SrcNum) + 1;
            Visits(Ind1) = SrcNum;
            fprintf(' %3d ',SrcNum);
            
            % next slots should follow immediately after the first one
            for Ind = Ind1+1:Ind1+DailyVisits-1
                SkipSlot = false;
                % if the same source can not be observed more, find another one nearby
                if SrcTab.Filled(SrcNum) == SrcTab.Visits(SrcNum) || Limits(Ind,SrcNum) == 0
                    Available = zeros(Np,1);
                    Available(Limits(Ind,:) == 1) = 1;              % all the objects available for this slot
                    Available(SrcNum) = 0;                          % exclude the previous object
                    Available(SrcTab.Filled == SrcTab.Visits) = 0;  % exclude already filled points
                    if sum(Available) > 0 % at least 1 point is available
                        SrcNum = findNearest(SrcTab.RA(SrcNum)./RAD,SrcTab.Dec(SrcNum)./RAD,...
                            SrcTab.RA./RAD,SrcTab.Dec./RAD, Available);
                    else
                        fprintf('\nNo points are available for slot %d, skipping\n',Ind);
                        SkipSlot = true;
                        EmptySlots = EmptySlots + 1;
                    end
                end
                % mark the visit in the source table and in the visits table
                if ~SkipSlot
                    SrcTab.Filled(SrcNum) = SrcTab.Filled(SrcNum) + 1;
                    Visits(Ind) = SrcNum;
                    fprintf(' %3d ',SrcNum);
                end
            end
            
            fprintf('\n');
        end
    end
    
    % print time and visit margins        
    fprintf('AllSS hours per day: %.1f\n',Args.BlockLength);
    VisitSlots = DailyVisits*Args.NumDays;
    fprintf('Max AllSS visits per 180 days: %d\n',VisitSlots);
    GalVisits    = height(SrcTab(SrcTab.Gal==1,:))*Args.VisitsGal;
    ExtGalVisits = height(SrcTab(SrcTab.Gal==0,:))*Args.VisitsExtraGal;
    fprintf('AllSS visits for galactic objects: %d\n',GalVisits);
    fprintf('AllSS visits for extragalactic objects: %d\n',ExtGalVisits);
    fprintf('Total AllSS visits required: %d\n',ExtGalVisits+GalVisits);
    Margin = VisitSlots-(ExtGalVisits+GalVisits);
    fprintf('Available margin: %d (%.1f percent)\n',Margin,Margin*100/VisitSlots);
    
    Available = ones(Np,1);                         % make all the objects available
    Available(SrcTab.Filled == SrcTab.Visits) = 0;  % exclude already filled points
    Av = find(Available > 0);  
    fprintf('%d objects of %d have not been fully covered\n',numel(Av),Np);
    fprintf('%d empty days of %d\n',EmptyDays,Args.NumDays);
    fprintf('%d empty slots of %d\n',EmptySlots+EmptyDays*DailyVisits,Args.NumDays*DailyVisits);
end





function [Schedule, Tab] = greedyRec(Limits, Tab, Ind, DailyVisits, Args)        
    % a greedy algorithm with recursion
    arguments
        Limits
        Tab
        Ind
        DailyVisits
        % minimal intervals between blocks of 4 observations of an extragalactic point
        Args.MinIntervals = [1 4 16]; % [1 1 1]; [1 3 10]; [1 4 16];     [1 5 25]; [1 10 100];        
        Args.Jump         = 1; % 10; 100; % how many levels up we jump when a point is stuck
    end
    Np     = numel(Ind);     % number of grid points
    Branch = zeros(1,Np);    % indicates the number of branch for the particular point
    Schedule = zeros(size(Limits,1),1); % the schedule to be filled 
    Start = 90.*ones(1,180); Stop = 1.*ones(1,180); % daily block limits 
    
    % make a matrix of distances to be used below as an additional day selection criterion
%     Dist  = 

    Ip = 0;
    while Ip < Np % main loop by sky points
        Ip = Ip+1; 
        % try to settle the next point: if it is not possible, go to the previous point and choose the next branch    
        
        Stuck = false; 
        LastTriedSlot = 0;
        
        SrcNum = Ind(Ip);
        Nvis   = Tab.Visits(SrcNum);
        
        if Tab.Gal(SrcNum) == 1 % for Galactic sources all the 2 visits are on the same day
            VisPerDay = Nvis;
        else
            VisPerDay = Nvis/4; % for extragalactic sources the 16 visits should be done on 4 separate days
        end
        
        fprintf('step %d point %d\n',Ip, SrcNum);
        
        while ~Stuck && Tab.Filled(SrcNum) < Nvis %                      
            
            % find the 1+Branch(Ip) group of VisPerDay ones:  
            SrcLimits = Limits(LastTriedSlot+1:end,SrcNum);     % limits for the current point from LastTriedSlot on 
            SrcLimits = SrcLimits .* ( Schedule(LastTriedSlot+1:end) == 0 ); % mark the already occupied slots with 0 
            FoundSlots = findGroupOfConsecutiveVals(SrcLimits, 1+Branch(Ip), VisPerDay, 1);
            
            if isempty(FoundSlots) % the algorithm is stuck, go up Args.Jump points                
                Stuck = true;  
                Ijump = Ip-Args.Jump; % the point where we start a new branch
                fprintf('Stuck at step %d, point %d, going up to branch %d of point %d..\n',Ip,SrcNum,Branch(Ijump)+1,Ind(Ijump)); 
                Branch(Ijump) = Branch(Ijump) + 1; % advance the branch of the previous point
                Branch(Ijump+1:end) = 0;           % clear the branch numbers of all the next points
                Schedule = Schedule .* ~ismember(Schedule, Ind(Ijump:Ip)); % clean the schedule for Ip and Ip-1 
                Tab.Filled(Ind(Ijump:Ip)) = 0; % clean the number of allocations                
                Ip = Ijump-1;                                      
            else 
                Slots = FoundSlots + LastTriedSlot;
                LastTriedSlot = min(Slots);
                % check if the found slots are available, otherwise look for the next opportunity
                [Day, IntSlots] = daySlot(Slots);
                if Day(end)-Day(1) == 0 % the set fits in 1 day
                    Day1 = Day(1);
                    SlMin = min(Start(Day1),min(IntSlots));
                    SlMax = max(Stop(Day1),max(IntSlots));
                    AttemptedBlockLength = SlMax-SlMin+1;
%                     fprintf('trying day %d, slots %d-%d ... \n',Day1,min(IntSlots),max(IntSlots));
                    %
%                   ADD HERE a distance condition: the maximal distance
%                   to the previous object or to all the daily objects should not exceed 10-20-30 deg? 
%                     AlreadyScheduledDayPoints = Schedule(SlMin+(Day1-1)*90:SlMax+(Day1-1)*90); 
%                     if all(Schedule( Slots ) == 0) && ... % the requested slots are free
%                             AttemptedBlockLength <= DailyVisits % the observation block does not exceed SlotsPerDay slots  
                    if AttemptedBlockLength <= DailyVisits % the observation block does not exceed SlotsPerDay slots
                        Schedule( Slots ) = SrcNum; % fill the Schedule 
                        Start(Day1) = SlMin;
                        Stop (Day1) = SlMax;
                        Tab.Filled(SrcNum) = Tab.Filled(SrcNum) + VisPerDay;
                        if Tab.Filled(SrcNum) == Nvis/4     % move the next available slot to today+Args.MinIntervals(1)
                            LastTriedSlot = (Day1+Args.MinIntervals(1)-1)*90;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(SrcNum) == Nvis/2 % move the next available slot to today+Args.MinIntervals(2)
                            LastTriedSlot = (Day1+Args.MinIntervals(2)-1)*90;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(SrcNum) == 3*Nvis/4 % move the next available slot to today+Args.MinIntervals(3)
                            LastTriedSlot = (Day1+Args.MinIntervals(3)-1)*90;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(SrcNum) == Nvis % all the exposure for the point are scheduled
%                             fprintf('settled \n');
                        else
                            error('number of filled slots is incorrect');
                        end
                    end                
                end
            end            
        end        
    end
end

function [Day, IntSlot] = daySlot(Slot) % get day and daily slot number from the global slot number 
    Day     = ceil(Slot./90);
    IntSlot = Slot - (Day-1).*90;    
end

function Result = findNearest(RA0, Dec0, RA, Dec, Available)
    % find a nearest object from the list 
    if sum(Available) == 0
        error('findNearest input error: list of Available is empty!'); 
    end
              
    Dist = celestial.coo.sphere_dist_fast(RA0,Dec0,RA,Dec);
    Dist(Available == 0) = 1e30; % some large number to exclude these obj.
    
    [~,Result] = min(Dist);        
    
end

function Index = findGroupOfConsecutiveVals(A, M, N, Val)
    % in vector A find Mth group of N consecutive values of Val
    % Input: - a vector of values
    %        - the number of identical value group to be indexed
    %        - the length of identical value groups 
    %        - the value (we are looking for consecutive groups of this value)
    % Output: - a vector of indices of the Mth group of N consecutive values of Val
    % Author: A.M. Krassilchtchikov (Jan 2024)
    % Example: A = [0 0 1 1 1 0 0 1 1 0 1 1 1 0 0 0 1 0 1 1 1];
    %          Ind = tools.find.findGroupOfConsecutiveVals(A, 2, 3, 1); 
    %          [will give the indices of the M = 2nd group of N = 3 values Val = 1 in vector A]
    %          B = [1 2 7 8 7 8 9 0 7 7 1 6 7 7 6 5 7 7 1 0 7 8];
    %          Ind = tools.find.findGroupOfConsecutiveVals(B, 3, 2, 7); 
    %          [will give the indices of the M = 3rd group of N = 2 values Val = 7 in vector B]
    %          Ind = tools.find.findGroupOfConsecutiveVals(B, 4, 2, 7);
    %          will be empty, as there is no 4th group of two 7th
    Ind0 = find(A == Val);
    for i = 1:length(Ind0)-N+1
        ConsecutiveGroup = Ind0(i:i+N-1);
        if all(diff(ConsecutiveGroup) == 1)
            M = M - 1;
            Index = ConsecutiveGroup;
            if M == 0
                return;
            end
        end
    end
    Index = []; % nothing is found 
end

function plotSchedule(Schedule,Tab)

    RAD = 180/pi;
           
    S = Schedule(Schedule > 0 );
    NObs = length(S);
    ObsPoint = zeros(NObs,2);
    for IObs = 1:NObs
         ObsPoint(IObs,1) = Tab.RA (S(IObs));
         ObsPoint(IObs,2) = Tab.Dec(S(IObs));
    end
    
    figure(1); plot(ObsPoint(:,1),ObsPoint(:,2));
    
    RA1 = ObsPoint(1:NObs-1,1)./RAD;
    RA2 = ObsPoint(2:NObs,1)./RAD;
    Dec1 = ObsPoint(1:NObs-1,2)./RAD;
    Dec2 = ObsPoint(2:NObs,2)./RAD;
    
    D=celestial.coo.sphere_dist_fast(RA1,Dec1,RA2,Dec2);
    D=D.*RAD;
    figure(2); histogram(D);
        
end
