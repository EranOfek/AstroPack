function [Schedule] = distributeAllSS(Limits, PointType, DailyVisits, DailySlots, Args)
    % Distibute All Sky Survey visits according to visibility Limits and Types
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: ultrasat.planner.distributeAllSS(Limits,PointType, DailyVisits, DailySlots,'VisitsByType',[2 16])
    arguments
        Limits
        PointType  
        DailyVisits         = 20;        % maximal number of AllSS visits per day
        DailySlots          = 89;        % number of possible visits per day (depends on the visit duration w/account of retargeting)
        Args.VisitsByType   = [1 4];     % number of visits or each PointType
        Args.MinIntervals   = [1 4 16];  % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point (Type=2)        
        Args.AllowPartial   = false;
        Args.Verbose        = true;
    end
    %
    [TotalSlots, NPoints] = size(Limits);  % determine the total numbers of slots and points
    NDays                 = floor(TotalSlots/DailySlots); 
    
    FreeSlots             = sum(Limits,1); % number of freeslots
    
    
end

%%%%%%%%%%

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