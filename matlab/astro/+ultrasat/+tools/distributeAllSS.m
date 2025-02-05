function [DailyTab, PointTabSorted, Ind, LinearSchedule] = distributeAllSS(Limits, PointType, DailyVisits, DailySlots, Args)
    % Distibute All Sky Survey visits according to visibility limits and point types
    %     a primitive greedy algorithm is employed
    % Input  : - the visibility limits table: N slots x N points
    %          - the point types vector 
    %          - the maximal number of AllSS visits per day
    %          - the number of possible visits per day (depends on the visit duration w/account of retargeting)
    %          * ...,key,val,... 
    %          'VisitsByType' - number of visits or each PointType
    %          'MinIntervals' - 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point (Type=2
    %          'Jump'         - how many levels up we jump when a point is stuck
    %          'AllowPartial' - allow scheduling even if some of the visits cannot be placed
    %          'MaxBranch'    - maximal number of branches allowed: 
    %                           if AllowPartial is true, give up for the current point after MaxBranch branches 
    % Output : - the schedule: point numbers per each bin or 0
    %          - the sorted table of points where the number of filled visits is indicated
    %          - the index of the original point number in the sorted table
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: [DailyTab, PointTabSorted, ~, ~] = ultrasat.tools.distributeAllSS(Limits, PointType, DailyVisits, DailySlots, 'VisitsByType',[2 16])
    arguments
        Limits
        PointType  
        DailyVisits         = 20;        % maximal number of AllSS visits per day
        DailySlots          = 89;        % number of possible visits per day (depends on the visit duration w/account of retargeting)
        Args.VisitsByType   = [2 16];    % number of visits or each PointType
        Args.MinIntervals   = [1 3 9];  % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point (Type=2)        
        Args.Jump           = 1;         % how many levels up we jump when a point is stuck
        Args.AllowPartial   = false;
        Args.MaxBranch      = 10;        % maximal branch to try before skipping a point
        Args.FieldNames     = [];
        Args.Verbose        = true;
    end
    %
    [TotalSlots, NPoints] = size(Limits);  % determine the total numbers of slots and points
    NDays                 = floor(TotalSlots/DailySlots); 
    
    SrcVisits    = zeros(NPoints,1); 
    FilledVisits = zeros(NPoints,1);
    SrcVisits(PointType == 1) = Args.VisitsByType(1);
    SrcVisits(PointType == 2) = Args.VisitsByType(2);   
    
    if isempty(Args.FieldNames)
        FieldNames = 1:NPoints;
    else
        FieldNames = Args.FieldNames;
    end
    
    FreeSlots = sum(Limits,1); % number of free slots for each point (used for prioritizing points)    
    
    PointTab = table(FieldNames,SrcVisits, FilledVisits, FreeSlots',...
        'VariableNames', {'FieldNames','Visits','Filled','FreeSlots'});
    
    % sort points by the total number of free slots so that the points with
    % less number of free slots are distributed first 
    [PointTabSorted, Ind] = sortrows(PointTab,{'FreeSlots'}); 
    
    [LinearSchedule, PointTabSorted] = greedyRec(Limits, PointTabSorted, Ind, DailyVisits, DailySlots, NDays,...
                                     'MinIntervals', Args.MinIntervals, 'Jump', Args.Jump, ...
                                     'AllowPartial', Args.AllowPartial, 'MaxBranch', Args.MaxBranch,...
                                     'Verbose', Args.Verbose);
                                 
    % check that all the scheduled points are indeed visible 
    ScheduledVisits = sum(LinearSchedule > 0);
    ValidVisits = 0;
    for ISlot = 1:TotalSlots
        Point = LinearSchedule(ISlot);
        if Point > 0
            ValidVisits = ValidVisits + Limits(ISlot, Point);
        end
    end
    if ScheduledVisits > ValidVisits
        error('Some of the scheduled visits are not valid!\n');
    end
    
    % cut the SlotSchedule into days, determine starting slots and point lists:
    DailySchedule = reshape(LinearSchedule(1:DailySlots*NDays),DailySlots,NDays);
    DailyTab = table([],{},'VariableNames',{'StartSlot','Points'});
    for IDay = 1:NDays        
        NonZero = find(DailySchedule(:, IDay) ~= 0);
        if ~isempty(NonZero)
            DailyTab.StartSlot(IDay) = NonZero(1);  % First non-zero index
            DailyTab.Points(IDay) = {DailySchedule(NonZero(1):NonZero(end),IDay)};
        else
            DailyTab.StartSlot(IDay) = 0;
        end
    end    
end

%%%%%%%%%%

function [Schedule, Tab] = greedyRec(Limits, Tab, Ind, DailyVisits, DailySlots, NDays, Args)        
    % a greedy algorithm with recursion
    arguments
        Limits
        Tab
        Ind
        DailyVisits
        DailySlots
        NDays
        % minimal intervals between blocks of 4 observations of an extragalactic point
        Args.MinIntervals = [1 4 16]; % [1 1 1]; [1 3 10]; [1 4 16];     [1 5 25]; [1 10 100];        
        Args.Jump         = 1; % 10; 100; % how many levels up we jump when a point is stuck
        Args.AllowPartial = false;
        Args.MaxBranch    = 10;
        Args.Verbose      = true;
    end
    Np     = numel(Ind);     % number of grid points
    Branch = zeros(1,Np);    % indicates the number of branch for the particular point
    Schedule = zeros(size(Limits,1),1); % the schedule to be filled 
    % initial daily block limits: 
    Start = DailySlots.*ones(1,NDays); 
    Stop  = 1.*ones(1,NDays); 
    
    % make a matrix of distances to be used below as an additional day selection criterion
%     Dist  = 

    Ip = 0;
    while Ip < Np % main loop by sky points
        Ip = Ip+1; 
        % try to settle the next point: if it is not possible, go to the previous point and choose the next branch    
        
        Stuck = false; 
        LastTriedSlot = 0;
        
        SrcNum = Ind(Ip);
        Nvis   = Tab.Visits(Ip);
        
        if Nvis == 2                  % for Galactic sources all the 2 visits are on the same day
            VisPerDay = Nvis;
        elseif Nvis == 16
            VisPerDay = Nvis/4;       % for extragalactic sources the 16 visits should be done on 4 separate days
        else
            error('Incorrect number of visits');
        end
        
        if Args.Verbose
            fprintf('step %d point %d\n',Ip, SrcNum);
        end
        
        while ~Stuck && Tab.Filled(Ip) < Nvis %                      
            
            % find the 1+Branch(Ip) group of VisPerDay ones:  
            SrcLimits = Limits(LastTriedSlot+1:end,SrcNum);     % limits for the current point from LastTriedSlot on 
            SrcLimits = SrcLimits .* ( Schedule(LastTriedSlot+1:end) == 0 ); % mark the already occupied slots with 0 
            FoundSlots = findGroupOfConsecutiveVals(SrcLimits, 1+Branch(Ip), VisPerDay, 1);
            
            if isempty(FoundSlots)    % the algorithm is stuck, go up Args.Jump points                
                Stuck = true;  
                Ijump = Ip-Args.Jump; % the point where we start a new branch
                if Args.AllowPartial && Branch(Ijump)+1 > Args.MaxBranch
                    if Args.Verbose
                        fprintf('Stuck at step %d, point %d. Maxmimal number of branches at point %d exceeded, giving up..',...
                            Ip,SrcNum,Ind(Ijump))
                    end
                    break
                end
                if Args.Verbose
                    fprintf('Stuck at step %d, point %d, going up to branch %d of point %d..\n',Ip,SrcNum,Branch(Ijump)+1,Ind(Ijump));
                end
                Branch(Ijump) = Branch(Ijump) + 1; % advance the branch of the previous point
                Branch(Ijump+1:end) = 0;           % clear the branch numbers of all the next points
                Schedule = Schedule .* ~ismember(Schedule, Ind(Ijump:Ip)); % clean the schedule in the range for Ip and Ip-1 
                Tab.Filled(Ijump:Ip) = 0;          % clean the number of allocations from the table               
                Ip = Ijump-1;                                      
            else 
                Slots = FoundSlots + LastTriedSlot;
                LastTriedSlot = min(Slots);
                % check if the found slots are available, otherwise look for the next opportunity
                [Day, IntSlots] = daySlot(Slots,DailySlots);
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
                        Schedule( Slots ) = SrcNum; % fill the Schedule with point numbers 
                        Start(Day1) = SlMin;
                        Stop (Day1) = SlMax;
                        Tab.Filled(Ip) = Tab.Filled(Ip) + VisPerDay;
                        if Tab.Filled(Ip) == Nvis/4     % move the next available slot to today+Args.MinIntervals(1)
                            LastTriedSlot = (Day1+Args.MinIntervals(1)-1)*DailySlots;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(Ip) == Nvis/2 % move the next available slot to today+Args.MinIntervals(2)
                            LastTriedSlot = (Day1+Args.MinIntervals(2)-1)*DailySlots;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(Ip) == 3*Nvis/4 % move the next available slot to today+Args.MinIntervals(3)
                            LastTriedSlot = (Day1+Args.MinIntervals(3)-1)*DailySlots;
%                             fprintf('partly settled \n');
                        elseif Tab.Filled(Ip) == Nvis % all the exposure for the point are scheduled
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

function [Day, IntSlot] = daySlot(Slot,DailySlots) % get day and daily slot number from the global slot number 
    Day     = ceil(Slot./DailySlots);
    IntSlot = Slot - (Day-1).*DailySlots;    
end

% function Result = findNearest(RA0, Dec0, RA, Dec, Available)
%     % find a nearest object from the list 
%     if sum(Available) == 0
%         error('findNearest input error: list of Available is empty!'); 
%     end
%               
%     Dist = celestial.coo.sphere_dist_fast(RA0,Dec0,RA,Dec);
%     Dist(Available == 0) = 1e30; % some large number to exclude these obj.
%     
%     [~,Result] = min(Dist);        
%     
% end

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

