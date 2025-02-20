function [DailyTab, PointTabSorted, Ind, LinearSchedule] = distributeAllSS(Limits, Extragal, DailyVisits, DailySlots, Args)
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
    %          'FieldNames'   - names of sky points (needed to know thati each 4 dithered points should be observed in a row)
    % Output : - the schedule: point numbers per each bin or 0
    %          - the sorted table of points where the number of filled visits is indicated
    %          - the index of the original point number in the sorted table
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: [DailyTab, PointTabSorted, ~, ~] = ultrasat.tools.distributeAllSS(Limits, PointType, DailyVisits, DailySlots, 'VisitsByType',[2 4])
    arguments
        Limits
        Extragal  
        DailyVisits         = 20;        % maximal number of AllSS visits per day
        DailySlots          = 88;        % number of possible visits per day (depends on the visit duration w/account of retargeting)
        Args.VisitsByType   = [2 4];     % number of visits or each PointType
        Args.MinIntervals   = [1 2 4];   % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point (Type=2)        
        Args.Jump           = 1;         % how many levels up we jump when a point is stuck
        Args.AllowPartial   = false;
        Args.MaxBranch      = 0;         % SWITCHED OFF maximal branch to try before skipping a point
        Args.FieldNames     = [];
        Args.Verbose        = true;
    end
    % 
    [TotalSlots, NPoints] = size(Limits);  % determine the total numbers of slots and points
    NDays                 = floor(TotalSlots/DailySlots); 
    
    SrcVisits    = zeros(NPoints,1); 
    FilledVisits = zeros(NPoints,1);
    SrcVisits(Extragal == 0) = Args.VisitsByType(1);
    SrcVisits(Extragal > 0)  = Args.VisitsByType(2);   
    
    if isempty(Args.FieldNames)
        FieldNames = 1:NPoints;
    else
        FieldNames = Args.FieldNames;
        FieldNum   = floor(str2double(FieldNames));
    end
    
    FreeSlots = sum(Limits,1); % number of free slots for each point (used for prioritizing points)    
    
    PointTab = table(FieldNames,SrcVisits, FilledVisits, FreeSlots',(1:NPoints)',FieldNum,...
        'VariableNames', {'FieldNames','Visits','Filled','FreeSlots','PointNum','FieldNum'});    
    % sort points by the total number of free slots so that the points with
    % less number of free slots are distributed first 
    [PointTabSorted, Ind] = sortrows(PointTab,{'FreeSlots'});     
                                            tic    
    [LinearSchedule, PointTabSorted] = greedyRec_v2(Limits, PointTabSorted, Ind, DailyVisits, DailySlots, NDays,...
                                     'MinIntervals', Args.MinIntervals, 'Jump', Args.Jump, ...
                                     'AllowPartial', Args.AllowPartial, 'MaxBranch', Args.MaxBranch,...
                                     'Verbose', Args.Verbose);
                                 
                                            fprintf('Scheduling time: %.0f s \n', toc);    
    % check that all the scheduled points are visible 
    ScheduledVisits = sum(LinearSchedule > 0);
    ValidVisits = 0;
    for ISlot = 1:TotalSlots
        Point = LinearSchedule(ISlot);
        if Point > 0
            ValidVisits = ValidVisits + Limits(ISlot, Point);
            if Limits(ISlot, Point) == 0
               fprintf('Slot %d: object %d is not visible?\n',ISlot, LinearSchedule(ISlot));
            end
        end
    end
    if ScheduledVisits > ValidVisits
        fprintf('Warning: some of the scheduled visits may be not valid.\n');
    end
    
    % cut the SlotSchedule into days, determine starting slots and point lists:
    DailySchedule = reshape(LinearSchedule(1:DailySlots*NDays),DailySlots,NDays);
    DailyTab = table([],{},'VariableNames',{'StartSlot','Points'});
    for IDay = 1:NDays        
        NonZeroSlots = find(DailySchedule(:, IDay) ~= 0);
        if ~isempty(NonZeroSlots)
            DailyTab.StartSlot(IDay) = NonZeroSlots(1);  
            DailyTab.Points(IDay)    = {DailySchedule(NonZeroSlots(1):NonZeroSlots(end),IDay)};
        else
            DailyTab.StartSlot(IDay) = 0;
        end
    end    
end

%%%%%%%%%%

function [Schedule, Tab] = greedyRec_v2(Limits, Tab, Ind, DailyVisits, DailySlots, NDays, Args)        
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
    Start = DailySlots.*ones(1,NDays);  % initial daily block limits 
    Stop  = 1.*ones(1,NDays); 
    
    Ip = 0;
    while Ip < Np % main loop by sky points
        Ip = Ip+1; 
        % try to settle the next point: if it is not possible, go to the previous point and choose the next branch    
        % (currently branching is switched off, because with the dithering the "previous point" is not well defined
        if Tab.Filled(Ip)==Tab.Visits(Ip) % the point has been scheduled 
            fprintf('step %d point %d already scheduled, skipping\n',Ip, Ind(Ip));
            continue
        end
        
        Stuck = false; 
        LastTriedSlot = 0;
        
        SrcNum = Ind(Ip);
        Nvis   = Tab.Visits(Ip);
        
        if Nvis == 2                  % for Galactic sources all the 2 visits are on the same day
            VisPerDay = Nvis;
        elseif Nvis == 4
            VisPerDay = Nvis/4;      % for extragalactic sources the 4 visits should be done on 4 separate days
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
                Ip = Ijump-1; SrcNum = Ind(Ip);                                     
            else 
                Slots = FoundSlots + LastTriedSlot;               
                % if this is a type 2 point, try to set all the 4 related type 2 points
                if Nvis == 4
                    [SrcNum4, Slots4, Shift] = settle4points(Ip, Slots, Tab, Limits, Ind);
                    if isempty(SrcNum4)
                        LastTriedSlot = min(Slots);
                        continue
                    else
                        Slots = Slots4;                        
                    end
                else
                    Shift = 0;
                end
                LastTriedSlot = min(Slots)+Shift; % the shift is essential when the slots before LastTriedSlot are allocated (see settle4points)                
                % check a) if the found slots fit in 1 day and b) if the daily block does not exceed SlotsPerDay,
                % otherwise look for the next opportunity
                [Day, IntSlots] = daySlot(Slots,DailySlots);
                if Day(end)-Day(1) == 0 % the set fits in 1 day
                    Day1 = Day(1);
                    SlMin = min(Start(Day1),min(IntSlots));
                    SlMax = max(Stop(Day1),max(IntSlots));
                    AttemptedBlockLength = SlMax-SlMin+1;
                    if all(Schedule( Slots ) == 0) && AttemptedBlockLength <= DailyVisits % the observation block does not exceed SlotsPerDay slots
                        if Nvis == 4
                            Schedule( Slots ) = Ind(SrcNum4); % 4 type 2 points
                        else
                            Schedule( Slots ) = SrcNum;       % 2 type 1 points
                        end                        
                        Start(Day1) = SlMin;
                        Stop (Day1) = SlMax;
                        if Nvis == 4 % 4 type 2 points
                            Tab.Filled(SrcNum4) = Tab.Filled(SrcNum4) + VisPerDay;
                        else % type 1 point
                            Tab.Filled(Ip)      = Tab.Filled(Ip)      + VisPerDay;
                        end
                        %
                        if Tab.Filled(Ip) == Nvis/4     % move the next available slot to today+Args.MinIntervals(1)
                            LastTriedSlot = (Day1+Args.MinIntervals(1)-1)*DailySlots;
                        elseif Tab.Filled(Ip) == Nvis/2 % move the next available slot to today+Args.MinIntervals(2)
                            LastTriedSlot = (Day1+Args.MinIntervals(2)-1)*DailySlots;
                        elseif Tab.Filled(Ip) == 3*Nvis/4 % move the next available slot to today+Args.MinIntervals(3)
                            LastTriedSlot = (Day1+Args.MinIntervals(3)-1)*DailySlots;
                        elseif Tab.Filled(Ip) == Nvis % all the exposure for the point are scheduled, nothing to do 
                        else
                            error('number of filled slots is incorrect');
                        end
                    end % check the block length                  
                end % check the day
            end % find slots            
        end % branch        
    end % Ip
end

%%%%%%%%%%

function [Day, IntSlot] = daySlot(Slot,DailySlots) % get day and daily slot number from the global slot number 
    Day     = ceil(Slot./DailySlots);
    IntSlot = Slot - (Day-1).*DailySlots;    
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

function [SrcNum, Slots, Shift] = settle4points(Ip,StartSlot,Tab,Vis,IndFun)
    % find a place for 4 type 2 points given the number of one of them 
    SrcNum = [];
    SrcNumbers = [];
    % find the 4 points by the major number 
    Ind = find( Tab.FieldNum == Tab.FieldNum(Ip) );
    % try 4 windows containing StartSlot: 
    if StartSlot+3 < size(Vis,1)+1
        Slots = StartSlot:StartSlot+3;
        Vis4 = Vis(Slots,IndFun(Ind));
        SrcNumbers = find_bipartite_matching(Vis4);         
        Shift = 0;
    end
    if isempty(SrcNumbers) && StartSlot+2 < size(Vis,1)+1 && StartSlot-1 > 0
        Slots = StartSlot-1:StartSlot+2;
        Vis4 = Vis(Slots,IndFun(Ind));
        SrcNumbers = find_bipartite_matching(Vis4);
        Shift = 1;
    end
    if isempty(SrcNumbers) && StartSlot+1 < size(Vis,1)+1 && StartSlot-2 > 0
        Slots = StartSlot-2:StartSlot+1;
        Vis4 = Vis(Slots,IndFun(Ind));
        SrcNumbers = find_bipartite_matching(Vis4);
        Shift = 2;
    end
    if isempty(SrcNumbers) && StartSlot-3 > 0
        Slots = StartSlot-3:StartSlot;
        Vis4 = Vis(Slots,IndFun(Ind));
        SrcNumbers = find_bipartite_matching(Vis4);
        Shift = 3;
    end    
    if ~isempty(SrcNumbers)        
        SrcNum = Ind(SrcNumbers);
        % validity check (for the case of error in bipartite matching)
        if Vis4(1,SrcNumbers(1))+Vis4(2,SrcNumbers(2))+Vis4(3,SrcNumbers(3))+Vis4(4,SrcNumbers(4)) < 4 
            SrcNum = [];
        end
    end
end

function matching = find_bipartite_matching(A)
    [N, M] = size(A);
    matching = zeros(1, M); % Store matched target for each slot
    visited = false(1, M);  % Track visited slots during DFS

    function found = dfs(target)
        for slot = 1:M
            if A(target, slot) == 1 && ~visited(slot)
                visited(slot) = true;
                if matching(slot) == 0 || dfs(matching(slot)) 
                    matching(slot) = target;
                    found = true;
                    return;
                end
            end
        end
        found = false;
    end

    % Try to match each target
    for target = 1:N
        visited(:) = false;
        dfs(target);
    end

    % Convert matching to desired format: (target -> slot)
    final_matching = zeros(N, 1);
    for slot = 1:M
        if matching(slot) > 0
            final_matching(matching(slot)) = slot;
        end
    end

    % Check if a perfect matching was found
    if any(final_matching == 0)
%         disp('No perfect matching exists.');
        matching = [];
    else
        matching = final_matching;
    end
end


