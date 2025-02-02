function [Schedule] = distributeAllSS(Limits, PointType, DailyVisits, Args)
    % Distibute All Sky Survey visits according to visibility Limits and Types
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: ultrasat.planner.distributeAllSS(Limits,)
    arguments
        Limits
        PointType  
        DailyVisits
        Args.VisitsByType   = [1 4];     % number of visits or each PointType
        Args.MinIntervals   = [1 4 16];  % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point (Type=2)        
        Args.AllowPartial   = false;
        Args.Verbose        = true;
    end
    %
    FreeSlots = sum(Limits,1);   
end
