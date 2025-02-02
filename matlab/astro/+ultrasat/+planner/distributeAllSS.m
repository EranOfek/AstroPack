function [Result] = distributeAllSS(X, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: ultrasat.planner.distributeAllSS()

    arguments
        X
        Y
        Args.AllowPartial   = false;
        Args.MinIntervals   = [1 4 16];  % 3 minimal intervals (in days) between 4 observation blocks of each extragalactic point        
    end

end
