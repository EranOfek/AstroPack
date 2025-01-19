function [UnVal,Count]=unique_count(Vec, CmpFun, Algo)
    % Unique values and count the number of apperances of each value.
    % Description: Select unique values in numeric or char vector and count the
    %              number of apperances of each value.
    % Input  : - Numeric, string, or cell of chars vector.
    %          - Comparison function for strings. Default is @strcmpi
    %          - Algorithm:
    %            'search' - for each unique value search all.
    %            'scan' - for each value in vector search unique value.
    %            'sort' - sort and search (usually the fastest).
    %            Default is 'sort'.
    % Output : - Unique values
    %          - Count of appearances per unique value.
    % Tested : Matlab R2014a
    %     By : Eran O. Ofek                    Jan 2015
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [UnVal,Count]=tools.array.unique_count(Vec)
    
    arguments
        Vec
        CmpFun   = @strcmpi;
        Algo     = 'sort';
    end

    
    switch Algo
        case 'search'
            UnVal = unique(Vec);
            Nun   = numel(UnVal);
            Count = zeros(Nun,1);

            if isnumeric(Vec)        
                for I=1:1:Nun
                    Count(I) = sum(UnVal(I)==Vec);
                end
            else
                for I=1:1:Nun
                    Count(I) = sum(CmpFun(UnVal(I),Vec));
                end
            end
        case 'scan'
            UnVal = []; %unique(Vec);
            Nun   = numel(UnVal);
            Count = 0; %zeros(Nun,1);

            Nall = numel(Vec);
            if isnumeric(Vec)
                for Iall=1:1:Nall
                    I = find(Vec(Iall)==UnVal);
                    Count(I) = Count(I) + 1;
                end
            else
                for Iall=1:1:Nall
                    I = find(CmpFun(Vec(Iall),UnVal));
                    Count(I) = Count(I) + 1;
                end
            end
            

        case 'sort'
            Vec = sort(Vec);
            UnVal = unique(Vec);
            Nun   = numel(UnVal);
            Count = zeros(Nun,1);
            Nall = numel(Vec);
            if isnumeric(Vec)
                Counter = 1;
                UnI     = 1;
                UnVal(UnI) = Vec(1); 
                for Iall=2:1:Nall
                    if Vec(Iall)==Vec(Iall-1)
                        Counter = Counter + 1;
                    else
                        Count(UnI) = Counter;
                        UnI = UnI + 1;
                        UnVal(UnI) = Vec(Iall);
                        Counter    = 1;
                    end
                end
                Count(UnI) = Counter;
            else
                Counter = 1;
                UnI     = 1;
                UnVal(UnI) = Vec(1); 
                for Iall=2:1:Nall
                    if CmpFun(Vec(Iall),Vec(Iall-1))
                        Counter = Counter + 1;
                    else
                        Count(UnI) = Counter;
                        UnI = UnI + 1;
                        UnVal(UnI) = Vec(Iall);
                        Counter    = 1;
                    end
                end
                Count(UnI) = Counter;

            end

        otherwise
            error('Unknown Algo option');
    end

    
end
