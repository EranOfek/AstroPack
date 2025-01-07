function [UnVal,Count]=unique_count(Vec, CmpFun)
    % Unique values and count the number of apperances of each value.
    % Description: Select unique values in numeric or char vector and count the
    %              number of apperances of each value.
    % Input  : - Numeric, string, or cell of chars vector.
    %          - Comparison function for strings. Default is @strcmpi
    % Output : - Unique values
    %          - Count of appearances per unique value.
    % Tested : Matlab R2014a
    %     By : Eran O. Ofek                    Jan 2015
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [UnVal,Count]=tools.array.unique_count(Vec)
    
    arguments
        Vec
        CmpFun   = @strcmpi;
    end

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
end
