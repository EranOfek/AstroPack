function [T] = insertIntegerTime2table(T, Args)
    % Insert time or creation time in integer format to table or vector.
    %     Integer time format is the UTC time in ms since J2000.
    % Input  : - Table or empty.
    %          * ...,key,val,... 
    %            'ColJD' - A column name in which the JD is stored in the
    %                   input table. Alternatively, this can be a numeric
    %                   scalar or vector with JD. If the input is numeric
    %                   scalar, then the output will be a vector which
    %                   number of elements is equal to the number of lines
    %                   in the input table.
    %                   Default is 'JD'.
    %            'IntTimeFun' - Function of the form F(JD) that returns the
    %                   integer time.
    %                   Default is uint64((jd-2451545.5).*86400.*1000);
    %            'ColIntTime' - Column name in the table in which to insert
    %                   the integer time.
    %                   If empty, then the output will be a vector.
    % Output : - If 'ColIntTime is empty, then this is a
    %            vector of integer time in unit64.
    %            Otherwise, this is the input table to which a column
    %            named Args.ColIntTime with the integer time was added.
    % Author : Eran Ofek (2024 Oct) 
    % Example: db.util.insertIntegerTime2table([],'ColJD',2451546,'ColIntTime',[])
    %          db.util.insertIntegerTime2table([],'ColJD',2451546)
    %          db.util.insertIntegerTime2table(T)

    arguments
        T
        Args.ColJD             = 'JD';  % column name or numeric (if numeric, then use as is)
        Args.IntTimeFun        = @(jd) uint64((jd-2451545.5).*86400.*1000);  % number of ms since J2000
        Args.ColIntTime        = 'ID_TIME';
    end

    % add integer time column to table
    if isnumeric(Args.ColJD)
        IntTime = Args.IntTimeFun(Args.ColJD);
        if numel(IntTime)==1
            Nt      = size(T, 1);
            if Nt==0
                Nt = 1;
            end
            IntTime = repmat(IntTime, Nt, 1);
        end
        
    else
        IntTime = Args.IntTimeFun(T.(Args.ColJD));
    end

    if isempty(Args.ColIntTime)
        T = IntTime;
    else
        T.(Args.ColIntTime) = IntTime;
    end
    
end
