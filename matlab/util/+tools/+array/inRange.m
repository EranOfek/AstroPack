function Result = inRange(Array, Range, Type)
    % Check if values in array are within some ranges
    % Input  : - An array.
    %          - A two column matrix of [Min Max] ranges.
    %          - If true then requires the values to be within all the
    %            ranges. If false, then within one of the ranges.
    %            Default is true.
    % Output : - An array of logicals indicating if the values in the input
    %            array are within all or one of the ranges.
    % Author : Eran Ofek (Nov 2022)
    % Example: tools.array.inRange(rand(5,3),[0.1 0.2; 0.4 0.9],0)
   
    arguments
        Array
        Range
        Type logical   = true;
    end
    
    Nr = size(Range,1);
    if Type
        Result = true(size(Array));
    else
        Result = false(size(Array));
    end
    for Ir=1:1:Nr
        if Type
            Result = Result & (Array>=Range(Ir,1) & Array<=Range(Ir,2));
        else
            Result = Result | (Array>=Range(Ir,1) & Array<=Range(Ir,2));
        end
    end
    
end