function [Result] = sprintf2string(Format, Array)
    % Given an array of numbers (or chars) create a string array using sprintf formatting.
    % Input  : - Format. If this is a cell array of formats, then each
    %            element will be applied to its corresponding column in the
    %            output string array.
    %          - Array.
    % Output : - A string array.
    % Author : Eran Ofek (2024 Oct) 
    % Example: R=tools.string.sprintf2string('%03d',[2 3 4;1 2 3])
    %          R=tools.string.sprintf2string({'%03d','%04d'},[1 2;3 4])
    %          R=tools.string.sprintf2string({'%4.1f','%5s'},{1.5 'aa';2.5 'bb'})

    SizeArray = size(Array);
    if iscell(Format)
        N = numel(Array);
        Result = strings(SizeArray);
        for K=1:1:N
            [~, J] = ind2sub(SizeArray, K);
            
            if iscell(Array)
                Result(K) = sprintf(Format{J}, Array{K});
            else
                Result(K) = sprintf(Format{J}, Array(K));
            end
        end
    else
        N = numel(Array);
        Result = strings(size(Array));
        for K=1:1:N
            if iscell(Array)
                Result(K) = sprintf(Format, Array{K});
            else
                Result(K) = sprintf(Format, Array(K));
            end
        end
    end
    
end
