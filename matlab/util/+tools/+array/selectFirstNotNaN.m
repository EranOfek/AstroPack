function [Result] = selectFirstNotNaN(varargin)
    % Given arrays with NaN/values create a new array that contains the non-NaN element that appears in the first input array.
    % Input  : * Arbitrary number of columns
    %            All the arrays must have the same size.
    %            Therefore, the order of input arrays has meaning.
    % Output : - An array containing (in each element) the first non-NaN
    %            value that appear in one of the input arguments.
    % Author : Eran Ofek (2026 Mar) 
    % Example: a=[1 NaN NaN NaN 2]; b=[NaN 3 NaN NaN 5]; c=nan(1,5);  
    %          R=tools.array.selectFirstNotNaN(a,b,c) 

    Narg = numel(varargin);
    for Iarg=1:1:Narg
        if Iarg==1
            Flag = ~isnan(varargin{Iarg});
            Result = nan(size(Flag));
        else
            Flag = ~isnan(varargin{Iarg}) & isnan(Result);
        end

        Result(Flag) = varargin{Iarg}(Flag);
    end

end
