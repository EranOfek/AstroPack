function [Sum, N, Mean] = sumInRange(Array, MinVal, MaxVal)
    % Approximate summation of all elements that are not NaN and between MinVal and MaxVal (mex).
    %   This function is equivalent to:
    %   F=~isnan(A) & A>0.25 & A<0.75; S1=sum(A(F),'all'); N1=sum(F,'all');
    %   but it is based on an optimized  mex function.
    % Input  : - Array.
    %          - Min Val.
    %          - Max Val.
    % Output : - Approximate sum (maybe inaccurate for single arrays) of
    %            all values in the input array which are not NaN and larger
    %            than MinVal and smaller than MaxVal.
    %          - Number of elements in the array which satisfy the
    %            summation condition.
    %          - Mean value.
    % Author : Eran Ofek (2024 Aug) 
    % Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp" sumInRange.cpp
    % Example: A=rand(1700,1700);
    %          tic;for I=1:1:100, [S,N]=sumInRange(A,0.25,0.75); end,toc, tic; for I=1:1:100, F=~isnan(A) & A>0.25 & A<0.75; S1=sum(A(F),'all'); N1=sum(F,'all'); end,toc, [N1-N], [S1-S]
    

    [Sum, N] = tools.array.mex.sumInRange(Array, MinVal, MaxVal);
    
    if nargout>2
        Mean = Sum./N;
    end

end
