function [S2, S, N, Mean, M2] = sum2InRange(Array, MinVal, MaxVal)
    % Sum of squares of an array for all values that are not NaN and between MinVal and MaxVal (mex).
    %   This function is equivalent to:
    %   F=~isnan(A) & A>0.25 & A<0.75; S2=sum(A(F).^2,'all'); S=sum(A(F),'all'); N1=sum(F,'all');
    %   but it is based on an optimized  mex function.
    % Input  : - Array.
    %          - Min Val.
    %          - Max Val.
    % Output : - Approximate sum of squares (maybe inaccurate for single arrays) of
    %            all values in the input array which are not NaN and larger
    %            than MinVal and smaller than MaxVal.
    %          - Approximate sum.
    %          - Number of elements in the array which satisfy the
    %            summation condition.
    %          - Mean value.
    %          - Second (non central) moment. Calculated using N-1.
    % Author : Eran Ofek (2024 Aug) 
    % Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp -mavx" LDFLAGS="\$LDFLAGS -fopenmp" sum2InRange.cpp
    % Example: A=rand(1700,1700);
    %          tic;for I=1:1:100, [S2,S,N]=sum2InRange(A,0.25,0.75); end,toc, tic; for I=1:1:100, F=~isnan(A) & A>0.25 & A<0.75; S1=sum(A(F),'all'); N1=sum(F,'all'); end,toc, [N1-N], [S1-S]

    [S2, S, N] = tools.array.mex.sum2InRange(Array, MinVal, MaxVal);
    
    if nargout>3g
        Mean = S./N;
        if nargout>4
            M2 = sqrt(S2./(N-1) - S.^2./((N-1).^2) );
        end
    end

end
