function M=conditionalReplace(M,A,B,V)
    % Perform an operation of the type: M(A>B)=V, where M and A are arrays, using mex.
    %   Only effiennt for large array (in this case may be x5 times faster
    %   than matlab).
    %   To compile the code use:
    %       mex -O CXXFLAGS="\$CXXFLAGS -mavx" mex_conditionalReplace_single.cpp
    %       mex -O CXXFLAGS="\$CXXFLAGS -mavx" mex_conditionalReplace_double.cpp
    % Input  : - M (array; single or double)
    %          - A (array)
    %          - B (scalar)
    %          - V (scalar)
    % Output : - M
    % Author : Eran Ofek (Jul 2024)
    % Compilation :  mex CXXFLAGS='$CXXFLAGS -mavx' mex_conditionalReplace_double.cpp
    % Example: M=rand(1700,1700); A=rand(1700,1700); B=0.5; V=0;
    %          tic;for I=1:1:100, M(A>B)=V;end,toc
    %          tic;for I=1:1:100, M = tools.array.conditionalReplace(M, A, B, V);end,toc

    
    
    if isa(M,'single')
        M = tools.array.mex.mex_conditionalReplace_single(M,A,B,V);
    elseif isa(M,'double')
        M = tools.array.mex.mex_conditionalReplace_double(M,A,B,V);
    else
        error('Can treat only single or double inputs');
    end
end