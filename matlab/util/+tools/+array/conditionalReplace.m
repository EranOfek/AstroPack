function M=conditionalReplace(M,A,B,V)
    % Perform an operation of the type: M(A>B)=V, where M and A are arrays, using mex.
    %   To compile the code use:
    %       mex -O CXXFLAGS="\$CXXFLAGS -mavx" mex_conditionalReplace_single.cpp
    %       mex -O CXXFLAGS="\$CXXFLAGS -mavx" mex_conditionalReplace_double.cpp
    % Input  : - M
    %          - A
    %          - B
    %          - V
    % Output : - M
    % Author : Eran Ofek (Jul 2024)
    % Example: M=rand(1700,1700); A=rand(1700,1700); B=0.5; V=0;
    %          tic;for I=1:1:100, M(A>B)=V;end,toc
    %          tic;for I=1:1:100, M = tools.array.conditionalReplace(M, A, B, V);end,toc

    
    
    if isa(M,'single')
        M = mex_conditionalReplace_single(M,A,B,V);
    elseif isa(M,'double')
        M = mex_conditionalReplace_double(M,A,B,V);
    else
        error('Can treat only single or double inputs');
    end
end