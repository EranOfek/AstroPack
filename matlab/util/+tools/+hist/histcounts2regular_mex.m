function [Result] = histcounts2regular_mex(X, Y, EdgesX, EdgesY, IsEdges)
    % Fast 2D histogram in a regular grid (mex)
    %   This is about 15 times faster compared to histcounts2
    % Input  : - Vector of X coordinates.
    %          - Vector of Y coordinates.
    %          - Vector of X edges.
    %          - Vector of Y edges.
    %          - A logical indicating if the EdgesX, EdgesY are Edges
    %            (true) or triplets of [Start, End, StepSize] (false).
    %            Default is true.
    % Output : - A matrix of counts in each histogram bin (uint32).
    % Compilation: mex CXXFLAGS="\$CXXFLAGS -O3 -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_histcounts2regular.cpp
    %              mex CXXFLAGS="\$CXXFLAGS -O3 -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_histcounts2regularNE_double.cpp
    % Author : Eran Ofek (2024 Aug) 
    % Example: X=rand(1e6,1); Y=rand(1e6,1); E=(0:0.01:1);
    %          N=tools.hist.histcounts2regular_mex(X,Y,E,E);
    %          N1=tools.hist.histcounts2regular_mex(X,Y,[0 1 0.01],[0 1 0.01], false);

    arguments
        X
        Y
        EdgesX
        EdgesY
        IsEdges logical   = true;
    end
    
    switch class(X)
        case 'double'
            if IsEdges
                Result=tools.hist.mex.mex_histcounts2regular_double(X, Y, EdgesX, EdgesY);
            else
                Result=tools.hist.mex.mex_histcounts2regularNE_double(X, Y, EdgesX(1), EdgesX(2), EdgesX(3), EdgesY(1), EdgesY(2), EdgesY(3));
            end
        %case 'single'
        %    Result=tools.hist.mex.mex_histcounts2regular_single(X, Y, EdgesX, EdgesY);
        otherwise
            error('All inputs must be double class');
    end
end
