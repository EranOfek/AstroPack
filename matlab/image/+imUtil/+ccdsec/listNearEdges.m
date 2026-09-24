function [FlagNearEdge, DistToEdge] = listNearEdges(List, CCDSEC, EdgeDist)
    % Given a list of [X,Y] coordinates and image boundries flag near edge positions.
    %   See also: imUtil.ccdsec.selectNearEdges
    % Input  : - Two column matrix of [X,Y] positions to test.
    %          - A 4 elemen vector of [Xmin, Xmax, Ymin, Ymax] makking the
    %            image bounderies.
    %          - (EdgeDist) Threshold edge distance. Default is 10.
    % Output : - A column vector of logical which length equal to the
    %            number of positions to test.
    %            True is for positions that are near edge (distance from
    %            edge <EdgeDist).
    %          - A vector of distances of each position to the nearest
    %            edge.
    % Author : Eran Ofek (2026 Jan) 
    % Example: [FlagNearEdge, DistToEdge] = imUtil.ccdsec.listNearEdges([X,Y], [1 100 1 200], 10);

    arguments
        List
        CCDSEC
        EdgeDist    = 10;
    end

    % Extract coordinates
    X = List(:,1);
    Y = List(:,2);

    % Extract CCD boundaries
    Xmin = CCDSEC(1);
    Xmax = CCDSEC(2);
    Ymin = CCDSEC(3);
    Ymax = CCDSEC(4);

    % Distances to each edge
    DistLeft   = X - Xmin;
    DistRight  = Xmax - X;
    DistBottom = Y - Ymin;
    DistTop    = Ymax - Y;

    % Distance to nearest edge
    DistToEdge = min([DistLeft, DistRight, DistBottom, DistTop], [], 2);

    % Flag positions near the edge
    FlagNearEdge = DistToEdge < EdgeDist;

end
