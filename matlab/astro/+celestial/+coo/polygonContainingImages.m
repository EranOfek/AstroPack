function Result = polygonContainingImages(Corn, Args)
    % from a set of image corners get a minimal polygon containing all the input images
    %     Optional detailed description
    % Input  : - a cell array of image corners [deg]: {ra1, dec1; ra2, dec2; ra3, dec3; ra4, dec4}      
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Jun) 
    % Example: 
    % 
    arguments
        Corn
        Args.A                 = [];
        Args.B                 = [];
    end
    % Assume rects is a cell array of Nx1, each cell a 4x2 matrix of RA, Dec in degrees
    points = [];
    for i = 1:length(rects)
        points = [points; rects{i}];  % Stack all vertices
    end
    % convert to 3D
    RA  = deg2rad(points(:,1));
    Dec = deg2rad(points(:,2));
    
    x = cos(Dec) .* cos(RA);
    y = cos(Dec) .* sin(RA);
    z = sin(Dec);
    V = [x, y, z];  % Each row is a unit vector
    % Compute Spherical Convex Hull
    K = convhull(V); 
    % Extract Polygon Boundary (Outline)
    edges = [K(:, [1 2]); K(:, [2 3]); K(:, [3 1])];
    edges = sort(edges, 2);  % Canonical ordering
    [unique_edges, ~, ic] = unique(edges, 'rows');
    counts = accumarray(ic, 1);
    boundary_edges = unique_edges(counts == 1, :);
    %
    boundary_points =  order_boundary_edges(boundary_edges);
    % back to RA, Dec:
    x = boundary_points(:,1);
    y = boundary_points(:,2);
    z = boundary_points(:,3);
    
    RA_poly  = atan2(y, x);
    Dec_poly = asin(z);
    
    RA_poly = mod(rad2deg(RA_poly), 360);
    Dec_poly = rad2deg(Dec_poly);
end

function loop = order_boundary_edges(edges)
% Input: edges - Nx2 array of vertex indices (each row is an edge)
% Output: loop - ordered 1xM vector of vertex indices forming a closed polygon

    if isempty(edges)
        loop = [];
        return;
    end

    % Create adjacency map (undirected)
    adj = containers.Map('KeyType', 'int32', 'ValueType', 'any');
    for i = 1:size(edges,1)
        a = edges(i,1);
        b = edges(i,2);
        if ~isKey(adj, a)
            adj(a) = [];
        end
        if ~isKey(adj, b)
            adj(b) = [];
        end
        adj(a) = [adj(a), b];
        adj(b) = [adj(b), a];
    end

    % Start at a vertex with degree 1 or arbitrary
    v_start = edges(1,1);
    loop = [v_start];
    visited = false(size(edges,1),1);

    % Walk the loop
    current = v_start;
    prev = -1;

    while true
        neighbors = adj(current);
        % Exclude the previous point to avoid backtracking
        next = neighbors(neighbors ~= prev);

        if isempty(next)
            break;  % Closed loop reached
        end

        prev = current;
        current = next(1);  % Follow the next neighbor
        if current == loop(1)
            break;  % Loop closed
        end
        loop(end+1) = current;
    end
end