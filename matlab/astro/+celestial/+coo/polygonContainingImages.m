function [RA_poly, Dec_poly] = polygonContainingImages(Rectangles, varargin)
% CONVEX_HULL_SPHERICAL_RECTANGLES computes the minimal convex spherical polygon
% that fully contains all input spherical rectangles.
%
% Input:
%   Rectangles - cell array of Nx4 matrices, each with 4 rows (corners):
%                [RA1 Dec1; RA2 Dec2; RA3 Dec3; RA4 Dec4] in degrees
% Optional:
%   'Plot' - if true, visualize the input rectangles and resulting polygon
%
% Output:
%   RA_poly, Dec_poly - vectors of coordinates of the convex polygon boundary (degrees)
% Example: Corn = {[20, 10; 21, 10; 21, 11; 20, 12],[22, 9; 21, 9; 21, 8; 22, 8]};
%   [RA, Dec] = celestial.coo.polygonContainingImages(Corn,'Plot',true)


% Parse optional arguments
p = inputParser;
p.addParameter('Plot', false, @islogical);
p.parse(varargin{:});
plot_flag = p.Results.Plot;

% Step 1: Gather all corner points
all_vertices = [];
for i = 1:length(Rectangles)
    all_vertices = [all_vertices; Rectangles{i}];
end

% Step 2: Convert to 3D Cartesian coordinates
V = sph2vec(all_vertices(:,1), all_vertices(:,2));

% Step 3: Compute 3D convex hull
K = convhull(V);  % returns triangular faces (indices into V)

% Step 4: Extract unique hull vertex indices
unique_indices = unique(K(:));
hull_points = V(unique_indices, :);

% Step 5: Project hull points to local tangent plane for ordering
% Use centroid as reference
centroid = mean(hull_points);
centroid = centroid / norm(centroid);

% Construct local tangent basis at centroid
z = centroid;
x = null(z);  % 3x2 matrix: orthonormal basis in tangent plane
proj_coords = hull_points * x;  % 2D coordinates in local plane
angles = atan2(proj_coords(:,2), proj_coords(:,1));
[~, sort_idx] = sort(angles);
ordered_indices = unique_indices(sort_idx);

% Step 6: Convert ordered 3D points back to RA/Dec
polygon_cart = V(ordered_indices, :);
[RA_poly, Dec_poly] = vec2sph(polygon_cart);

% Optional plotting
if plot_flag
    figure; hold on; grid on;
    for i = 1:length(Rectangles)
        RA_rect = Rectangles{i}(:,1);
        Dec_rect = Rectangles{i}(:,2);
        RA_rect = [RA_rect; RA_rect(1)];
        Dec_rect = [Dec_rect; Dec_rect(1)];
        plot(RA_rect, Dec_rect, 'b-');
    end
    RA_poly_closed = [RA_poly; RA_poly(1)];
    Dec_poly_closed = [Dec_poly; Dec_poly(1)];
    plot(RA_poly_closed, Dec_poly_closed, 'r-', 'LineWidth', 2);
    xlabel('RA (deg)'); ylabel('Dec (deg)');
    title('Convex Hull of Spherical Rectangles');
    axis equal;
end

end

function v = sph2vec(RA_deg, Dec_deg)
RA = deg2rad(RA_deg);
Dec = deg2rad(Dec_deg);
x = cos(Dec) .* cos(RA);
y = cos(Dec) .* sin(RA);
z = sin(Dec);
v = [x, y, z];
end

function [RA, Dec] = vec2sph(v)
x = v(:,1); y = v(:,2); z = v(:,3);
RA = atan2(y, x);
RA = mod(rad2deg(RA), 360);
Dec = asind(z);
end


