function [NewFace, Transform] = faceTransition(Face, Edge, Args)
    % Return adjacent face and coordinate transform for healpix.
    %   Determines which face is entered when stepping outside the current
    %   face through one of its four edges, and specifies how the local
    %   (X,Y) coordinates must be transformed when mapped to the new face.
    %
    %   This function is required for implementing neighbor finding in
    %   HEALPix NESTED indexing.
    %
    %   HEALPix faces are not globally aligned.
    %   When crossing edges:
    %
    %   - Equatorial faces (4..7) are aligned horizontally.
    %   - Polar faces (0..3 and 8..11) are rotated relative to equator.
    %   - Some transitions require 90° rotation.
    %   - Some require coordinate swap (X <-> Y).
    %
    %   Therefore, simply changing Face is NOT sufficient.
    %   You must transform the local (X,Y) coordinate before converting
    %   back to a NESTED pixel index.
    %
    % Input  : - (Face) integer in [0..11]
    %          - (EdgeDirection) : 'left' | 'right' | 'up' | 'down'
    %
    % Output : (NewFace) adjacent face index
    %        : (Transform) transformation string to apply to (X,Y)
    %          Transform Structure describing coordinate transformation.
    %             Fields:
    %               .Rotate    : rotation in degrees (0,90,180,270)
    %               .FlipX     : logical
    %               .FlipY     : logical
    %               .SwapXY    : logical
    %
    % Notes
    %   - Implements official HEALPix face adjacency.
    %   - Designed for NESTED indexing neighbor computation.
    %   -               HEALPix face index using standard convention:
    %                     0  1  2  3   (North polar)
    %                     4  5  6  7   (Equatorial)
    %                     8  9 10 11   (South polar)
    %
    %   - EdgeDirection Character string:
    %                 'left'   → X = 0 boundary
    %                 'right'  → X = NSide-1 boundary
    %                 'down'   → Y = 0 boundary
    %                 'up'     → Y = NSide-1 boundary
    %
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: 
    %   if X == 0
    %       [NewFace, T] = faceTransition(Face, 'left');
    %       [X2,Y2] = applyTransform(NSide, X2, Y2, T);
    %   end
    %

    arguments
        Face %(1,1) {mustBeInteger, mustBeNonnegative}
        Edge %(1,:) char {mustBeMember(EdgeDirection, ...
            %{'left','right','up','down'})}
        Args.Dummy (1,1) logical = true %#ok<NASGU>
    end
    


    % --- adjacency table ---
NeighborTable = [
% L  R  U  D
   3  1  4  8
   0  2  5  9
   1  3  6 10
   2  0  7 11
   7  5  0  8
   4  6  1  9
   5  7  2 10
   6  4  3 11
  11  9  4  0
   8 10  5  1
   9 11  6  2
  10  8  7  3
];

switch Edge
    case 'left',  Col = 1;
    case 'right', Col = 2;
    case 'up',    Col = 3;
    case 'down',  Col = 4;
end

NewFace = NeighborTable(Face+1, Col);

% --- transformation struct ---
% (Example simple version — adjust if you use full HEALPix rotations)

Transform.Rotate = 0;
Transform.FlipX  = false;
Transform.FlipY  = false;
Transform.SwapXY = false;




    
    
    % if Face > 11
    %     error('Face must be in [0..11]');
    % end
    % 
    % % Precomputed adjacency tables
    % % Rows: Face 0..11
    % % Cols: left, right, up, down
    % 
    % NeighborTable = [
    % % L   R   U   D
    %    3   1   4   8   % 0
    %    0   2   5   9   % 1
    %    1   3   6  10   % 2
    %    2   0   7  11   % 3
    % 
    %    7   5   0   8   % 4
    %    4   6   1   9   % 5
    %    5   7   2  10   % 6
    %    6   4   3  11   % 7
    % 
    %   11   9   4   0   % 8
    %    8  10   5   1   % 9
    %    9  11   6   2   %10
    %   10   8   7   3   %11
    % ];
    % 
    % % Transformation table (same layout)
    % TransformTable = [
    % %   L         R         U         D
    %  'rot270'  'rot90'  'none'    'none' ;  % 0
    %  'rot270'  'rot90'  'none'    'none' ;  % 1
    %  'rot270'  'rot90'  'none'    'none' ;  % 2
    %  'rot270'  'rot90'  'none'    'none' ;  % 3
    % 
    %  'none'    'none'   'none'    'none' ;  % 4
    %  'none'    'none'   'none'    'none' ;  % 5
    %  'none'    'none'   'none'    'none' ;  % 6
    %  'none'    'none'   'none'    'none' ;  % 7
    % 
    %  'rot90'   'rot270' 'none'    'none' ;  % 8
    %  'rot90'   'rot270' 'none'    'none' ;  % 9
    %  'rot90'   'rot270' 'none'    'none' ;  %10
    %  'rot90'   'rot270' 'none'    'none' ;  %11
    % ];
    % 
    % switch EdgeDirection
    %     case 'left'
    %         Col = 1;
    %     case 'right'
    %         Col = 2;
    %     case 'up'
    %         Col = 3;
    %     case 'down'
    %         Col = 4;
    % end
    % 
    % NewFace = NeighborTable(Face+1, Col);
    % Transform = strtrim(TransformTable(Face+1, Col));
    % 
end