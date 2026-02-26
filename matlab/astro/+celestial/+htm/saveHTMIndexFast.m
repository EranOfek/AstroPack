function saveHTMIndexFast(Level, FileName, VarName, Attrib, Nsrc)
% Save HTM index HDF5 file computed analytically (fast, no struct build)
% Package: celestial.htm
% Description: Fast alternative to HDF5.save_htm_ind that computes the
%              13-column HTM index Data matrix directly from the level
%              number using vectorized level-by-level subdivision.
%              Avoids building the HTM struct array (htm_build)
%              and the O(N^2) Nsrc lookup of save_htm_ind.
%              1Output is identical to save_htm_ind.
% Input  :   - Level: HTM level (integer). Tree has levels 0..Level-1.
%                     Level L has 8*4^L cells. Total nodes = 8*(4^Level - 1)/3.
%            - FileName: HDF5 file name for output.
%            - VarName: Variable name for the HTM dataset.
%                       Default is derived from FileName (e.g., 'PS1DR2_HTM').
%            - Attrib: Cell array of attributes {Key,Val} for 'ColNames'
%                      dataset. Default is {}.
%            - Nsrc: Matrix [IndHTM, Nsrc] with source counts per leaf cell.
%                    Default is [].
% Output :   null
% Author : Dana Kovaleva (Feb 2026)
% Example: celestial.htm.saveHTMIndexFast(7, 'PS1DR2_htm.hdf5', 'PS1DR2_HTM', {}, Nsrc)

    %------------------------------------------------------------------
    % Handle default arguments
    %------------------------------------------------------------------
    if nargin < 3 || isempty(VarName)
        Tmp = regexp(FileName, '[/\\]', 'split');
        BaseName = Tmp{end};
        Parts = regexp(BaseName, '_', 'split');
        VarName = sprintf('%s_HTM', Parts{1});
    end
    if nargin < 4, Attrib = {}; end
    if nargin < 5, Nsrc = []; end

    % Total number of HTM nodes across all levels 0..Level-1
    % Level L (0-indexed depth) has 8*4^L nodes
    Nhtm = round(8 * (4^Level - 1) / 3);

    fprintf('  Building HTM index analytically (%d nodes, %d levels)...\n', Nhtm, Level);
    Tic = tic;

    % Pre-allocate output: [level, father, son1..4, poles1..6, nsrc]
    Data = nan(Nhtm, 13);

    % Build indexed Nsrc lookup for O(1) access per node
    % (avoids the O(N) linear scan per node in save_htm_ind)
    NsrcLookup = nan(Nhtm, 1);
    if ~isempty(Nsrc)
        ValidMask = Nsrc(:,1) >= 1 & Nsrc(:,1) <= Nhtm;
        NsrcLookup(Nsrc(ValidMask, 1)) = Nsrc(ValidMask, 2);
    end

    %------------------------------------------------------------------
    % Level 0: 8 root triangles (hardcoded geometry from htm_build)
    %------------------------------------------------------------------

    % North hemisphere (4 triangles): equatorial base + north pole vertex
    % South hemisphere (4 triangles): equatorial base + south pole vertex
    % Vertex coordinates in [Long, Lat] radians
    RootLong = zeros(8, 3);
    RootLat  = zeros(8, 3);
    for I = 1:4
        % North: [0,0], [pi/2,0], [0,pi/2] rotated by pi/2*(I-1)
        RootLong(I,:) = [0, pi/2, 0] + pi/2 * (I - 1);
        RootLat(I,:)  = [0, 0, pi/2];
    end
    for I = 1:4
        % South: [0,0], [0,-pi/2], [pi/2,0] rotated by pi/2*(I-1)
        RootLong(I+4,:) = [0, 0, pi/2] + pi/2 * (I - 1);
        RootLat(I+4,:)  = [0, -pi/2, 0];
    end

    % Convert root vertices to cosine directions (8 triangles x 3 components)
    [CD1_1, CD2_1, CD3_1] = celestial.coo.coo2cosined(RootLong(:,1), RootLat(:,1));
    [CD1_2, CD2_2, CD3_2] = celestial.coo.coo2cosined(RootLong(:,2), RootLat(:,2));
    [CD1_3, CD2_3, CD3_3] = celestial.coo.coo2cosined(RootLong(:,3), RootLat(:,3));

    CurrV1 = [CD1_1, CD2_1, CD3_1];  % 8x3 cosine directions of vertex 1
    CurrV2 = [CD1_2, CD2_2, CD3_2];  % 8x3 cosine directions of vertex 2
    CurrV3 = [CD1_3, CD2_3, CD3_3];  % 8x3 cosine directions of vertex 3

    % Compute poles for root triangles (vectorized)
    [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(CurrV1, CurrV2, CurrV3);

    % Fill Data for level 0
    Data(1:8, 1) = 0;       % level depth
    Data(1:8, 2) = NaN;     % father (roots have no parent)
    if Level > 1
        % Children start at index 9 (8 roots + 1)
        SonBase = 8 + (0:7)' * 4 + 1;
        Data(1:8, 3:6) = [SonBase, SonBase+1, SonBase+2, SonBase+3];
    end
    Data(1:8, 7)  = PL1;   Data(1:8, 8)  = PL2;
    Data(1:8, 9)  = PL3;   Data(1:8, 10) = PL4;
    Data(1:8, 11) = PL5;   Data(1:8, 12) = PL6;
    Data(1:8, 13) = NsrcLookup(1:8);

    %------------------------------------------------------------------
    % Levels 1..Level-1: vectorized subdivision
    %------------------------------------------------------------------
    PrevStartIdx = 1;

    for L = 1:Level-1
        Nprev  = size(CurrV1, 1);
        Nchild = Nprev * 4;
        StartIdx = round(1 + 8 * (4^L - 1) / 3);
        IsLeaf = (L == Level - 1);

        % Midpoints on great circles (normalized average of endpoints)
        Mid12 = CurrV1 + CurrV2;
        Mid12 = Mid12 ./ sqrt(sum(Mid12.^2, 2));
        Mid23 = CurrV2 + CurrV3;
        Mid23 = Mid23 ./ sqrt(sum(Mid23.^2, 2));
        Mid31 = CurrV3 + CurrV1;
        Mid31 = Mid31 ./ sqrt(sum(Mid31.^2, 2));

        % Assemble 4 children per parent (same vertex order as htm_build_son)
        % Child 1: V1, Mid12, Mid31
        % Child 2: V2, Mid23, Mid12
        % Child 3: V3, Mid31, Mid23
        % Child 4: Mid12, Mid23, Mid31
        NewV1 = zeros(Nchild, 3);
        NewV2 = zeros(Nchild, 3);
        NewV3 = zeros(Nchild, 3);

        NewV1(1:4:end,:) = CurrV1;   NewV2(1:4:end,:) = Mid12;  NewV3(1:4:end,:) = Mid31;
        NewV1(2:4:end,:) = CurrV2;   NewV2(2:4:end,:) = Mid23;  NewV3(2:4:end,:) = Mid12;
        NewV1(3:4:end,:) = CurrV3;   NewV2(3:4:end,:) = Mid31;  NewV3(3:4:end,:) = Mid23;
        NewV1(4:4:end,:) = Mid12;    NewV2(4:4:end,:) = Mid23;  NewV3(4:4:end,:) = Mid31;

        % Free memory from previous level before computing poles
        clear CurrV1 CurrV2 CurrV3 Mid12 Mid23 Mid31;

        % Compute poles for all children (vectorized)
        [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(NewV1, NewV2, NewV3);

        % Fill Data rows for this level
        Indices = StartIdx:(StartIdx + Nchild - 1);
        Data(Indices, 1) = L;  % level depth

        % Father: each group of 4 consecutive children shares one parent
        ParentIdx = repelem((PrevStartIdx:(PrevStartIdx + Nprev - 1))', 4);
        Data(Indices, 2) = ParentIdx;

        % Son indices (NaN for leaf nodes, computed for internal nodes)
        if ~IsLeaf
            NextStart = round(1 + 8 * (4^(L+1) - 1) / 3);
            ChildPos = (0:Nchild-1)';
            SonBase = NextStart + ChildPos * 4;
            Data(Indices, 3:6) = [SonBase, SonBase+1, SonBase+2, SonBase+3];
        end

        % Poles
        Data(Indices, 7)  = PL1;   Data(Indices, 8)  = PL2;
        Data(Indices, 9)  = PL3;   Data(Indices, 10) = PL4;
        Data(Indices, 11) = PL5;   Data(Indices, 12) = PL6;

        % Source counts
        Data(Indices, 13) = NsrcLookup(Indices);

        % Prepare cosine directions for next level
        CurrV1 = NewV1;
        CurrV2 = NewV2;
        CurrV3 = NewV3;
        PrevStartIdx = StartIdx;

        fprintf('    Level %d: %d cells done (%.1f s)\n', L, Nchild, toc(Tic));
    end

    %------------------------------------------------------------------
    % Save to HDF5 (same format as HDF5.save_htm_ind)
    %------------------------------------------------------------------
    AttribHTM = {'Table.Col.1','Level'; ...
              'Table.Col.2','Father'; ...
              'Table.Col.3','Son1'; ...
              'Table.Col.4','Son2'; ...
              'Table.Col.5','Son3'; ...
              'Table.Col.6','Son4'; ...
              'Table.Col.7', 'Poles1Lon';...
              'Table.Col.8', 'Poles1Lat';...
              'Table.Col.9', 'Poles2Lon';...
              'Table.Col.10','Poles2Lat';...
              'Table.Col.11','Poles3Lon';...
              'Table.Col.12','Poles3Lat';...
              'Table.Col.13','Nsrc'};
    HDF5.save(single(Data), FileName, VarName, AttribHTM);
    % Save column names dataset (for compatibility with load_htm_ind)
    HDF5.save([], FileName, 'ColNames', Attrib);

    fprintf('  HTM index saved: %s (%.1f s)\n', FileName, toc(Tic));

end


%==========================================================================
% LOCAL HELPER FUNCTIONS
%==========================================================================

function [PL1, PL2, PL3, PL4, PL5, PL6] = computeHTMPolesVec(CV1, CV2, CV3)
% Compute polysphere poles for N triangles (vectorized)
% Replicates celestial.htm.polysphere_poles for N triangles at once.
% Sorts vertices by position angle from centroid for consistent pole
% orientation, then computes cross products of consecutive edges.
% Input  : - CV1: Nx3 cosine direction matrix for vertex 1.
%          - CV2: Nx3 cosine direction matrix for vertex 2.
%          - CV3: Nx3 cosine direction matrix for vertex 3.
% Output : - PL1..PL6: Nx1 pole coordinates as
%            [Pole1Lon, Pole1Lat, Pole2Lon, Pole2Lat, Pole3Lon, Pole3Lat].

    N = size(CV1, 1);

    % Centroid of each triangle (mean of cosine directions)
    CenCD1 = (CV1(:,1) + CV2(:,1) + CV3(:,1)) / 3;
    CenCD2 = (CV1(:,2) + CV2(:,2) + CV3(:,2)) / 3;
    CenCD3 = (CV1(:,3) + CV2(:,3) + CV3(:,3)) / 3;
    [CenLong, CenLat] = celestial.coo.cosined2coo(CenCD1, CenCD2, CenCD3);

    % Vertex lon/lat for position angle computation
    [Long1, Lat1] = celestial.coo.cosined2coo(CV1(:,1), CV1(:,2), CV1(:,3));
    [Long2, Lat2] = celestial.coo.cosined2coo(CV2(:,1), CV2(:,2), CV2(:,3));
    [Long3, Lat3] = celestial.coo.cosined2coo(CV3(:,1), CV3(:,2), CV3(:,3));

    % Position angle from centroid to each vertex (for sorting)
    [~, PA1] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long1, Lat1);
    [~, PA2] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long2, Lat2);
    [~, PA3] = celestial.coo.sphere_dist_fast(CenLong, CenLat, Long3, Lat3);

    % Sort vertices by PA per triangle (consistent winding order)
    AllPA = [PA1, PA2, PA3];    % Nx3
    [~, SI] = sort(AllPA, 2);   % Nx3 sort indices per row

    % Gather cosine directions in sorted vertex order
    RowIdx = repmat((1:N)', 1, 3);
    LinIdx = sub2ind([N, 3], RowIdx, SI);

    AllCD1 = [CV1(:,1), CV2(:,1), CV3(:,1)];   % Nx3
    AllCD2 = [CV1(:,2), CV2(:,2), CV3(:,2)];
    AllCD3 = [CV1(:,3), CV2(:,3), CV3(:,3)];

    SortCD1 = AllCD1(LinIdx);   % Nx3 sorted
    SortCD2 = AllCD2(LinIdx);
    SortCD3 = AllCD3(LinIdx);

    % Sorted vertices as 3-component direction vectors
    SV1 = [SortCD1(:,1), SortCD2(:,1), SortCD3(:,1)];  % sorted vertex 1
    SV2 = [SortCD1(:,2), SortCD2(:,2), SortCD3(:,2)];  % sorted vertex 2
    SV3 = [SortCD1(:,3), SortCD2(:,3), SortCD3(:,3)];  % sorted vertex 3

    % Cross products of consecutive edges (inline cross_fast for speed):
    % Pole1 = cross(sv1, sv2), Pole2 = cross(sv2, sv3), Pole3 = cross(sv3, sv1)
    P1 = [SV1(:,2).*SV2(:,3) - SV1(:,3).*SV2(:,2), ...
          SV1(:,3).*SV2(:,1) - SV1(:,1).*SV2(:,3), ...
          SV1(:,1).*SV2(:,2) - SV1(:,2).*SV2(:,1)];
    P2 = [SV2(:,2).*SV3(:,3) - SV2(:,3).*SV3(:,2), ...
          SV2(:,3).*SV3(:,1) - SV2(:,1).*SV3(:,3), ...
          SV2(:,1).*SV3(:,2) - SV2(:,2).*SV3(:,1)];
    P3 = [SV3(:,2).*SV1(:,3) - SV3(:,3).*SV1(:,2), ...
          SV3(:,3).*SV1(:,1) - SV3(:,1).*SV1(:,3), ...
          SV3(:,1).*SV1(:,2) - SV3(:,2).*SV1(:,1)];

    % Convert pole direction vectors to lon/lat
    [PL1, PL2] = celestial.coo.cosined2coo(P1(:,1), P1(:,2), P1(:,3));
    [PL3, PL4] = celestial.coo.cosined2coo(P2(:,1), P2(:,2), P2(:,3));
    [PL5, PL6] = celestial.coo.cosined2coo(P3(:,1), P3(:,2), P3(:,3));

end
