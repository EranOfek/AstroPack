function [Az, WallAlt, TelAlt] = obstructionModel(Itel, Walls, Telescopes, Args)
    % General sky-obstruction model for a telescope within an enclosure.
    %
    % For each azimuth direction the function returns the minimum altitude above
    % which the sky is unobstructed, considering:
    %   (1) enclosure walls (with optional linearly-varying height), and
    %   (2) neighbouring telescope tubes (modelled as cylinders).
    %
    % Both wall and telescope obstructions are returned on the same sorted
    % azimuth grid covering [0, 360) degrees with a uniform step size.
    %
    % Input  : - Itel       : Index of the observing telescope in Telescopes.
    %                         Default: 3.
    %          - Walls      : Struct array of wall segments. Each element has:
    %              .Start  [1x2]  (X,Y) position of segment start [m]
    %              .Stop   [1x2]  (X,Y) position of segment end   [m]
    %              .Height [1x2]  wall-top height at Start and Stop [m]
    %                         Height is linearly interpolated along the segment,
    %                         so [h, h] gives a flat (constant-height) wall.
    %                         If empty, the LAST observatory default is used.
    %          - Telescopes : Struct array of telescope descriptors. Each element:
    %              .Pos       [1x2]  (X,Y) horizontal position [m]
    %              .HeightCOA        height of centre of axes above floor [m]
    %              .Diam             tube outer diameter used for obstruction [m]
    %                         If empty, the LAST observatory default is used.
    %          * ...,key,val,...
    %            'Rotation'  Global building rotation: angle of the building's
    %                        +X axis measured CCW from true East [deg].
    %                        Default: 5.7 (LAST building).
    %            'AzStep'    Output azimuth grid step [deg]. Controls resolution
    %                        of both WallAlt and TelAlt outputs. Default: 0.5.
    %            'NWallPts'  Number of sample points per wall segment used
    %                        internally before binning onto the grid.
    %                        Must satisfy NWallPts >= 360/AzStep to guarantee
    %                        at least one sample per bin; auto-raised if needed.
    %                        Default: 1000.
    %            'Buffer'    Altitude safety buffer added to all obstruction
    %                        values [deg]. Default: 3.5.
    %
    % Output : - Az      [N x 1]  Azimuth grid, sorted, [0 .. 360-AzStep] deg.
    %                             N = round(360 / AzStep).
    %          - WallAlt [N x 1]  Wall obstruction altitude on Az grid [deg].
    %          - TelAlt  [N x 1]  Tel-to-tel obstruction altitude on Az grid [deg].
    %
    % Coordinate system
    %   Building frame: X along building length (≈ East), Y along width (≈ North).
    %   The building is rotated Rotation degrees CCW from true East.
    %   Sky azimuth: North = 0 deg, East = 90 deg, clockwise-positive.
    %   Conversion: AzSky = mod( (90 - Rotation) - atan2d(DY, DX) , 360 )
    %
    % Key algorithms
    %   Wall obstruction (vectorised over all wall sample points):
    %     DH    = wall-top height - ObsHt   (signed; negative if wall below axis)
    %     Alpha = atand( DH / D )           signed elevation to wall top
    %     Beta  = atand( R_obs / Lw )       extra angle from finite tube radius
    %     Alt   = Alpha + Beta + Buffer
    %   The formula handles all signs of DH without special-casing:
    %     DH > 0 -> obstructing wall, Alpha > 0
    %     DH = 0 -> wall at axis height, Alpha = 0, Beta still non-zero (close
    %              walls at telescope height can still obstruct via tube radius)
    %     DH < 0 -> wall below axis, negative Alpha partially cancels Beta
    %   Scattered wall points are then reduced to the output grid by taking the
    %   maximum Alt per bin (accumarray) and filling empty bins by circular
    %   linear interpolation.
    %
    %   Tel-to-tel obstruction (analytical closed form, no grid search):
    %     MaxChi = arcsin( (R_obs + R_block) / Dt )
    %   Derived by maximising the tangent-line elevation angle between two
    %   cylinders; the optimum satisfies sin(chi) = (R_obs+R_block)/Dt exactly.
    %   The azimuth-dependent envelope scales MaxChi by the fractional chord of
    %   the blocking cylinder intercepted by each look direction.
    %
    % Author : Claude + Eran Ofek (May 2025)
    % Example: [Az,WAlt,TAlt] = telescope.geometry.obstructionModel(3)
    %          [Az,WAlt,TAlt] = telescope.geometry.obstructionModel(3,[],[],'AzStep',0.1,'Buffer',2)

    arguments
        Itel                (1,1) double {mustBeInteger, mustBePositive} = 3
        Walls               struct                                       = struct([])
        Telescopes          struct                                       = struct([])
        Args.Rotation       (1,1) double                                 = 5.7
        Args.AzStep         (1,1) double {mustBePositive}                = 0.5
        Args.NWallPts       (1,1) double {mustBeInteger, mustBePositive} = 1000
        Args.Buffer         (1,1) double                                 = 3.5
    end

    % ── Fall back to LAST defaults if either input is empty ──────────────────
    if isempty(Walls) || isempty(Telescopes)
        [Walls, Telescopes] = defaultLAST();
    end

    % ── Common output azimuth grid ────────────────────────────────────────────
    % Sorted, uniform, covers [0, 360) with step AzStep.
    NAz = round(360 / Args.AzStep);
    Az  = (0 : Args.AzStep : Args.AzStep*(NAz-1)).';   % [NAz x 1]

    % ── Ensure NWallPts is large enough: at least MinPtsPerBin per output bin ─
    MinPtsPerBin = 3;
    NWallPts = max(Args.NWallPts, MinPtsPerBin * NAz);
    if NWallPts > Args.NWallPts
        warning('obstructionModel:NWallPtsRaised', ...
            'NWallPts raised from %d to %d to guarantee >= %d sample(s) per azimuth bin.', ...
            Args.NWallPts, NWallPts, MinPtsPerBin);
    end

    % ── Observing-telescope properties ───────────────────────────────────────
    ObsPos = Telescopes(Itel).Pos;          % [X, Y]  m
    ObsHt  = Telescopes(Itel).HeightCOA;   % m
    ObsR   = Telescopes(Itel).Diam / 2;    % tube radius  m

    % =========================================================================
    % 1. WALL OBSTRUCTION
    %
    %    Geometry (vertical plane through telescope and wall point):
    %
    %      wall top  *
    %               /|  DH = Wh - ObsHt
    %              / |
    %    obs axis *--+------------ D (horizontal) -----------+
    %
    %    Alpha = atand(DH / D)         elevation angle to wall top
    %    Lw    = hypot(DH, D)          slant distance to wall top
    %    Beta  = atand(R_obs / Lw)     extra angle: the tube itself occludes
    %                                  beyond the wall tip
    %    Alt   = Alpha + Beta + Buffer
    %
    %    After computing (AzScatter, AltScatter) for all sample points:
    %      - bin points onto Az grid by taking max per bin  (accumarray)
    %      - fill empty bins by circular linear interpolation
    % =========================================================================

    NWalls   = numel(Walls);
    TotalPts = NWalls * NWallPts;

    WxAll = zeros(TotalPts, 1);
    WyAll = zeros(TotalPts, 1);
    WhAll = zeros(TotalPts, 1);

    T = linspace(0, 1, NWallPts).';        % parameter along each segment

    for Iw = 1:NWalls
        Idx        = (Iw-1)*NWallPts + (1:NWallPts).';
        Seg        = Walls(Iw);
        WxAll(Idx) = Seg.Start(1) + T .* (Seg.Stop(1) - Seg.Start(1));
        WyAll(Idx) = Seg.Start(2) + T .* (Seg.Stop(2) - Seg.Start(2));
        WhAll(Idx) = Seg.Height(1) + T .* (Seg.Height(2) - Seg.Height(1));
    end

    % ── Vectorised obstruction for all wall sample points ────────────────────
    DX = WxAll - ObsPos(1);
    DY = WyAll - ObsPos(2);
    D  = hypot(DX, DY);                    % horizontal distance  [m]
    DH = WhAll - ObsHt;                    % wall top minus telescope axis [m]
    Lw = hypot(DH, D);                     % slant distance to wall top  [m]

    % Elevation angle to wall top.
    %   DH > 0 : wall top above axis -> Alpha > 0  (obstructing)
    %   DH = 0 : wall top at axis    -> Alpha = 0, but Beta still > 0
    %   DH < 0 : wall top below axis -> Alpha < 0, partially cancels Beta
    % All three cases are handled correctly by the same formula, matching
    % the behaviour of the original wall_alt function.  No special-casing.
    Alpha = atand(DH ./ D);                % [deg]  signed elevation to wall top
    Beta  = atand(ObsR ./ Lw);             % [deg]  extra angle from tube radius

    % Guard: wall sample coincident with telescope (D == 0) would give Inf.
    % This cannot occur in a valid layout but we zero it defensively.
    BadPts           = (D < 1e-9);
    Alpha(BadPts)    = 0;
    Beta(BadPts)     = 0;

    AltScatter = Alpha + Beta + Args.Buffer;

    % Building-frame atan2 -> sky azimuth  [0, 360)
    AzScatter = buildingToSky(atan2d(DY, DX), Args.Rotation);

    % ── Reduce scattered wall points onto the common Az grid ─────────────────
    % Map each scattered point to its nearest bin index (1-based, with wrap).
    BinIdx = mod(round(AzScatter ./ Args.AzStep), NAz) + 1;   % [1 .. NAz]

    % Maximum wall altitude within each bin  (NaN for empty bins).
    WallAlt = accumarray(BinIdx, AltScatter, [NAz, 1], @max, NaN);

    % Fill any empty bins by circular linear interpolation.
    WallAlt = fillCircular(Az, WallAlt);

    % =========================================================================
    % 2. TELESCOPE-TO-TELESCOPE OBSTRUCTION
    %
    %    Analytical maximum obstruction angle (two tangent cylinders):
    %      MaxChi = arcsin( (R_obs + R_block) / Dt )
    %
    %    Azimuth-dependent scaling: when looking at angle AzDiff away from the
    %    direct line of centres, the blocker's effective chord is
    %    sqrt(R_block^2 - Rperp^2) / R_block  (Rperp = Dt*sin(AzDiff)).
    %    Zero beyond the angular half-width  arcsin(R_block / Dt).
    %
    %    Computed directly on the Az grid; no interpolation needed.
    % =========================================================================

    TelAlt = zeros(NAz, 1);
    Ntel   = numel(Telescopes);

    for Jtel = 1:Ntel
        if Jtel == Itel
            continue
        end

        BlkPos = Telescopes(Jtel).Pos;
        BlkR   = Telescopes(Jtel).Diam / 2;

        % Horizontal displacement: observer -> blocker
        DDX = BlkPos(1) - ObsPos(1);
        DDY = BlkPos(2) - ObsPos(2);
        Dt  = hypot(DDX, DDY);

        % Guard: cylinders must not overlap
        if (ObsR + BlkR) >= Dt
            warning('obstructionModel:overlap', ...
                ['Cylinders of telescopes %d and %d overlap ' ...
                 '(ObsR+BlkR=%.3f >= Dt=%.3f); skipping pair.'], ...
                Itel, Jtel, ObsR + BlkR, Dt);
            continue
        end

        % Analytical maximum obstruction angle  [deg]
        MaxChi = asind((ObsR + BlkR) / Dt);

        % Sky azimuth from observer to blocker  [deg, 0..360)
        AzTT = buildingToSky(atan2d(DDY, DDX), Args.Rotation);

        % Angular half-width: beyond this the blocker is out of the beam
        DAzCut = asind(BlkR / Dt);            % [deg]

        % Signed azimuth difference in [-180, 180]
        AzDiff = mod(Az - AzTT + 180, 360) - 180;

        % Perpendicular distance from each look direction to blocker axis  [m]
        Rperp = Dt .* sind(AzDiff);

        % Fractional chord of blocker in this look direction
        Factor = sqrt(max(BlkR^2 - Rperp.^2, 0)) / BlkR;

        AltTT               = MaxChi .* Factor;
        AltTT(abs(AzDiff) > DAzCut) = 0;

        % Accumulate worst-case across all blocking telescopes
        TelAlt = max(TelAlt, AltTT);
    end

    TelAlt = TelAlt + Args.Buffer;

end  % obstructionModel


% =========================================================================
% LOCAL HELPER FUNCTIONS
% =========================================================================

function AzSky = buildingToSky(AzBuild, Rotation)
% Convert a standard atan2 angle in the building frame [deg] to sky azimuth.
%
%   Building frame : +X ≈ East, +Y ≈ North, CCW-positive (mathematical).
%   Sky azimuth    : North = 0, East = 90, clockwise-positive (astronomical).
%
%   The building's +X axis is tilted Rotation degrees CCW from true East, so:
%     AzSky = mod( (90 - Rotation) - AzBuild , 360 )
%
%   Sanity check (Rotation = 0):
%     +X direction (AzBuild=0)   -> AzSky = 90  (East)  ✓
%     +Y direction (AzBuild=90)  -> AzSky = 0   (North) ✓

    AzSky = mod((90 - Rotation) - AzBuild, 360);
end


function AltOut = fillCircular(Az, AltIn)
% Fill NaN entries in AltIn by circular linear interpolation on Az [0,360).
%
% The grid is treated as circular by tripling the data (shifted by ±360) so
% that interp1 can interpolate across the 0/360 seam without special casing.

    NanMask = isnan(AltIn);
    if ~any(NanMask)
        AltOut = AltIn;
        return
    end

    ValidAz  = Az(~NanMask);
    ValidAlt = AltIn(~NanMask);

    % Tile data across three periods to handle the circular boundary
    AzTiled  = [ValidAz - 360;  ValidAz;  ValidAz + 360];
    AltTiled = [ValidAlt;        ValidAlt; ValidAlt     ];

    AltOut        = AltIn;
    AltOut(NanMask) = interp1(AzTiled, AltTiled, Az(NanMask), 'linear');
end


function [Walls, Telescopes] = defaultLAST()
% Build the default LAST observatory configuration.
%
% Physical layout
%   A rectangular building of internal width BW (N-S) and length BL (E-W).
%   Full outer length BLw includes end walls.
%   Twelve telescopes in a 6 (E-W) x 2 (N-S) grid.
%   Telescope indices use MATLAB column-major order in the 2x6 layout:
%     Itel=1 : (row 1, col 1)  southernmost N-S, westernmost E-W
%     Itel=2 : (row 2, col 1)  northernmost N-S, westernmost E-W
%     ...
%     Itel=12: (row 2, col 6)  northernmost N-S, easternmost E-W

    % Building dimensions [m]
    BW  = 5.32;    % internal width  (N-S)
    BL  = 10.8;    % usable telescope-field length (E-W)
    BLw = 12.2;    % full outer length including end walls

    % Telescope array layout
    NRow = 6;      % columns along E-W
    NCol = 2;      % rows along N-S

    % Telescope mechanical parameters
    HeightCOA = 1.2;           % centre-of-axes height above floor [m]
    Diam      = 2 * 0.63;      % tube diameter [m]  (= 2 * r1 from original)

    % Wall heights [m]
    HwN = 1.2;   % North
    HwS = 1.2;   % South
    HwE = 1.2;   % East
    HwW = 2.7;   % West  (taller — matches original hwW)

    % Walls struct array (1x4): constant height per wall -> Height(1)==Height(2)
    Walls = struct( ...
        'Start',  { [0,   0  ], [0,   BW ], [0,   0  ], [BLw, 0  ] }, ...
        'Stop',   { [BLw, 0  ], [BLw, BW ], [0,   BW ], [BLw, BW ] }, ...
        'Height', { [HwN, HwN], [HwS, HwS], [HwE, HwE], [HwW, HwW] });
    % Wall index: 1=North, 2=South, 3=East, 4=West

    % Telescope positions (column-major, reproducing original Xall(Itel) logic)
    Xall = BL/NRow/2 : BL/NRow : BL;           % 1 x NRow
    Yall = (BW/NCol/2 : BW/NCol : BW).';        % NCol x 1

    XMat = repmat(Xall, [NCol, 1]);   % NCol x NRow
    YMat = repmat(Yall, [1,   NRow]); % NCol x NRow
    Xpos = XMat(:);                   % 12 x 1  (column-major)
    Ypos = YMat(:);

    Ntel = NRow * NCol;
    Telescopes = repmat( ...
        struct('Pos', [], 'HeightCOA', HeightCOA, 'Diam', Diam), 1, Ntel);
    for It = 1:Ntel
        Telescopes(It).Pos = [Xpos(It), Ypos(It)];
    end
end
