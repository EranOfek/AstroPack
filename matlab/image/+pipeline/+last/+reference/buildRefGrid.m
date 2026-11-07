function [LAST_RefIm_Grid, LAST_SubIm_Grid] = buildRefGrid(Args)
    % Building a grid for LAST reference images
    %     The reference images are laid out on the LAST field (mount) grid:
    %     7x5 reference images per telescope footprint, at the offsets of
    %     RefIm_offset_Grid (see ~/matlab/data/LAST/RefGrid/regenerate_grid.m),
    %     so that adjacent reference images overlap by >= 410 pix
    %     ("AstroPack - Reference Images Scheme", issue #1045).
    %     The offsets are fixed in the mount frame, as the telescopes and
    %     cameras of an equatorial mount are. Each reference image is
    %     therefore rotated from its local North by the convergence of the
    %     meridians between the mount and the reference centre (PA column),
    %     i.e. it is aligned to the sub-images of its mount up to the
    %     mechanical camera rotation (a few deg), and its corners are those
    %     of the WCS that buildRefImages builds with this PA.
    %     By default ('Orientation','north', 'RotateAboveDec',70) only the
    %     reference images of the mounts with |Dec| >= 70 are aligned to
    %     the mount frame, and all the others are North-up (PA=0). Below
    %     |Dec| 70 the overlap of the 7x5 grid absorbs the rotation of the
    %     sub-images and North-up needs at most a few deg more rotation at
    %     subtraction; nearer the poles North turns by tens of deg across a
    %     telescope footprint, and North-up images leave holes above |Dec| 85.
    %     'RotateAboveDec',Inf gives an all North-up grid, and
    %     'Orientation','mount' an all mount-frame grid.
    %     Near the poles the patterns of neighbouring mounts overlap heavily:
    %     a reference image with |Dec| >= 'PruneMinDec' is marked as not
    %     used (use=false) when every 'PruneSubSize' square in it, at a step
    %     of 'PruneStep', is fully inside one of the other used reference
    %     images.
    % Input  : * ...,key,val,...
    %          'Save2mat' - logical, save output to a .mat file.
    %            Default is false.
    %          'FileName' - File name for 'Save2mat'.
    %            Default is 'LAST_refGrid.mat'.
    %          'GridFile' - File with the mount grid and the telescope,
    %            reference and sub-image offsets.
    %            Default is '~/matlab/data/LAST/RefGrid/LAST_Grid_comb.mat'.
    %          'RefImSizePix' - reference-grid image size in pixels (square).
    %            Default is 1716.
    %          'PixScale' - [arcsec/pix]. Default is 1.25.
    %          'Orientation' - orientation of the reference images:
    %            'mount' - aligned to the mount frame (see above).
    %            'north' - North-up (PA=0).
    %            Default is 'north'.
    %          'RotateAboveDec' - [deg] with 'north': the reference images of
    %            the mounts with |Dec| >= this are aligned to the mount frame
    %            instead (all 35 of a mount, by the mount Dec). Ignored with
    %            'mount'. Default is 70.
    %          'Prune' - logical, mark the redundant reference images near
    %            the poles as not used. Default is true.
    %          'PruneMinDec' - [deg] only reference images with |Dec| above
    %            this are pruning candidates. Default is 45.
    %          'PruneSubSize' - [pix] size of the sub-crop (with margin for its
    %            rotation) that must stay inside one used reference image.
    %            If empty: 320 for an image aligned to the mount frame (a 272
    %            pix sub-crop rotated by up to 10 deg), 385 for a North-up one
    %            (rotated by any angle).
    %            Default is [].
    %          'PruneStep' - [pix] step of the sub-crop positions tested in
    %            each candidate. The tested squares are PruneSubSize+PruneStep
    %            wide, so that any PruneSubSize square inside the candidate
    %            is inside one of them. Default is 64.
    %          'Verbose' - Default is true.
    % Output : - table of reference images, one row per image, all angles in deg:
    %            REFID - reference image ID: a running number of every image
    %                    of the grid, including those with use=false. The rows
    %                    are ordered by the Dec of the image centre (rounded
    %                    to 1e-6 deg), south to north, then by increasing RA,
    %                    and REFID is the row number. It is written to the
    %                    REFID header keyword and used in the file names.
    %            OBJECT - reference image name, 'RefRRR.RRR+DD.DDD' from the
    %                    centre (e.g. 'Ref022.099-86.843'); 3 decimals make
    %                    it unique in the grid. Written to the OBJECT keyword.
    %            FIELDID - the LAST_Grid field (FieldName) whose 7x5 pattern
    %                    of a telescope the image belongs to. This is NOT
    %                    necessarily the nearest field: near the poles a
    %                    corner image of a field is often closer to the
    %                    centre of the adjacent field (about 5% of the images,
    %                    43% above |Dec| 80). Written to the FIELDID keyword.
    %            telescope - telescope (1..4) of the field the image belongs to.
    %            offset - position of the image in the 7x5 pattern of the
    %                    telescope (row of RefIm_offset_Grid).
    %            RA, Dec - centre; RA1..RA4, Dec1..Dec4 - corners.
    %            PA - rotation of the image X axis from East towards North,
    %                    as the 'PA' of AstroWCS.buildSimpleWCS.
    %            use - false for the images that buildRefImages does not
    %                    build (see 'Prune').
    %          - table of sub-image centres: FIELDID, telescope, RA, Dec.
    % Author : Yossi Shvartzvald (2025 Jul)
    % Example: [LAST_RefIm_Grid, LAST_SubIm_Grid] = pipeline.last.reference.buildRefGrid('Save2mat',true);
    arguments
        Args.Save2mat              = false;
        Args.FileName              = 'LAST_refGrid.mat';
        Args.GridFile              = '~/matlab/data/LAST/RefGrid/LAST_Grid_comb.mat';
        Args.RefImSizePix          = 1716;    % reference-grid image size [pix]
        Args.PixScale              = 1.25;    % [arcsec/pix]
        Args.Orientation           = 'north'; % 'north'|'mount'
        Args.RotateAboveDec        = 70;      % [deg]
        Args.Prune logical         = true;
        Args.PruneMinDec           = 45;      % [deg]
        Args.PruneSubSize          = [];      % [pix]
        Args.PruneStep             = 64;      % [pix]
        Args.Verbose logical       = true;
    end
    %
    G = load(Args.GridFile, 'LAST_Grid', 'Telescope_offset_Grid', 'RefIm_offset_Grid', 'SubIm_offset_Grid');
    LAST_Grid             = G.LAST_Grid;
    Telescope_offset_Grid = G.Telescope_offset_Grid;
    RefIm_offset_Grid     = G.RefIm_offset_Grid;
    SubIm_offset_Grid     = G.SubIm_offset_Grid;

    RAD      = 180/pi;
    PixScale = Args.PixScale/3600;               % [deg/pix]
    HalfPix  = Args.RefImSizePix/2;               % [pix]
    Nmount   = height(LAST_Grid);
    Ntel     = height(Telescope_offset_Grid);

    % reference images: one row per (mount, telescope, offset), in this order
    [Ioff, Itel, Imnt] = ndgrid(1:height(RefIm_offset_Grid), 1:Ntel, 1:Nmount);
    Ioff = Ioff(:); Itel = Itel(:); Imnt = Imnt(:);
    dRA  = Telescope_offset_Grid.dRA(Itel)  + RefIm_offset_Grid.dRA(Ioff);
    dDec = Telescope_offset_Grid.dDec(Itel) + RefIm_offset_Grid.dDec(Ioff);
    RA0  = LAST_Grid.RA(Imnt);
    Dec0 = LAST_Grid.Dec(Imnt);

    LAST_RefIm_Grid = table();
    LAST_RefIm_Grid.FIELDID   = fieldId(LAST_Grid, Imnt);
    LAST_RefIm_Grid.telescope = Itel;
    LAST_RefIm_Grid.offset    = Ioff;
    [LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec] = mountOffset(RA0, Dec0, dRA, dDec);
    LAST_RefIm_Grid.RA = mod(LAST_RefIm_Grid.RA, 360);

    % PA: direction of the mount-frame +dRA axis at the reference centre,
    % from East towards North in the tangent plane of the centre
    Eps = 0.01;   % [deg]
    [RAp, Decp] = mountOffset(RA0, Dec0, dRA+Eps, dDec);
    [RAm, Decm] = mountOffset(RA0, Dec0, dRA-Eps, dDec);
    [Xp, Yp] = tanProj(RAp, Decp, LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec);
    [Xm, Ym] = tanProj(RAm, Decm, LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec);
    switch lower(Args.Orientation)
        case 'mount'
            Rot = true(height(LAST_RefIm_Grid),1);
        case 'north'
            Rot = abs(Dec0) >= Args.RotateAboveDec;
        otherwise
            error('Unknown Orientation: %s', Args.Orientation);
    end
    LAST_RefIm_Grid.PA = atan2(Yp-Ym, Xp-Xm).*RAD.*Rot;

    % corners of the rotated square (the footprint of the WCS that
    % buildRefImages builds), in the order (+dRA,+dDec), (+dRA,-dDec),
    % (-dRA,-dDec), (-dRA,+dDec) in the image frame
    CornX = [1 1 -1 -1].*HalfPix.*PixScale;
    CornY = [1 -1 -1 1].*HalfPix.*PixScale;
    CosPA = cosd(LAST_RefIm_Grid.PA);
    SinPA = sind(LAST_RefIm_Grid.PA);
    for Ic = 1:4
        Xi  = CosPA.*CornX(Ic) - SinPA.*CornY(Ic);
        Eta = SinPA.*CornX(Ic) + CosPA.*CornY(Ic);
        [LAST_RefIm_Grid.(sprintf('RA%d',Ic)), LAST_RefIm_Grid.(sprintf('Dec%d',Ic))] = ...
            tanInv(Xi, Eta, LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec);
    end
    LAST_RefIm_Grid = movevars(LAST_RefIm_Grid, 'PA', 'After', 'Dec4');

    % sub-images
    [Ioff, Itel, Imnt] = ndgrid(1:height(SubIm_offset_Grid), 1:Ntel, 1:Nmount);
    Ioff = Ioff(:); Itel = Itel(:); Imnt = Imnt(:);
    LAST_SubIm_Grid = table();
    LAST_SubIm_Grid.FIELDID   = fieldId(LAST_Grid, Imnt);
    LAST_SubIm_Grid.telescope = Itel;
    [LAST_SubIm_Grid.RA, LAST_SubIm_Grid.Dec] = mountOffset(LAST_Grid.RA(Imnt), LAST_Grid.Dec(Imnt), ...
        Telescope_offset_Grid.dRA(Itel) + SubIm_offset_Grid.dRA(Ioff), ...
        Telescope_offset_Grid.dDec(Itel) + SubIm_offset_Grid.dDec(Ioff));

    % redundant reference images near the poles
    LAST_RefIm_Grid.use = true(height(LAST_RefIm_Grid),1);
    if Args.Prune
        if isempty(Args.PruneSubSize)
            SubSize = 385 - 65.*Rot;   % 320 aligned to the mount frame, 385 North-up
        else
            SubSize = Args.PruneSubSize + zeros(size(Rot));
        end
        LAST_RefIm_Grid.use = pruneGrid(LAST_RefIm_Grid, SubSize, Args);
    end
    % REFID: the row number after ordering by Dec, then by RA
    [~, Ord] = sortrows([round(LAST_RefIm_Grid.Dec, 6), LAST_RefIm_Grid.RA]);
    LAST_RefIm_Grid = LAST_RefIm_Grid(Ord,:);
    LAST_RefIm_Grid.REFID  = (1:height(LAST_RefIm_Grid)).';
    LAST_RefIm_Grid.OBJECT = compose("Ref%07.3f%+07.3f", mod(round(LAST_RefIm_Grid.RA, 3), 360), round(LAST_RefIm_Grid.Dec, 3));
    LAST_RefIm_Grid = movevars(LAST_RefIm_Grid, {'REFID','OBJECT'}, 'Before', 'FIELDID');

    if Args.Verbose
        fprintf('buildRefGrid: %d reference images, %d used\n', height(LAST_RefIm_Grid), nnz(LAST_RefIm_Grid.use));
    end

    if Args.Save2mat
        save(Args.FileName,'LAST_RefIm_Grid','LAST_SubIm_Grid');
    end
end

% internal functions
function FieldID = fieldId(LAST_Grid, Imnt)
    % LAST_Grid field name (as in the OBJECT keyword of the raw images) of the rows Imnt
    if any(strcmp(LAST_Grid.Properties.VariableNames, 'FieldName'))
        FieldID = str2double(string(LAST_Grid.FieldName(Imnt)));
    else
        FieldID = Imnt;
    end
end

function [RA, Dec] = mountOffset(RA0, Dec0, dRA, dDec)
    % position at the mount-frame offset (dRA, dDec) [deg] from the mount
    % centre (RA0, Dec0) [deg]: the offset is taken at the equator and moved
    % along a great circle with the same distance and position angle
    RAD = 180/pi;
    [Dist, PA] = celestial.coo.sphere_dist(0, 0, dRA, dDec, 'deg');
    [Dec, RA]  = reckon(Dec0, RA0, Dist.*RAD, PA.*RAD);
    RA = mod(RA, 360);   % reckon returns longitudes in [-180, 180]
end

function [Xi, Eta] = tanProj(RA, Dec, RA0, Dec0)
    % gnomonic projection [deg] around (RA0, Dec0): Xi to the East, Eta to the North
    CosC = sind(Dec0).*sind(Dec) + cosd(Dec0).*cosd(Dec).*cosd(RA-RA0);
    Xi   = cosd(Dec).*sind(RA-RA0)./CosC.*180/pi;
    Eta  = (cosd(Dec0).*sind(Dec) - sind(Dec0).*cosd(Dec).*cosd(RA-RA0))./CosC.*180/pi;
end

function [RA, Dec] = tanInv(Xi, Eta, RA0, Dec0)
    % inverse of tanProj. Dec is taken from the length of the projection
    % on the equatorial plane, which is also right for a point on the far
    % side of the pole (Den < 0), e.g. a corner of a reference image that
    % contains the pole
    RAD = 180/pi;
    X   = Xi./RAD;
    Y   = Eta./RAD;
    Den = cosd(Dec0) - Y.*sind(Dec0);
    RA  = mod(RA0 + atan2(X, Den).*RAD, 360);
    Dec = atan2(sind(Dec0) + Y.*cosd(Dec0), sqrt(X.^2 + Den.^2)).*RAD;
end

function Use = pruneGrid(Grid, SubSize, Args)
    % greedy removal of the reference images that are not needed to keep
    % every SubSize(I) square of image I inside one used reference image
    PixScale = Args.PixScale/3600;
    Half     = Args.RefImSizePix/2;
    % corners of every tested square, in the candidate's own frame [pix],
    % for each sub-crop size
    Sizes = unique(SubSize);
    [CxS, CyS] = deal(cell(numel(Sizes),1));
    for K = 1:numel(Sizes)
        Test     = Sizes(K) + Args.PruneStep;       % [pix] tested square
        Pos      = (-Half+Test/2):Args.PruneStep:(Half-Test/2);
        Pos      = unique([Pos, Half-Test/2]);
        [Px, Py] = meshgrid(Pos, Pos);
        CxS{K}   = Px(:) + [-1 1 1 -1].*Test/2;
        CyS{K}   = Py(:) + [-1 -1 1 1].*Test/2;
    end

    Ngrid = height(Grid);
    Use   = true(Ngrid,1);
    Cand  = find(abs(Grid.Dec) >= Args.PruneMinDec);
    [~, Ord] = sort(abs(Grid.Dec(Cand)), 'descend');   % the most redundant first
    Cand = Cand(Ord);

    [Vx, Vy, Vz] = celestial.coo.coo2cosined(Grid.RA./180.*pi, Grid.Dec./180.*pi);
    MaxSep = cosd(2*sqrt(2)*Half*PixScale);   % squares can overlap only within this distance
    for I = Cand(:).'
        Near = find(Use & (Vx.*Vx(I) + Vy.*Vy(I) + Vz.*Vz(I)) > MaxSep);
        Near(Near==I) = [];
        if isempty(Near)
            continue;
        end
        % the tested squares on the sky, in the candidate's tangent plane [pix]
        K  = find(Sizes==SubSize(I));
        Cx = CxS{K}; Cy = CyS{K};
        Nsq = size(Cx,1);
        CosPA = cosd(Grid.PA(I)); SinPA = sind(Grid.PA(I));
        Sx = CosPA.*Cx - SinPA.*Cy;
        Sy = SinPA.*Cx + CosPA.*Cy;
        Covered = false(Nsq,1);
        for J = Near(:).'
            [Qx, Qy] = tanProj([Grid.RA1(J) Grid.RA2(J) Grid.RA3(J) Grid.RA4(J)], ...
                               [Grid.Dec1(J) Grid.Dec2(J) Grid.Dec3(J) Grid.Dec4(J)], Grid.RA(I), Grid.Dec(I));
            Qx = Qx./PixScale; Qy = Qy./PixScale;
            Covered = Covered | allInsideQuad(Sx, Sy, Qx, Qy);
            if all(Covered)
                break;
            end
        end
        if all(Covered)
            Use(I) = false;
        end
    end
end

function Flag = allInsideQuad(Sx, Sy, Qx, Qy)
    % Flag(i) is true when all the points Sx(i,:), Sy(i,:) are inside the
    % convex quadrilateral Qx, Qy (vertices in order, either direction)
    Sgn  = sign((Qx(2)-Qx(1))*(Qy(3)-Qy(1)) - (Qy(2)-Qy(1))*(Qx(3)-Qx(1)));
    Flag = true(size(Sx,1),1);
    for K = 1:4
        K2   = mod(K,4) + 1;
        Side = (Qx(K2)-Qx(K)).*(Sy-Qy(K)) - (Qy(K2)-Qy(K)).*(Sx-Qx(K));
        Flag = Flag & all(Sgn.*Side >= 0, 2);
    end
end
