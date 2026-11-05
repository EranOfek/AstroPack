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
    %     With 'Orientation','north' the reference images are North-up
    %     instead (PA=0): the coverage holds as well, since the overlap of
    %     the 7x5 grid absorbs the rotation of the sub-images, but the
    %     subtraction has to rotate the reference by the full angle.
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
    %            Default is 'mount'.
    %          'Prune' - logical, mark the redundant reference images near
    %            the poles as not used. Default is true.
    %          'PruneMinDec' - [deg] only reference images with |Dec| above
    %            this are pruning candidates. Default is 45.
    %          'PruneSubSize' - [pix] size of the sub-crop (with margin for its
    %            rotation) that must stay inside one used reference image.
    %            If empty: 320 for 'mount' (a 272 pix sub-crop rotated by up
    %            to 10 deg), 385 for 'north' (rotated by any angle).
    %            Default is [].
    %          'PruneStep' - [pix] step of the sub-crop positions tested in
    %            each candidate. The tested squares are PruneSubSize+PruneStep
    %            wide, so that any PruneSubSize square inside the candidate
    %            is inside one of them. Default is 64.
    %          'Verbose' - Default is true.
    % Output : - table of reference images: mount, telescope, RA, Dec (centre),
    %            RA1..RA4, Dec1..Dec4 (corners), PA [deg] (rotation of the
    %            image X axis from East towards North, as the 'PA' of
    %            AstroWCS.buildSimpleWCS), use (logical), all angles in deg.
    %            The row number is the reference ID.
    %          - table of sub-image centres: mount, telescope, RA, Dec.
    % Author : Yossi Shvartzvald (2025 Jul)
    % Example: [LAST_RefIm_Grid, LAST_SubIm_Grid] = pipeline.last.reference.buildRefGrid('Save2mat',true);
    arguments
        Args.Save2mat              = false;
        Args.FileName              = 'LAST_refGrid.mat';
        Args.GridFile              = '~/matlab/data/LAST/RefGrid/LAST_Grid_comb.mat';
        Args.RefImSizePix          = 1716;    % reference-grid image size [pix]
        Args.PixScale              = 1.25;    % [arcsec/pix]
        Args.Orientation           = 'mount'; % 'mount'|'north'
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
    LAST_RefIm_Grid.mount     = Imnt;
    LAST_RefIm_Grid.telescope = Itel;
    [LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec] = mountOffset(RA0, Dec0, dRA, dDec);

    % PA: direction of the mount-frame +dRA axis at the reference centre,
    % from East towards North in the tangent plane of the centre
    Eps = 0.01;   % [deg]
    [RAp, Decp] = mountOffset(RA0, Dec0, dRA+Eps, dDec);
    [RAm, Decm] = mountOffset(RA0, Dec0, dRA-Eps, dDec);
    [Xp, Yp] = tanProj(RAp, Decp, LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec);
    [Xm, Ym] = tanProj(RAm, Decm, LAST_RefIm_Grid.RA, LAST_RefIm_Grid.Dec);
    switch lower(Args.Orientation)
        case 'mount'
            LAST_RefIm_Grid.PA = atan2(Yp-Ym, Xp-Xm).*RAD;
        case 'north'
            LAST_RefIm_Grid.PA = zeros(height(LAST_RefIm_Grid),1);
        otherwise
            error('Unknown Orientation: %s', Args.Orientation);
    end

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
    LAST_SubIm_Grid.mount     = Imnt;
    LAST_SubIm_Grid.telescope = Itel;
    [LAST_SubIm_Grid.RA, LAST_SubIm_Grid.Dec] = mountOffset(LAST_Grid.RA(Imnt), LAST_Grid.Dec(Imnt), ...
        Telescope_offset_Grid.dRA(Itel) + SubIm_offset_Grid.dRA(Ioff), ...
        Telescope_offset_Grid.dDec(Itel) + SubIm_offset_Grid.dDec(Ioff));

    % redundant reference images near the poles
    LAST_RefIm_Grid.use = true(height(LAST_RefIm_Grid),1);
    if Args.Prune
        if isempty(Args.PruneSubSize)
            if strcmpi(Args.Orientation,'north')
                Args.PruneSubSize = 385;
            else
                Args.PruneSubSize = 320;
            end
        end
        LAST_RefIm_Grid.use = pruneGrid(LAST_RefIm_Grid, Args);
    end
    if Args.Verbose
        fprintf('buildRefGrid: %d reference images, %d used\n', height(LAST_RefIm_Grid), nnz(LAST_RefIm_Grid.use));
    end

    if Args.Save2mat
        save(Args.FileName,'LAST_RefIm_Grid','LAST_SubIm_Grid');
    end
end

% internal functions
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
    % inverse of tanProj
    RAD = 180/pi;
    X   = Xi./RAD;
    Y   = Eta./RAD;
    Den = cosd(Dec0) - Y.*sind(Dec0);
    RA  = mod(RA0 + atan2(X, Den).*RAD, 360);
    Dec = atan2((sind(Dec0) + Y.*cosd(Dec0)).*cosd(RA-RA0), Den).*RAD;
end

function Use = pruneGrid(Grid, Args)
    % greedy removal of the reference images that are not needed to keep
    % every PruneSubSize square inside one used reference image
    PixScale = Args.PixScale/3600;
    Half     = Args.RefImSizePix/2;
    Test     = Args.PruneSubSize + Args.PruneStep;       % [pix] tested square
    Pos      = (-Half+Test/2):Args.PruneStep:(Half-Test/2);
    Pos      = unique([Pos, Half-Test/2]);
    [Px, Py] = meshgrid(Pos, Pos);
    % corners of every tested square, in the candidate's own frame [pix]
    Cx = Px(:) + [-1 1 1 -1].*Test/2;
    Cy = Py(:) + [-1 -1 1 1].*Test/2;
    Nsq = numel(Px);

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
