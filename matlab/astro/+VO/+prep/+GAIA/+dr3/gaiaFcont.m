function Fcont = gaiaFcont(RA, Dec, Mag, Args)
    % Light of the Gaia DR3 neighbors in the photometric aperture of each source, relative to its own.
    % Description: For a circular Gaussian PSF of FWHM Args.FWHM, every
    %              Gaia DR3 source within Args.NeighRad of a target adds
    %              10^(-0.4*(G_neighbor - G_target)) times the fraction of
    %              its PSF that falls into an aperture of radius
    %              Args.AperRad centered on the target, over that fraction
    %              for a centered source:
    %                FCONT = sum_j 10^(-0.4*(G_j - G)) * EE(d_j) / EE(0).
    %              The target itself is excluded: the Gaia source within
    %              SelfDist of it whose G agrees within SelfDMag (the
    %              GAIADR3 and GAIADR3spec positions are the same J2016
    %              positions). The value does not depend on the image, so it
    %              can be stored in a catalog (VO.prep.GAIA.dr3.addFcontGAIADR3spec)
    %              and used by the calibrator audit (PhotCalibTrans,
    %              issue #1454).
    %              The neighbors are read from the GAIADR3 catsHTM by HTM
    %              cell; the catalog index is loaded once per MATLAB session.
    % Input  : - RA [rad], vector.
    %          - Dec [rad], vector.
    %          - Gaia G magnitude of the targets, vector.
    %          * ...,key,val,...
    %            'FWHM'     - PSF FWHM [arcsec]. Default 5.
    %            'AperRad'  - Aperture radius [arcsec]. Default 7.5
    %                         (6 pix of LAST, FLUX_APER_3).
    %            'NeighRad' - Neighbor search radius [arcsec]. Default 30.
    %            'SelfDist' - Max distance of the target's own Gaia entry
    %                         [arcsec]. Default 0.5.
    %            'SelfDMag' - Max |dG| of the target's own Gaia entry [mag].
    %                         Default 0.1.
    %            'CatName'  - catsHTM catalog of the neighbors. Default 'GAIADR3'.
    %            'MagCol'   - Magnitude column of CatName. Default 'phot_g_mean_mag'.
    %            'NfilesInHDF' - Cells per data file of CatName. Default 100.
    % Output : - FCONT, a column vector with an element per target; NaN
    %            where the target has no finite position or magnitude.
    % Author : D. Kovaleva (Oct 2026)
    % Example: F = VO.prep.GAIA.dr3.gaiaFcont(RA, Dec, G);
    %          M = catsHTM.cone_search('GAIADR3spec', RA0, Dec0, 600);
    %          F = VO.prep.GAIA.dr3.gaiaFcont(M(:,1), M(:,2), M(:,14), 'FWHM', 4);

    arguments
        RA
        Dec
        Mag
        Args.FWHM          (1,1) double = 5
        Args.AperRad       (1,1) double = 7.5
        Args.NeighRad      (1,1) double = 30
        Args.SelfDist      (1,1) double = 0.5
        Args.SelfDMag      (1,1) double = 0.1
        Args.CatName       char         = 'GAIADR3'
        Args.MagCol        char         = 'phot_g_mean_mag'
        Args.NfilesInHDF   (1,1) double = 100
    end

    ARCSEC_RAD = 180/pi*3600;

    RA    = double(RA(:));
    Dec   = double(Dec(:));
    Mag   = double(Mag(:));
    Ntarg = numel(RA);
    Fcont = nan(Ntarg, 1);
    Valid = isfinite(RA) & isfinite(Dec) & isfinite(Mag);
    if ~any(Valid)
        return;
    end

    % projection center: the mean direction (safe across RA = 0)
    V = [mean(cos(Dec(Valid)).*cos(RA(Valid))), mean(cos(Dec(Valid)).*sin(RA(Valid))), mean(sin(Dec(Valid)))];
    Center = [mod(atan2(V(2), V(1)), 2*pi), atan2(V(3), hypot(V(1), V(2)))];
    RadMax = max(celestial.coo.sphere_dist_fast(Center(1), Center(2), RA(Valid), Dec(Valid))) + ...
             Args.NeighRad / ARCSEC_RAD;

    % Gaia neighbors around the targets
    [GRA, GDec, GMag] = loadNeighbors(Args.CatName, Args.MagCol, Args.NfilesInHDF, Center, RadMax);
    Fcont(Valid) = 0;
    if isempty(GMag)
        return;
    end
    [Gx, Gy] = celestial.proj.pr_gnomonic(GRA, GDec, ARCSEC_RAD, Center);
    [Cx, Cy] = celestial.proj.pr_gnomonic(RA, Dec, ARCSEC_RAD, Center);

    % aperture fraction of a Gaussian PSF offset by D [arcsec]
    Sig = Args.FWHM / (2*sqrt(2*log(2)));
    Dg  = (0:0.05:Args.NeighRad + 0.05).';
    EE  = encircledGauss(Dg, Args.AperRad, Sig);

    % target-neighbor pairs
    [Gy, Ord] = sort(Gy);
    Gx   = Gx(Ord);
    GMag = GMag(Ord);
    IndV = find(Valid);
    Ind  = VO.search.search_sortedY_multi([Gx, Gy], Cx(IndV), Cy(IndV), -Args.NeighRad);
    It   = reshape(repelem(IndV, [Ind.Nmatch].'), [], 1);   % a column also for a single target
    Ig   = vertcat(Ind.Ind);
    D    = vertcat(Ind.Dist);
    dG   = GMag(Ig) - Mag(It);

    % the target itself: per target, the closest pair if it is close enough
    % and its G agrees (a G mismatch pushes the score out of reach)
    Score = D + 100 * (abs(dG) > Args.SelfDMag);
    [~, OrdP] = sortrows([It, Score]);
    First    = [true; diff(It(OrdP)) ~= 0];
    SelfPair = OrdP(First);
    SelfPair = SelfPair(Score(SelfPair) < Args.SelfDist);
    Keep = true(numel(D), 1);
    Keep(SelfPair) = false;

    Contrib = 10.^(-0.4 * dG(Keep)) .* interp1(Dg, EE, D(Keep)) ./ EE(1);
    Sum     = accumarray(reshape(It(Keep), [], 1), Contrib(:), [Ntarg 1]);   % a scalar indexed by false is 0x0
    Fcont(Valid) = Sum(Valid);
end


function [GRA, GDec, GMag] = loadNeighbors(CatName, MagCol, NfilesInHDF, Center, Radius)
    % RA, Dec [rad] and magnitude of the CatName sources in the cone, from
    % the populated HTM cells overlapping it (index cached per session)
    persistent Cache
    if isempty(Cache) || ~strcmp(Cache.CatName, CatName) || ~strcmp(Cache.MagCol, MagCol)
        [IF, IV]   = catsHTM.get_index_filename(CatName);
        [HTM, DataHTM] = catsHTM.load_htm_ind(IF, IV);
        ColCell    = catsHTM.load_colcell(CatName);
        Cache = struct('CatName',CatName, 'MagCol',MagCol, 'HTM',HTM, 'DataHTM',DataHTM, ...
                       'Ncol',numel(ColCell), 'MagIdx',find(strcmpi(ColCell, MagCol), 1));
        if isempty(Cache.MagIdx)
            error('VO:prep:GAIA:dr3:gaiaFcont:MissingMagCol', 'Column "%s" not found in %s.', MagCol, CatName);
        end
    end

    ID = celestial.htm.htm_search_cone(Cache.HTM, Center(1), Center(2), Radius);
    ID = ID(Cache.DataHTM(ID, 13) > 0);          % populated cells only
    Parts = cell(numel(ID), 1);
    for K = 1:numel(ID)
        C = catsHTM.load_cat(CatName, ID(K), [], Cache.Ncol, NfilesInHDF);
        if ~isempty(C)
            if size(C,2) ~= Cache.Ncol && size(C,1) == Cache.Ncol
                C = C.';
            end
            Parts{K} = double(C(:, [1 2 Cache.MagIdx]));
        end
    end
    G = vertcat(Parts{:});
    if isempty(G)
        G = zeros(0, 3);
    end
    G = G(isfinite(G(:,3)) & ...
          celestial.coo.sphere_dist_fast(Center(1), Center(2), G(:,1), G(:,2)) <= Radius, :);
    GRA  = G(:,1);
    GDec = G(:,2);
    GMag = G(:,3);
end


function F = encircledGauss(D, R, Sig)
    % Fraction of a circular Gaussian (sigma Sig) centered at distance D inside a circle of radius R
    %   F(D) = int_0^R (r/Sig^2) exp(-(r^2+D^2)/(2 Sig^2)) I0(r D/Sig^2) dr,
    %   with the exponentially scaled Bessel function for stability.
    Rg = linspace(0, R, 400);
    F  = trapz(Rg, (Rg ./ Sig^2) .* exp(-(Rg - D).^2 ./ (2*Sig^2)) .* besseli(0, Rg .* D ./ Sig^2, 1), 2);
end
