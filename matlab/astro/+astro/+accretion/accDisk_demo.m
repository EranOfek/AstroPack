function accDisk_demo()
    % Worked examples for the astro.accretion.accDisk_* functions.
    % Package: astro.accretion
    % Description: (1) Reproduces a fiducial single-object AGN
    %              calculation (M_BH = 1e8 Msun, Eddington ratio 0.1,
    %              z=0.5, an SDSS-g-like passband 4000-5500 Angstrom)
    %              and plots R_eff(lambda) across the band; (2)
    %              demonstrates the vectorized/grid calling convention
    %              by evaluating R_eff simultaneously on a grid of BH
    %              masses (rows) x wavelengths (columns).
    % Author : (fill in your name) (Sep 2026)
    % Example: astro.accretion.accDisk_demo();

    M_BH    = 1e8;      % Msun
    EddRat  = 0.1;      % Eddington ratio
    z       = 0.5;
    Lambda1 = 4000;     % Angstrom, observed
    Lambda2 = 5500;     % Angstrom, observed

    % ---- single object ----
    Result = astro.accretion.accDisk_effectiveRadiusBand(M_BH, EddRat, Lambda1, Lambda2, z, 'MdotUnits','Edd');

    R1 = astro.accretion.accDisk_effectiveRadius(M_BH, EddRat, Lambda1, z, 'MdotUnits','Edd');
    R2 = astro.accretion.accDisk_effectiveRadius(M_BH, EddRat, Lambda2, z, 'MdotUnits','Edd');

    fprintf('R_eff(%d A)        = %.3f Rg\n', Lambda1, R1./Result.Rg);
    fprintf('R_eff(%d A)        = %.3f Rg\n', Lambda2, R2./Result.Rg);
    fprintf('R_eff(pivot=%.0fA)  = %.3f Rg\n', Result.LambdaPivot, Result.RPivot_Rg);
    fprintf('R_eff (band, flux-weighted) = %.3f Rg\n', Result.Reff_Rg);

    Lam  = linspace(Lambda1, Lambda2, 50);
    Rlam = astro.accretion.accDisk_effectiveRadius(M_BH, EddRat, Lam, z, 'MdotUnits','Edd') ./ Result.Rg;

    figure;
    plot(Lam, Rlam, 'LineWidth', 1.5);
    xlabel('Observed wavelength [Angstrom]');
    ylabel('R_{eff} [R_g]');
    title('Disk effective radius across the passband');
    grid on;

    % ---- vectorized grid example: M_BH (rows) x Lambda (columns) ----
    % An Nx1 mass vector combined with a 1xM wavelength vector broadcasts
    % to an NxM grid, computed with no explicit loop over the grid in
    % the calling code (accDisk_effectiveRadius itself still solves one
    % root per grid point internally, but the API call is a single
    % vectorized expression).
    MBHgrid = [1e7; 1e8; 1e9];           % 3x1
    LamGrid = [3000 4000 5000 6000];     % 1x4

    [~, RrgGrid] = astro.accretion.accDisk_effectiveRadius(MBHgrid, EddRat, LamGrid, z, 'MdotUnits','Edd');

    fprintf('\nGrid example: R_eff [R_g], rows = M_BH = [1e7 1e8 1e9] Msun, columns = Lambda = [3000 4000 5000 6000] A\n');
    disp(RrgGrid);

    figure;
    imagesc(LamGrid, 1:3, log10(RrgGrid));
    set(gca, 'YTick', 1:3, 'YTickLabel', {'1e7','1e8','1e9'});
    xlabel('Observed wavelength [Angstrom]');
    ylabel('M_{BH} [M_\odot]');
    Cb = colorbar;
    Cb.Label.String = 'log_{10} R_{eff} [R_g]';
    title('Vectorized R_{eff}(M_{BH}, \lambda) grid');
end
