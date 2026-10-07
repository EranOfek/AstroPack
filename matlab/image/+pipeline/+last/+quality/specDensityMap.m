function [T, FigH] = specDensityMap(Args)
    % Map the spectral-calibrator surface density over the LAST field grid.
    %   For every field of the LAST all-sky tiling, sums the per-cell source
    %   counts of a catsHTM spectral catalog (default GAIADR3spec) over the HTM
    %   cells that fall inside the field, and converts them to a surface density
    %   and to an expected number of calibrators per crop.
    %
    %   Motivation: Gaia DR3 published sampled XP spectra for a selected subset
    %   of sources, and its sky coverage is strongly non-uniform - whole regions
    %   hold ~10% of the spectra per Gaia star that the typical sky holds. A LAST
    %   field there cannot reach the calibrator floor of the photometric fit no
    %   matter how good the night is, so the condition is a property of the
    %   pointing and can be known in advance rather than discovered per visit.
    %
    %   Counts come from the HTM index only (catsHTM.nsrc), so no catalog rows
    %   are read and the whole sky takes seconds rather than hours.
    %
    % Input  : * ...,key,val,...
    %            'CatName' - catsHTM catalog to map. Default 'GAIADR3spec'.
    %            'RefCatName' - Second catalog mapped the same way, to form a
    %                   per-field ratio (spectra per catalog source). Empty
    %                   skips it. Default 'GAIADR3'.
    %            'N_LonLat' - LAST tiling, forwarded to celestial.grid.tile_the_sky.
    %                   Default [88 30], i.e. the 1784-field LAST grid whose
    %                   index is the FIELDID of the data products.
    %            'CropArea' - Area of one crop [deg^2], used to turn the density
    %                   into an expected count per crop. Default 0.52.
    %            'Thresholds' - Calibrator floors to mark, expected count per
    %                   crop. Default [9 16]: 9 is the bare mathematical
    %                   minimum for the 8-parameter fit (one degree of
    %                   freedom), 16 is 2x the parameter count. A field below
    %                   the larger value cannot be calibrated reliably, below
    %                   the smaller one not at all.
    %            'FieldFile' - Text table of the official LAST field list
    %                   (RA,Dec,MinRA,MaxRA,MinDec,MaxDec,Ebv,Area), whose row
    %                   number is the field ID; it also supplies each field's
    %                   Ebv and exact area. Default '' regenerates the same
    %                   grid with celestial.grid.tile_the_sky, which gives
    %                   identical RA/Dec but no Ebv.
    %            'SpecFraction' - Fraction of catalog spectra that survive the
    %                   calibrator cuts (S/N, isolation, magnitude range).
    %                   Default 0.35, a central value: on four crops of
    %                   2026-10-04 the ratio PT_NCALI / (spectra in the crop)
    %                   was 0.12, 0.18, 0.44 and 0.57, so treat the predicted
    %                   count as an order-of-magnitude expectation, not a
    %                   threshold to trust per field.
    %            'Plot' - Produce the figure. Default true.
    %            'OutFile' - If not empty, save the figure (PNG) and write the
    %                   table as .csv next to it. Default ''.
    %            'Verbose' - Default true.
    % Output : - A table with one row per LAST field: Index (= FIELDID), RA, Dec,
    %            GalLat, Ebv, Nspec, SpecDens [deg^-2], Nref, RefDens, Ratio,
    %            ExpectedPerCrop, plus one logical column Below<N> per entry of
    %            'Thresholds'.
    %          - Handle of the figure (empty when 'Plot' is false).
    % Author : D. Kovaleva (Oct 2026)
    % Example: T = pipeline.last.quality.specDensityMap();
    %          T = pipeline.last.quality.specDensityMap('FieldFile','LAST_FieldsID.txt');
    %          T = pipeline.last.quality.specDensityMap('OutFile','/home/dana/specmap.png');
    %          T.Index(T.Below9)   % fields that cannot be calibrated at all

    arguments
        Args.CatName char        = 'GAIADR3spec'
        Args.RefCatName char     = 'GAIADR3'
        Args.N_LonLat            = [88 30]
        Args.CropArea (1,1) double = 0.52
        Args.Thresholds double    = [9 16]
        Args.FieldFile char       = ''
        Args.SpecFraction (1,1) double = 0.35
        Args.ObsLat (1,1) double  = 30.053
        Args.MinAlt (1,1) double  = 30
        Args.Plot logical        = true
        Args.OutFile char        = ''
        Args.Verbose logical     = true
    end

    RAD = 180./pi;

    % ---- LAST field grid; the row index is the FIELDID carried by the products
    Ebv = [];
    if ~isempty(Args.FieldFile) && isfile(Args.FieldFile)
        FT   = readtable(Args.FieldFile, 'Delimiter',',', 'ReadVariableNames',true);
        RA   = FT.RA(:)  ./ RAD;
        Dec  = FT.Dec(:) ./ RAD;
        Nfld = numel(RA);
        FieldAreaDeg = FT.Area(:) .* RAD.^2;     % steradian -> deg^2
        if ismember('Ebv', FT.Properties.VariableNames)
            Ebv = FT.Ebv(:);
        end
        if Args.Verbose
            fprintf('specDensityMap: field list from %s\n', Args.FieldFile);
        end
    else
        [TileList, TileArea] = celestial.grid.tile_the_sky(Args.N_LonLat(1), Args.N_LonLat(2));
        RA   = TileList(:,1);
        Dec  = TileList(:,2);
        Nfld = numel(RA);
        FieldAreaDeg = TileArea(:) .* RAD.^2;
        if isscalar(FieldAreaDeg)
            FieldAreaDeg = repmat(FieldAreaDeg, Nfld, 1);
        end
    end
    if isempty(Ebv)
        Ebv = nan(Nfld,1);
    end
    % radius of a circle of the same area, used to collect the HTM cells
    FieldRad = sqrt(FieldAreaDeg./pi) ./ RAD;
    if Args.Verbose
        fprintf('specDensityMap: %d LAST fields, %.2f deg^2 each\n', Nfld, mean(FieldAreaDeg));
    end

    % ---- per-field counts for the spectral catalog and the reference catalog
    [Nspec, SpecDens] = countPerField(Args.CatName, RA, Dec, FieldRad, FieldAreaDeg, Args.Verbose);
    if isempty(Args.RefCatName)
        Nref = nan(Nfld,1);  RefDens = nan(Nfld,1);
    else
        [Nref, RefDens] = countPerField(Args.RefCatName, RA, Dec, FieldRad, FieldAreaDeg, Args.Verbose);
    end

    [~, GalLat] = celestial.coo.convert_coo(RA, Dec, 'j2000.0', 'g');

    % Sky visibility: a field culminates at 90 - |ObsLat - Dec|; one that never
    % reaches MinAlt cannot be observed from the site at all, so it must not
    % enter the statistics - otherwise the far south, which LAST never sees,
    % dilutes the fraction of fields that matter.
    MaxAlt  = 90 - abs(Args.ObsLat - Dec.*RAD);
    Visible = MaxAlt >= Args.MinAlt;

    ExpPerCrop = SpecDens .* Args.CropArea .* Args.SpecFraction;
    T = table((1:Nfld).', RA.*RAD, Dec.*RAD, GalLat.*RAD, Ebv, MaxAlt, Visible, ...
              Nspec, SpecDens, Nref, RefDens, Nspec./max(Nref,1), ExpPerCrop, ...
        'VariableNames', {'Index','RA','Dec','GalLat','Ebv','MaxAlt','Visible', ...
                          'Nspec','SpecDens','Nref','RefDens','Ratio','ExpectedPerCrop'});
    Thr = sort(Args.Thresholds(:)).';
    for Ith = 1:numel(Thr)
        T.(sprintf('Below%g', Thr(Ith))) = ExpPerCrop < Thr(Ith);
    end

    if Args.Verbose
        fprintf('specDensityMap: %d/%d fields observable (Dec >= %.1f for MinAlt=%g at lat %.3f)\n', ...
            sum(Visible), Nfld, Args.ObsLat - (90-Args.MinAlt), Args.MinAlt, Args.ObsLat);
        fprintf('specDensityMap: %s density median=%.0f/deg^2 (5-95%%: %.0f-%.0f) [observable fields]\n', ...
            Args.CatName, median(SpecDens(Visible),'omitnan'), ...
            prctile(SpecDens(Visible),5), prctile(SpecDens(Visible),95));
        for Ith = 1:numel(Thr)
            Bl = T.(sprintf('Below%g', Thr(Ith)));
            fprintf('specDensityMap: %4d/%d observable fields (%.1f%%) expect < %g calibrators per crop  [all-sky: %d, %.1f%%]\n', ...
                sum(Bl & Visible), sum(Visible), 100*sum(Bl & Visible)/sum(Visible), Thr(Ith), ...
                sum(Bl), 100*sum(Bl)/Nfld);
        end
    end

    FigH = [];
    if Args.Plot
        FigH = drawMap(T, Args);
        if ~isempty(Args.OutFile)
            try
                exportgraphics(FigH, Args.OutFile, 'Resolution',130);
            catch
                print(FigH, Args.OutFile, '-dpng', '-r130');
            end
            [P,N] = fileparts(Args.OutFile);
            writetable(T, fullfile(P, [N '.csv']));
            if Args.Verbose
                fprintf('specDensityMap: wrote %s and %s\n', Args.OutFile, fullfile(P,[N '.csv']));
            end
        end
    end
end

% ======================================================================
function [Ntot, Dens] = countPerField(CatName, RA, Dec, FieldRad, FieldArea, Verbose)
    % Sum the HTM per-cell source counts over each field's footprint.
    %   The HTM id IS the row number of the index table, and column 13 holds the
    %   per-cell source count (NaN on the internal nodes). Note that
    %   catsHTM.nsrc pairs that count with column 2, which is a different
    %   counter, so the index is read here directly.
    %   search_htm_ind returns every cell *overlapping* the cone, which covers
    %   more sky than the cone itself, so the density is formed against the area
    %   of the returned cells rather than the field area; the field count is then
    %   that density times the field area.
    IndexFile = sprintf('%s_htm.hdf5', CatName);
    VarName   = sprintf('%s_HTM', CatName);
    D         = HDF5.load(IndexFile, VarName);
    Count     = D(:,13);
    IsLeaf    = isfinite(Count);
    CellArea  = 4*pi*(180/pi)^2 / sum(IsLeaf);   % deg^2 per leaf cell

    Nfld = numel(RA);
    Dens = nan(Nfld,1);
    Step = max(1, floor(Nfld/10));
    for Ifld = 1:Nfld
        ID = catsHTM.search_htm_ind(IndexFile, VarName, RA(Ifld), Dec(Ifld), FieldRad(Ifld));
        ID = ID(ID >= 1 & ID <= numel(Count));
        ID = ID(IsLeaf(ID));
        if isempty(ID)
            Dens(Ifld) = 0;
        else
            Dens(Ifld) = sum(Count(ID)) ./ (numel(ID) .* CellArea);
        end
        if Verbose && mod(Ifld, Step) == 0
            fprintf('  %s: %d / %d fields\n', CatName, Ifld, Nfld);
        end
    end
    Ntot = Dens .* FieldArea;
end

% ----------------------------------------------------------------------
function FigH = drawMap(T, Args)
    % All-sky density map plus the diagnostic distributions.
    Thr  = sort(Args.Thresholds(:)).';
    Col  = [0.85 0.10 0.10; 0.95 0.60 0.10];   % strictest threshold first (reddest)
    FigH = figure('Visible','off', 'Position',[10 10 1500 820], 'Color','w');

    % --- main panel: density on the sky
    subplot(2,2,[1 2]);
    Vis = T.Visible;
    % Unobservable fields are drawn faint, so the sky stays recognisable while
    % the eye is drawn only to what LAST can actually point at.
    scatter(T.RA(~Vis), T.Dec(~Vis), 18, log10(max(T.SpecDens(~Vis), 1)), ...
            'filled', 'Marker','s', 'MarkerFaceAlpha',0.18, 'MarkerEdgeAlpha',0);
    hold on;
    scatter(T.RA(Vis), T.Dec(Vis), 26, log10(max(T.SpecDens(Vis), 1)), 'filled', 'Marker','s');
    set(gca, 'XDir','reverse');
    Cb = colorbar;  Cb.Label.String = sprintf('log_{10} %s [deg^{-2}]', strrep(Args.CatName,'_','\_'));
    colormap(gca, parula);
    caxis([log10(max(prctile(T.SpecDens(Vis),2),1)) log10(max(prctile(T.SpecDens(Vis),98),10))]);
    % Legend order follows plotting order: the faint (unobservable) set first.
    Leg = {sprintf('below horizon (Dec < %.0f, %d fields)', ...
                   Args.ObsLat - (90-Args.MinAlt), sum(~Vis)), ...
           sprintf('%s density', strrep(Args.CatName,'_','\_'))};
    for Ith = numel(Thr):-1:1
        M = T.(sprintf('Below%g', Thr(Ith))) & Vis;
        Ci = Col(min(Ith, size(Col,1)), :);
        plot(T.RA(M), T.Dec(M), 'o', 'MarkerSize',4, 'MarkerEdgeColor',Ci, ...
             'MarkerFaceColor',Ci);
        Leg{end+1} = sprintf('< %g per crop (%d of %d observable, %.1f%%)', ...
                             Thr(Ith), sum(M), sum(Vis), 100*sum(M)/sum(Vis)); %#ok<AGROW>
    end
    legend(Leg, 'Location','southoutside', 'Orientation','horizontal', 'FontSize',8);
    xlabel('RA [deg]');  ylabel('Dec [deg]');
    xlim([0 360]);  ylim([-90 90]);  grid on;
    title(sprintf('%s per LAST field (%d of %d observable from lat %.1f)', ...
          strrep(Args.CatName,'_','\_'), sum(Vis), height(T), Args.ObsLat), 'FontSize',10);

    % --- density vs galactic latitude
    subplot(2,2,3);
    plot(T.GalLat(~Vis), T.SpecDens(~Vis), '.', 'Color',[.85 .87 .90], 'MarkerSize',4);
    hold on;
    plot(T.GalLat(Vis), T.SpecDens(Vis), '.', 'Color',[.55 .6 .75], 'MarkerSize',5);
    for Ith = numel(Thr):-1:1
        M  = T.(sprintf('Below%g', Thr(Ith))) & Vis;
        Ci = Col(min(Ith, size(Col,1)), :);
        plot(T.GalLat(M), T.SpecDens(M), '.', 'Color',Ci, 'MarkerSize',7);
    end
    set(gca,'YScale','log');  grid on;
    xlabel('galactic latitude [deg]');
    ylabel(sprintf('%s [deg^{-2}]', strrep(Args.CatName,'_','\_')));
    title('Density vs galactic latitude (faint: below horizon)', 'FontSize',10);

    % --- expected calibrators per crop
    subplot(2,2,4);
    Ed = logspace(log10(max(min(T.ExpectedPerCrop),0.5)), log10(max(T.ExpectedPerCrop)), 60);
    histogram(T.ExpectedPerCrop(~Vis), Ed, 'FaceColor',[.85 .87 .90], 'EdgeColor','none');
    hold on;
    histogram(T.ExpectedPerCrop(Vis), Ed, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
    hold on;
    for Ith = 1:numel(Thr)
        Ci = Col(min(Ith, size(Col,1)), :);
        xline(Thr(Ith), '-', sprintf('%g', Thr(Ith)), 'Color',Ci, 'LineWidth',1.6, ...
              'LabelOrientation','horizontal');
    end
    set(gca,'XScale','log','YScale','log');  grid on;
    xlabel('expected calibrators per crop');  ylabel('N fields');
    title(sprintf('Expected per crop (%.2f deg^2, %.0f%% survive cuts; faint: below horizon)', ...
          Args.CropArea, 100*Args.SpecFraction), 'FontSize',10);
end
