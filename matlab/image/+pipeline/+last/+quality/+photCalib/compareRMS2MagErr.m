function Result = compareRMS2MagErr(MS, Args)
    % Compare per-source light-curve RMS with the catalog magnitude error
    % Description: Validation tool for the aperture-photometry error
    %              formula (issue #1134). For every matched source the
    %              function computes the RMS of its light curve in a
    %              chosen magnitude column (plain std and a robust
    %              1.4826*MAD version) together with the median of the
    %              corresponding catalog magnitude-error column, pools
    %              the sources of all crops, bins them by median
    %              magnitude and overlays the binned-median RMS on the
    %              binned-median predicted error. If the catalog errors
    %              are correct, the two curves must converge at the
    %              faint (Poisson-dominated) end; the bright end is
    %              dominated by systematics (ZP jitter, flat-field,
    %              scintillation) where RMS > predicted error is
    %              expected.
    %
    %              The per-bin ratio RMS/Err and a single faint-end
    %              summary ratio (over the bins whose predicted error
    %              falls inside 'FaintErrRange') are returned:
    %              ratio ~ 1  - errors are correct;
    %              ratio > 1  - errors are underestimated (too small);
    %              ratio < 1  - errors are overestimated.
    %
    % Input  : - MS - a MatchedSources array (one element per crop, e.g.
    %            from pipeline.last.load.loadMergedMat / loadVisit /
    %            matchEpochs) or a cell of MatchedSources.
    %          * ...,key,val,...
    %            'MagField'    - Magnitude column for the light curves.
    %                            Default is 'MAG_APER_3'.
    %            'ErrField'    - Magnitude-error column to validate.
    %                            Default is 'MAGERR_APER_3'.
    %            'MinEpochs'   - Minimal number of finite epochs (after
    %                            the FLAGS filter) per source. Default
    %                            is 15.
    %            'FilterFlags' - FLAGS bit names to NaN out per epoch
    %                            before the reductions. Default is
    %                            {'Saturated', 'NearEdge', 'NaN'}.
    %            'MinSN'       - Per-epoch S/N cut: (epoch,source)
    %                            entries with MS.Data.SN <= MinSN are
    %                            NaN'd. Default is 0 (disabled) - a S/N
    %                            cut removes exactly the low-flux epochs
    %                            of faint sources and biases their RMS
    %                            low, so keep it off for this test.
    %            'BinWidth'    - Magnitude bin width. Default is 0.25.
    %            'MagRange'    - [Min Max] magnitude range for binning.
    %                            Default is [] (auto from the data,
    %                            2nd..98th percentile).
    %            'FaintErrRange' - [Min Max] of the binned predicted
    %                            error defining the "Poisson end" bins
    %                            used for the summary ratio. Default is
    %                            [0.05 0.3] mag.
    %            'MinSrcPerBin'- Minimal sources per bin for the bin to
    %                            enter the plot/summary. Default is 20.
    %            'ExcludeForced' - Drop sources that are forced-photometry
    %                            entries (MS.Data.FORCED==1) in more than
    %                            'MaxForcedFrac' of their finite epochs.
    %                            Forced entries fainter than the
    %                            single-epoch detection limit carry
    %                            censored/negative-flux photometry that
    %                            corrupts both the RMS and the error
    %                            columns. Silently ignored when the MS
    %                            has no FORCED field. Default is true.
    %            'MaxForcedFrac' - See 'ExcludeForced'. Default is 0.5.
    %            'Plot'        - Plot the comparison figure. Default is
    %                            true.
    %            'FigFile'     - When not empty, save the figure to this
    %                            file (extension selects the format).
    %                            Default is ''.
    %            'Verbose'     - Print the per-bin table and the summary.
    %                            Default is true.
    % Output : - Result structure with fields:
    %            .PerSource - struct of pooled per-source vectors:
    %                         MedMag, RmsStd, RmsRob, MedErr, Nep, Crop.
    %            .Bins      - table with one row per magnitude bin:
    %                         MagBin, N, MedRmsStd, MedRmsRob, MedErr,
    %                         RatioStd, RatioRob.
    %            .FaintRatioStd - median over the faint-end bins of
    %                         MedRmsStd/MedErr (NaN when no bin
    %                         qualifies).
    %            .FaintRatioRob - same for the robust RMS.
    %            .FaintBinMask  - logical mask of the faint-end bins in
    %                         .Bins.
    %            .Args      - the applied arguments.
    % Author : D. Kovaleva (Aug 2026)
    % Example: MS = pipeline.last.load.loadMergedMat('MergedMatDir', Dir);
    %          R  = pipeline.last.quality.photCalib.compareRMS2MagErr(MS);
    %          % PSF photometry on the same data:
    %          R  = pipeline.last.quality.photCalib.compareRMS2MagErr(MS,...
    %              'MagField','MAG_PSF', 'ErrField','MAGERR_PSF');

    arguments
        MS
        Args.MagField                       = 'MAG_APER_3';
        Args.ErrField                       = 'MAGERR_APER_3';
        Args.MinEpochs                      = 15;
        Args.FilterFlags cell               = {'Saturated', 'NearEdge', 'NaN'};
        Args.MinSN                          = 0;
        Args.BinWidth                       = 0.25;
        Args.MagRange                       = [];
        Args.FaintErrRange                  = [0.05 0.3];
        Args.MinSrcPerBin                   = 20;
        Args.ExcludeForced logical          = true;
        Args.MaxForcedFrac                  = 0.5;
        Args.Plot logical                   = true;
        Args.FigFile                        = '';
        Args.Verbose logical                = true;
    end

    if iscell(MS)
        MSList = MS;
    else
        MSList = num2cell(MS);
    end
    Nms = numel(MSList);

    AllMedMag = [];
    AllRmsStd = [];
    AllRmsRob = [];
    AllMedErr = [];
    AllNep    = [];
    AllCrop   = [];

    for Ims = 1:Nms
        MSobj = MSList{Ims};
        HaveCols = isfield(MSobj.Data, Args.MagField) && ...
                   isfield(MSobj.Data, Args.ErrField);
        if ~HaveCols
            warning('compareRMS2MagErr:MissingColumn', ...
                'MS #%d lacks %s/%s - skipped', ...
                Ims, Args.MagField, Args.ErrField);
        else
            Y = MSobj.Data.(Args.MagField);
            E = MSobj.Data.(Args.ErrField);

            Bad = buildBadEpochMask(MSobj, Args);
            if Args.MinSN > 0 && isfield(MSobj.Data, 'SN')
                Bad = Bad | ~(MSobj.Data.SN > Args.MinSN);
            end
            Y(Bad) = NaN;
            E(Bad | isnan(Y)) = NaN;

            Good = sum(~isnan(Y), 1) >= Args.MinEpochs;
            if Args.ExcludeForced && isfield(MSobj.Data, 'FORCED')
                ForcedFrac = mean(MSobj.Data.FORCED == 1, 1, 'omitnan');
                Good = Good & ~(ForcedFrac > Args.MaxForcedFrac);
            end
            Y = Y(:, Good);
            E = E(:, Good);

            MedMag = median(Y, 1, 'omitnan');
            RmsStd = std(Y, 0, 1, 'omitnan');
            RmsRob = 1.4826 .* median(abs(Y - MedMag), 1, 'omitnan');
            MedErr = median(E, 1, 'omitnan');
            Nep    = sum(~isnan(Y), 1);

            AllMedMag = [AllMedMag, MedMag];             %#ok<AGROW>
            AllRmsStd = [AllRmsStd, RmsStd];             %#ok<AGROW>
            AllRmsRob = [AllRmsRob, RmsRob];             %#ok<AGROW>
            AllMedErr = [AllMedErr, MedErr];             %#ok<AGROW>
            AllNep    = [AllNep,    Nep];                %#ok<AGROW>
            AllCrop   = [AllCrop,   repmat(Ims, 1, numel(MedMag))]; %#ok<AGROW>
        end
    end

    Fin = isfinite(AllMedMag) & isfinite(AllRmsStd) & isfinite(AllMedErr);
    AllMedMag = AllMedMag(Fin);
    AllRmsStd = AllRmsStd(Fin);
    AllRmsRob = AllRmsRob(Fin);
    AllMedErr = AllMedErr(Fin);
    AllNep    = AllNep(Fin);
    AllCrop   = AllCrop(Fin);

    Result.PerSource = struct('MedMag',AllMedMag, 'RmsStd',AllRmsStd, ...
        'RmsRob',AllRmsRob, 'MedErr',AllMedErr, 'Nep',AllNep, 'Crop',AllCrop);

    % --- binning ---
    if isempty(Args.MagRange)
        MagRange = [floor(prctile(AllMedMag,2)./Args.BinWidth), ...
                    ceil( prctile(AllMedMag,98)./Args.BinWidth)] .* Args.BinWidth;
    else
        MagRange = Args.MagRange;
    end
    Edges  = MagRange(1):Args.BinWidth:MagRange(2);
    Nbin   = numel(Edges) - 1;
    MagBin = 0.5 .* (Edges(1:Nbin) + Edges(2:Nbin+1));

    N         = zeros(1, Nbin);
    MedRmsStd = nan(1, Nbin);
    MedRmsRob = nan(1, Nbin);
    MedErrBin = nan(1, Nbin);
    for Ib = 1:Nbin
        In = AllMedMag >= Edges(Ib) & AllMedMag < Edges(Ib+1);
        N(Ib) = sum(In);
        if N(Ib) >= Args.MinSrcPerBin
            MedRmsStd(Ib) = median(AllRmsStd(In));
            MedRmsRob(Ib) = median(AllRmsRob(In));
            MedErrBin(Ib) = median(AllMedErr(In));
        end
    end
    RatioStd = MedRmsStd ./ MedErrBin;
    RatioRob = MedRmsRob ./ MedErrBin;

    Result.Bins = table(MagBin(:), N(:), MedRmsStd(:), MedRmsRob(:), ...
        MedErrBin(:), RatioStd(:), RatioRob(:), ...
        'VariableNames', {'MagBin','N','MedRmsStd','MedRmsRob','MedErr', ...
                          'RatioStd','RatioRob'});

    FaintMask = MedErrBin >= Args.FaintErrRange(1) & ...
                MedErrBin <= Args.FaintErrRange(2) & N >= Args.MinSrcPerBin;
    Result.FaintBinMask  = FaintMask(:);
    if any(FaintMask)
        Result.FaintRatioStd = median(RatioStd(FaintMask));
        Result.FaintRatioRob = median(RatioRob(FaintMask));
    else
        Result.FaintRatioStd = NaN;
        Result.FaintRatioRob = NaN;
    end
    Result.Args = Args;

    if Args.Verbose
        fprintf('\n=== compareRMS2MagErr: %s vs %s (%d crops, %d sources) ===\n', ...
            Args.MagField, Args.ErrField, Nms, numel(AllMedMag));
        fprintf('%7s %6s %10s %10s %10s %9s %9s\n', ...
            'MagBin','N','RMS(std)','RMS(rob)','MedErr','R(std)','R(rob)');
        for Ib = 1:Nbin
            if N(Ib) >= Args.MinSrcPerBin
                fprintf('%7.2f %6d %10.4f %10.4f %10.4f %9.3f %9.3f\n', ...
                    MagBin(Ib), N(Ib), MedRmsStd(Ib), MedRmsRob(Ib), ...
                    MedErrBin(Ib), RatioStd(Ib), RatioRob(Ib));
            end
        end
        fprintf('Faint-end (MedErr in [%.3f %.3f], %d bins): RMS/Err = %.3f (std), %.3f (robust)\n', ...
            Args.FaintErrRange, sum(FaintMask), ...
            Result.FaintRatioStd, Result.FaintRatioRob);
    end

    if Args.Plot
        Fig = figure('Position',[100 100 750 850]);
        Ax1 = subplot(3,1,[1 2]);
        semilogy(AllMedMag, AllRmsRob, '.', 'Color',[0.8 0.8 0.8], ...
            'MarkerSize',3, 'DisplayName','per-source robust RMS');
        hold(Ax1, 'on');
        semilogy(MagBin, MedRmsStd, 'o-', 'Color',[0 0.35 0.75], ...
            'LineWidth',1.5, 'DisplayName','binned median RMS (std)');
        semilogy(MagBin, MedRmsRob, 's-', 'Color',[0 0.6 0.3], ...
            'LineWidth',1.5, 'DisplayName','binned median RMS (robust)');
        semilogy(MagBin, MedErrBin, 'd-', 'Color',[0.85 0.2 0.1], ...
            'LineWidth',1.5, 'DisplayName',sprintf('binned median %s', ...
            strrep(Args.ErrField,'_','\_')));
        ylabel('RMS / predicted error [mag]');
        title(sprintf('%s LC RMS vs %s (issue #1134)', ...
            strrep(Args.MagField,'_','\_'), strrep(Args.ErrField,'_','\_')));
        legend(Ax1, 'Location','northwest');
        grid(Ax1, 'on');

        Ax2 = subplot(3,1,3);
        plot(MagBin, RatioStd, 'o-', 'Color',[0 0.35 0.75], ...
            'LineWidth',1.5, 'DisplayName','RMS(std)/Err');
        hold(Ax2, 'on');
        plot(MagBin, RatioRob, 's-', 'Color',[0 0.6 0.3], ...
            'LineWidth',1.5, 'DisplayName','RMS(robust)/Err');
        yline(1, 'k--', 'HandleVisibility','off');
        if any(FaintMask)
            plot(MagBin(FaintMask), RatioRob(FaintMask), 'ks', ...
                'MarkerSize',9, 'LineWidth',1.2, ...
                'DisplayName','faint-end bins');
        end
        xlabel(sprintf('median %s', strrep(Args.MagField,'_','\_')));
        ylabel('RMS / Err');
        legend(Ax2, 'Location','northwest');
        grid(Ax2, 'on');
        linkaxes([Ax1 Ax2], 'x');

        if ~isempty(Args.FigFile)
            saveas(Fig, Args.FigFile);
        end
    end
end

% =========================================================================
function Mask = buildBadEpochMask(MSobj, Args)
    % Build a per-(epoch,source) bad mask [Nepochs x Nsrc] from FLAGS bits.
    Mask = false(size(MSobj.Data.(Args.MagField)));
    if ~isempty(Args.FilterFlags) && isfield(MSobj.Data, 'FLAGS')
        FlagMat = MSobj.Data.FLAGS;
        FlagMat(isnan(FlagMat)) = 0;
        try
            BD = BitDictionary;
            for Ifl = 1:numel(Args.FilterFlags)
                [~, ~, BitDec] = BD.name2bit(Args.FilterFlags{Ifl});
                Mask = Mask | (bitand(uint32(FlagMat), uint32(BitDec)) > 0);
            end
        catch
            % dictionary unavailable - no flag filtering
        end
    end
end
