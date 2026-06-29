function Result = checkGAIADR3specMerge(Args)
    % Post-merge sanity check: non-NaN fraction of the 8 appended Gaia columns.
    % Description: After VO.prep.GAIA.dr3.mergeGAIADR3spec has appended the
    %              Gaia DR3 columns to GAIADR3spec, this samples HTM cells of
    %              the merged catalog and reports, per new column, the
    %              fraction of finite (non-NaN) values plus min/median/max.
    %              A column that comes back all-NaN almost always means the
    %              positional join failed for the whole sky -- typically
    %              because the temporary GAIADR3xpcols catalog was not on the
    %              catsHTM path when mergeGAIADR3spec ran (match_catsHTM then
    %              returns no match and every value is NaN).
    %
    %              Cells are sampled across the sorted data files (stride
    %              over files spreads the sample across declination) and
    %              capped at MaxCells, so the check is fast even on the full
    %              ~35M-source catalog. Set MaxCells=[] to scan everything.
    % Input  : * ...,key,val,...
    %            'CatName'    - Catalog to check. Default 'GAIADR3spec'.
    %            'BaseDir'    - catsHTM root. Default '' (resolved from
    %                           registry / ASTROPACK_CATSHTM_PATH).
    %            'CatRelDir'  - Catalog subdir under BaseDir. Default ''.
    %            'ColNames'   - Cellstr of new columns to check. Default is
    %                           the 8 appended by mergeGAIADR3spec.
    %            'SampleFiles'- Number of data files to sample (evenly spread).
    %                           Default 40. Ignored when MaxCells=[].
    %            'MaxCells'   - Cap on cells scanned. Default 500. [] = all.
    %            'NfilesInHDF'- Cells per data file. Default 100.
    %            'Verbose'    - Print a summary table. Default true.
    % Output : - Result struct with fields:
    %            .Table       - table [Ncheck x 6]: ColName, ColIndex,
    %                           NonNaNFrac, Min, Median, Max.
    %            .CellsScanned, .RowsScanned, .Ncol (total columns found).
    %            .MissingCols - names not found in the catalog ColCell.
    %            .AllNaNCols  - names whose sampled values are all NaN.
    %            .Ok          - true iff no requested column is missing and
    %                           none is all-NaN.
    % Author : Dana Kovaleva (Jun 2026)
    % Example:
    %   R = VO.prep.GAIA.dr3.checkGAIADR3specMerge('BaseDir','/data/tmp/GAIADR3spec_merged');

    arguments
        Args.CatName        char    = 'GAIADR3spec'
        Args.BaseDir        char    = ''
        Args.CatRelDir      char    = ''
        Args.ColNames       cell    = {'PMRA','PMDec','phot_g_mean_mag', ...
                                       'phot_bp_mean_mag','phot_rp_mean_mag', ...
                                       'bp_rp','phot_bp_rp_excess_factor', ...
                                       'classprob_dsc_combmod_star'}
        Args.SampleFiles    double  = 40
        Args.MaxCells               = 500
        Args.NfilesInHDF    double  = 100
        Args.Verbose        logical = true
    end

    % --- Resolve paths and the catalog ColCell --------------------------
    [BaseDir, CatRelDir] = catsHTM.resolve_cat_paths(Args.CatName, Args.BaseDir, Args.CatRelDir);
    SrcDir = fullfile(BaseDir, CatRelDir);
    if ~isfolder(SrcDir)
        error('VO:prep:GAIA:dr3:checkGAIADR3specMerge:NoSrcDir', ...
            'Catalog directory does not exist: %s', SrcDir);
    end
    [ColCell, ~] = catsHTM.load_colcell_from_dir(SrcDir, Args.CatName);
    Ncol = numel(ColCell);

    % Resolve requested column names (case-insensitive) to indices.
    Ncheck      = numel(Args.ColNames);
    ColIndex    = zeros(Ncheck, 1);
    MissingCols = {};
    for Ic = 1:Ncheck
        J = find(strcmpi(ColCell, Args.ColNames{Ic}), 1);
        if isempty(J)
            MissingCols{end+1} = Args.ColNames{Ic};   %#ok<AGROW>
        else
            ColIndex(Ic) = J;
        end
    end

    % --- Choose which data files to scan --------------------------------
    Files = dir(fullfile(SrcDir, sprintf('%s_htm_*.hdf5', Args.CatName)));
    if isempty(Files)
        error('VO:prep:GAIA:dr3:checkGAIADR3specMerge:NoFiles', ...
            'No %s_htm_*.hdf5 files in %s.', Args.CatName, SrcDir);
    end
    [~, Ord] = sort({Files.name});
    Files = Files(Ord);
    Nfiles = numel(Files);

    if isempty(Args.MaxCells)
        FileSel = 1:Nfiles;            % scan everything
    else
        Nsel    = max(1, min(Nfiles, Args.SampleFiles));
        FileSel = unique(round(linspace(1, Nfiles, Nsel)));
    end

    % --- Accumulate finite/total counts and values per column -----------
    Counts = zeros(Ncheck, 2);            % [nFinite, nTotal] per column
    Vals   = cell(Ncheck, 1);             % sampled finite values for stats
    CellsScanned = 0;
    RowsScanned  = 0;

    for If = FileSel
        SrcFile = fullfile(SrcDir, Files(If).name);
        Info  = h5info(SrcFile);
        Names = {Info.Datasets.Name};
        % Cell datasets are 'htm_<id>' (one underscore); skip '<...>_Ind'.
        IndH  = find(cellfun(@(s) numel(strfind(s,'_'))==1, Names));

        DoneFile = false;
        for Iih = 1:numel(IndH)
            DataSetName = Names{IndH(Iih)};
            Cat   = HDF5.load(SrcFile, ['/' DataSetName]);
            Nrows = size(Cat, 1);
            if Nrows == 0
                continue
            end
            CellsScanned = CellsScanned + 1;
            RowsScanned  = RowsScanned + Nrows;

            for Ic = 1:Ncheck
                if ColIndex(Ic) == 0 || ColIndex(Ic) > size(Cat,2)
                    continue
                end
                V = Cat(:, ColIndex(Ic));
                Fin = isfinite(V);
                Counts(Ic,1) = Counts(Ic,1) + sum(Fin);
                Counts(Ic,2) = Counts(Ic,2) + Nrows;
                Vals{Ic} = [Vals{Ic}; V(Fin)];
            end

            if ~isempty(Args.MaxCells) && CellsScanned >= Args.MaxCells
                DoneFile = true;
            end
            if DoneFile
                break
            end
        end
        if DoneFile
            break
        end
    end

    % --- Per-column statistics ------------------------------------------
    NonNaNFrac = nan(Ncheck, 1);
    MinV       = nan(Ncheck, 1);
    MedV       = nan(Ncheck, 1);
    MaxV       = nan(Ncheck, 1);
    AllNaNCols = {};
    for Ic = 1:Ncheck
        if Counts(Ic,2) > 0
            NonNaNFrac(Ic) = Counts(Ic,1) ./ Counts(Ic,2);
        end
        if ~isempty(Vals{Ic})
            MinV(Ic) = min(Vals{Ic});
            MedV(Ic) = median(Vals{Ic});
            MaxV(Ic) = max(Vals{Ic});
        end
        if ColIndex(Ic) > 0 && (Counts(Ic,1) == 0)
            AllNaNCols{end+1} = Args.ColNames{Ic};   %#ok<AGROW>
        end
    end

    T = table(Args.ColNames(:), ColIndex, NonNaNFrac, MinV, MedV, MaxV, ...
        'VariableNames', {'ColName','ColIndex','NonNaNFrac','Min','Median','Max'});

    Result = struct();
    Result.Table        = T;
    Result.CellsScanned = CellsScanned;
    Result.RowsScanned  = RowsScanned;
    Result.Ncol         = Ncol;
    Result.MissingCols  = MissingCols;
    Result.AllNaNCols   = AllNaNCols;
    Result.Ok           = isempty(MissingCols) && isempty(AllNaNCols);

    if Args.Verbose
        fprintf('checkGAIADR3specMerge: %s (%d total columns)\n', Args.CatName, Ncol);
        fprintf('  scanned %d cells / %d rows from %d/%d data files\n', ...
            CellsScanned, RowsScanned, numel(FileSel), Nfiles);
        disp(T);
        if ~isempty(MissingCols)
            fprintf(2, '  MISSING columns (not in ColCell): %s\n', strjoin(MissingCols, ', '));
        end
        if ~isempty(AllNaNCols)
            fprintf(2, '  ALL-NaN columns (join likely failed): %s\n', strjoin(AllNaNCols, ', '));
        end
        if Result.Ok
            fprintf('  OK: all requested columns present with finite values.\n');
        end
    end
end
