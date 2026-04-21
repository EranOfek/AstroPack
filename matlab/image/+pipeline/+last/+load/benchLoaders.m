function Result = benchLoaders(VisitDir, Args)
    % Benchmark loadVisitCatHdr / loadMergedMat vs AstroPack equivalents
    % Description: Times four loaders on the same visit directory to make
    %              a data-driven decision about which to use:
    %                (A) loadVisitCatHdr (Cat + Header only, no pixels)
    %                (B) loadVisit       (full AstroImage: Image+Mask+PSF+Cat)
    %                (C) directLoad(Cat) (just AstroCatalog via directLoad)
    %                (D) loadMergedMat vs directLoad(MergedMat)
    %
    % Input  : - VisitDir (single visit folder with proc FITS files).
    %          * ...,key,val,...
    %            'FileType'  - 'proc' | 'coadd'. Default is 'proc'.
    %            'Verbose'   - Default is true.
    % Output : - Result struct with timings (seconds) and element counts.
    % Author : D. Kovaleva (Apr 2026)
    % Example:
    %   R = pipeline.last.load.benchLoaders('~/222625v1');
    %   R = pipeline.last.load.benchLoaders('~/222625v0', 'FileType','coadd');

    arguments
        VisitDir
        Args.FileType = 'proc'
        Args.Verbose logical = true
    end

    Result = struct();

    % (A) loadVisitCatHdr — Cat + Header only
    t0 = tic;
    A = pipeline.last.load.loadVisitCatHdr( ...
        'DataDir', VisitDir, 'FileType', Args.FileType, 'Verbose', false);
    Result.tA = toc(t0);
    Result.NA = 0;
    if ~isempty(A)
        Fv = find(~cellfun(@isempty, A), 1);
        if ~isempty(Fv); Result.NA = numel(A{Fv}); end
    end

    % (B) loadVisit — full AstroImage array
    t0 = tic;
    try
        B = pipeline.last.load.loadVisit(VisitDir, ...
            'TempName_IndivIm', sprintf('*_sci_%s_Image_*.fits', Args.FileType), ...
            'TempName_Coadd', '', 'TempName_MS', '');
    catch ME
        B = [];
        if Args.Verbose; fprintf('loadVisit failed: %s\n', ME.message); end
    end
    Result.tB = toc(t0);
    Result.NB = numel(B);

    % (C) directLoad of Cat files only
    t0 = tic;
    try
        C = pipeline.last.load.directLoad( ...
            sprintf('*_sci_%s_Cat_*.fits', Args.FileType), 'Path', VisitDir);
    catch ME
        C = [];
        if Args.Verbose; fprintf('directLoad(Cat) failed: %s\n', ME.message); end
    end
    Result.tC = toc(t0);
    Result.NC = numel(C);

    % (D) loadMergedMat vs directLoad(MergedMat)
    t0 = tic;
    try
        pipeline.last.load.loadMergedMat( ...
            'MergedMatDir', VisitDir, 'Verbose', false);
    catch
    end
    Result.tD_quality = toc(t0);

    t0 = tic;
    try
        pipeline.last.load.directLoad( ...
            '*_sci_merged_MergedMat_*.hdf5', 'Path', VisitDir);
    catch
    end
    Result.tD_direct = toc(t0);

    if Args.Verbose
        fprintf('\n=== Loader benchmark (%s) ===\n', VisitDir);
        fprintf('%-40s %8s %6s\n', 'Loader', 'Seconds', 'N');
        fprintf('%s\n', repmat('-', 1, 60));
        fprintf('%-40s %8.2f %6d\n', '(A) loadVisitCatHdr (Cat+Header)', Result.tA, Result.NA);
        fprintf('%-40s %8.2f %6d\n', '(B) loadVisit (full AstroImage)', Result.tB, Result.NB);
        fprintf('%-40s %8.2f %6d\n', '(C) directLoad(Cat only)',        Result.tC, Result.NC);
        fprintf('%-40s %8.2f\n',     '(D) loadMergedMat',               Result.tD_quality);
        fprintf('%-40s %8.2f\n',     '(D) directLoad(MergedMat)',       Result.tD_direct);
        if Result.tA > 0
            fprintf('\nRatio B/A (full image vs. Cat+Header): %.1fx\n', Result.tB / Result.tA);
        end
    end
end
