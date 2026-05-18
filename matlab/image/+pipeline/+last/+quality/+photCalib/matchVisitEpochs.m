function MS = matchVisitEpochs(VisitPath, Args)
    % Build a within-visit MatchedSources from a visit's proc catalogs
    % Description: Cross-matches the individual (per sub-exposure) proc
    %              catalogs of a single LAST visit into per-crop
    %              MatchedSources objects. Unlike the pipeline's pre-built
    %              merged MatchedMat HDF5 files (read by loadVisit /
    %              loadMergedMat), which carry only a fixed slim column
    %              set, this builder lets you choose ANY catalog column to
    %              propagate into MS.Data — in particular FLUX_PSF, which
    %              the merged products do not store.
    %
    %              Pipeline:
    %                1. loadVisit -> AllSI [Nepoch x Nsub] AstroImage with
    %                   CatData populated from the proc Cat files,
    %                2. collect AllSI(epoch,:).CatData per crop and
    %                   cross-match across epochs (matchPhotEpochs:
    %                   imProc.match.unify + setBadPhotToNan).
    %
    %              Returns a flat 1xNcrop MatchedSources array — one per
    %              crop, no mode layer — directly consumable by
    %              compareStabilityMS, plotPhotScatter or
    %              batchARMSHistograms ('MSData' option).
    %
    % Input  : - VisitPath char/string — a single visit directory holding
    %            the individual *_sci_proc_Image_1.fits (+ Cat) files.
    %                    * ...,key,val,...
    %            'MatchedColumns' - Catalog columns propagated into MS.Data.
    %                               Default includes RA, Dec, X1, Y1, SN,
    %                               MAG_PSF, MAG_APER_3, MAGERR_PSF,
    %                               MAGERR_APER_3, FLUX_APER_3, FLUX_PSF,
    %                               FLAGS. MAG_PSF must stay in the list —
    %                               matchPhotEpochs/setBadPhotToNan keys on
    %                               it.
    %            'MagFields'      - Magnitude columns; the FIRST is the
    %                               reference for the source-level
    %                               MinEpochs purge. Default
    %                               {'MAG_PSF','MAG_APER_3'} (always
    %                               present in proc catalogs; the AB
    %                               columns may be all-NaN pre-calibration).
    %            'MatchRadius'    - Cross-match radius [arcsec]. Default 3.
    %            'BadFlags'       - FLAGS bits passed to setBadPhotToNan.
    %                               Default {'Saturated','NearEdge',
    %                               'Overlap'}.
    %            'MinEpochs'      - Min finite epochs per source forwarded
    %                               to matchPhotEpochs. NOTE this purge is
    %                               destructive; default 1 keeps every
    %                               source with >=1 finite epoch so the MS
    %                               can be filtered non-destructively
    %                               downstream (ARMS / compareStabilityMS).
    %            'CropsToAnalyze' - Vector of crop indices. Default []
    %                               (all crops present in the visit).
    %            'PipelineVer'    - 'v0' | 'v1' — forwarded to loadVisit
    %                               for the AllSI reshape. Default 'v0'.
    %            'OutDir'         - Directory for matchPhotEpochs' cached
    %                               MS_all.mat. Default '' (no caching).
    %            'ForceRecalc'    - Recompute even if a cache exists.
    %                               Default false.
    %            'Verbose'        - Print progress. Default false.
    % Output : - MS, a 1xNcrop MatchedSources array (one element per
    %            crop). MS(crop).Data carries the requested columns,
    %            including FLUX_PSF. Crops that failed to match are an
    %            empty MatchedSources (Nsrc == 0).
    % Author : D. Kovaleva (May 2026)
    % Example: % Build a visit MS that carries FLUX_PSF:
    %          MS = pipeline.last.quality.photCalib.matchVisitEpochs( ...
    %                  '/archimedes/.../2025/05/04/proc/000838v0');
    %          Std = std(MS(10).Data.FLUX_PSF, 0, 1, 'omitnan');   % crop 10
    %
    %          % Feed several visits into batchARMSHistograms:
    %          MSData = struct('MS',{},'Visit',{},'Cohort',{});
    %          for k = 1:numel(VisitPaths)
    %              M = pipeline.last.quality.photCalib.matchVisitEpochs(VisitPaths{k});
    %              [~,vn] = fileparts(VisitPaths{k});
    %              MSData(end+1) = struct('MS',M,'Visit',vn,'Cohort','v0');
    %          end
    %          R = pipeline.last.quality.photCalib.batchARMSHistograms('', ...
    %                  'MSData', MSData, ...
    %                  'Quantities',  {'RA','Dec','FLUX_PSF'}, ...
    %                  'PanelGroups', {{'RA','Dec'},{'FLUX_PSF'}});

    arguments
        VisitPath            {mustBeTextScalar}
        Args.MatchedColumns  cell = {'RA','Dec','X1','Y1','SN', ...
                                     'MAG_PSF','MAG_APER_3', ...
                                     'MAGERR_PSF','MAGERR_APER_3', ...
                                     'FLUX_APER_3','FLUX_PSF','FLAGS'}
        Args.MagFields       cell = {'MAG_PSF','MAG_APER_3'}
        Args.MatchRadius     (1,1) double {mustBePositive} = 3
        Args.BadFlags        cell = {'Saturated','NearEdge','Overlap'}
        Args.MinEpochs       (1,1) double {mustBeNonnegative} = 1
        Args.CropsToAnalyze  double = []
        Args.PipelineVer     (1,:) char ...
            {mustBeMember(Args.PipelineVer, {'v0','v1'})} = 'v0'
        Args.OutDir          {mustBeText} = ''
        Args.ForceRecalc     logical = false
        Args.Verbose         logical = false
    end

    VisitPath = char(VisitPath);
    if ~exist(VisitPath, 'dir')
        error('matchVisitEpochs:NoDir', 'Visit directory not found: %s', VisitPath);
    end

    % --- Load the individual proc images (Cat + Header) ----------------
    % Skip coadd and merged-MS products: we only need the per-exposure
    % catalogs. loadVisit reshapes AllSI to [Nepoch x Nsub] using the
    % MIDJD/CROPID/COUNTER header keys.
    AllSI = pipeline.last.load.loadVisit(VisitPath, ...
        'TempName_Coadd', '', ...
        'TempName_MS',    '', ...
        'PipelineVer',    Args.PipelineVer, ...
        'GenError',       true);

    if isempty(AllSI)
        error('matchVisitEpochs:NoImages', ...
            'No individual proc images found under %s', VisitPath);
    end

    [Nepoch, Nsub] = size(AllSI);
    if Args.Verbose
        fprintf('matchVisitEpochs: %s -> %d epochs x %d crops\n', ...
            VisitPath, Nepoch, Nsub);
    end

    % Catalogs must be populated (readProducts pulls the sibling Cat
    % files). Bail early with an actionable message if they are not.
    CatOK = false;
    if ~isempty(AllSI(1,1).CatData)
        [Nr, ~] = AllSI(1,1).CatData.sizeCatalog;
        CatOK   = Nr > 0;
    end
    if ~CatOK
        error('matchVisitEpochs:NoCatalogs', ...
            ['Individual images loaded but CatData is empty for %s. ', ...
             'The proc Cat files may be missing or still bz2-compressed.'], ...
            VisitPath);
    end

    % --- Repackage epoch catalogs per crop -----------------------------
    % matchPhotEpochs still takes a mode-keyed Cats struct; 'percrop' is
    % an internal transport key only — it never reaches the caller.
    Cats = struct();
    Cats.percrop = cell(1, Nepoch);
    for Ie = 1:Nepoch
        Cats.percrop{Ie} = [AllSI(Ie, :).CatData];   % 1 x Nsub AstroCatalog
    end

    % --- Cross-match across the visit's sub-exposures ------------------
    MStruct = pipeline.last.quality.photCalib.matchPhotEpochs(Cats, ...
        'Modes',          {'percrop'}, ...
        'Ncrop',          Nsub, ...
        'CropsToAnalyze', Args.CropsToAnalyze, ...
        'MatchRadius',    Args.MatchRadius, ...
        'MatchedColumns', Args.MatchedColumns, ...
        'MagFields',      Args.MagFields, ...
        'BadFlags',       Args.BadFlags, ...
        'MinEpochs',      Args.MinEpochs, ...
        'OutDir',         Args.OutDir, ...
        'ForceRecalc',    Args.ForceRecalc, ...
        'Verbose',        Args.Verbose);

    % --- Unwrap the mode layer -> flat 1xNcrop MatchedSources array ----
    % matchPhotEpochs leaves unmatched crops as [] cells; fill them with
    % an empty MatchedSources so the result is a clean object array.
    CropCell = MStruct.percrop;
    MS(1, Nsub) = MatchedSources;
    for Ic = 1:Nsub
        if Ic <= numel(CropCell) && ~isempty(CropCell{Ic})
            MS(Ic) = CropCell{Ic};
        end
    end
end
