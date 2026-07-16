function Report = batchPhotCalibTrans(BaseDir, Args)
    % Multi-config PhotCalibTrans campaign over coadd images beneath a path
    % Description: Discovers coadd Image_1 FITS files under BaseDir, groups
    %              them by visit stem (24 crops per visit), then for every
    %              (RunMode x OptSeqName) combination in the two config
    %              lists it invokes imProc.calib.fitPhotCalibTrans and
    %              collects fitted parameters, quality metrics, and the
    %              CalibTrajectory into a flat struct-array Report.
    %              Per-crop mode produces one Report row per crop (24 rows
    %              per visit per config); joint mode produces one row
    %              (CropNumber = 0) per visit per config.
    % Input  : - BaseDir - Root path holding coadd images (searched with
    %                     dir() and 'Recursive' toggle).
    %          * ...,key,val,...
    %            'FilePattern'          - dir() glob for the discovery
    %                                     product (one file per crop).
    %                                     Default 'LAST*_sci_coadd_Image_1.fits'.
    %                                     Each match is passed to
    %                                     AstroImage.readProducts, which
    %                                     reads the FITS header into
    %                                     HeaderData and auto-attaches the
    %                                     sibling `_Cat_1.fits` into
    %                                     CatData. Both are required for
    %                                     calibrator selection to succeed.
    %            'Recursive'            - Walk sub-tree. Default true.
    %            'FieldId'              - Restrict discovery to a specific
    %                                     field. Matches files whose
    %                                     basename contains `_<FieldId>_`
    %                                     (LAST filename convention:
    %                                     `LAST.*_<filter>_<FieldId>_...`).
    %                                     Examples: '1716.c', '923',
    %                                     '350+02'. Empty (default) means
    %                                     no field filter.
    %            'Filter'               - Restrict to a specific filter
    %                                     (LAST filter/color slot). Same
    %                                     substring-with-underscores match
    %                                     as FieldId. Typical values:
    %                                     'clear', 'blue', 'red'. Empty
    %                                     (default) means no filter.
    %            'CropId'               - Restrict to a specific crop
    %                                     number (1..24). When set, the
    %                                     joint mode is skipped for
    %                                     matching visits (the 24-crop
    %                                     stitch has no meaning if only
    %                                     one crop is loaded); only
    %                                     per-crop configs run. Empty
    %                                     (default) means all crops.
    %            'MaxVisits'            - Cap on how many visits to process.
    %                                     Default Inf.
    %            'OptSeqNames'          - Cell of OptSeq names to run.
    %                                     Default {'LAST_Joint_1Iter_AtmosFirst_Split3',
    %                                              'LAST_Joint_1Iter_Split3'}.
    %            'RunModes'             - Cell of run modes to run. Any of
    %                                     'per-crop', 'joint'. Default {'per-crop','joint'}.
    %            'Tran2DTypePerCrop'    - Tran2DType for per-crop mode.
    %                                     Default 'cheby1_2'.
    %            'Tran2DTypeJoint'      - Tran2DType for joint mode.
    %                                     Default 'cheby1_4'.
    %            'XPixelJoint'          - Full-frame X size handed to
    %                                     fitPhotCalibTrans in joint mode; sets
    %                                     the Tran2D normalisation (ParNX =
    %                                     XPixel/2) over the XFULL coordinate.
    %                                     Empty (default) auto-derives it from
    %                                     ceil(max(XFULL)) of the first joined
    %                                     visit and holds it constant (the
    %                                     full-frame extent is fixed LAST
    %                                     focal-plane geometry, not per-visit).
    %                                     Pass an explicit value to override; it
    %                                     is still cross-checked against the
    %                                     measured extent in a log line. The
    %                                     legacy constant was 6388.
    %            'YPixelJoint'          - As XPixelJoint, for Y (YFULL). Empty
    %                                     (default) auto-derives from
    %                                     ceil(max(YFULL)). Legacy constant 9576.
    %            'MagColName'           - Calibrator magnitude column
    %                                     forwarded via CalibArgs. Default
    %                                     'phot_g_mean_mag'.
    %            'MagRange'             - [min, max] magnitude range.
    %                                     Default [12, 16].
    %            'SigmaClipMethod'      - Default 'median'.
    %            'AirmassSource'        - Default 'compute'.
    %            'ApplyNutation'        - Default true.
    %            'AuditCalibrators'     - Default true.
    %            'CollectCalibTrajectory' - Default true (required to
    %                                     populate Report.CalibTrajectory).
    %            'RefSpecSlope'         - Passed to fitPhotCalibTrans.
    %                                     Default 1.5.
    %            'RefSpecPivot'         - Passed to fitPhotCalibTrans.
    %                                     Default 5500.
    %            'UseTypicalX'          - Default true.
    %            'WeightingMode'        - Calibrator MagErr formula used at
    %                                     startup and (for 2-iter recipes)
    %                                     for iter-2 weighted refinement.
    %                                     'combined' (default) uses
    %                                     sqrt(MagErr_spectral^2 + (1.086
    %                                     *FluxErr)^2). Set 'spectral' to
    %                                     ignore per-source flux errors,
    %                                     'flux' for bandpass-propagated
    %                                     instrumental only, or 'none' for
    %                                     unweighted. Iter 1 of the
    %                                     LAST_Joint_2Iter_* recipes is
    %                                     always unweighted (Recipe iter
    %                                     override wipes MagErr) regardless
    %                                     of this setting.
    %            'SystematicErr'        - Floor on the returned combined
    %                                     MagErr in magnitudes, applied
    %                                     element-wise as MagErr = max(
    %                                     MagErr, SystematicErr). Guards
    %                                     the chi^2 from being dominated
    %                                     by a handful of bright stars
    %                                     whose formal photon-noise error
    %                                     underestimates real per-star
    %                                     systematics (flat-field, aperture
    %                                     correction residuals, colour
    %                                     mismatch). Default 0.001 mag.
    %            'NormConvention'       - Post-fit gauge convention for
    %                                     the (Norm, Tran2D-DC) pair.
    %                                     'center' (default) rotates them
    %                                     so Tran2D(field-centre) = 0 and
    %                                     Norm carries the full field-
    %                                     centre ZP. Every model prediction
    %                                     (ZP, calibrated mag, residuals,
    %                                     RMS, chi2) is bit-identical to
    %                                     'raw' — only .Norm and
    %                                     .FitParams.Tran2D_ParX(1) change
    %                                     value. Choose 'raw' as an escape
    %                                     hatch to see the fit's untouched
    %                                     values. Forwarded to
    %                                     PhotCalibTrans.calibrate.
    %            'CalibArgsExtra'       - Extra key-val pairs appended to
    %                                     every CalibArgs cell. Default {}.
    %            'OutDir'               - When non-empty, write the calibrated
    %                                     catalogs to disk as FITS in addition
    %                                     to building the Report. Products are
    %                                     grouped by config into subdirectories
    %                                     OutDir/<RunMode>__<OptSeqName>__<Tran2DType>/.
    %                                     Per-crop mode writes one _Cat_ file per
    %                                     crop (named after the source crop's Cat
    %                                     product, header from the calibrated
    %                                     Result). Joint mode writes according to
    %                                     JointWriteMode. Empty (default) writes
    %                                     nothing.
    %            'JointWriteMode'       - What joint mode writes to OutDir:
    %                                     'per-crop' (default) applies the single
    %                                     global joint fit back to each crop's
    %                                     native catalogue and writes 24
    %                                     self-contained per-crop _Cat_ FITS
    %                                     (native X/Y; the Tran2D is translated
    %                                     into each crop's local frame and
    %                                     Norm/kx0 re-gauged so Tran2D=0 at the
    %                                     crop centre). 'joined' writes the single
    %                                     full-frame catalog
    %                                     <VisitStem>_joint_coadd_Cat_1.fits.
    %                                     'both' writes both. Calibrated mags are
    %                                     identical across the choices (gauge-
    %                                     and frame-invariant); only header
    %                                     bookkeeping and file layout differ.
    %            'OverWrite'            - Overwrite existing output files.
    %                                     Default true (recompute everything).
    %                                     Set false to RESUME: before each
    %                                     (visit, config) the expected output(s)
    %                                     are checked, and if all are present the
    %                                     fit is skipped entirely (not just the
    %                                     write) so a restarted run continues
    %                                     from where it stopped. A partially
    %                                     written per-crop visit is re-run and
    %                                     only its missing crops are filled.
    %                                     Only meaningful together with OutDir.
    %            'Verbose'              - Print per-visit progress. Default true.
    %            'InnerVerbose'         - When true, forwards Verbose=true to
    %                                     fitPhotCalibTrans / PhotCalibTrans.calibrate
    %                                     so their per-object diagnostics
    %                                     (including 'No calibrators found')
    %                                     show up in the console. Default false.
    % Output : - Report - 1xN struct array. Each entry has:
    %            .VisitStem      - visit basename (up to '_XXX_sci_coadd')
    %            .RunMode        - 'per-crop' | 'joint'
    %            .OptSeqName     - the recipe used
    %            .Tran2DType     - Tran2D basis for this row
    %            .CropNumber     - 1..24 for per-crop, 0 for joint,
    %                              NaN if the fit failed before crop-level.
    %            .Norm           - Fitted overall normalisation. Also
    %                              present under FitParams; promoted here
    %                              as a scalar column for easy table use.
    %            .TauAOD500      - Fitted aerosol optical depth at 500 nm
    %                              (from FitParams.TauAod500).
    %            .PWV_cm         - Fitted precipitable water vapour [cm]
    %                              (from FitParams.PWV_cm).
    %            .Center_Ang     - Fitted QE-centre wavelength [Angstrom]
    %                              (from FitParams.Center_Ang).
    %            .FitParams      - struct with one scalar field per
    %                              TransModel parameter (Norm, QE_Center,
    %                              PWV_cm, TauAod500, ...), plus
    %                              Tran2D_ParX (10-vec). Kept for full
    %                              rebuildability; the four promoted
    %                              scalars above are shortcuts.
    %            .RMS            - sqrt(mean(R^2)) over surviving calibrators.
    %            .MedianRMS      - sqrt(median(R^2)) over survivors.
    %            .ARMS           - bright-end sliding-window ARMS from the
    %                              trajectory's final snapshot.
    %            .Chi2           - PC.TransModel.Chi2 (last stage).
    %            .DOF            - PC.TransModel.DOF (last stage).
    %            .NCalib         - Number of surviving calibrators.
    %            .AIRMASS        - AIRMASS header keyword from the input FITS
    %                              (per-crop AI header for per-crop mode,
    %                              joint header for joint mode). NaN when
    %                              the keyword is absent.
    %            .FWHM           - FWHM header keyword [pixels] read the same
    %                              way. NaN when absent.
    %            .CalFound       - PC.CalFound. false means selectCalibrators
    %                              produced no matches (fit was skipped).
    %            .NSelectedCalibrators - height(PC.SourceData.Table) i.e. the
    %                              raw calibrator count returned by
    %                              selectCalibrators before any fit-time
    %                              clipping. 0 when CalFound is false.
    %            .CalibTrajectory - PC.CalibTrajectory verbatim (per-stage
    %                              snapshots, each with .SourceData.Table
    %                              carrying Residuals, Used, MagErr,
    %                              MagErr_spectral, MagErr_flux etc.).
    %            .ErrorMessage   - '' on success; a diagnostic string when
    %                              the fit was skipped (e.g. "No calibrators
    %                              matched") or the MException.message on
    %                              a thrown error.
    % Author : D. Kovaleva (Jul 2026)
    % Example:
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans('/data/coadd_run', ...
    %             'MaxVisits', 3, 'Verbose', true);
    %
    %   plot([Rep.NCalib], [Rep.RMS], '.');
    %
    %   % Joint LAST_Joint_2Iter_Split3 campaign (Tran2D cheby1_4), writing 24
    %   % self-contained per-crop calibrated FITS per visit (JointWriteMode
    %   % defaults to 'per-crop'). XPixelJoint/YPixelJoint are left empty so the
    %   % full-frame size is measured from XFULL/YFULL of the first joined visit
    %   % (verify the logged extent matches the LAST geometry):
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans( ...
    %             '/euclid/last/data/LAST.01.05.03', ...
    %             'FieldId',      '1716.c', ...
    %             'Filter',       'clear', ...
    %             'RunModes',     {'joint'}, ...
    %             'OptSeqNames',  {'LAST_Joint_2Iter_Split3'}, ...
    %             'Tran2DTypeJoint', 'cheby1_4', ...
    %             'OutDir',       '/home/dana/tmp/N3/joint_2Iter_Split3');
    %
    %   % Resume that same campaign after an interruption (skip finished visits,
    %   % recompute only what is missing): rerun the identical call with
    %   % 'OverWrite', false.
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans( ...
    %             '/euclid/last/data/LAST.01.05.03', 'FieldId','1716.c', 'Filter','clear', ...
    %             'RunModes', {'joint'}, 'OptSeqNames', {'LAST_Joint_2Iter_Split3'}, ...
    %             'Tran2DTypeJoint', 'cheby1_4', ...
    %             'OutDir', '/home/dana/tmp/N3/joint_2Iter_Split3', 'OverWrite', false);
    %
    %   % Same, but pin the full-frame size explicitly (still logged vs measured):
    %   Rep = pipeline.last.quality.photCalib.batchPhotCalibTrans( ...
    %             '/euclid/last/data/LAST.01.05.03', 'FieldId','1716.c', ...
    %             'RunModes', {'joint'}, 'OptSeqNames', {'LAST_Joint_2Iter_Split3'}, ...
    %             'Tran2DTypeJoint', 'cheby1_4', ...
    %             'XPixelJoint', 6388, 'YPixelJoint', 9576, ...
    %             'OutDir', '/home/dana/tmp/N3/joint_2Iter_Split3');

    arguments
        BaseDir                                 (1,:) char
        Args.FilePattern                        (1,:) char    = 'LAST*_sci_coadd_Image_1.fits'
        Args.Recursive                                logical = true
        Args.FieldId                            (1,:) char    = ''
        Args.Filter                             (1,:) char    = ''
        Args.CropId                                   double  = []
        Args.MaxVisits                          (1,1) double  = Inf
        Args.OptSeqNames                              cell    = {'LAST_Joint_1Iter_AtmosFirst_Split3', ...
                                                                 'LAST_Joint_1Iter_Split3'}
        Args.RunModes                                 cell    = {'per-crop', 'joint'}
        Args.FunListName                        (1,:) char    = 'DefaultLASTFunList'
        Args.Tran2DTypePerCrop                  (1,:) char    = 'cheby1_2'
        Args.Tran2DTypeJoint                    (1,:) char    = 'cheby1_4'
        Args.XPixelPerCrop                      (1,1) double  = 1716
        Args.YPixelPerCrop                      (1,1) double  = 1716
        Args.XPixelJoint                              double  = []
        Args.YPixelJoint                              double  = []
        Args.MagColName                         (1,:) char    = 'phot_g_mean_mag'
        Args.MagRange                           (1,2) double  = [12, 16]
        Args.SigmaClipMethod                    (1,:) char    = 'median'
        Args.AirmassSource                      (1,:) char    = 'compute'
        Args.ApplyNutation                            logical = true
        Args.AuditCalibrators                         logical = true
        Args.CollectCalibTrajectory                   logical = true
        Args.RefSpecSlope                       (1,1) double  = 1.5
        Args.RefSpecPivot                       (1,1) double  = 5500
        Args.UseTypicalX                              logical = true
        Args.NormConvention                     (1,:) char    {mustBeMember(Args.NormConvention, {'raw','center'})} = 'center'
        Args.WeightingMode                      (1,:) char    {mustBeMember(Args.WeightingMode, {'none','spectral','flux','combined'})} = 'combined'
        Args.SystematicErr                      (1,1) double  {mustBeNonnegative} = 0.001
        Args.CalibArgsExtra                           cell    = {}
        Args.OutDir                             (1,:) char    = ''
        Args.OverWrite                                logical = true
        Args.JointWriteMode                     (1,:) char    {mustBeMember(Args.JointWriteMode, {'per-crop','joined','both'})} = 'per-crop'
        Args.Verbose                                  logical = true
        Args.InnerVerbose                             logical = false
    end

    if Args.Verbose
        RecTag = 'non-recursive';
        if Args.Recursive; RecTag = 'recursive'; end
        fprintf('batchPhotCalibTrans: discovering %s under %s (%s glob "%s") ...\n', ...
                RecTag, BaseDir, RecTag, Args.FilePattern);
        DiscT0 = tic;
    end
    Visits = discoverVisits(BaseDir, Args.FilePattern, Args.Recursive, ...
                            Args.FieldId, Args.Filter, Args.CropId);
    if Args.Verbose
        fprintf('  discovery: %d visit group(s) found in %.1f s\n', ...
                numel(Visits), toc(DiscT0));
    end
    if isempty(Visits)
        error('pipeline:last:quality:photCalib:batchPhotCalibTrans:NoVisits', ...
              'No visits matched pattern %s under %s', Args.FilePattern, BaseDir);
    end
    if isfinite(Args.MaxVisits) && Args.MaxVisits < numel(Visits)
        if Args.Verbose
            fprintf('  MaxVisits=%d applied: dropping %d of %d discovered visits\n', ...
                    Args.MaxVisits, numel(Visits) - Args.MaxVisits, numel(Visits));
        end
        Visits = Visits(1:Args.MaxVisits);
    end
    NVisits = numel(Visits);
    NOpt    = numel(Args.OptSeqNames);
    NMode   = numel(Args.RunModes);

    if Args.Verbose
        fprintf('batchPhotCalibTrans: %d visit(s), %d mode(s) x %d recipe(s) = %d configs/visit\n', ...
                NVisits, NMode, NOpt, NMode * NOpt);
    end

    Report = reportRowTemplate();
    Report = Report([]);   % start with 0-length struct array of the right shape

    if ~isempty(Args.OutDir) && ~exist(Args.OutDir, 'dir')
        mkdir(Args.OutDir);
    end

    % Effective joint full-frame size for the Tran2D normalisation (XPixel/YPixel
    % set ParNX = XPixel/2). These are a fixed property of the LAST focal-plane
    % geometry (the CCDSEC/ORIGSEC -> XFULL/YFULL mapping), NOT a per-visit
    % overlap measurement — so they are measured once from the first joined
    % visit's XFULL/YFULL extent and then held constant. Empty means "derive";
    % an explicit XPixelJoint/YPixelJoint is honoured (and still cross-checked
    % against the measured extent for a one-line sanity log).
    XPJoint = Args.XPixelJoint;
    YPJoint = Args.YPixelJoint;
    JointGeomLogged = false;

    for IV = 1:NVisits
        Vis = Visits(IV);
        if Args.Verbose
            fprintf('[%d/%d] %s: loading %d crops...\n', IV, NVisits, Vis.Stem, numel(Vis.Files));
        end

        try
            % Idiomatic LAST coadd loader — the same one used by bulkCalibrate
            % and the rest of the LAST tooling. `AstroImage.readProducts`
            % parses the LAST filename via `AstroFileName`, derives the
            % sibling `_Cat_1.fits` path, reads the FITS header from the
            % Image file into `HeaderData`, and reads the catalog into
            % `CatData`. Both are required by `PhotCalibTrans.calibrate`
            % (header for observation metadata, catalog for calibrator
            % selection). Neither the bare `AstroImage(FileList)`
            % constructor nor `readFileNamesObj` produce this combination
            % correctly for coadd products on this codebase.
            AI = AstroImage.readProducts(Vis.Files, 'ExtraOutProduct', "Cat");
        catch ME
            warning('pipeline:last:quality:photCalib:batchPhotCalibTrans:LoadFailed', ...
                    '%s: AstroImage load failed (%s); skipping visit', Vis.Stem, ME.message);
            continue;
        end

        JCat = []; JHdr = [];
        % Joint mode requires all 24 crops of the visit. When the caller
        % restricted discovery to a single CropId, or the visit only has
        % one file after any filter, skip the join step outright.
        NeedsJoint = any(strcmpi(Args.RunModes, 'joint')) ...
                     && isempty(Args.CropId) ...
                     && numel(Vis.Files) > 1;
        if NeedsJoint
            try
                [JCat, JHdr, ~] = imProc.cat.joinCropsToCatalog(AI, 'Verbose', false);
            catch ME
                warning('pipeline:last:quality:photCalib:batchPhotCalibTrans:JoinFailed', ...
                        '%s: joinCropsToCatalog failed (%s); joint mode will be skipped for this visit', ...
                        Vis.Stem, ME.message);
                JCat = [];
            end

            % Measure the joint full-frame extent from XFULL/YFULL once, and
            % derive XPJoint/YPJoint from it when the caller left them empty.
            if ~isempty(JCat) && ~JointGeomLogged
                MxX = ceil(max(JCat.getCol('XFULL'), [], 'omitnan'));
                MxY = ceil(max(JCat.getCol('YFULL'), [], 'omitnan'));
                XDerived = isempty(XPJoint);
                YDerived = isempty(YPJoint);
                if XDerived; XPJoint = MxX; end
                if YDerived; YPJoint = MxY; end
                if Args.Verbose
                    fprintf(['  joint full-frame extent from XFULL/YFULL of %s: ', ...
                             'measured max=[%g, %g]; using XPixel=%g (%s), YPixel=%g (%s) ', ...
                             '(legacy constant [6388, 9576])\n'], ...
                            Vis.Stem, MxX, MxY, ...
                            XPJoint, ternary(XDerived, 'derived', 'supplied'), ...
                            YPJoint, ternary(YDerived, 'derived', 'supplied'));
                end
                JointGeomLogged = true;
            end
        end

        for IM = 1:NMode
            RunMode = lower(char(Args.RunModes{IM}));
            for IO = 1:NOpt
                OptSeqName = char(Args.OptSeqNames{IO});

                % Resume: when writing to disk and not overwriting, skip any
                % config whose output(s) already exist so a restarted campaign
                % continues instead of recomputing finished work. A visit is
                % "done" for this config only when ALL its expected products
                % are present (a partial per-crop visit is re-run, and
                % writeCalibProduct then fills only the missing crops).
                if ~isempty(Args.OutDir) && ~Args.OverWrite
                    ExpFiles = expectedOutFiles(Args, RunMode, OptSeqName, Vis);
                    if ~isempty(ExpFiles) && all(cellfun(@isfile, ExpFiles))
                        if Args.Verbose
                            fprintf('  mode=%-8s recipe=%s : resume skip (%d output(s) present)\n', ...
                                    RunMode, OptSeqName, numel(ExpFiles));
                        end
                        Row = reportRowTemplate();
                        Row.VisitStem    = Vis.Stem;
                        Row.RunMode      = RunMode;
                        Row.OptSeqName   = OptSeqName;
                        if strcmp(RunMode, 'joint')
                            Row.Tran2DType = Args.Tran2DTypeJoint;
                            Row.CropNumber = 0;
                        else
                            Row.Tran2DType = Args.Tran2DTypePerCrop;
                            Row.CropNumber = NaN;
                        end
                        Row.ErrorMessage = 'resume: output(s) already exist; fit skipped';
                        Report(end+1) = Row; %#ok<AGROW>
                        continue;
                    end
                end

                if Args.Verbose
                    fprintf('  mode=%-8s recipe=%s ...\n', RunMode, OptSeqName);
                end

                try
                    switch RunMode
                        case 'per-crop'
                            Tran2DType = Args.Tran2DTypePerCrop;
                            CA = buildCalibArgs(Args, OptSeqName, Tran2DType, false, [], ...
                                Args.XPixelPerCrop, Args.YPixelPerCrop);
                            [ResultArr, PCarr] = imProc.calib.fitPhotCalibTrans(AI, ...
                                'CreateNewObj', true, ...
                                'UseTypicalX',  Args.UseTypicalX, ...
                                'RefSpecSlope', Args.RefSpecSlope, ...
                                'RefSpecPivot', Args.RefSpecPivot, ...
                                'CalibArgs',    CA, ...
                                'Verbose',      Args.InnerVerbose);
                            for IC = 1:numel(PCarr)
                                if IC <= numel(AI) && ~isempty(AI(IC).HeaderData)
                                    CropHdr = AI(IC).HeaderData;
                                else
                                    CropHdr = [];
                                end
                                Row = extractReportRow(PCarr(IC), RunMode, OptSeqName, ...
                                    Tran2DType, Vis.Stem, IC, CropHdr, ...
                                    Args.FunListName, Args.XPixelPerCrop, Args.YPixelPerCrop);
                                Report(end+1) = Row; %#ok<AGROW>

                                % Write the calibrated per-crop catalog. Name it
                                % after the source crop's Cat product; the header
                                % is taken from the calibrated Result (carries the
                                % PT_* keywords stamped by calibrate).
                                if ~isempty(Args.OutDir) && IC <= numel(ResultArr)
                                    [~, BImg, EImg] = fileparts(Vis.Files{IC});
                                    OutName = strrep([BImg, EImg], '_Image_', '_Cat_');
                                    if ~isempty(ResultArr(IC).HeaderData)
                                        Hdr = ResultArr(IC).HeaderData.Data;
                                    else
                                        Hdr = {};
                                    end
                                    writeCalibProduct(ResultArr(IC).CatData, Args.OutDir, ...
                                        configTag(RunMode, OptSeqName, Tran2DType), ...
                                        OutName, Hdr, Args.OverWrite, Args.Verbose);
                                end
                            end

                        case 'joint'
                            if isempty(JCat)
                                if ~isempty(Args.CropId)
                                    Reason = sprintf('joint mode skipped: CropId=%g restricts discovery to a single crop', Args.CropId);
                                elseif numel(Vis.Files) <= 1
                                    Reason = sprintf('joint mode skipped: only %d crop file(s) matched for this visit', numel(Vis.Files));
                                else
                                    Reason = 'joinCropsToCatalog produced no catalog';
                                end
                                warning('pipeline:last:quality:photCalib:batchPhotCalibTrans:NoJointCat', ...
                                        '%s: %s', Vis.Stem, Reason);
                                Row = reportRowTemplate();
                                Row.VisitStem    = Vis.Stem;
                                Row.RunMode      = RunMode;
                                Row.OptSeqName   = OptSeqName;
                                Row.Tran2DType   = Args.Tran2DTypeJoint;
                                Row.CropNumber   = 0;
                                Row.ErrorMessage = Reason;
                                Report(end+1) = Row; %#ok<AGROW>
                                continue;
                            end
                            Tran2DType = Args.Tran2DTypeJoint;
                            CA = buildCalibArgs(Args, OptSeqName, Tran2DType, true, JHdr, ...
                                XPJoint, YPJoint);
                            [JResult, PC] = imProc.calib.fitPhotCalibTrans(JCat, ...
                                'CreateNewObj', true, ...
                                'UseTypicalX',  Args.UseTypicalX, ...
                                'RefSpecSlope', Args.RefSpecSlope, ...
                                'RefSpecPivot', Args.RefSpecPivot, ...
                                'CalibArgs',    CA, ...
                                'Verbose',      Args.InnerVerbose);
                            Row = extractReportRow(PC, RunMode, OptSeqName, ...
                                Tran2DType, Vis.Stem, 0, JHdr, ...
                                Args.FunListName, XPJoint, YPJoint);
                            Report(end+1) = Row; %#ok<AGROW>

                            % Write calibrated products to disk. 'per-crop'
                            % (default) applies the single global fit back to
                            % each crop's native catalogue (self-contained
                            % per-crop FITS, Tran2D translated to the crop-local
                            % frame and Norm/kx0 re-gauged so Tran2D=0 at the
                            % crop centre). 'joined' writes the single
                            % full-frame catalogue. 'both' writes both.
                            if ~isempty(Args.OutDir)
                                Tag = configTag(RunMode, OptSeqName, Tran2DType);
                                DoPerCrop = any(strcmp(Args.JointWriteMode, {'per-crop','both'}));
                                DoJoined  = any(strcmp(Args.JointWriteMode, {'joined','both'}));

                                if DoPerCrop
                                    writeJointPerCrop(PC, AI, Vis, Tag, Args);
                                end
                                if DoJoined
                                    OutName = sprintf('%s_joint_coadd_Cat_1.fits', Vis.Stem);
                                    if ~isempty(JHdr)
                                        Hdr = JHdr.Data;
                                    else
                                        Hdr = {};
                                    end
                                    writeCalibProduct(JResult, Args.OutDir, Tag, ...
                                        OutName, Hdr, Args.OverWrite, Args.Verbose);
                                end
                            end

                        otherwise
                            error('pipeline:last:quality:photCalib:batchPhotCalibTrans:BadRunMode', ...
                                  'Unknown RunMode %s (allowed: ''per-crop'', ''joint'')', RunMode);
                    end
                catch ME
                    warning('pipeline:last:quality:photCalib:batchPhotCalibTrans:FitFailed', ...
                            '%s | %s | %s: %s', Vis.Stem, RunMode, OptSeqName, ME.message);
                    Row = reportRowTemplate();
                    Row.VisitStem    = Vis.Stem;
                    Row.RunMode      = RunMode;
                    Row.OptSeqName   = OptSeqName;
                    if strcmpi(RunMode, 'joint')
                        Row.Tran2DType = Args.Tran2DTypeJoint;
                        Row.CropNumber = 0;
                    else
                        Row.Tran2DType = Args.Tran2DTypePerCrop;
                        Row.CropNumber = NaN;
                    end
                    Row.ErrorMessage = ME.message;
                    Report(end+1) = Row; %#ok<AGROW>
                end
            end
        end
    end
end


function Visits = discoverVisits(BaseDir, Pattern, Recursive, FieldId, Filter, CropId)
    % Group coadd files by visit stem. Every LAST coadd filename ends in
    % '_<crop>_sci_coadd_<Cat|Image>_<N>.fits' where <crop> is a zero-padded
    % integer. Strip that suffix to identify the visit and sort files by
    % <crop> so downstream AstroImage arrays are always in crop-index order.
    % Optional filters (empty = no filter):
    %   FieldId : substring that must appear as _<FieldId>_ in the basename
    %   Filter  : substring that must appear as _<Filter>_  in the basename
    %   CropId  : specific crop number (1..24)
    if Recursive
        D = dir(fullfile(BaseDir, '**', Pattern));
    else
        D = dir(fullfile(BaseDir, Pattern));
    end
    D = D(~[D.isdir]);
    if isempty(D)
        Visits = struct('Stem', {}, 'Files', {});
        return;
    end

    % Filename-substring filters (fast — no header peek).
    if ~isempty(FieldId)
        D = D(contains({D.name}, ['_' FieldId '_']));
    end
    if ~isempty(Filter)
        D = D(contains({D.name}, ['_' Filter '_']));
    end
    if isempty(D)
        Visits = struct('Stem', {}, 'Files', {});
        return;
    end

    Files = fullfile({D.folder}, {D.name});
    NF = numel(Files);
    Stems = cell(1, NF);
    CropIdx = nan(1, NF);
    for K = 1:NF
        [~, FName, ~] = fileparts(D(K).name);
        Tok = regexp(FName, '^(.+?)_(\d+)_sci_coadd_(?:Image|Cat)_\d+$', 'tokens', 'once');
        if isempty(Tok)
            Stems{K}   = FName;
            CropIdx(K) = NaN;
        else
            Stems{K}   = Tok{1};
            CropIdx(K) = str2double(Tok{2});
        end
    end

    % Explicit-crop filter runs after stem parsing so we can compare against
    % the extracted crop number (also handles zero-padding automatically).
    if ~isempty(CropId)
        KeepC = (CropIdx == CropId);
        Files   = Files(KeepC);
        Stems   = Stems(KeepC);
        CropIdx = CropIdx(KeepC);
    end
    if isempty(Files)
        Visits = struct('Stem', {}, 'Files', {});
        return;
    end

    [Us, ~, IUs] = unique(Stems);
    Visits = repmat(struct('Stem', '', 'Files', {{}}), 1, numel(Us));
    for K = 1:numel(Us)
        Sel = find(IUs == K);
        [~, Order] = sort(CropIdx(Sel));
        Visits(K).Stem  = Us{K};
        Visits(K).Files = Files(Sel(Order));
    end
end


function CA = buildCalibArgs(Args, OptSeqName, Tran2DType, IsJoint, JHdr, XPix, YPix)
    % XPix/YPix are the effective detector size for this mode (per-crop size
    % for per-crop mode; the measured/derived full-frame size for joint mode).
    CA = { ...
        'MagColName',              Args.MagColName, ...
        'MagRange',                Args.MagRange, ...
        'FunListName',             Args.FunListName, ...
        'OptSeqName',              OptSeqName, ...
        'CollectCalibTrajectory',  Args.CollectCalibTrajectory, ...
        'AuditCalibrators',        Args.AuditCalibrators, ...
        'SigmaClipMethod',         Args.SigmaClipMethod, ...
        'AirmassSource',           Args.AirmassSource, ...
        'ApplyNutation',           Args.ApplyNutation, ...
        'Tran2DType',              Tran2DType, ...
        'NormConvention',          Args.NormConvention, ...
        'WeightingMode',           Args.WeightingMode, ...
        'SystematicErr',           Args.SystematicErr, ...
        'Verbose',                 Args.InnerVerbose};
    if IsJoint
        CA = [CA, { ...
            'Metadata',            JHdr, ...
            'PosColNameX',         'XFULL', ...
            'PosColNameY',         'YFULL', ...
            'XPixel',              XPix, ...
            'YPixel',              YPix}];
    else
        CA = [CA, { ...
            'XPixel',              XPix, ...
            'YPixel',              YPix}];
    end
    if ~isempty(Args.CalibArgsExtra)
        CA = [CA, Args.CalibArgsExtra];
    end
end


function S = ternary(Cond, ATrue, AFalse)
    % Small inline branch for building log strings.
    if Cond
        S = ATrue;
    else
        S = AFalse;
    end
end


function Tag = configTag(RunMode, OptSeqName, Tran2DType)
    % Per-config output subdirectory name, keeping products from different
    % (RunMode x OptSeq x Tran2D) configs from colliding on disk.
    Tag = sprintf('%s__%s__%s', RunMode, OptSeqName, Tran2DType);
end


function Files = expectedOutFiles(Args, RunMode, OptSeqName, Vis)
    % Full paths of the calibrated product(s) a given (visit, config) would
    % write. Must mirror the write-path naming exactly so the resume check and
    % the writer agree. Per-crop -> one file per crop. Joint -> per-crop files
    % and/or the joined catalog, per JointWriteMode.
    if strcmp(RunMode, 'joint')
        Tag   = configTag(RunMode, OptSeqName, Args.Tran2DTypeJoint);
        Files = {};
        if any(strcmp(Args.JointWriteMode, {'per-crop', 'both'}))
            for IC = 1:numel(Vis.Files)
                [~, BImg, EImg] = fileparts(Vis.Files{IC});
                OutName = strrep([BImg, EImg], '_Image_', '_Cat_');
                Files{end+1} = fullfile(Args.OutDir, Tag, OutName); %#ok<AGROW>
            end
        end
        if any(strcmp(Args.JointWriteMode, {'joined', 'both'}))
            Files{end+1} = fullfile(Args.OutDir, Tag, ...
                sprintf('%s_joint_coadd_Cat_1.fits', Vis.Stem)); %#ok<AGROW>
        end
    else
        Tag   = configTag(RunMode, OptSeqName, Args.Tran2DTypePerCrop);
        Files = cell(1, numel(Vis.Files));
        for IC = 1:numel(Vis.Files)
            [~, BImg, EImg] = fileparts(Vis.Files{IC});
            OutName    = strrep([BImg, EImg], '_Image_', '_Cat_');
            Files{IC}  = fullfile(Args.OutDir, Tag, OutName);
        end
    end
end


function writeJointPerCrop(PC, AI, Vis, Tag, Args)
    % Apply a joint (full-frame) fit back to each crop's native catalogue and
    % write self-contained per-crop calibrated FITS: one file per crop, named
    % after the source crop's Cat product, with the crop's own header stamped
    % with the crop-local, re-gauged photometric model. Uses the single joint
    % PC for every crop (no per-crop PhotCalibTrans copies) via
    % PhotCalibTrans.calibrateCropFromJointFit, which restores the PC after
    % each crop.
    Ncrop = min(numel(AI), numel(Vis.Files));
    for IC = 1:Ncrop
        if isempty(AI(IC).CatData) || isempty(AI(IC).HeaderData)
            continue;
        end
        [~, BImg, EImg] = fileparts(Vis.Files{IC});
        OutName = strrep([BImg, EImg], '_Image_', '_Cat_');
        OutFile = fullfile(Args.OutDir, Tag, OutName);
        if isfile(OutFile) && ~Args.OverWrite
            if Args.Verbose
                fprintf('    write skip (exists): %s\n', OutFile);
            end
            continue;
        end

        % Crop full-frame section (ORIGSEC) -> [xmin xmax ymin ymax].
        try
            CCDSEC = imUtil.ccdsec.ccdsecStr2num( ...
                AI(IC).HeaderData.getVal('ORIGSEC', 'UseDict', false));
        catch
            warning('pipeline:last:quality:photCalib:batchPhotCalibTrans:NoORIGSEC', ...
                    '%s crop %d: ORIGSEC unreadable; skipping per-crop write', Vis.Stem, IC);
            continue;
        end

        CatOut = AI(IC).CatData.copy();
        [CatOut, HdrOut] = PC.calibrateCropFromJointFit(CatOut, CCDSEC, ...
            AI(IC).HeaderData, 'Verbose', Args.InnerVerbose);

        writeCalibProduct(CatOut, Args.OutDir, Tag, OutName, HdrOut.Data, ...
            Args.OverWrite, Args.Verbose);
    end
end


function writeCalibProduct(CatObj, OutDir, ConfigTag, FileName, HeaderData, OverWrite, Verbose)
    % Write one calibrated AstroCatalog to OutDir/<ConfigTag>/<FileName> as
    % FITS with the given 3-column header cell. Honours OverWrite (skip when
    % the file already exists and OverWrite is false), so campaigns resume.
    Dir = fullfile(OutDir, ConfigTag);
    if ~exist(Dir, 'dir')
        mkdir(Dir);
    end
    OutFile = fullfile(Dir, FileName);
    if isfile(OutFile) && ~OverWrite
        if Verbose
            fprintf('    write skip (exists): %s\n', OutFile);
        end
        return;
    end
    CatObj.write1(OutFile, 'FileType', 'fits', 'Header', HeaderData, 'OverWrite', true);
    if Verbose
        fprintf('    wrote: %s\n', OutFile);
    end
end


function Row = reportRowTemplate()
    Row = struct( ...
        'VisitStem',            '', ...
        'RunMode',              '', ...
        'OptSeqName',           '', ...
        'FunListName',          '', ...
        'Tran2DType',           '', ...
        'XPixel',               NaN, ...
        'YPixel',               NaN, ...
        'CropNumber',           NaN, ...
        'AIRMASS',              NaN, ...
        'FWHM',                 NaN, ...
        'ObsMetadata',          struct(), ...
        'Norm',                 NaN, ...
        'TauAOD500',            NaN, ...
        'PWV_cm',               NaN, ...
        'Center_Ang',           NaN, ...
        'FitParams',            struct(), ...
        'RMS',                  NaN, ...
        'MedianRMS',            NaN, ...
        'ARMS',                 NaN, ...
        'Chi2',                 NaN, ...
        'DOF',                  NaN, ...
        'NCalib',               NaN, ...
        'CalFound',             false, ...
        'NSelectedCalibrators', 0, ...
        'CalibTrajectory',      [], ...
        'ErrorMessage',         '');
end


function Row = extractReportRow(PC, RunMode, OptSeqName, Tran2DType, VisitStem, CropNumber, SrcHeader, FunListName, XPixel, YPixel)
    Row              = reportRowTemplate();
    Row.VisitStem    = VisitStem;
    Row.RunMode      = RunMode;
    Row.OptSeqName   = OptSeqName;
    Row.FunListName  = FunListName;
    Row.Tran2DType   = Tran2DType;
    Row.XPixel       = XPixel;
    Row.YPixel       = YPixel;
    Row.CropNumber   = CropNumber;

    % Header-derived observation metadata. AIRMASS is stamped both here and
    % on PC.AirMass by calibrate (which reads the same keyword); we prefer
    % the header value and fall back to PC.AirMass if the keyword is absent.
    Row.AIRMASS = readHeaderKey(SrcHeader, 'AIRMASS');
    Row.FWHM    = readHeaderKey(SrcHeader, 'FWHM');

    if isempty(PC)
        Row.ErrorMessage = 'PC missing (fitPhotCalibTrans returned no object)';
        return;
    end

    if ~isfinite(Row.AIRMASS) && ~isempty(PC.AirMass) && isfinite(PC.AirMass)
        Row.AIRMASS = PC.AirMass;
    end

    % Observation metadata snapshot — everything rebuildPCFromReportRow
    % needs to reconstruct a PhotCalibTrans without re-reading the FITS.
    Row.ObsMetadata = struct( ...
        'AirMass',  PC.AirMass, ...
        'ExpTime',  PC.ExpTime, ...
        'NCoadd',   PC.NCoadd, ...
        'Temp',     PC.Temp, ...
        'Pressure', PC.Pressure);
    Row.CalFound = logical(PC.CalFound);
    if ~isempty(PC.SourceData) && ~isempty(PC.SourceData.Table)
        Row.NSelectedCalibrators = height(PC.SourceData.Table);
    end

    if isempty(PC.TransModel)
        Row.ErrorMessage = 'PC.TransModel empty (calibrate aborted before model init — likely a header/metadata problem)';
        return;
    end

    % Fit parameters: one scalar field per TransModel parameter, keyed by
    % the parameter Description sanitised into a valid MATLAB identifier.
    % Present regardless of whether the fit ran — when CalFound=false the
    % values are the model's initial defaults, not a fit result.
    FP = struct();
    try
        Info = PC.TransModel.getAllFunPar();
        for K = 1:numel(Info.Name)
            Nm = matlab.lang.makeValidName(char(Info.Name{K}));
            FP.(Nm) = Info.Val(K);
        end
    catch
        % leave FP empty
    end
    if ~isempty(PC.TransModel.Tran2DObj) && PC.TransModel.UseTran2D
        FP.Tran2D_ParX = PC.TransModel.Tran2DObj.ParX(:).';
    end
    Row.FitParams = FP;

    % Promote the four canonical fitted scalars to top-level columns so a
    % table conversion doesn't need to reach inside the FitParams struct.
    % Source parameter names are the CompositeFun descriptions; column
    % names on the Report row follow the caller-facing convention.
    Row.Norm       = getStructFieldOr(FP, 'Norm',       NaN);
    Row.TauAOD500  = getStructFieldOr(FP, 'TauAod500',  NaN);
    Row.PWV_cm     = getStructFieldOr(FP, 'PWV_cm',     NaN);
    Row.Center_Ang = getStructFieldOr(FP, 'Center_Ang', NaN);

    if ~PC.CalFound
        Row.ErrorMessage = sprintf( ...
            'selectCalibrators found no matches — fit skipped. Check catsHTM catalog reachability (SOC_PATH/catsHTM env), CalibCatName (=%s), MagRange, and SearchRadius.', ...
            defaultIfEmpty(getFieldSafe(PC, 'CalibCatName'), 'GAIADR3spec'));
        return;
    end

    % Quality metrics derived from the final trajectory snapshot (final
    % post-stage state after every optimisation stage of every outer iter).
    if ~isempty(PC.CalibTrajectory)
        Traj = PC.CalibTrajectory;
        Last = Traj(end);
        Row.CalibTrajectory = Traj;
        Row.ARMS   = Last.ARMS;
        Row.NCalib = Last.NumRemaining;

        Tab = Last.SourceData.Table;
        if ismember('Residuals', Tab.Properties.VariableNames) && ...
                ismember('Used', Tab.Properties.VariableNames)
            R    = Tab.Residuals;
            Used = logical(Tab.Used);
            Rok  = R(Used & isfinite(R));
            if ~isempty(Rok)
                Row.RMS       = sqrt(mean(Rok.^2));
                Row.MedianRMS = sqrt(median(Rok.^2));
            end
        end
    end

    % Chi2 / DOF from the CompositeFun (updated at the end of every fitPar).
    Row.Chi2 = PC.TransModel.Chi2;
    Row.DOF  = PC.TransModel.DOF;
end


function V = getFieldSafe(Obj, Field)
    if isprop(Obj, Field) || isfield(Obj, Field)
        V = Obj.(Field);
    else
        V = [];
    end
end


function V = defaultIfEmpty(V, Default)
    if isempty(V)
        V = Default;
    end
end


function V = getStructFieldOr(S, F, Default)
    % Return S.(F) if the field exists and is non-empty, else Default.
    if isstruct(S) && isfield(S, F) && ~isempty(S.(F))
        V = S.(F);
    else
        V = Default;
    end
end


function Val = readHeaderKey(Header, Key)
    % Read a scalar numeric keyword from an AstroHeader, returning NaN when
    % the key is missing / non-numeric / the header is empty. Handles the
    % two accessor styles: getStructKey (returns a struct) and getVal
    % (returns the value directly).
    Val = NaN;
    if isempty(Header)
        return;
    end
    try
        if ismethod(Header, 'getStructKey')
            S = Header.getStructKey(Key);
            if isfield(S, Key) && ~isempty(S.(Key)) && isnumeric(S.(Key))
                Val = double(S.(Key));
            end
        elseif ismethod(Header, 'getVal')
            Raw = Header.getVal(Key);
            if ~isempty(Raw) && isnumeric(Raw)
                Val = double(Raw);
            end
        end
    catch
        % leave Val = NaN
    end
end
