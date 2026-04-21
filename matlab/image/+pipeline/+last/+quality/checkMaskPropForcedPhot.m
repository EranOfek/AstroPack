function Report = checkMaskPropForcedPhot(NewAI, RefAI, Args)
    % Check whether a named mask bit survives forced photometry.
    %   Applies an operation Op to (NewAI, RefAI) — by default AstroDiff.register
    %   — that typically modifies the mask of one or both images (e.g. register
    %   adds the NaN bit on the non-overlap region of Ref). Then compares, per
    %   source, the ground truth (does a FlagsHalfSize x FlagsHalfSize window
    %   of the post-Op mask contain the bit?) against the FLAGS column returned
    %   by forced photometry. Runs against imProc.sources.forcedPhotNew (new)
    %   and/or imProc.sources.forcedPhot (old) so the user can confirm a fix
    %   or a regression.
    %
    %   Verdict per (Target, Version) is Kept = (FN == 0) among evaluable
    %   sources (edge-clipped sources with FLAGS = NaN are excluded from the
    %   per-source contingency table).
    %
    %   Verbose defaults to true, matching the pipeline.last.quality package
    %   convention.
    %
    %   Side effect: Back / Var are populated on the post-Op AstroImages if
    %   unpopulated (forcedPhotNew requires them).
    %
    % Input  : - NewAI. A populated AstroImage (Image, Mask, WCS).
    %          - RefAI. A populated AstroImage (Image, Mask, WCS).
    %          * ...,key,val,...
    %            'Op'           - Function handle: [NewOut, RefOut] = Op(NewAI, RefAI).
    %                             Default is [], which selects AstroDiff.register.
    %            'BitName'      - Name of the mask bit to track, resolved via
    %                             BitDictionary. Default is 'NaN'.
    %            'BitDictName'  - BitDictionary name. Default is
    %                             'BitMask.Image.Default'.
    %            'Coo'          - Nx2 matrix of source positions. Empty
    %                             auto-generates a pixel grid in the post-Op
    %                             frame. Default is [].
    %            'CooUnits'     - 'pix' | 'deg' | 'rad'. Default is 'pix'.
    %            'GridStep'     - Step in pixels for the auto-grid. Default 60.
    %            'GridMargin'   - Margin from image edge for the auto-grid.
    %                             Default 40.
    %            'FlagsHalfSize'- HalfSize of the OR window read by
    %                             forcedPhot{,New} and used for ground truth.
    %                             Default is 3.
    %            'MinEdgeDist'  - MinEdgeDist passed to forcedPhot{,New}.
    %                             Sources closer to the edge get FLAGS=NaN
    %                             and are excluded from the contingency.
    %                             Default is 20.
    %            'TestNew'      - Run imProc.sources.forcedPhotNew. Default true.
    %            'TestOld'      - Run imProc.sources.forcedPhot. Default true.
    %            'ReportFile'   - Optional .mat save path. Default is ''.
    %            'Verbose'      - Print progress/summary. Default is true.
    % Output : - Report struct with fields:
    %            .Timestamp   - datetime of the run
    %            .BitName, .BitIndex, .BitValue - the tracked bit
    %            .OpName      - string form of the operation applied
    %            .X, .Y       - pixel positions used (Nsrc x 1)
    %            .MaskHasBit_New, .MaskHasBit_Ref - logical Nsrc x 1: does a
    %                           FlagsHalfSize window around (X,Y) on the post-Op
    %                           mask of that target contain the bit?
    %            .FLAGS       - struct: FLAGS.(Target)_(Version) = Nsrc x 1
    %                           numeric column from the forced-phot catalog;
    %                           NaN where forcedPhot edge-clipped the source.
    %            .Results     - Results table: Target, Version, Nsrc,
    %                           NmaskHasBit, NcatHasBit, TP, FN, FP, TN,
    %                           EdgeClipped, Kept.
    %            .Errors      - struct of per-step error messages.
    %            .Summary     - text summary.
    % Author : Dana Kovaleva (Apr 2026)
    % Example: % Default: register + NaN + auto-grid + both versions
    %          R = pipeline.last.quality.checkMaskPropForcedPhot(NewAI, RefAI);
    %          % Custom operation and bit:
    %          Op = @(N,R) myCustomOp(N,R);
    %          R = pipeline.last.quality.checkMaskPropForcedPhot(NewAI, RefAI, ...
    %                  'Op', Op, 'BitName', 'Overlap');
    %          % Inspect verdict:
    %          disp(R.Results);

    arguments
        NewAI AstroImage
        RefAI AstroImage
        Args.Op                                = [];
        Args.BitName       (1,:) char          = 'NaN';
        Args.BitDictName   (1,:) char          = 'BitMask.Image.Default';
        Args.Coo                               = [];
        Args.CooUnits      (1,:) char          = 'pix';
        Args.GridStep      (1,1) double        = 60;
        Args.GridMargin    (1,1) double        = 40;
        Args.FlagsHalfSize (1,1) double        = 3;
        Args.MinEdgeDist   (1,1) double        = 20;
        Args.TestNew       (1,1) logical       = true;
        Args.TestOld       (1,1) logical       = true;
        Args.ReportFile    (1,:) char          = '';
        Args.Verbose       (1,1) logical       = true;
    end

    Errors = struct();

    % --- Resolve Op ---
    if isempty(Args.Op)
        Op     = @localDefaultRegisterOp;
        OpName = 'AstroDiff.register';
    else
        Op     = Args.Op;
        OpName = func2str(Op);
    end

    % --- Resolve bit via dictionary ---
    BD     = BitDictionary(Args.BitDictName);
    BitInd = double(BD.name2bit({Args.BitName}));
    if isnan(BitInd)
        error('Bit name "%s" not in dictionary %s', Args.BitName, Args.BitDictName);
    end
    BitVal = bitshift(uint32(1), uint32(BitInd));

    if Args.Verbose
        fprintf('checkMaskPropForcedPhot: Op=%s, bit=%s (%d, val=%u)\n', ...
                OpName, Args.BitName, BitInd, BitVal);
    end

    % --- 1. Apply operation ---
    try
        [NewPost, RefPost] = Op(NewAI, RefAI);
    catch ME
        Errors.Op = ME.message;
        error('Operation failed: %s', ME.message);
    end

    if Args.Verbose
        fprintf('  operation done\n');
    end

    % --- 2. Resolve positions to pixel (X, Y) in the post-Op frame ---
    if isempty(Args.Coo)
        [Ny, Nx] = size(RefPost.Image);
        Step   = Args.GridStep;
        Margin = Args.GridMargin;
        [Xg, Yg] = meshgrid(Margin:Step:(Nx-Margin), Margin:Step:(Ny-Margin));
        X = Xg(:);
        Y = Yg(:);
        CooUnitsFP = 'pix';
        CooFP      = [X, Y];
        if Args.Verbose
            fprintf('  auto-grid: %d positions (step=%d, margin=%d)\n', ...
                    numel(X), Step, Margin);
        end
    else
        CooFP      = Args.Coo;
        CooUnitsFP = Args.CooUnits;
        if strcmpi(Args.CooUnits, 'pix')
            X = Args.Coo(:,1);
            Y = Args.Coo(:,2);
        else
            [X, Y] = NewPost.WCS.sky2xy(Args.Coo(:,1), Args.Coo(:,2), ...
                                        'InUnits', Args.CooUnits);
        end
    end
    Nsrc = numel(X);

    % --- 3. Populate Back on both targets (required by forcedPhotNew) ---
    try
        NewPost = imProc.background.background(NewPost);
    catch ME
        Errors.background_New = ME.message;
    end
    try
        RefPost = imProc.background.background(RefPost);
    catch ME
        Errors.background_Ref = ME.message;
    end

    % --- 4. Ground truth: window lookup on each target's post-Op mask ---
    MaskHasBit_New = localWindowHasBit(uint32(NewPost.Mask), X, Y, BitVal, Args.FlagsHalfSize);
    MaskHasBit_Ref = localWindowHasBit(uint32(RefPost.Mask), X, Y, BitVal, Args.FlagsHalfSize);

    % --- 5. Run forced photometry on each target, per requested version ---
    FLAGS = struct();
    if Args.TestNew
        [FLAGS.New_new, Errors] = localCallForcedPhotNew(NewPost, CooFP, CooUnitsFP, ...
                                    Args.MinEdgeDist, Args.FlagsHalfSize, ...
                                    'New_new', Errors, Args.Verbose);
        [FLAGS.Ref_new, Errors] = localCallForcedPhotNew(RefPost, CooFP, CooUnitsFP, ...
                                    Args.MinEdgeDist, Args.FlagsHalfSize, ...
                                    'Ref_new', Errors, Args.Verbose);
    end
    if Args.TestOld
        [FLAGS.New_old, Errors] = localCallForcedPhotOld(NewPost, CooFP, CooUnitsFP, ...
                                    Args.MinEdgeDist, Args.FlagsHalfSize, ...
                                    'New_old', Errors, Args.Verbose);
        [FLAGS.Ref_old, Errors] = localCallForcedPhotOld(RefPost, CooFP, CooUnitsFP, ...
                                    Args.MinEdgeDist, Args.FlagsHalfSize, ...
                                    'Ref_old', Errors, Args.Verbose);
    end

    % --- 6. Results table ---
    Results = localBuildResults({'New','Ref'}, {MaskHasBit_New, MaskHasBit_Ref}, ...
                                FLAGS, BitVal, Args.TestNew, Args.TestOld, Nsrc);

    % --- 7. Summary and assemble Report ---
    Summary = localBuildSummary(Results, Args.BitName, OpName, Nsrc, Errors);

    Report                = struct();
    Report.Timestamp      = datetime('now');
    Report.BitName        = Args.BitName;
    Report.BitIndex       = BitInd;
    Report.BitValue       = double(BitVal);
    Report.OpName         = OpName;
    Report.X              = X;
    Report.Y              = Y;
    Report.MaskHasBit_New = MaskHasBit_New;
    Report.MaskHasBit_Ref = MaskHasBit_Ref;
    Report.FLAGS          = FLAGS;
    Report.Results        = Results;
    Report.Errors         = Errors;
    Report.Summary        = Summary;

    if Args.Verbose
        fprintf('\n%s\n', Summary);
        disp('Results:'); disp(Results);
    end

    if ~isempty(Args.ReportFile)
        save(Args.ReportFile, 'Report');
        if Args.Verbose
            fprintf('Report saved to: %s\n', Args.ReportFile);
        end
    end
end


% =========================================================================

function [NewOut, RefOut] = localDefaultRegisterOp(NewAI, RefAI)
    % Default operation: AstroDiff.register (Ref resampled into New frame).
    AD = AstroDiff(NewAI, RefAI);
    AD.register;
    NewOut = AD.New;
    RefOut = AD.Ref;
end


function HasBit = localWindowHasBit(Mask, X, Y, BitVal, HalfSize)
    % Per source, is any pixel in a (2*HalfSize+1)^2 window around (X,Y)
    % carrying BitVal? Clamps the window at the image boundary.
    [Ny, Nx] = size(Mask);
    Nsrc     = numel(X);
    HasBit   = false(Nsrc, 1);
    Xr = round(X);
    Yr = round(Y);
    for k = 1:Nsrc
        Xl = max(1,  Xr(k) - HalfSize);
        Xh = min(Nx, Xr(k) + HalfSize);
        Yl = max(1,  Yr(k) - HalfSize);
        Yh = min(Ny, Yr(k) + HalfSize);
        if Xl<=Xh && Yl<=Yh
            HasBit(k) = any(bitand(Mask(Yl:Yh, Xl:Xh), BitVal) > 0, 'all');
        end
    end
end


function [Flags, Errors] = localCallForcedPhotNew(Target, Coo, CooUnits, MinEdgeDist, FlagsHalfSize, Tag, Errors, Verbose)
    % Call imProc.sources.forcedPhotNew, extract FLAGS as Nsrc x 1 double.
    % Uses suffixed aperture names + explicit FLAGS to work around the
    % known forcedPhotNew ColCell pitfalls.
    ColCell = {'JD','XPEAK','YPEAK','BACK_IM','VAR_IM', ...
               'X1','Y1','X2','Y2','XY', ...
               'FLUX_APER_1','FLUX_APER_2','FLUX_APER_3', ...
               'APER_AREA_1','APER_AREA_2','APER_AREA_3', ...
               'BACK_ANNULUS','STD_ANNULUS','BACKMAG_ANNULUS', ...
               'FLUXERR_APER_1','FLUXERR_APER_2','FLUXERR_APER_3', ...
               'MAG_APER_1','MAG_APER_2','MAG_APER_3', ...
               'MAGERR_APER_1','MAGERR_APER_2','MAGERR_APER_3', ...
               'FLUX_PSF','MAG_PSF','MAGERR_PSF','SN','CHI2DOF', ...
               'FLUX_XYPEAK','FLAGS','X','Y'};

    try
        MS = imProc.sources.forcedPhotNew(Target, ...
                'Coo',             Coo, ...
                'CooUnits',        CooUnits, ...
                'AddRefStarsDist', 0, ...
                'ColCell',         ColCell, ...
                'ReconstructPSF',  true, ...
                'OutputType',      'MatchedSources', ...
                'MinEdgeDist',     MinEdgeDist, ...
                'FlagsHalfSize',   FlagsHalfSize);
        Flags = MS.Data.FLAGS(:);
    catch ME
        Flags = [];
        Errors.(['forcedPhotNew_' Tag]) = ME.message;
        if Verbose
            fprintf('  forcedPhotNew(%s) failed: %s\n', Tag, ME.message);
        end
    end
end


function [Flags, Errors] = localCallForcedPhotOld(Target, Coo, CooUnits, MinEdgeDist, FlagsHalfSize, Tag, Errors, Verbose)
    % Call imProc.sources.forcedPhot, extract FLAGS as Nsrc x 1 double.
    try
        MS = imProc.sources.forcedPhot(Target, ...
                'Coo',             Coo, ...
                'CooUnits',        CooUnits, ...
                'AddRefStarsDist', 0, ...
                'ReconstructPSF',  true, ...
                'OutType',         'MatchedSources', ...
                'MinEdgeDist',     MinEdgeDist, ...
                'FlagsHalfSize',   FlagsHalfSize);
        Flags = MS.Data.FLAGS(:);
    catch ME
        Flags = [];
        Errors.(['forcedPhot_' Tag]) = ME.message;
        if Verbose
            fprintf('  forcedPhot(%s) failed: %s\n', Tag, ME.message);
        end
    end
end


function R = localBuildResults(Targets, MaskC, FLAGS, BitVal, TestNew, TestOld, Nsrc)
    % One row per (Target, Version). Evaluable sources = those not
    % edge-clipped (FLAGS ~ NaN). TP/FN/FP/TN over evaluable sources.
    Versions = {};
    if TestNew, Versions{end+1} = 'new'; end
    if TestOld, Versions{end+1} = 'old'; end

    Ntgt = numel(Targets);
    Nver = numel(Versions);
    Nrow = Ntgt * Nver;

    TargetCol  = strings(Nrow,1);
    VersionCol = strings(Nrow,1);
    NsrcCol    = zeros(Nrow,1);
    NmaskCol   = zeros(Nrow,1);
    NcatCol    = zeros(Nrow,1);
    TPcol      = zeros(Nrow,1);
    FNcol      = zeros(Nrow,1);
    FPcol      = zeros(Nrow,1);
    TNcol      = zeros(Nrow,1);
    EdgeCol    = zeros(Nrow,1);
    KeptCol    = false(Nrow,1);

    K = 0;
    for iT = 1:Ntgt
        Mask = MaskC{iT};
        for iV = 1:Nver
            K = K + 1;
            FKey = sprintf('%s_%s', Targets{iT}, Versions{iV});
            TargetCol(K)  = string(Targets{iT});
            VersionCol(K) = string(Versions{iV});
            NsrcCol(K)    = Nsrc;
            NmaskCol(K)   = nnz(Mask);

            Has = isfield(FLAGS, FKey) && ~isempty(FLAGS.(FKey));
            if Has
                F      = FLAGS.(FKey);
                Edge   = isnan(F);
                Fu     = uint32(F);
                Fu(Edge) = 0;
                CatHas = bitand(Fu, BitVal) > 0;
                Eval   = ~Edge;
                NcatCol(K) = nnz(CatHas);
                TPcol(K)   = nnz(Eval &  Mask &  CatHas);
                FNcol(K)   = nnz(Eval &  Mask & ~CatHas);
                FPcol(K)   = nnz(Eval & ~Mask &  CatHas);
                TNcol(K)   = nnz(Eval & ~Mask & ~CatHas);
                EdgeCol(K) = nnz(Edge);
                KeptCol(K) = (FNcol(K) == 0);
            end
        end
    end

    R = table(TargetCol, VersionCol, NsrcCol, NmaskCol, NcatCol, ...
              TPcol, FNcol, FPcol, TNcol, EdgeCol, KeptCol, ...
        'VariableNames', {'Target','Version','Nsrc', ...
                          'NmaskHasBit','NcatHasBit', ...
                          'TP','FN','FP','TN','EdgeClipped','Kept'});
end


function S = localBuildSummary(Results, BitName, OpName, Nsrc, Errors)
    % Short text summary.
    Nerr  = numel(fieldnames(Errors));
    if height(Results) > 0
        AllKept = all(Results.Kept);
        AnyFN   = any(Results.FN > 0);
    else
        AllKept = false;
        AnyFN   = false;
    end
    Verdict = 'UNDETERMINED';
    if height(Results) > 0
        if AllKept
            Verdict = 'KEPT (FN = 0 across all tested combinations)';
        elseif AnyFN
            Verdict = 'LOST (FN > 0 in at least one combination)';
        end
    end

    S = sprintf(['checkMaskPropForcedPhot Report\n', ...
                 '==============================\n', ...
                 'Operation : %s\n', ...
                 'Bit       : %s\n', ...
                 'Positions : %d\n', ...
                 'Errors    : %d\n', ...
                 'Verdict   : %s\n'], ...
                OpName, BitName, Nsrc, Nerr, Verdict);
end
