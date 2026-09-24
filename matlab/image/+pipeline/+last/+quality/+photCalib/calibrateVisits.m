function Result = calibrateVisits(Input, Args)
    % Calibrate visits with PhotCalibTrans and summarise the per-crop fit.
    % Description: Runs imProc.calib.fitPhotCalibTrans (the mainstream
    %              per-crop photometric calibration) on every visit and
    %              returns the PhotCalibTrans objects, the calibrated
    %              catalogs, and per-crop fit diagnostics (fit RMS and centre
    %              zero-point) as flat per-visit arrays. Replaces the
    %              mode-based calibratePhotModes and the disk-survey
    %              batchPhotCalib; the legacy shapeimage / perimage / perset
    %              calibration modes are gone — only the mainstream fit
    %              (formerly 'percrop') remains.
    %
    %              Input forms:
    %                (1) char/string BaseDir - visit subdirectories are
    %                    discovered and loaded (catalog + header) via
    %                    loadVisitCatHdr;
    %                (2) cell of AstroImage arrays - one pre-loaded visit per
    %                    cell (each a 1xNcrop array);
    %                (3) a single AstroImage array - treated as one visit.
    %
    % Input  : - Input - BaseDir path, cell of AstroImage arrays, or one
    %            AstroImage array.
    %          * ...,key,val,...
    %            'VisitGlob'     - Glob for visit subdirectories (path input).
    %                              Default '*v*'.
    %            'Recursive'     - Recurse for visit dirs (path input).
    %                              Default false.
    %            'ExcludeVisits' - Visit names/paths to skip (path input).
    %                              Default {}.
    %            'FileType'      - 'coadd' | 'proc' (path input).
    %                              Default 'coadd'.
    %            'FieldId'       - Keep only this field id (path input).
    %                              Default '' (no filter).
    %            'CropID'        - Keep only this crop index (path input).
    %                              Default [] (all crops).
    %            'CalibArgs'     - Cell of extra NV pairs forwarded to
    %                              imProc.calib.fitPhotCalibTrans (e.g.
    %                              {'ApplyConstBand',true,'ConstBandParams',CBP}).
    %                              Default {}.
    %            'Ncrop'         - Crop count for the output matrices.
    %                              Default [] (inferred from the data).
    %            'OutFile'       - .mat path to cache the Result struct.
    %                              Default '' (no caching).
    %            'ForceRecalc'   - Recompute even if OutFile exists.
    %                              Default false.
    %            'Verbose'       - Print per-visit progress. Default false.
    % Output : - Result struct with fields:
    %            .PC        - 1xNvisit cell of PhotCalibTrans arrays.
    %            .Cats      - 1xNvisit cell of calibrated AstroCatalog arrays.
    %            .FitRMS    - [Nvisit x Ncrop] transmission-fit RMS.
    %            .ZPcenter  - [Nvisit x Ncrop] centre zero-point.
    %            .Success   - [Nvisit x Ncrop] logical fit-success flags.
    %            .JD        - [Nvisit x Ncrop] Julian Date (header 'JD').
    %            .VisitName - 1xNvisit cell of visit names ('' in-memory).
    %            .VisitDirs - 1xNvisit cell of visit paths ('' in-memory).
    %            .Ncrop     - crop count of the output matrices.
    %            .Args      - echo of input arguments.
    % Author : photCalib package refactor (2026-05)
    % Example: R = pipeline.last.quality.photCalib.calibrateVisits( ...
    %              '/data/2025/07/08', 'FileType', 'coadd');
    %          R = pipeline.last.quality.photCalib.calibrateVisits(AIcell);

    arguments
        Input
        Args.VisitGlob     {mustBeTextScalar} = '*v*'
        Args.Recursive     logical            = false
        Args.ExcludeVisits                    = {}
        Args.FileType      {mustBeMember(Args.FileType,{'coadd','proc'})} = 'coadd'
        Args.FieldId                          = ''
        Args.CropID        double {mustBeInteger, mustBeNonnegative} = []
        Args.CalibArgs     cell               = {}
        Args.Ncrop                            = []
        Args.OutFile       {mustBeText}       = ''
        Args.ForceRecalc   logical            = false
        Args.Verbose       logical            = false
    end

    % --- Cache load ----------------------------------------------------
    if ~isempty(Args.OutFile) && exist(char(Args.OutFile),'file') && ~Args.ForceRecalc
        try
            S = load(char(Args.OutFile), 'Result');
            if isfield(S, 'Result')
                Result = S.Result;
                if Args.Verbose
                    fprintf('calibrateVisits: loaded cached %s\n', char(Args.OutFile));
                end
                return;
            end
        catch ME
            if Args.Verbose
                fprintf('calibrateVisits: cache %s unreadable (%s) - recomputing\n', ...
                    char(Args.OutFile), ME.message);
            end
        end
    end

    % --- Resolve input to a cell of AstroImage arrays (one per visit) --
    if ischar(Input) || (isstring(Input) && isscalar(Input))
        [VisitDirs, VisitName] = discoverVisits(char(Input), ...
            'VisitGlob', Args.VisitGlob, 'Recursive', Args.Recursive, ...
            'ExcludeVisits', Args.ExcludeVisits);
        AIc = pipeline.last.load.loadVisitCatHdr( ...
            'VisitDirs', string(VisitDirs), ...
            'FileType',  Args.FileType, ...
            'FieldId',   Args.FieldId, ...
            'CropID',    Args.CropID, ...
            'Verbose',   Args.Verbose);
        IsInMemory = false;
    else
        [AIc, ~] = resolveInput(Input);
        if isempty(AIc)
            error('photCalib:calibrateVisits:BadInput', ...
                ['Input must be a BaseDir path, a cell of AstroImage ', ...
                 'arrays, or an AstroImage array.']);
        end
        IsInMemory = true;
        VisitName  = repmat({''}, 1, numel(AIc));
        VisitDirs  = repmat({''}, 1, numel(AIc));
    end

    Nvisit = numel(AIc);
    if Nvisit == 0
        error('photCalib:calibrateVisits:NoVisits', 'No visits to calibrate.');
    end
    if numel(VisitName) ~= Nvisit
        % loadVisitCatHdr returned a differently-sized cell - fall back to
        % blank labels rather than risk a misaligned name vector.
        VisitName = repmat({''}, 1, Nvisit);
        VisitDirs = repmat({''}, 1, Nvisit);
    end

    % --- Crop count ----------------------------------------------------
    if isempty(Args.Ncrop)
        Ncrop = 0;
        for Iv = 1:Nvisit
            if ~isempty(AIc{Iv}); Ncrop = max(Ncrop, numel(AIc{Iv})); end
        end
        if Ncrop == 0; Ncrop = 24; end
    else
        Ncrop = Args.Ncrop;
    end

    % --- Per-visit calibration -----------------------------------------
    PC       = cell(1, Nvisit);
    Cats     = cell(1, Nvisit);
    FitRMS   = nan(Nvisit, Ncrop);
    ZPcenter = nan(Nvisit, Ncrop);
    Success  = false(Nvisit, Ncrop);
    JD       = nan(Nvisit, Ncrop);

    for Iv = 1:Nvisit
        AIv = AIc{Iv};
        if isempty(AIv); continue; end
        if IsInMemory
            % fitPhotCalibTrans mutates its input - never touch the
            % caller's in-memory objects.
            AIv = AIv.copy();
        end

        T0 = tic;
        try
            [Res, PCv] = imProc.calib.fitPhotCalibTrans(AIv, ...
                'Verbose', false, Args.CalibArgs{:});
        catch ME
            if Args.Verbose
                fprintf('  [%2d/%2d] %s : fit failed (%s)\n', ...
                    Iv, Nvisit, VisitName{Iv}, ME.message);
            end
            continue;
        end

        PC{Iv}   = PCv;
        Cats{Iv} = [Res.CatData];

        for Ic = 1:min(numel(PCv), Ncrop)
            PCic = PCv(Ic);
            Success(Iv,Ic) = ~isempty(PCic.TransModel);
            try
                Jv = Res(Ic).HeaderData.getVal('JD');
                if isnumeric(Jv) && isscalar(Jv) && isfinite(Jv)
                    JD(Iv,Ic) = Jv;
                end
            catch
            end
            if ~isempty(PCic.TransModel)
                FitRMS(Iv,Ic)   = PCic.TransModel.RMS;
                ZPcenter(Iv,Ic) = localCenterZP(PCic);
            end
        end

        if Args.Verbose
            fprintf('  [%2d/%2d] %s : %d/%d success, %.1f s\n', ...
                Iv, Nvisit, VisitName{Iv}, sum(Success(Iv,:)), numel(PCv), toc(T0));
        end
    end

    % --- Assemble ------------------------------------------------------
    Result = struct();
    Result.PC        = PC;
    Result.Cats      = Cats;
    Result.FitRMS    = FitRMS;
    Result.ZPcenter  = ZPcenter;
    Result.Success   = Success;
    Result.JD        = JD;
    Result.VisitName = VisitName;
    Result.VisitDirs = VisitDirs;
    Result.Ncrop     = Ncrop;
    Result.Args      = Args;

    if Args.Verbose
        Vals = FitRMS(isfinite(FitRMS));
        if ~isempty(Vals)
            fprintf('calibrateVisits: %d visit(s), FitRMS median %.4f\n', ...
                Nvisit, median(Vals));
        end
    end

    if ~isempty(Args.OutFile)
        saveResult(char(Args.OutFile), Result, 'VarName', 'Result', ...
            'Verbose', Args.Verbose);
    end
end

% =========================================================================
function ZP = localCenterZP(PCic)
    % Centre zero-point: base ZP with the Tran2D centre offset removed, so
    % Norm absorbs the Tran2D centre value (Tran2D = 0 at the crop centre).
    ZP = PCic.evaluateZP();
    if ~isempty(PCic.TransModel.Tran2DObj) && PCic.TransModel.UseTran2D
        Xc = PCic.TransModel.Tran2DObj.ParNX(1);
        Yc = PCic.TransModel.Tran2DObj.ParNY(1);
        ZP = ZP - PCic.TransModel.Tran2DObj.forward([Xc, Yc]);
    end
end
