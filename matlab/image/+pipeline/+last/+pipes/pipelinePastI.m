function [Status, TableRaw, AllSI, MS, Coadd, OnlyMP, JD, GaiaCone] = pipelinePastI(RawImageList, CI, Args)
    % pipelinePastI - Full single-visit reduction for PAST (1..N same-field images).
    %
    %   A drop-in replacement for pipeline.last.pipes.pipelineI, with the SAME
    %   input/output interface, so pipeline.last.pipes.PipelineDemon can call
    %   either one. A "visit" is 1..N images of the SAME field. Each image is
    %   reduced (prePrep quality -> optional dark/flat -> bin -> background ->
    %   source+PSF extraction -> astrometry -> Gaia colour -> photometric
    %   calibration -> catalog/header completion). When N>1 the images are
    %   coadded and the coadd is processed the same way, plus a matched-sources
    %   product across the epochs. When N==1 there is NO coadd and NO
    %   matched-sources product. Asteroid matching runs on the final product.
    %
    %   Differences from LAST pipelineI:
    %     - FULL frame (no 24 crops): small field, long f-ratio, low distortion.
    %     - Visit size 1..N (LAST assumes >10); prePrep MinNim default 1.
    %     - PAST plate scale ~0.204"/pix native (0.408 binned 2x2); the astrometry
    %       pattern match is scale-sensitive to ~0.5%, so this is a hard default.
    %     - Oversampled PSF: detection/photometry/moment radii scaled up.
    %     - Mask-aware, variance-propagating binning.
    %     - No pipelineII (image subtraction): the demon skips it for PAST.
    %
    %   Part of the AstroPack LAST pipeline package (pipeline.last.pipes); the
    %   PAST counterpart of pipeline.last.pipes.pipelineI.
    %
    % Input  : - RawImageList: cellstr of the N raw FITS of ONE visit (same field),
    %            a single file name, or an AstroImage array. [] = read 'LAST*.fit*'.
    %          - CI: a CalibImages object (dark/flat); [] skips basic calibration.
    %          * ...,key,val,...  (PAST defaults in the arguments block)
    % Output : - Status : struct (PipeI, ME, NoGoodImages, Msg, ...), as pipelineI.
    %          - TableRaw: per-raw-image table (prePrep schema; SelectedImages etc.).
    %          - AllSI   : AstroImage array [Nepoch x 1], the processed epochs.
    %          - MS      : MatchedSources across the epochs ([] if N==1).
    %          - Coadd   : the processed coadd AstroImage ([] if N==1).
    %          - OnlyMP  : known-asteroid matches on the final product (AstroCatalog).
    %          - JD      : [Nepoch x 1] epoch JDs.
    %          - GaiaCone: [] (PAST has no pipelineII consumer).
    %
    % Example: [St,TR,AllSI]          = pipeline.last.pipes.pipelinePastI('LAST...M27..._Image_1.fits', []);
    %          [St,TR,AllSI,MS,Coadd] = pipeline.last.pipes.pipelinePastI(FileCell, CI);  % N-image visit
    %
    % Author : Claude (Opus 4.8) for E. Ofek, 2026-11-18.

    arguments
        RawImageList                       = [];
        CI                                 = [];

        % ---- mode / geometry ----
        Args.BinFactor                     = [2 2];   % [1 1] native; [2 2] default binned (cols,rows)
        Args.NativeScaleAS                 = 0.204;   % native "/pix (0.408 binned 2x2). Accurate to ~0.5%.
        Args.Coadd logical                 = true;    % coadd when N>1

        % ---- prePrep (read + quality) / calib ----
        Args.MinNim                        = 1;       % min good images for a visit (LAST=5); PAST allows 1
        Args.prePrepArgs                   = {};
        Args.histAnomalyArgs               = {'CCDSEC',[1 6388 25 9600], 'OverscanSec',[6389 6422 1 9600]};
        Args.TempName                      = 'LAST*.fit*';
        Args.basicCalibArgs                = {};
        Args.KeyMidJD                      = 'MIDJD';

        % ---- background / variance ----
        Args.backVarArgs                   = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256]};

        % ---- source extraction (PAST-scaled for the ~7px binned PSF) ----
        Args.Threshold                     = [500 50 10 5];
        Args.RadiusPSF                     = 30;
        Args.AperRadius                    = [8 13 15 18];
        Args.Annulus                       = [25 30];
        Args.PsfAnnulus                    = [40 50];
        Args.FitRadius                     = 8;
        Args.NighRadius                    = 15;
        Args.SigmaWidth                    = [7 4];
        Args.MaxRadiusM2                   = 15;
        Args.MomHalfSize                   = 30;
        Args.multiIterExtractorArgs        = {};

        % ---- astrometry (full frame, header pointing, robust to pointing error) ----
        Args.CatName                       = 'GAIADR3';
        Args.RefRangeMag                   = [10 17];
        Args.CatRadiusAS                   = 2200;
        Args.SearchRadius                  = 25;
        Args.RangeXY                       = [-3000 3000];
        Args.FilterSigmaAstrom             = 8;
        Args.FitPatternThreshold           = 3;
        Args.RA                            = [];
        Args.Dec                           = [];
        Args.astrometryCoreArgs            = {};

        % ---- photometric calibration / completion ----
        Args.DoPhotCalib logical           = true;
        Args.fitPhotCalibTransArgs         = {};
        Args.DoExternalMatch logical       = true;
        Args.DoAsteroids logical           = true;
        Args.AsteroidSearchRadius          = 5;

        % ---- LAST-compatible passthrough (so the demon can call either pipeline) ----
        Args.MagType char {mustBeMember(Args.MagType,{'lup','mag'})} = 'mag';
        Args.NaNUncalibMag logical         = false;   % accepted for interface parity
        Args.GeoPos                        = [];
        Args.OrbEl                         = [];
        Args.INPOP                         = [];

        % ---- coadd / matched sources ----
        % 'wrobust' (weighted robust, the LAST default): clean source count and
        % correct variance. 'sigmaclip' correlates the registration-interpolated
        % noise and massively over-detects on the coadd; avoid it.
        Args.StackMethod char              = 'wrobust';
        Args.procCoaddArgs                 = {};
        Args.proc2MatchedSourcesArgs       = {};
        % columns carried into the matched-sources product. PAST (full frame)
        % has no 'primary'/'FORCED' columns (those are LAST crop/forced-phot
        % columns), so they are omitted - otherwise proc2MatchedSources errors
        % with "Column primary not found".
        Args.MatchedCols                   = {'RA','Dec','X','Y','X1','Y1','X2','Y2','SN','SN_1','SN_2', ...
                                              'MAG_PSF','MAGERR_PSF','PSF_CHI2DOF','FLUX_PSF', ...
                                              'MAG_APER_2','MAGERR_APER_2','MAG_APER_3','MAGERR_APER_3','FLUX_APER_3', ...
                                              'FLAGS','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS'};

        Args.Status                        = [];
    end

    AchievedScaleAS = Args.NativeScaleAS .* mean(Args.BinFactor);
    EXTargs = i_extractorArgs(Args);

    % ---- init outputs / Status ----
    if ~isempty(Args.Status);  Status = Args.Status;  end
    Status.PipeI   = true;
    Status.ME      = [];
    TableRaw = table;
    AllSI = [];  MS = [];  Coadd = [];  OnlyMP = AstroCatalog;  JD = [];  GaiaCone = [];

    % manual mode: read files from current dir
    if isempty(RawImageList)
        Files = dir(Args.TempName);
        if isempty(Files);  error('No files matching %s', Args.TempName);  end
        RawImageList = {Files.name};
    end
    RawImageList    = RawImageList(:);
    RawImageListAll = RawImageList;

    % asteroid ephemerides (load once; light after INPOP is initiated)
    if Args.DoAsteroids
        try
            if isempty(Args.OrbEl);  Args.OrbEl = celestial.OrbitalEl.loadSolarSystem;  end
            if isempty(Args.INPOP);  Args.INPOP = celestial.INPOP.init;                 end
        catch ME
            warning('pipelinePastI:asteroidInit','asteroid ephemerides unavailable (%s) - skipping', ME.message);
            Args.DoAsteroids = false;
        end
    end

    % ---------------- read + quality (prePrep) ----------------
    PrePrepArgs = Args.prePrepArgs(:).';
    if ~isempty(Args.histAnomalyArgs)
        PrePrepArgs = [PrePrepArgs, {'histAnomalyArgs', Args.histAnomalyArgs}];
    end
    try
        [AI, TableForDB, TableHeader, JD_AI, FlagGoodImages, ExpTime_AI] = ...
            pipeline.generic.prePrep(RawImageList, 'MinNim',Args.MinNim, PrePrepArgs{:});
        TableRaw = [TableHeader, TableForDB];
        TableRaw.PrePrepOK = true(size(TableRaw,1), 1);
        if isempty(AI)
            Status.PipeI        = false;
            Status.NoGoodImages = true;
            Status.Msg          = sprintf('No useful RAW images in visit - all %d rejected by prePrep', numel(RawImageList));
            TableRaw.PrePrepOK  = false(size(TableRaw,1), 1);
            TableRaw.Exception  = false(size(TableRaw,1), 1);
            return;
        else
            RawImageList = RawImageList(FlagGoodImages,:);
        end
    catch ME
        Status.PipeI       = false;
        Status.ME          = ME;
        TableRaw.FileName  = string(RawImageListAll(:));
        TableRaw.PrePrepOK = false(numel(RawImageListAll), 1);
        TableRaw.Exception = true(numel(RawImageListAll), 1);
        return;
    end

    % ---------------- reduce the visit ----------------
    try
        % basic calibration (dark/flat); raw frames run without it. An empty [],
        % or a CalibImages with no Bias/Flat loaded (e.g. no masters yet), is
        % skipped so the visit still reduces on raw frames.
        HasCalib = ~isempty(CI) && isa(CI,'CalibImages') && ~any(CI.isemptyProp({'Bias','Flat'}));
        if HasCalib
            AI = pipeline.generic.basicCalib(AI, CI, Args.basicCalibArgs{:}, 'UpdateJD',false);
            TableRaw.BasicCalib(TableRaw.SelectedImages) = true(numel(AI),1);
        end

        N  = numel(AI);
        JD = JD_AI(:);
        % MIDJD header (used by the coadd naming)
        for Ie=1:1:N
            AI(Ie).HeaderData.insertKey({Args.KeyMidJD, JD_AI(Ie)});
        end

        % per-epoch processing
        AllSI = AstroImage([N 1]);
        for Ie = 1:1:N
            AIi = i_processEpoch(AI(Ie), Args, EXTargs, AchievedScaleAS);
            if Args.DoPhotCalib;  AIi = i_photcal(AIi, Args);  end
            AIi = i_completeImage(AIi, Args);
            AllSI(Ie) = AIi;
        end
        clear AI
        EpochWCS = arrayfun(@(a) imProc.astrometry.isSuccessWCS(a), AllSI);

        % visit coadd (only if N>1 and >1 good epochs)
        if N>1 && Args.Coadd && sum(EpochWCS)>1
            Good = EpochWCS(:);
            MS = pipeline.generic.proc2MatchedSources(AllSI(Good), 'MatchedCols',Args.MatchedCols, ...
                                                      Args.proc2MatchedSourcesArgs{:}, 'DimEpoch',1);
            [Coadd, ~] = pipeline.generic.procCoadd(AllSI(Good), Args.procCoaddArgs{:}, ...
                                'DefScale',AchievedScaleAS, 'StackMethod',Args.StackMethod, ...
                                'SubBack',false, 'EqualizeBack',true, 'ReMeasureBack',true, ...
                                'PhotCalibSimple',false, 'PhotCalibTrans',false, ...
                                'CatName',Args.CatName, 'multiIterExtractorArgs',EXTargs);
            % procCoadd already solves the coadd astrometry (astrometryRefine
            % from the registered epochs' WCS). Keep that solution; only run the
            % full pattern-match astrometryCore if procCoadd did not solve it
            % (re-solving from scratch on the deep coadd was failing and
            % overwriting the good WCS).
            if imProc.astrometry.isSuccessWCS(Coadd)
                Coadd = imProc.astrometry.addCoordinates2catalog(Coadd, 'UpdateCoo',true, 'OutUnits','deg');
            else
                Coadd = i_astrometry(Coadd, Args, AchievedScaleAS);
            end
            if Args.DoPhotCalib && imProc.astrometry.isSuccessWCS(Coadd);  Coadd = i_photcal(Coadd, Args);  end
            Coadd = i_completeImage(Coadd, Args);
        end

        % asteroid matching on the final science product (coadd if N>1 else epoch)
        FinalProd = Coadd;
        if isempty(FinalProd)
            Ig = find(EpochWCS, 1);
            if ~isempty(Ig);  FinalProd = AllSI(Ig);  end
        end
        if Args.DoAsteroids && ~isempty(FinalProd) && imProc.astrometry.isSuccessWCS(FinalProd)
            try
                OnlyMP = imProc.match.match2solarSystem(FinalProd, 'JD',[], 'GeoPos',Args.GeoPos, ...
                            'OrbEl',Args.OrbEl, 'INPOP',Args.INPOP, 'SearchRadius',Args.AsteroidSearchRadius);
            catch ME
                Status.KnownAstFailed = ME.message;
                warning('pipelinePastI:asteroid','asteroid match failed: %s', ME.message);
            end
        end

        Status.PipeI    = any(EpochWCS);
        Status.NgoodEpoch = sum(EpochWCS);
        Status.Nepoch     = N;

    catch ME
        Status.PipeI       = false;
        Status.ME          = ME;
        TableRaw.FileName  = string(RawImageListAll(:));
        if any(strcmp(TableRaw.Properties.VariableNames,'Exception'))
            TableRaw.Exception(TableRaw.SelectedImages) = true(sum(TableRaw.SelectedImages),1);
        end
        AllSI = [];  MS = [];  Coadd = [];  OnlyMP = AstroCatalog;  JD = [];
    end
end

% =====================================================================
function AI = i_processEpoch(AI, Args, EXTargs, Scale)
    % One epoch: bin -> background -> extract -> astrometry -> sky coords ->
    % Gaia colour -> fwhm/airmass/stat headers.
    AI.cast('single');
    if any(Args.BinFactor > 1)
        AI = imProc.background.backVar(AI, Args.backVarArgs{:});   % needed by binimage rejection
        AI = local_binAstroImage(AI, Args.BinFactor);
    end
    AI = imProc.background.backVar(AI, Args.backVarArgs{:});
    AI = imProc.sources.multiIterExtractor(AI, EXTargs{:});
    AI = i_astrometry(AI, Args, Scale);
    if imProc.astrometry.isSuccessWCS(AI)
        AI = imProc.cat.addColor(AI, 'CatName',Args.CatName);
        AI = imProc.psf.fwhm(AI, 'AddMorphology',true, 'AddErr',true, 'UseLegacy',true, 'DefScale',Scale);
        AI = imProc.header.addAirMass(AI, 'JD',AI.julday);
        AI = imProc.header.writeStat2Header(AI, 'WriteBack',false);
    end
end

% =====================================================================
function AI = i_astrometry(AI, Args, Scale)
    % Full-frame astrometry seeded by the header pointing (robust to error).
    if isempty(Args.RA);  RAg  = AI.HeaderData.getVal('RA');  else; RAg  = Args.RA;  end
    if isempty(Args.Dec); Decg = AI.HeaderData.getVal('DEC'); else; Decg = Args.Dec; end
    [~, AI] = imProc.astrometry.astrometryCore(AI, 'RA',RAg, 'Dec',Decg, 'Scale',Scale, ...
                'CatName',Args.CatName, 'CatRadius',Args.CatRadiusAS, 'CatRadiusUnits','arcsec', ...
                'RefRangeMag',Args.RefRangeMag, 'SearchRadius',Args.SearchRadius, ...
                'RangeX',Args.RangeXY, 'RangeY',Args.RangeXY, 'FilterSigma',Args.FilterSigmaAstrom, ...
                'argsFitPattern',{'Threshold',Args.FitPatternThreshold}, Args.astrometryCoreArgs{:});
    if imProc.astrometry.isSuccessWCS(AI)
        AI = imProc.astrometry.addCoordinates2catalog(AI, 'UpdateCoo',true, 'OutUnits','deg');
    end
end

% =====================================================================
function AI = i_photcal(AI, Args)
    % Photometric calibration: colour-term native-AB zero point (needs BP_RP).
    if ~imProc.astrometry.isSuccessWCS(AI) || AI.sizeCatalog==0;  return;  end
    AI = imProc.calib.fitPhotCalibTrans(AI, 'MagType',Args.MagType, 'AddMagErr',true, ...
                                        'Verbose',false, Args.fitPhotCalibTransArgs{:});
end

% =====================================================================
function AI = i_completeImage(AI, Args)
    % Per-image catalog/header completion (NOT asteroids - those run once on the
    % final product). Fragile stages guarded so one failure does not lose the image.
    if AI.sizeCatalog==0;  return;  end
    PS = AI.HeaderData.getVal('PIXSCALE');
    if isempty(PS) || ~isfinite(PS)
        AI.HeaderData.insertKey({'PIXSCALE', Args.NativeScaleAS.*mean(Args.BinFactor), 'pixel scale [arcsec/pix]'});
    end
    try; AI = imProc.calib.limmag(AI);                              catch; end
    try; AI = imProc.calib.backmag(AI, 'KeyZP','PT_ZP');            catch; end  % default 'Header' method (reads MEDBCK)
    try; [~,AI] = imProc.cat.addXYfull(AI);                         catch; end
    try; [~,AI] = imProc.psf.aperFrac(AI, 'AperRadius',Args.AperRadius); catch; end
    if Args.DoExternalMatch
        try; AI = imProc.match.match_catsHTMmerged(AI, 'SameField',false, 'CreateNewObj',false);
        catch ME; warning('pipelinePastI:external','external match failed: %s', ME.message); end
    end
end

% =====================================================================
function C = i_extractorArgs(Args)
    C = {'Threshold',Args.Threshold, 'RadiusPSF',Args.RadiusPSF, 'AperRadius',Args.AperRadius, ...
         'Annulus',Args.Annulus, 'PsfAnnulus',Args.PsfAnnulus, 'FitRadius',Args.FitRadius, ...
         'populatePSFArgs',{'CropByQuantile',false,'SuppressWidth',3,'SmoothWings',false,'NighRadius',Args.NighRadius}, ...
         'momentsArgs',{'SigmaWidth',Args.SigmaWidth,'MaxRadiusM2',Args.MaxRadiusM2,'HalfSize',Args.MomHalfSize}, ...
         'MagType',Args.MagType, Args.multiIterExtractorArgs{:}};
end

% =====================================================================
function AI = local_binAstroImage(AI, BinFactor)
    Back = AI.Back;  Var = AI.Var;
    if isempty(Back) || isempty(Var)
        error('local_binAstroImage: run backVar before binning (binimage needs Back & Var).');
    end
    [BinImage, BinVar] = imUtil.image.binimage(AI.Image, BinFactor, Back, Var);
    % binimage returns double (sum/count); keep products 'single' to match the
    % rest of the pipeline and the mex FITS writer (which rejects double).
    AI.Image = single(BinImage);  AI.Var = single(BinVar);  AI.Back = [];
    if ~isempty(AI.Mask)
        AI.MaskData.Image = local_binMaskOr(AI.Mask, BinFactor);
    end
end

% =====================================================================
function BM = local_binMaskOr(M, BinFactor)
    bx = BinFactor(1);  by = BinFactor(2);
    [ny, nx] = size(M);
    nyb = floor(ny./by);  nxb = floor(nx./bx);
    M   = M(1:nyb*by, 1:nxb*bx);
    M   = reshape(M, by, nyb, bx, nxb);
    BM  = zeros(nyb, nxb, 'like', M);
    for iy = 1:by
        for ix = 1:bx
            BM = bitor(BM, squeeze(M(iy,:,ix,:)));
        end
    end
end
