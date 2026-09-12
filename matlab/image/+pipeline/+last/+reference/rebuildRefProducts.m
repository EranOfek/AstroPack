function [Info, Result] = rebuildRefProducts(RefList, Args)
    % Rebuild the PSF, catalog and header of existing reference images with the v1 chain.
    %   The v4 reference images were produced by the old pipeline: their PSF
    %   carries no wings (~99.98% of the light inside r=6 pix, against ~96%
    %   for a v1 PSF), their catalog columns differ from the v1 ones, and
    %   their photometric zero point is not on the absolute scale of the v1
    %   images - the same stars come out ~0.3 mag fainter in a v4 reference
    %   catalog than in a v1 image. That last point is what breaks the image
    %   subtraction: AstroDiff/estimateFnFr takes the flux ratio between New
    %   and Ref straight from the two header zero points, so any difference
    %   between the two calibration scales enters the difference image as a
    %   flux-matching error of exactly that size (issue #1267).
    %
    %   This function reads a reference image (the pixels are NOT modified),
    %   re-measures background and variance, rebuilds the PSF and the source
    %   catalog with the same code and the same arguments the v1 pipeline
    %   uses for its coadds, calibrates it with imProc.calib.fitPhotCalibTrans
    %   (so the output carries PT_ZP on the absolute scale), and writes the
    %   products into a parallel output tree.
    %
    %   IMPORTANT: the photometry arguments below must match those used by
    %   the pipeline that produces the New images (pipeline.last.pipes.pipelineI),
    %   NOT the bare defaults of imProc.sources.multiIterExtractor, which
    %   differ in PsfPhotMethod, ShiftMethod, AperPhotMethod and Threshold.
    %   Processing the reference differently would put it back on a slightly
    %   different photometric system, which is the problem being fixed here.
    %
    % Input  : - Reference images to rebuild, given as a file name, a cell
    %            array of file names, a struct array returned by dir, or a
    %            glob pattern (e.g. '/path/to/v4/*/*_sci_ref_Image_1.fits').
    %          * ...,key,val,...
    %            'OutDir' - Root directory of the output tree (e.g. the v4b
    %                   directory). The sub-directory structure of each input
    %                   file below 'InRoot' is preserved. If empty, nothing
    %                   is written ('Write' is ignored). Default is ''.
    %            'InRoot' - Root of the input tree, used to derive the
    %                   relative path of every input file. If empty, the
    %                   files are written flat into 'OutDir'. Default is ''.
    %            'Write' - Write the products to disk. Default is true.
    %            'SkipExisting' - Skip a reference whose output Image already
    %                   exists (resume a batch). Default is true.
    %            'OutProduct' - Products to write. The Image is written with
    %                   its pixels unchanged and the new header.
    %                   Default is ["Image","Mask","Cat","PSF"].
    %            --- photometry: keep in sync with pipelineI ---
    %            'AperRadius' - Aperture radii [pix]. Default is [3 5 6 7].
    %            'Annulus' - Background annulus [pix]. Default is [10 12].
    %            'Threshold' - Detection thresholds per iteration.
    %                   Default is [500 50 4].
    %            'MomentsMethod' - Default is 'mex'.
    %            'AperPhotMethod' - Default is 'simple'.
    %            'PsfPhotMethod' - Default is '2DGN'.
    %            'ShiftMethod' - Default is 'lanczos3'.
    %            'MagType' - 'mag'|'lup'. Default is 'mag'.
    %            'multiIterExtractorArgs' - Extra args for the extractor.
    %                   Default is {}.
    %            'fitPhotCalibTransArgs' - Extra args for the photometric
    %                   calibration. Default is {}.
    %            'backVarArgs' - Args for imProc.background.backVar. Must be
    %                   an EMPIRICAL estimator for references (see the note
    %                   in the code). Default is the recipe used by
    %                   pipeline.last.reference.buildRefImages.
    %            --- reference-specific ---
    %            'GainKey' - Header keyword holding the effective gain of the
    %                   reference (the mean number of coadded images per
    %                   pixel). Default is 'AVNCOADD'.
    %            'Gain' - Effective gain. If not empty, overrides 'GainKey'.
    %                   Default is [].
    %            'NFramesPerCoadd' - Number of frames per individual input
    %                   coadd. A reference is a coadd of coadds, so its
    %                   ExpTime_eff = EXPTIME/(NCOADD * NFramesPerCoadd).
    %                   Forwarded to the calibration through
    %                   fitPhotCalibTrans 'IsMeanImages'/'NProcsPerCoadd'
    %                   when greater than 1, and reported as ExpTimeEff in
    %                   Info. PT_ZP is the raw-count zero point, so it is
    %                   invariant to this bookkeeping. Default is 1.
    %            'CalibMaxSN' - Upper S/N gate for calibrator selection,
    %                   passed to fitPhotCalibTrans as ExtraCalibArgs. The
    %                   pipeline default (1000) rejects every calibrator on a
    %                   deep reference, so the gate is opened here.
    %                   Default is 1e9.
    %            'ExtraCalibArgs' - Further key-value overrides appended to
    %                   the predefined calibration recipe. Default is {}.
    %            'BS_BackMaxR' - Bright-source background radius, as used
    %                   when the references were built. Default is 1501.
    %            'CleanSN' - As used when the references were built.
    %                   Default is 4.
    %            'RunPhotometricZP' - Re-fit the legacy zero point (PH_ZP)
    %                   over the new catalog, before the transmission
    %                   calibration, as procCoadd does. The v4 references
    %                   carry degenerate PH solutions inherited from the old
    %                   catalog; re-fitting over the v1 catalog cures them,
    %                   so consumers that still default to PH_ZP (such as
    %                   AstroDiff/estimateFnFr) read an honest value.
    %                   Default is true.
    %            'photometricZPArgs' - Extra args for imProc.calib.photometricZP.
    %                   Default is {}.
    %            'MaxPhotColTerm' - Reject the PH fit (and remove the PH_*
    %                   keywords) when |PH_COL1| exceeds this. Default is 1.
    %            'MaxPhotRMS' - Reject the PH fit when PH_RMS exceeds this.
    %                   Default is 0.05.
    %            'ReAstrometry' - Re-refine the WCS. The reference defines
    %                   the astrometric grid of everything registered onto
    %                   it, so the default keeps the existing WCS and only
    %                   attaches sky coordinates to the new catalog.
    %                   Default is false.
    %            'Verbose' - Default is true.
    % Output : - A struct array with one element per input reference,
    %            carrying the quality-control numbers of the rebuild:
    %            .File .OutFile .Success .Nsrc .FWHM .PT_ZP .PH_ZP_in
    %            .ZPimplied (zero point implied by the new catalog itself)
    %            .dZP (ZPimplied - PT_ZP; a few mmag when the calibration is
    %                  internally consistent)
    %            .PSF_AF3 (fraction of PSF light within r=6 pix; ~0.96 for a
    %                  proper winged PSF, 0.9998 for the wing-less v4 ones)
    %            .PSF_RPK .ExpTime .NCoadd .ExpTimeEff .Gain .Msg
    %            .PH_ZP_out .PH_COL1 .PH_RMS (the regenerated legacy zero
    %                  point and its fit quality; NaN when the fit was
    %                  rejected and the PH_* keywords removed)
    %          - The rebuilt AstroImage array (only for small inputs; the
    %            batch use writes to disk and returns the Info struct).
    % Author : Dana Kovaleva (Sep 2026)
    % Example: Info = pipeline.last.reference.rebuildRefProducts( ...
    %              '/mnt/euclid/last/data/references/v4/1677/*_sci_ref_Image_1.fits', ...
    %              'InRoot','/mnt/euclid/last/data/references/v4', ...
    %              'OutDir','/mnt/euclid/last/data/references/v4b');

    arguments
        RefList
        Args.OutDir           (1,:) char   = ''
        Args.InRoot           (1,:) char   = ''
        Args.Write            logical      = true
        Args.SkipExisting     logical      = true
        Args.OutProduct                    = ["Image","Mask","Cat","PSF"]

        % --- photometry: keep in sync with pipeline.last.pipes.pipelineI ---
        Args.AperRadius                    = [3, 5, 6, 7]
        Args.Annulus                       = [10 12]
        Args.Threshold                     = [500 50 4]
        Args.MomentsMethod    (1,:) char   = 'mex'
        Args.AperPhotMethod   (1,:) char   = 'simple'
        Args.PsfPhotMethod    (1,:) char   = '2DGN'
        Args.ShiftMethod      (1,:) char   = 'lanczos3'
        Args.MagType          (1,:) char {mustBeMember(Args.MagType,{'mag','lup'})} = 'mag'
        Args.multiIterExtractorArgs cell   = {}
        Args.fitPhotCalibTransArgs  cell   = {}
        Args.backVarArgs      cell         = {'Method','backBertinLowerRMS', 'MethodArgs',{}}

        % --- reference-specific ---
        Args.GainKey          (1,:) char   = 'AVNCOADD'
        Args.Gain                          = []
        Args.NFramesPerCoadd  (1,1) double {mustBePositive} = 1
        Args.CalibMaxSN       (1,1) double {mustBePositive} = 1e9
        Args.ExtraCalibArgs   cell         = {}
        Args.BS_BackMaxR      (1,1) double = 1501
        Args.CleanSN          (1,1) double = 4
        Args.RunPhotometricZP logical      = true
        Args.photometricZPArgs cell        = {}
        Args.MaxPhotColTerm   (1,1) double = 1.0
        Args.MaxPhotRMS       (1,1) double = 0.05
        Args.ReAstrometry     logical      = false
        Args.Verbose          logical      = true
    end

    FileList = i_resolveList(RefList);
    Nf = numel(FileList);
    if Args.Verbose
        fprintf('rebuildRefProducts: %d reference image(s)\n', Nf);
    end

    Info = repmat(struct('File','', 'OutFile','', 'Success',false, 'Nsrc',0, ...
                         'FWHM',NaN, 'PT_ZP',NaN, 'PH_ZP_in',NaN, 'ZPimplied',NaN, ...
                         'dZP',NaN, 'PSF_AF3',NaN, 'PSF_RPK',NaN, 'ExpTime',NaN, ...
                         'PH_ZP_out',NaN, 'PH_COL1',NaN, 'PH_RMS',NaN, ...
                         'NCoadd',NaN, 'ExpTimeEff',NaN, 'Gain',NaN, 'Msg',''), 1, Nf);
    Result = AstroImage(0);

    for If=1:1:Nf
        InFile = FileList{If};
        Info(If).File = InFile;
        OutFile = i_outFile(InFile, Args.InRoot, Args.OutDir);
        Info(If).OutFile = OutFile;

        DoWrite = Args.Write && ~isempty(Args.OutDir);
        if DoWrite && Args.SkipExisting && isfile(OutFile)
            Info(If).Msg = 'skipped (output exists)';
            if Args.Verbose
                fprintf('  [%d/%d] skip (exists): %s\n', If, Nf, OutFile);
            end
            continue
        end

        try
            [Info(If), AI] = i_rebuildOne(InFile, OutFile, DoWrite, Args, Info(If));
            if nargout>1
                Result(numel(Result)+1) = AI;
            end
        catch ME
            Info(If).Success = false;
            Info(If).Msg = ME.message;
            if Args.Verbose
                fprintf(2, '  [%d/%d] FAILED %s: %s\n', If, Nf, InFile, ME.message);
            end
        end

        if Args.Verbose && Info(If).Success
            fprintf('  [%d/%d] %s | Nsrc=%d FWHM=%.2f PT_ZP=%.4f dZP=%+.4f PSF_AF3=%.4f RPK=%.1f\n', ...
                If, Nf, i_shortName(InFile), Info(If).Nsrc, Info(If).FWHM, ...
                Info(If).PT_ZP, Info(If).dZP, Info(If).PSF_AF3, Info(If).PSF_RPK);
        end
    end

    if Args.Verbose
        Ns = sum([Info.Success]);
        fprintf('rebuildRefProducts: %d/%d succeeded\n', Ns, Nf);
        if Ns>0
            fprintf('  median PSF_AF3 = %.4f (v4 input is ~0.9998; a winged PSF is ~0.96)\n', ...
                median([Info([Info.Success]).PSF_AF3], 'omitnan'));
            fprintf('  median |dZP|   = %.4f mag (internal ZP consistency)\n', ...
                median(abs([Info([Info.Success]).dZP]), 'omitnan'));
        end
    end
end

% ------------------------------------------------------------------------
function [I, AI] = i_rebuildOne(InFile, OutFile, DoWrite, Args, I)
    % Rebuild a single reference image.

    % 1. read the reference: pixels and mask are taken as they are
    AI = AstroImage.readProducts(InFile, 'ExtraOutProduct',"Mask");
    if isemptyImage(AI)
        error('rebuildRefProducts:EmptyImage','no image data in %s', InFile);
    end
    H = AI.HeaderData;
    I.PH_ZP_in = H.getVal('PH_ZP');
    I.ExpTime  = H.getVal('EXPTIME');
    I.NCoadd   = H.getVal('NCOADD');
    I.ExpTimeEff = I.ExpTime ./ (I.NCoadd .* Args.NFramesPerCoadd);

    % 2. effective gain: for a reference the noise is set by the number of
    %    images that went into each pixel, not by the detector gain (which
    %    the header reports as 1 after the single-epoch gain correction)
    if isempty(Args.Gain)
        Gain = H.getVal(Args.GainKey);
        if ~isfinite(Gain) || Gain<=0
            Gain = 1;
        end
    else
        Gain = Args.Gain;
    end
    I.Gain = Gain;

    % 3. background and variance, measured on the reference itself.
    %    The default is the empirical estimator that pipeline.last.reference.
    %    buildRefImages uses when it builds a reference. An ANALYTIC Poisson
    %    model must not be used here: backVar would form
    %    Var = (Back + RN2)/Ncoadd, and a reference is a stack of hundreds of
    %    images with a per-pixel depth that varies across the field - with
    %    Ncoadd=1 the variance is overestimated and the faint detections are
    %    lost, while with Ncoadd=AVNCOADD the S/N is inflated past the
    %    calibrator selection window and the photometric fit finds no
    %    calibrators at all. Measuring the noise off the image itself avoids
    %    both failure modes.
    AI = imProc.background.backVar(AI, Args.backVarArgs{:});

    % 4. PSF and source catalog - the same extractor, and the same
    %    arguments, that pipelineI applies to its coadds
    AI = imProc.sources.multiIterExtractor(AI, Args.multiIterExtractorArgs{:}, ...
                'Gain',            Gain, ...
                'AperRadius',      Args.AperRadius, ...
                'Annulus',         Args.Annulus, ...
                'Threshold',       Args.Threshold, ...
                'MomentsMethod',   Args.MomentsMethod, ...
                'AperPhotMethod',  Args.AperPhotMethod, ...
                'PsfPhotMethod',   Args.PsfPhotMethod, ...
                'ShiftMethod',     Args.ShiftMethod, ...
                'MagType',         Args.MagType, ...
                'BS_BackMaxR',     Args.BS_BackMaxR, ...
                'CleanSN',         Args.CleanSN, ...
                'AddBackNoise',    true, ...
                'AddExtraBack',    true, ...
                'AddExtraVar',     true, ...
                'AddSkyCoo',       false, ...
                'UpdateHeaderDataBkgVar', false);
    I.Nsrc = AI.CatData.sizeCatalog;
    if I.Nsrc==0
        error('rebuildRefProducts:NoSources','no sources extracted from %s', InFile);
    end

    % 5. sky coordinates. The reference defines the astrometric grid that
    %    every science image is registered onto, so its WCS is kept and only
    %    the new catalog positions are converted (ReAstrometry re-refines it).
    if Args.ReAstrometry
        AI = imProc.astrometry.astrometryRefine(AI, 'WCS',AI.WCS, 'CreateNewObj',false);
    end
    AI = imProc.astrometry.addCoordinates2catalog(AI, 'UpdateCoo',true, 'OutUnits','deg');

    % 6. PSF morphology keywords (FWHM, PSF_NPK/PKR/DPK/RPK) and the
    %    aperture light fractions (PSF_AF_*)
    imProc.psf.fwhm(AI, 'AddMorphology',true, 'AddErr',true, 'UseLegacy',false);
    [~, AI] = imProc.psf.aperFrac(AI, 'AperRadius',Args.AperRadius);

    % 6b. legacy photometric zero point (PH_ZP). Regenerating it is the
    %    point: the degenerate colour solutions seen on v4 references
    %    (PH_COL1 ~ -2.6, PH_RMS ~ 0.14) come from the OLD catalog, not from
    %    the image - re-fitting over the v1 catalog produces a sane value.
    %    Written before the transmission calibration, as procCoadd does, and
    %    with UpdateMagCols=false so the magnitude columns stay the ones
    %    fitPhotCalibTrans produces. Consumers that still default to PH_ZP
    %    (AstroDiff/estimateFnFr does) then read an honest number.
    if Args.RunPhotometricZP
        try
            AI = imProc.calib.photometricZP(AI, 'CreateNewObj',false, ...
                        'UpdateMagCols',false, Args.photometricZPArgs{:});
            I.PH_ZP_out = AI.HeaderData.getVal('PH_ZP');
            I.PH_COL1   = AI.HeaderData.getVal('PH_COL1');
            I.PH_RMS    = AI.HeaderData.getVal('PH_RMS');
            % Sanity-gate the fit before letting the value stand. A runaway
            % colour term or a large residual means the solution is
            % degenerate; keeping it would hand a bad zero point to anything
            % reading PH_ZP, which is exactly the failure this rebuild
            % exists to remove. Drop the keywords instead, so such a
            % consumer gets NaN and fails visibly rather than quietly.
            BadFit = ~isfinite(I.PH_ZP_out) || ...
                     (isfinite(I.PH_COL1) && abs(I.PH_COL1) > Args.MaxPhotColTerm) || ...
                     (isfinite(I.PH_RMS)  && I.PH_RMS  > Args.MaxPhotRMS);
            if BadFit
                AI.HeaderData.deleteKey({'PH_ZP','PH_COL1','PH_COL2','PH_W', ...
                                         'PH_MEDC','PH_MEDW','PH_RMS','PH_NSRC'});
                I.Msg = sprintf('photometricZP rejected (COL1=%.3f RMS=%.3f) - PH_* removed', ...
                                I.PH_COL1, I.PH_RMS);
                I.PH_ZP_out = NaN;
            end
        catch ME
            AI.HeaderData.deleteKey({'PH_ZP','PH_COL1','PH_COL2','PH_W', ...
                                     'PH_MEDC','PH_MEDW','PH_RMS','PH_NSRC'});
            I.Msg = sprintf('photometricZP failed (%s) - PH_* removed', ME.message);
        end
    end

    % 7. absolute photometric calibration - the reason for the rebuild.
    %    Writes PT_ZP and the calibrated magnitude columns, and applies the
    %    positional aperture correction.
    %    'MaxSN' is opened via ExtraCalibArgs (which APPENDS to the predefined
    %    recipe rather than replacing it): a reference stacked from hundreds
    %    of exposures has every Gaia G=12-16 calibrator far above the default
    %    upper S/N gate of 1000, so with the gate closed the selection returns
    %    zero calibrators and the calibration fails outright.
    %    IsMeanImages/NProcsPerCoadd carry the coadd-of-coadds exposure
    %    bookkeeping into calibrate.
    ExtraCalib = [{'MaxSN', Args.CalibMaxSN}, Args.ExtraCalibArgs];
    [AI, ~, ~] = imProc.calib.fitPhotCalibTrans(AI, Args.fitPhotCalibTransArgs{:}, ...
                'MagType',         Args.MagType, ...
                'ExtraCalibArgs',  ExtraCalib, ...
                'IsMeanImages',    Args.NFramesPerCoadd > 1, ...
                'NProcsPerCoadd',  max(1, round(Args.NFramesPerCoadd)), ...
                'Verbose',         false);

    % 8. limiting and background magnitudes, on the new zero point
    AI = imProc.calib.limmag(AI);
    AI = imProc.calib.backmag(AI, 'KeyZP','PT_ZP');
    AI = imProc.header.writeStat2Header(AI, 'WriteBack',false);

    % 9. quality-control numbers
    H = AI.HeaderData;
    I.PT_ZP   = H.getVal('PT_ZP');
    I.FWHM    = H.getVal('FWHM');
    I.PSF_AF3 = H.getVal('PSF_AF_3');
    I.PSF_RPK = H.getVal('PSF_RPK');
    I.ZPimplied = i_impliedZP(AI);
    I.dZP     = I.ZPimplied - I.PT_ZP;

    % 10. write the products. The Image pixels are unchanged; the file is
    %     rewritten so that it carries the new header.
    if DoWrite
        OutPath = fileparts(OutFile);
        if ~isfolder(OutPath)
            mkdir(OutPath);
        end
        % Write each product under the input's own name, so the output tree
        % mirrors the input one file for one file. The header is written for
        % the Image and the Cat (write1 drops it for PSF in any case), which
        % is the convention imProc.io.saveProductImage uses for references.
        Stem = regexprep(OutFile, '_Image_1\.fits$', '');
        if strcmp(Stem, OutFile)
            error('rebuildRefProducts:BadName', ...
                  'input name does not end in _Image_1.fits: %s', OutFile);
        end
        Prods = cellstr(Args.OutProduct);
        for Ip = 1:1:numel(Prods)
            Prod  = Prods{Ip};
            PName = sprintf('%s_%s_1.fits', Stem, Prod);
            WrHdr = any(strcmpi(Prod, {'Image','Cat'}));
            AI.write1(PName, Prod, 'WriteHeader',WrHdr, 'OverWrite',true);
        end
    end

    I.Success = true;
    I.Msg = 'ok';
end

% ------------------------------------------------------------------------
function ZP = i_impliedZP(AI)
    % Zero point implied by the catalog itself: MAG = -2.5log10(FLUX) + ZP.
    % Compared with the header PT_ZP this checks that the delivered
    % magnitudes and the delivered zero point tell the same story - the test
    % that catches an exposure-time or normalisation slip, which no fit
    % residual would reveal.
    ZP = NaN;
    C = AI.CatData;
    if C.sizeCatalog==0 || ~C.isColumn('MAG_APER_3') || ~C.isColumn('FLUX_APER_3')
        return
    end
    M = C.getCol('MAG_APER_3');
    F = C.getCol('FLUX_APER_3');
    Ok = isfinite(M) & isfinite(F) & F>0 & M>13 & M<18;
    if C.isColumn('FLAGS')
        BD  = BitDictionary('BitMask.Image.Default');
        Bad = BD.findBit(C.getCol('FLAGS'), {'Saturated','NaN','NearEdge','Negative'}, 'Method','any');
        Ok  = Ok & ~Bad;
    end
    if nnz(Ok) < 5
        return
    end
    ZP = median(M(Ok) + 2.5.*log10(F(Ok)), 'omitnan');
end

% ------------------------------------------------------------------------
function List = i_resolveList(RefList)
    % Accept a file name, a glob, a cellstr, or a dir struct.
    if isstruct(RefList)
        List = arrayfun(@(S) fullfile(S.folder, S.name), RefList, 'UniformOutput',false);
    elseif iscell(RefList)
        List = RefList(:).';
    else
        RefList = char(RefList);
        if any(RefList=='*')
            D = dir(RefList);
            if isempty(D)
                D = dir(fullfile(fileparts(RefList), '**', regexprep(RefList,'^.*/','')));
            end
            D = D(~[D.isdir]);
            List = arrayfun(@(S) fullfile(S.folder, S.name), D, 'UniformOutput',false).';
        else
            List = {RefList};
        end
    end
end

% ------------------------------------------------------------------------
function Out = i_outFile(InFile, InRoot, OutDir)
    % Mirror the input tree below OutDir.
    if isempty(OutDir)
        Out = '';
        return
    end
    if ~isempty(InRoot) && startsWith(InFile, InRoot)
        Rel = InFile(numel(InRoot)+1:end);
        Rel = regexprep(Rel, '^/+', '');
    else
        [~, N, E] = fileparts(InFile);
        Rel = [N, E];
    end
    Out = fullfile(OutDir, Rel);
end

% ------------------------------------------------------------------------
function S = i_shortName(F)
    [~, N] = fileparts(F);
    if numel(N)>44
        S = N(end-43:end);
    else
        S = N;
    end
end
