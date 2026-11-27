function [Result, PC, Info] = prepSubImage(Obj, Args)
    % Prepare a New or Ref image for image subtraction: Back/Var, PSF, PSF photometry and calibration.
    %   One function for both images of a subtraction; the caller chooses
    %   the recipe (e.g., the populatePSF arguments) for each of them.
    %   For each image:
    %   1. If the Back or Var image is empty, fill it, as
    %      AstroDiff.estimateBackVar does: from the header keywords, or
    %      with imProc.background.backVar (a background-subtracted image
    %      with its own method).
    %   2. Optionally re-populate the PSF (imProc.psf.populatePSF). If the
    %      new PSF is not sane (imUtil.psf.isSanePSF, #1355), keep the
    %      original PSF and skip step 3.
    %   3. Re-fit the PSF photometry (imProc.sources.psfFitPhot) and the
    %      photometric zero point PH_ZP on MAG_PSF (imProc.calib.photometricZP).
    %   4. Optionally calibrate with imProc.calib.fitPhotCalibTrans, unless
    %      the header already has PT_SPEC.
    %   The function only reports: nothing is removed, and nothing is
    %   decided for the caller (see Info).
    %   Moved from pipeline.last.pipes.pipelineII (sections 6, New and Ref).
    % Input  : - An AstroImage object (array).
    %          * ...,key,val,...
    %            'IsBackgroundSubtracted' - A logical indicating if the
    %                   images are background subtracted (a reference built
    %                   as mean images). Selects the Back/Var method and is
    %                   passed to fitPhotCalibTrans as 'IsMeanImages'.
    %                   Default is false.
    %            'FillBackVar' - A logical indicating if to fill empty
    %                   Back/Var images (step 1). Default is true.
    %            'UseHeaderBackVar' - Fill Back/Var from the header keywords,
    %                   if they exist and the image is not background
    %                   subtracted. Default is true.
    %            'HeaderBackKey' - Default is 'MEDBCK'.
    %            'HeaderVarKey' - Default is 'MEDVAR'.
    %            'BackFun' - Back/Var method for images that are not
    %                   background subtracted.
    %                   Default is @imUtil.background.modeVar_LogHist.
    %            'BackFunPar' - Default is {'MinVal',30, 'MaxVal',7000}.
    %            'BackFunSub' - Back/Var method for background subtracted
    %                   images. Default is @imUtil.background.modeVar_Hist.
    %            'BackFunParSub' - Default is {'Range', [-50,50]}.
    %            'BackBlock' - 'Block' of imProc.background.backVar.
    %                   Default is [] (full image).
    %            'BackOverlap' - Default is 16.
    %            'RePopPSF' - A logical indicating if to re-populate the
    %                   PSF (steps 2-3). Default is true.
    %            'populatePSFArgs' - A cell array of arguments to pass to
    %                   imProc.psf.populatePSF, after 'RePopulatePSF',true.
    %                   Default is {'Method','new'} (the uniPSF defaults).
    %            'PsfPhotMethod' - Default is '2DGN'.
    %            'ShiftMethod' - Default is 'fft'.
    %            'CatName' - Gaia catalog for photometricZP.
    %                   Default is 'GAIADR3'.
    %            'GaiaCone' - The Gaia cones of the visit, passed to
    %                   photometricZP. Default is [].
    %            'GaiaProperMotion' - Move the Gaia sources to the image
    %                   epoch (imProc.cat.gaiaEpoch). Default is true.
    %            'Calibrate' - A logical indicating if to run
    %                   fitPhotCalibTrans (step 4). Default is true.
    %            'KeyZP' - Zero point keyword whose presence is reported in
    %                   Info.HasZP. Default is 'PT_ZP'.
    % Output : - The AstroImage object (the input is modified).
    %          - A cell array of the PhotCalibTrans objects returned by
    %            fitPhotCalibTrans, one per image; empty if not calibrated.
    %          - Info: a structure of arrays, one element per image:
    %            .BackVarFrom - "existing" | "header" | "backVar".
    %            .PSFRepopulated - true if a new PSF was populated and kept.
    %            .PSFNotSane - true if the new PSF was not sane and the
    %                   original one was kept.
    %            .Calibrated - true if fitPhotCalibTrans was run.
    %            .HasZP - true if the header has the 'KeyZP' keyword at the end.
    %            .Msg - A string message.
    % Author : Dana Kovaleva (Nov 2026)
    % Example: [New, PC_New, InfoN] = pipeline.generic.prepSubImage(New, 'populatePSFArgs', NewPopArgs);
    %          [Ref, PC_Ref, InfoR] = pipeline.generic.prepSubImage(Ref, 'IsBackgroundSubtracted', true);

    arguments
        Obj AstroImage
        Args.IsBackgroundSubtracted logical = false;

        Args.FillBackVar logical       = true;
        Args.UseHeaderBackVar logical  = true;
        Args.HeaderBackKey             = 'MEDBCK';
        Args.HeaderVarKey              = 'MEDVAR';
        Args.BackFun                   = @imUtil.background.modeVar_LogHist;
        Args.BackFunPar cell           = {'MinVal',30, 'MaxVal',7000};
        Args.BackFunSub                = @imUtil.background.modeVar_Hist;
        Args.BackFunParSub cell        = {'Range', [-50,50]};
        Args.BackBlock                 = [];
        Args.BackOverlap               = 16;

        Args.RePopPSF logical          = true;
        Args.populatePSFArgs cell      = {'Method','new'};
        Args.PsfPhotMethod             = '2DGN';
        Args.ShiftMethod               = 'fft';
        Args.CatName                   = 'GAIADR3';
        Args.GaiaCone                  = [];
        Args.GaiaProperMotion logical  = true;

        Args.Calibrate logical         = true;
        Args.KeyZP                     = 'PT_ZP';
    end

    Nobj = numel(Obj);
    PC   = cell(size(Obj));
    Info.BackVarFrom    = repmat("existing", size(Obj));
    Info.PSFRepopulated = false(size(Obj));
    Info.PSFNotSane     = false(size(Obj));
    Info.Calibrated     = false(size(Obj));
    Info.HasZP          = false(size(Obj));
    Info.Msg            = strings(size(Obj));

    for Iobj=1:1:Nobj
        % 1. Back/Var (as AstroDiff.estimateBackVar)
        if Args.FillBackVar && any(isemptyImage(Obj(Iobj), {'Back','Var'}), 'all')
            if ~Args.IsBackgroundSubtracted && Args.UseHeaderBackVar && ...
                    Obj(Iobj).HeaderData.isKeyExist(Args.HeaderBackKey) && Obj(Iobj).HeaderData.isKeyExist(Args.HeaderVarKey)
                [ImageSizeY, ImageSizeX] = Obj(Iobj).sizeImage;
                Obj(Iobj).Back = repmat(Obj(Iobj).HeaderData.getVal(Args.HeaderBackKey), ImageSizeY, ImageSizeX);
                Obj(Iobj).Var  = repmat(Obj(Iobj).HeaderData.getVal(Args.HeaderVarKey),  ImageSizeY, ImageSizeX);
                Info.BackVarFrom(Iobj) = "header";
            else
                if Args.IsBackgroundSubtracted
                    Method    = Args.BackFunSub;
                    MethodPar = {Args.BackFunParSub, {}};
                else
                    Method    = Args.BackFun;
                    MethodPar = {Args.BackFunPar, {}};
                end
                Obj(Iobj) = imProc.background.backVar(Obj(Iobj), 'Method',Method, 'MethodArgs',MethodPar, ...
                                                      'Block',Args.BackBlock, 'Overlap',Args.BackOverlap);
                Info.BackVarFrom(Iobj) = "backVar";
            end
        end

        % 2-3. PSF, PSF photometry and PH_ZP
        if Args.RePopPSF
            OrigPSF = Obj(Iobj).PSFData.copy();
            Obj(Iobj) = imProc.psf.populatePSF(Obj(Iobj), 'RePopulatePSF',true, Args.populatePSFArgs{:});
            if ~Obj(Iobj).isemptyPSF && ~imUtil.psf.isSanePSF(Obj(Iobj).PSFData.getPSF)
                Obj(Iobj).PSFData = OrigPSF;
                Info.PSFNotSane(Iobj) = true;
                Info.Msg(Iobj) = "Re-populated PSF is not sane (non-positive or off-center peak), the original PSF is kept";
            else
                Obj(Iobj) = imProc.sources.psfFitPhot(Obj(Iobj), 'PsfPhotMethod',Args.PsfPhotMethod, ...
                                                      'ShiftMethod',Args.ShiftMethod);
                Obj(Iobj) = imProc.calib.photometricZP(Obj(Iobj), 'CatColNameMag','MAG_PSF', 'CatName',Args.CatName, ...
                                                       'GaiaCone',Args.GaiaCone, ...
                                                       'EpochOut',imProc.cat.gaiaEpoch(Obj(Iobj), Args.GaiaProperMotion));
                Info.PSFRepopulated(Iobj) = true;
            end
        end

        % 4. Calibration
        if Args.Calibrate && ~Obj(Iobj).HeaderData.isKeyExist('PT_SPEC')
            [Obj(Iobj), PC{Iobj}] = imProc.calib.fitPhotCalibTrans(Obj(Iobj), 'IsMeanImages',Args.IsBackgroundSubtracted);
            Info.Calibrated(Iobj) = true;
        end

        Info.HasZP(Iobj) = Obj(Iobj).HeaderData.isKeyExist(Args.KeyZP);
    end

    Result = Obj;
end
