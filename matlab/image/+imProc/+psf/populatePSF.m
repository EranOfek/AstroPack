function [Obj,Result]=populatePSF(Obj, Args)
    % Populate the AstroPSF object of an AstroImage with a master PSF.
    %   For each element of the input AstroImage object, build a master
    %   PSF and (in the legacy branch) store it in the element's PSFData.
    %
    %   Three back-ends are selectable via the 'Method' argument:
    %     'new'    - calls imUtil.psf.buildPSF on the AstroImage's image.
    %                X / Y / SN are taken from the AstroImage's CatData
    %                if available; otherwise findSources is invoked inside
    %                buildPSF. Background / variance are computed via
    %                imProc.background.background if missing. NOTE: in this
    %                branch the resulting PSF is currently NOT written
    %                back to Obj.PSFData (only Result is returned).
    %     'legacy' - same flow as 'new' but using the older
    %                imUtil.psf.constructPSF, and writes MeanPSF/VarPSF/N
    %                into Obj.PSFData.{Data, Var, Nstars}. This is the
    %                default and the path the AstroImage actually gets
    %                populated through today.
    %     'old'    - thin wrapper around the previous imProc.psf.constructPSF
    %                (kept for backward compatibility).
    %
    % Input  : - An AstroImage object (array allowed; processed element by
    %            element).
    %          * ...,key,val,... grouped by context below.
    %
    %    --- populatePSF behavior ---
    %            'Method' - Back-end selector: 'new' | 'legacy' | 'old'.
    %                   Default is 'legacy'.
    %            'RePopulatePSF' - If true, rebuild even when PSFData is
    %                   already populated (legacy branch only).
    %                   Default is false.
    %            'ColSN' - Cell array with the S/N column names in the
    %                   AstroImage CatData. Column 1 is the delta-function
    %                   S/N, column 2 is the PSF-filter S/N.
    %                   Default is {'SN_1','SN_2'}.
    %            'oldconstructPSFArgs' - Cell array of key/val arguments
    %                   forwarded to the obsolete imProc.psf.constructPSF
    %                   when Method = 'old'. Default is {}.
    %
    %    --- background / variance estimation ---
    %            'backgroundArgs' - Cell of args forwarded to
    %                   imProc.background.background when the image's
    %                   Back / Var are empty. Default is {}.
    %            'SubAnnulusBack' - Subtract per-stamp annulus background
    %                   before building the PSF. Default is true.
    %            'Annulus' - [Rin, Rout] background annulus in pixels.
    %                   Default is [10 12].
    %            'BackQuantile' - [Qlow, Qhigh] quantiles on the per-stamp
    %                   annulus background; sources outside this range are
    %                   rejected. Empty -> skip. Default is [0.01 0.9].
    %            'StdQuantile' - Same as BackQuantile but on annulus StD.
    %                   Default is [0.01 0.9].
    %
    %    --- source detection (used only when catalog/SN are not supplied) ---
    %            'ThresholdPSF' - Detection S/N threshold for findSources.
    %                   Default is 20.
    %            'RangeSN' - [SNmin, SNmax] PSF-filter S/N window for
    %                   sources used to build the PSF. Default is [50 1000].
    %            'SNdiff' - Minimum required margin SN(:,2) - SN(:,1).
    %                   Default is 0.
    %            'InitPsf' - Function handle producing the initial-guess
    %                   PSF kernel(s) for findSources matched filtering.
    %                   Default is @imUtil.kernel2.gauss.
    %            'InitPsfArgs' - Cell of args to InitPsf.
    %                   Default is {[0.1;2]}.
    %            'Conn' - Connectivity for source detection. Default is 8.
    %            'CleanSources' - Apply source cleaning in findSources.
    %                   Default is true.
    %            'cleanSourcesArgs' - Cell of args to the source cleaner.
    %                   Default is {}.
    %
    %    --- stamp cutouts ---
    %            'RadiusPSF' - Half-size (in pixels) of the cutouts used
    %                   to build the PSF; stamps are 2*RadiusPSF+1 on a
    %                   side. Default is 8.
    %            'DeltaSigma' - If non-empty, the cutout half-size is
    %                   enlarged so the background annulus fits inside the
    %                   stamp. Empty -> skip. Default is 0.5.
    %            'image2cutoutsArgs' - Cell of extra args to
    %                   imUtil.cut.image2cutouts. Default is {}.
    %            'backgroundCubeArgs' - Cell of args forwarded to the
    %                   background-cube step. Default is {}.
    %
    %    --- source quality / shape filters ---
    %            'NighRadius' - Minimum allowed distance to nearest
    %                   neighbor; sources with a neighbor closer than this
    %                   are rejected. Empty -> skip. (Legacy name; this is
    %                   forwarded to buildPSF as 'NeighRadius'.)
    %                   Default is 7.
    %            'SigmaQuantile' - [Qlow, Qhigh] quantiles on the moment-
    %                   derived semi-major axis; sources outside this
    %                   range are rejected. Empty -> skip.
    %                   Default is [0.05 0.8].
    %
    %    --- centering / sub-pixel shift ---
    %            'ShiftMethod' - Method used to re-center each stamp to
    %                   its 1st-moment position: 'lanczos3' or 'fft'.
    %                   Default is 'fft'.
    %
    %    --- stack combination ---
    %            'SumMethod' - Stamp combination method:
    %                   'median' (default), 'mean', 'sigclip', 'sigclip_mex'.
    %            'VarOfMean' - If true, returned VarPSF is divided by Nsrc
    %                   to produce variance-of-the-mean. Default is true.
    %            'SigmaClip' - [low, high] sigma-clipping bounds used by
    %                   'sigclip_mex'. Default is [3 3].
    %            'SigmaClipNiter' - Sigma-clip iteration count.
    %                   Default is 2.
    %            'Weighted' - In 'sigclip_mex', weight stamps by
    %                   1/max(SN, WeightsMaxSN). Default is true.
    %            'WeightsMaxSN' - SN ceiling used in the weight formula
    %                   above (prevents very bright stars from dominating).
    %                   Default is 100.
    %            'mean_sigclipArgs' - Extra args to
    %                   imUtil.image.mean_sigclip when SumMethod='sigclip'.
    %                   Default is {}.
    %
    %    --- wing suppression ---
    %            'WingsMethod' - Wing-fixing back-end forwarded to buildPSF:
    %                   'analytic' | 'cosbell' | 'empirical'. 'empirical'
    %                   calibrates the wing from this image's own bright/
    %                   near-saturated stars (see imUtil.psf.buildPSF and
    %                   imUtil.psf.buildEmpiricalWing), falling back to
    %                   'cosbell' when too few such stars are available.
    %                   Default is 'analytic'.
    %            'WingsPowerLaw' - Power-law index forwarded to buildPSF for
    %                   the 'analytic' WingsMethod. Default is 2.
    %            'SuppressFun' - Window function used by suppressWings.
    %                   Default is @imUtil.kernel2.cosbell.
    %            'WingsThreshold' - Threshold for suppressWings. (Legacy
    %                   name; forwarded to buildPSF as 'SuppressThreshold'.)
    %                   Default is 1e-4.
    %            'SuppressWidth' - Width parameter for SuppressFun. (Legacy
    %                   name; forwarded to buildPSF as 'SuppressFunPars'.)
    %            'WingRangeSN' - [SNmin, SNmax] bright-star sample used by
    %                   WingsMethod='empirical'. Empty -> [RangeSN(2), Inf].
    %                   Default is [].
    %            'MinWingStars' - Minimum bright stars required to trust
    %                   WingsMethod='empirical'; below this, falls back to
    %                   'cosbell' for that image. Default is 8.
    %                   Default is 3.
    %            'BuildDetectionPSF' - Also populate a 'Purpose'-dimensioned
    %                   PSFData.Data cube: slice 1 (default, unchanged
    %                   behavior for existing callers) is the normal
    %                   photometry/subtraction PSF; slice 2 is a wing splice
    %                   built from the same core using WingsMethod='analytic'
    %                   and 'DetectionWingsPowerLaw', for source detection
    %                   (imProc.sources.multiIterExtractor), where Alpha=2 is
    %                   the only value validated safe against the #1103
    %                   bogus-detection-ring artifact. Retrieve via
    %                   PSFData.getPSF('PsfArgs',{'Purpose',2}). Default is
    %                   false.
    %            'DetectionWingsPowerLaw' - Power-law index for the
    %                   detection-PSF slice when BuildDetectionPSF=true.
    %                   Default is 2.
    %
    %    --- legacy / no-op options (accepted for backward compatibility
    %        but not forwarded to the current imUtil.psf.buildPSF) ---
    %            'moment2Args', 'MinNumGoodPsf', 'constructPSF_cutoutsArgs',
    %            'SmoothWings', 'SuppressWings', 'SuppressEdges',
    %            'DataType', 'CropByQuantile', 'Quantile'. These remain
    %            settable so existing callers do not break, but they have
    %            no effect on the 'new' Method path. The 'legacy' Method
    %            still passes them to imUtil.psf.constructPSF.
    %
    % Output : - The AstroImage object. For Method = 'legacy', element's
    %            PSFData is updated in-place with the new master PSF.
    %            For Method = 'new', the object is currently returned
    %            unchanged (only Result is populated).
    %          - Result, a struct array (one element per AstroImage
    %            element) describing the PSF stars selection and the
    %            resulting master PSF. The exact fields depend on the
    %            back-end selected (see imUtil.psf.buildPSF for 'new', and
    %            imUtil.psf.constructPSF for 'legacy'/'old'); typical
    %            fields include .Nsrc, .SN, .X, .Y, .M1, .M2.
    % Author : Eran Ofek (Jul 2023)
    % Example: AI = imProc.psf.populatePSF(AI);
    %          AI = imProc.psf.populatePSF(AI, 'Method','new');
    %          AI = imProc.psf.populatePSF(AI, 'RangeSN',[80 800], 'SumMethod','sigclip_mex');
   
    arguments
        Obj AstroImage

        % --- populatePSF behavior ---
        Args.Method                    = 'legacy';   % 'new' | 'legacy' | 'old'
        Args.RePopulatePSF             = false;
        Args.ColSN                     = {'SN_1','SN_2'};
        Args.oldconstructPSFArgs       = {};         % args of the obsoleted 'old' method

        % --- background / variance estimation (per-image and per-stamp) ---
        Args.backgroundArgs            = {};
        Args.SubAnnulusBack            = true;
        Args.Annulus                   = [10 12];
        Args.BackQuantile              = [0.01 0.9]; % if empty skip
        Args.StdQuantile               = [0.01 0.9]; % if empty skip

        % --- source detection (used only when catalog/SN not supplied) ---
        %Args.Threshold                 = 5;
        Args.ThresholdPSF              = 20;
        Args.RangeSN                   = [50 1000];
        Args.SNdiff                    = 0;          % if empty skip
        Args.InitPsf                   = @imUtil.kernel2.gauss;
        Args.InitPsfArgs               = {[0.1;2]};
        Args.Conn                      = 8;
        Args.CleanSources              = true;
        Args.cleanSourcesArgs          = {};

        % --- stamp cutouts ---
        Args.RadiusPSF                 = 8;
        Args.DeltaSigma                = 0.5;        % if empty skip
        Args.image2cutoutsArgs         = {};
        Args.backgroundCubeArgs        = {};

        % --- source quality / shape filters ---
        Args.NighRadius                = 7;          % legacy name -> buildPSF 'NeighRadius'
        Args.SigmaQuantile             = [0.05 0.8]; % if empty skip

        % --- centering / sub-pixel shift ---
        Args.ShiftMethod               = 'fft';      % 'lanczos3' | 'fft'

        % --- stack combination ---
        Args.SumMethod                 = 'median';
        Args.VarOfMean                 = true;
        Args.SigmaClip                 = [3 3];
        Args.SigmaClipNiter            = 2;
        Args.Weighted                  = true;
        Args.WeightsMaxSN              = 100;
        Args.mean_sigclipArgs          = {};

        % --- wing suppression ---
        Args.WingsMethod               = 'analytic';
        Args.WingsPowerLaw             = 2;
        Args.SuppressFun               = @imUtil.kernel2.cosbell;
        Args.WingsThreshold            = 1e-2; %1e-4;       % legacy name -> buildPSF 'SuppressThreshold'
        Args.SuppressWidth             = 3;          % legacy name -> buildPSF 'SuppressFunPars'
        Args.WingRangeSN               = [];         % bright-star sample for WingsMethod='empirical'; [] -> [RangeSN(2), Inf]
        Args.MinWingStars              = 8;          % min bright stars for WingsMethod='empirical'; else falls back to cosbell
        Args.SkipEllipticityFallback logical = false; % forward to buildPSF: skip the wingsFix ellipticity fallback for the main splice
        Args.EllipticalWings logical   = false;       % forward to buildPSF: elliptical main-splice wings matched to the core shape
        Args.WingProfile               = [];         % precomputed visit-level wing shape(s) (struct array from imProc.psf.visitWingProfile); scalar or one per input object (indexed per Iobj, clipped at end). Only used with Method='new' and WingsMethod='empirical'; empty -> legacy per-image wing calibration in buildPSF
        Args.BuildDetectionPSF         = false;      % also populate a 'Purpose'-dimensioned detection-PSF slice (analytic, Alpha=DetectionWingsPowerLaw) alongside the main photometry/subtraction PSF
        Args.DetectionWingsPowerLaw    = 2;          % power-law index for the detection-PSF slice; 2 is the only value validated safe for multiIterExtractor's matched filter

        % --- legacy / no-op options (kept for backward compatibility) ---
        Args.moment2Args               = {};
        Args.MinNumGoodPsf             = 5;
        Args.constructPSF_cutoutsArgs  = {};
        Args.SmoothWings               = true;       % old: psf_zeroConverge  !! set to false
        Args.SuppressWings             = false;      % suppressWings fun      !! set to true
        Args.SuppressEdges             = true;       % suppressEdges fun      !! set to false
        Args.DataType                  = [];
        Args.CropByQuantile logical    = false;
        Args.Quantile                  = 0.999;

        %--- Extended PSF ---
        Args.PopExtended               = false;
        Args.ExtendedSize              = [1501 1501];
        Args.Alpha                     = 1;
    end

    if Args.BuildDetectionPSF && ~strcmpi(Args.Method,'new')
        error('populatePSF:invalidMethod', 'BuildDetectionPSF requires Method=''new'' -- the ''%s'' backend has no concept of a detection-PSF slice and would silently drop it.', Args.Method);
    end

    %Result = [];
    Nobj   = numel(Obj);
    for Iobj=1:1:Nobj
        % for each AstroImage elenment
        
        switch lower(Args.Method)
            case 'new'
                if Obj(Iobj).PSFData.isemptyPSF || Args.RePopulatePSF
                    % check if catalog of sources is available
                    if Obj(Iobj).CatData.isemptyCatalog
                        % catalog is not available
                        X  = [];
                        Y  = [];
                        SN = [];
                    else
                        % catalog is available
                        XY = Obj(Iobj).CatData.getXY;
                        X  = XY(:,1);
                        Y  = XY(:,2);
                        SN = Obj(Iobj).CatData.getCol(Args.ColSN);
                    end
                    if isempty(Obj(Iobj).Back) || isempty(Obj(Iobj).Var)
                        % estimate background
                        Obj(Iobj) = imProc.background.background(Obj(Iobj), Args.backgroundArgs{:});
                    end

                    % Saturated-pixel mask, used only by WingsMethod='empirical'
                    % (to mask bright stars' saturated cores before they
                    % contribute to the wing calibration).
                    if Obj(Iobj).MaskData.isemptyImage
                        SaturatedMask = [];
                    else
                        SaturatedMask = Obj(Iobj).MaskData.findBit('Saturated');
                    end

                    % Renames at the call site (legacy populatePSF keys
                    % preserved for back-compat; mapped to new buildPSF
                    % names):
                    %   NighRadius     -> NeighRadius
                    %   WingsThreshold -> SuppressThreshold
                    %   SuppressWidth  -> SuppressFunPars
                    % populatePSF keys with no buildPSF counterpart are
                    % silently dropped: moment2Args, MinNumGoodPsf,
                    % constructPSF_cutoutsArgs, SmoothWings, SuppressWings,
                    % SuppressEdges, DataType, CropByQuantile, Quantile.

                    % Visit-level wing shape for this object ([] = legacy
                    % per-image calibration). A scalar WingProfile serves all
                    % objects; an array is indexed per object (clipped at end).
                    if isempty(Args.WingProfile)
                        WingProfI = [];
                    else
                        WingProfI = Args.WingProfile(min(Iobj, numel(Args.WingProfile)));
                    end
                    if Args.PopExtended
                        [Result(Iobj), MeanPSF, VarPSF, NimPSF, ExtendedPSF, DetectionPSF] = imUtil.psf.buildPSF(Obj(Iobj).Image,...
                                                'X',X, 'Y',Y,...
                                                'SN',SN,...
                                                'Back',Obj(Iobj).Back,...
                                                'Var',Obj(Iobj).Var,...
                                                ... % background / variance
                                                'SubAnnulusBack',Args.SubAnnulusBack,...
                                                'Annulus',Args.Annulus,...
                                                'BackQuantile',Args.BackQuantile,...
                                                'StdQuantile',Args.StdQuantile,...
                                                ... % source detection
                                                'ThresholdPSF',Args.ThresholdPSF,...
                                                'RangeSN',Args.RangeSN,...
                                                'SNdiff',Args.SNdiff,...
                                                'InitPsf',Args.InitPsf,...
                                                'InitPsfArgs',Args.InitPsfArgs,...
                                                'Conn',Args.Conn,...
                                                'CleanSources',Args.CleanSources,...
                                                'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                ... % stamp cutouts
                                                'RadiusPSF',Args.RadiusPSF,...
                                                'DeltaSigma',Args.DeltaSigma,...
                                                'image2cutoutsArgs',Args.image2cutoutsArgs,...
                                                'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                ... % source quality / shape filters
                                                'NeighRadius',Args.NighRadius,...
                                                'SigmaQuantile',Args.SigmaQuantile,...
                                                ... % centering / shift
                                                'ShiftMethod',Args.ShiftMethod,...
                                                ... % stack combination
                                                'SumMethod',Args.SumMethod,...
                                                'VarOfMean',Args.VarOfMean,...
                                                'SigmaClip',Args.SigmaClip,...
                                                'SigmaClipNiter',Args.SigmaClipNiter,...
                                                'Weighted',Args.Weighted,...
                                                'WeightsMaxSN',Args.WeightsMaxSN,...
                                                'mean_sigclipArgs',Args.mean_sigclipArgs,...
                                                ... % wing suppression
                                                'WingsMethod',Args.WingsMethod,...
                                                'WingsPowerLaw',Args.WingsPowerLaw,...
                                                'SuppressFun',Args.SuppressFun,...
                                                'SuppressThreshold',Args.WingsThreshold,...
                                                'SuppressFunPars',Args.SuppressWidth,...
                                                'SaturatedMask',SaturatedMask,...
                                                'WingRangeSN',Args.WingRangeSN,...
                                                'MinWingStars',Args.MinWingStars,...
                                                'WingProfile',WingProfI,...
                                                'SkipEllipticityFallback',Args.SkipEllipticityFallback,...
                                                'EllipticalWings',Args.EllipticalWings,...
                                                'BuildDetectionPSF',Args.BuildDetectionPSF,...
                                                'DetectionWingsPowerLaw',Args.DetectionWingsPowerLaw,...
                                                'ExtendedSize',Args.ExtendedSize,...
                                                'Alpha',Args.Alpha);

                        Obj(Iobj).PSFData.DataExtended = ExtendedPSF;
                    else
                        [Result(Iobj), MeanPSF, VarPSF, NimPSF, ~, DetectionPSF] = imUtil.psf.buildPSF(Obj(Iobj).Image,...
                                                'X',X, 'Y',Y,...
                                                'SN',SN,...
                                                'Back',Obj(Iobj).Back,...
                                                'Var',Obj(Iobj).Var,...
                                                ... % background / variance
                                                'SubAnnulusBack',Args.SubAnnulusBack,...
                                                'Annulus',Args.Annulus,...
                                                'BackQuantile',Args.BackQuantile,...
                                                'StdQuantile',Args.StdQuantile,...
                                                ... % source detection
                                                'ThresholdPSF',Args.ThresholdPSF,...
                                                'RangeSN',Args.RangeSN,...
                                                'SNdiff',Args.SNdiff,...
                                                'InitPsf',Args.InitPsf,...
                                                'InitPsfArgs',Args.InitPsfArgs,...
                                                'Conn',Args.Conn,...
                                                'CleanSources',Args.CleanSources,...
                                                'cleanSourcesArgs',Args.cleanSourcesArgs,...
                                                ... % stamp cutouts
                                                'RadiusPSF',Args.RadiusPSF,...
                                                'DeltaSigma',Args.DeltaSigma,...
                                                'image2cutoutsArgs',Args.image2cutoutsArgs,...
                                                'backgroundCubeArgs',Args.backgroundCubeArgs,...
                                                ... % source quality / shape filters
                                                'NeighRadius',Args.NighRadius,...
                                                'SigmaQuantile',Args.SigmaQuantile,...
                                                ... % centering / shift
                                                'ShiftMethod',Args.ShiftMethod,...
                                                ... % stack combination
                                                'SumMethod',Args.SumMethod,...
                                                'VarOfMean',Args.VarOfMean,...
                                                'SigmaClip',Args.SigmaClip,...
                                                'SigmaClipNiter',Args.SigmaClipNiter,...
                                                'Weighted',Args.Weighted,...
                                                'WeightsMaxSN',Args.WeightsMaxSN,...
                                                'mean_sigclipArgs',Args.mean_sigclipArgs,...
                                                ... % wing suppression
                                                'WingsMethod',Args.WingsMethod,...
                                                'WingsPowerLaw',Args.WingsPowerLaw,...
                                                'SuppressFun',Args.SuppressFun,...
                                                'SuppressThreshold',Args.WingsThreshold,...
                                                'SuppressFunPars',Args.SuppressWidth,...
                                                'SaturatedMask',SaturatedMask,...
                                                'WingRangeSN',Args.WingRangeSN,...
                                                'MinWingStars',Args.MinWingStars,...
                                                'WingProfile',WingProfI,...
                                                'SkipEllipticityFallback',Args.SkipEllipticityFallback,...
                                                'EllipticalWings',Args.EllipticalWings,...
                                                'BuildDetectionPSF',Args.BuildDetectionPSF,...
                                                'DetectionWingsPowerLaw',Args.DetectionWingsPowerLaw);
                    end
                    % insert PSF data
                    if Args.BuildDetectionPSF && ~isempty(DetectionPSF)
                        % 'Purpose'-dimensioned cube: slice 1 = photometry/
                        % subtraction PSF (default), slice 2 = detection PSF.
                        % Ndim in AstroPSF.getPSF is derived from
                        % ndims(DataPSF)-2, so for a 3D cube only DimName{1}/
                        % DimVals{1} are consulted -- overwrite that slot
                        % rather than appending a new dimension.
                        Obj(Iobj).PSFData.Data = cat(3, MeanPSF, DetectionPSF);
                        Obj(Iobj).PSFData.DimName{1} = 'Purpose';
                        Obj(Iobj).PSFData.DimVals{1} = [1 2];
                    else
                        Obj(Iobj).PSFData.Data   = MeanPSF;
                    end
                    Obj(Iobj).PSFData.Var    = VarPSF;
                    Obj(Iobj).PSFData.Nstars = NimPSF;
                    Obj(Iobj).PSFData.SuppressRad = Result(Iobj).SuppressRad;
                end

            case 'legacy'
        
                if Obj(Iobj).PSFData.isemptyPSF || Args.RePopulatePSF
                    % check if catalog of sources is available
                    if Obj(Iobj).CatData.isemptyCatalog
                        % catalog is not available
                        X  = [];
                        Y  = [];
                        SN = [];
                    else
                        % catalog is available
                        XY = Obj(Iobj).CatData.getXY;
                        X  = XY(:,1);
                        Y  = XY(:,2);
                        SN = Obj(Iobj).CatData.getCol(Args.ColSN);
                    end
                    if isempty(Obj(Iobj).Back) || isempty(Obj(Iobj).Var)
                        % estimate background
                        Obj(Iobj) = imProc.background.background(Obj(Iobj), Args.backgroundArgs{:});
                    end
                    
                    [Result(Iobj), MeanPSF, VarPSF, NimPSF] = imUtil.psf.constructPSF(Obj(Iobj).Image,...
                        'X',X, 'Y',Y,...
                        'SN',SN,...
                        'Back',Obj(Iobj).Back,...
                        'Var',Obj(Iobj).Var,...
                        'SubAnnulusBack',Args.SubAnnulusBack,...
                        'RadiusPSF',Args.RadiusPSF,...
                        'Annulus',Args.Annulus,...
                        'image2cutoutsArgs',Args.image2cutoutsArgs,...
                        'ThresholdPSF',Args.ThresholdPSF,...
                        'RangeSN',Args.RangeSN,...
                        'InitPsf',Args.InitPsf,...
                        'InitPsfArgs',Args.InitPsfArgs,...
                        'Conn',Args.Conn,...
                        'CleanSources',Args.CleanSources,...
                        'cleanSourcesArgs',Args.cleanSourcesArgs,...
                        'backgroundCubeArgs',Args.backgroundCubeArgs,...
                        'SNdiff',Args.SNdiff,...
                        'moment2Args',Args.moment2Args,...
                        'DeltaSigma',Args.DeltaSigma,...
                        'NighRadius',Args.NighRadius,...
                        'MinNumGoodPsf',Args.MinNumGoodPsf,...
                        'constructPSF_cutoutsArgs',Args.constructPSF_cutoutsArgs,...
                        'SumMethod',Args.SumMethod,...
                        'SmoothWings',Args.SmoothWings,...
                        'SuppressWings',Args.SuppressWings,...
                        'WingsThreshold',Args.WingsThreshold,...
                        'SuppressEdges',Args.SuppressEdges,...
                        'SuppressFun',Args.SuppressFun,...
                        'SuppressWidth',Args.SuppressWidth,...
                        'ShiftMethod',Args.ShiftMethod,...
                        'DataType',Args.DataType,...
                        'CropByQuantile',Args.CropByQuantile,...
                        'Quantile',Args.Quantile);
                    
                    % insert PSF data
                    Obj(Iobj).PSFData.Data = MeanPSF;
                    Obj(Iobj).PSFData.Var  = VarPSF;
                    Obj(Iobj).PSFData.Nstars = NimPSF;
                    
                end
        
            case 'old' % just for backward compatibility 
                Result(Iobj) = imProc.psf.constructPSF(Obj(Iobj), Args.oldconstructPSFArgs{:});
            otherwise
                error 'Incorrect method chosen in imProc.psf.populatePSF'
        end
                        
    end           
    
end
