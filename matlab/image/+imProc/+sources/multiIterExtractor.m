function [Result, SourceLess, SubtractedImage] = multiIterExtractor(Obj, Args)
    % Source finding and multi-iteration PSF fitting.
    %   This function finds, fits, and subtracts sources in S/N bins,
    %   starting with the brightest stars and proceeding to the faintest.
    %   The outcome is finding more stars than regular source finding.
    %   The function includes the following heuristic step: for stars in
    %   the brightest S/N bin, extra background and noise is injected
    %   around the stars in order to avoid finding artifacts due to the
    %   finite size of the PSF / scattered light.
    % Input  : - An AstroImage object (array allowed).
    %          * ...,key,val,...
    %
    %            --- PSF / photometry method selectors ---
    %            'MethodPSF' - Method passed to imProc.psf.populatePSF as
    %                   its 'Method' argument: 'new' | 'legacy' | 'old'.
    %                   Default is 'new'.
    %            'SumMethodPSF' - Stamp combination method used inside
    %                   populatePSF/buildPSF (the 'SumMethod' key):
    %                   'median' | 'mean' | 'sigclip' | 'sigclip_mex'.
    %                   Default is 'median'.
    %            'PsfPhotMethod' - Method forwarded to
    %                   imProc.sources.psfFitPhot as 'PsfPhotMethod'.
    %                   Default is 'legacy'.
    %            'ShiftMethod' - Sub-pixel shift method forwarded to both
    %                   populatePSF and psfFitPhot. 'lanczos3' | 'fft'.
    %                   Default is 'lanczos3'.
    %
    %            --- Pre-subtraction treatment ---
    %            'ExcludeEmpty' - If true, exclude AstroImage elements
    %                   with empty images from processing (and from the
    %                   output, so the result no longer matches the input
    %                   shape). Default is false.
    %            'BitDict' - A BitDictionary object for the bit mask image.
    %                   Default is BitDictionary('BitMask.Image.Default').
    %            'JD' - Vector of JD values for the input images. If
    %                   empty, the JD is read from the header.
    %                   Default is [].
    %            'KeyJD' - Header keyword to use when reading JD; only
    %                   used if 'JD' is empty. If empty, AstroImage/julday
    %                   uses its header-config defaults. Default is [].
    %            'KeyGain' - Header keyword for the image Gain. If the
    %                   keyword is missing, gain is set to 1.
    %                   Default is 'GAIN'.
    %            'KeyNcoadd' - Header keyword for the number of coadded
    %                   images. Default is 'NCOADD'.
    %
    %            --- Background / variance estimation ---
    %            'backVarArgs' - Cell of args forwarded to
    %                   imProc.background.backVar. Default is
    %                   {'Block',[256 256], 'Method',@imUtil.background.modeVar_LogHist, ...
    %                    'MethodArgs',{{'MinVal',10, 'MaxVal',6000},{}}}.
    %            'ReCalcBackIter' - List of iteration indices in which to
    %                   recompute the background and variance.
    %                   Default is [].
    %
    %            --- PSF measurement ---
    %            'ReCalcPsfIter' - Iteration indices in which to re-fit
    %                   the PSF from the data. If UseOriginalPSF=true,
    %                   there is no need to include iteration 1.
    %                   Default is [].
    %            'UseOriginalPSF' - If true, use the PSF already attached
    %                   to the input AstroImage. Setting it to false (or
    %                   listing iter 1 in ReCalcPsfIter) forces a rebuild.
    %                   Default is true.
    %            'populatePSFArgs' - Extra args forwarded verbatim to
    %                   imProc.psf.populatePSF on top of the explicit
    %                   keys also passed there. Default is
    %                   {'CropByQuantile',false, 'SuppressWidth',3, 'SmoothWings',false}.
    %            'RadiusPSF' - Half-size [pix] of the cutouts used for
    %                   PSF construction. Should be >= outer annulus.
    %                   Default is 12.
    %            'AperRadius' - Vector of aperture radii [pix] for
    %                   aperture photometry. Default is [2 4 6].
    %            'Annulus' - [Rin, Rout] of the sky background annulus
    %                   used for local background/variance estimation.
    %                   Default is [10 12].
    %            'MomentsMethod' - Implementation used for moment
    %                   measurements inside findMeasureSources:
    %                   'legacy' | 'mex'. Default is 'mex'.
    %            'AperPhotMethod' - Implementation used for aperture
    %                   photometry inside findMeasureSources:
    %                   'simple' | 'interp'. Default is 'interp'.
    %            'MomPar' - Extra key/val cell forwarded to the moments
    %                   step of findMeasureSources (in addition to the
    %                   per-iteration MomRadius value). Default is {}.
    %            'MomRadius' - Vector of radii [pix] used for moment
    %                   measurements at each iteration. If scalar it is
    %                   replicated to Niter. Recommended ~1.7*FWHM (for
    %                   LAST that is ~3.8). Used by MomentsMethod='legacy'.
    %                   Default is [4].
    %            'ThresholdPSF' - S/N threshold used to select stars for
    %                   PSF construction. Default is 100.
    %            'RangeSN' - [SNmin, SNmax] PSF-filter S/N window for
    %                   stars used by PSF construction.
    %                   Default is [50 1000].
    %            'InitPsf' - Function handle producing the initial-guess
    %                   PSF model used by populatePSF.
    %                   Default is @imUtil.kernel2.gauss.
    %            'InitPsfArgs' - Cell of arguments to InitPsf.
    %                   Default is {[0.1; 1.5]}.
    %            'ConvFunExtendedPSF' - Function handle producing an
    %                   extended-wing PSF kernel convolved with the
    %                   empirical PSF and appended as an extra detection
    %                   template. Set to [] to disable. Default is
    %                   @imUtil.kernel2.sersic.
    %            'ConvFunExtendedPSF_Args' - Args cell for
    %                   ConvFunExtendedPSF. Default is {[1 2 1]}.
    %
    %            --- PSF fitting ---
    %            'psfFitPhotArgs' - Extra args forwarded to
    %                   imProc.sources.psfFitPhot. Default is {}.
    %            'suppressEdgesArgs' - Args forwarded to
    %                   imUtil.psf.suppressEdges for tapering the PSF
    %                   edges. Default is
    %                   {'Fun',@imUtil.kernel2.cosbell, 'FunPars',[9 10], 'Norm',true}.
    %            'UsePSFInterpolant' - If true, build the per-source
    %                   shifted PSF via image interpolation
    %                   (imUtil.trans.shift_interp) rather than using the
    %                   FFT-shifted templates returned by psfFitPhot.
    %                   Default is false.
    %            'FitRadius' - Vector of PSF-fit radii [pix], one per
    %                   iteration. Scalar inputs are replicated to Niter.
    %                   Default is [3].
    %            'MaxIter' - Maximum number of PSF-fit iterations per
    %                   source. Default is 8.
    %            'mexCutout' - Use the MEX image-cutout routines where
    %                   available. Default is true.
    %            'CleanSN' - Minimum PSF-fit S/N for a detection to be
    %                   retained in the final catalog. If empty, no
    %                   cleaning is applied. Default is 4.
    %            'KeyCleanSN' - Catalog column name read for the CleanSN
    %                   filter. Default is 'SN'.
    %
    %            --- Source detection ---
    %            'FindWithEmpiricalPSF' - If true, run source detection
    %                   with the empirical PSF templates; if false, use
    %                   the analytic PSF defined by 'PsfFunPar'.
    %                   Default is true.
    %            'PsfFunPar' - Parameters of the analytic PSF model used
    %                   when FindWithEmpiricalPSF=false.
    %                   Default is {[0.1;1.0;1.5]}.
    %            'Threshold' - Vector of S/N thresholds (sigma) for the
    %                   multi-iteration extraction. Its length sets the
    %                   number of iterations (brightest -> faintest).
    %                   Default is [500 50 5].
    %            'ColCell' - Cell of column names requested from
    %                   imProc.sources.findMeasureSources and stored in
    %                   the AstroCatalog. Default is
    %                   {'XPEAK','YPEAK', 'X1','Y1','X2','Y2','XY',...
    %                    'SN','BACK_IM','VAR_IM', 'BACK_ANNULUS','STD_ANNULUS',...
    %                    'FLUX_APER','FLUXERR_APER','MAG_APER','MAGERR_APER','FLUX_XYPEAK'}.
    %            'ColNamesX' - X column-name dictionary used for flag
    %                   retrieval. Default is AstroCatalog.DefNamesX
    %                   (set to 'X1' to speed things up).
    %            'ColNamesY' - As ColNamesX for Y.
    %                   Default is AstroCatalog.DefNamesY.
    %
    %            --- Source cleaning and mask ---
    %            'RemoveEdgeDist' - Distance from image edges [pix]
    %                   within which sources are removed. NaN disables
    %                   removal. Default is 0.
    %            'FlagCR' - Flag/remove cosmic rays during source
    %                   detection/cleaning. Default is true.
    %            'maskCR_Args' - Extra args to imProc.mask.maskCR.
    %                   Default is {}.
    %            'FlagDiffXY' - Flag sources with inconsistent X/Y
    %                   positions (centroid vs. peak). Default is true.
    %            'maskDiffXY_Args' - Extra args to
    %                   imProc.mask.xpeak_x1_diff. Default is {}.
    %
    %            --- Bright-star back/var inflation ---
    %            'AddBackNoise' - Inject extra background/variance around
    %                   very bright sources in the first iteration in
    %                   order to suppress spurious detections in the PSF
    %                   wings. Default is true.
    %            'ScatteredLightFrac' - Fraction of bright-star flux used
    %                   to construct the legacy scattered-light map
    %                   (MethodBS='old'). Default is 0.05.
    %            'MethodBS' - Algorithm used to add scattered light /
    %                   noise around bright stars: 'prof' (radial
    %                   profile, default) or 'old' (heuristic
    %                   Lorentzian+circular convolution).
    %                   Default is 'prof'.
    %            'BS_R' - Radii grid [pix] used to sample the bright-star
    %                   radial profile when MethodBS='prof'.
    %                   Default is (0:1:1500)+0.1.
    %            'BS_Par' - Parameter vector forwarded to BS_Prof.
    %                   Default is [0.57111 -4.1984 4.7473].
    %            'BS_Prof' - Function handle BS_Prof(BS_Par, BS_R) giving
    %                   the bright-star radial profile, or a numeric
    %                   profile vector. The profile is normalized to a
    %                   reference flux of 1e5 inside the function.
    %                   Default is @(Par,R) 10.^polyval(Par,log10(R)).
    %                   The portion inside max(AperRadius) is flattened
    %                   to avoid double-counting the core.
    %            'BS_PL' - Power-law index used to scale the radial
    %                   profile with the source flux relative to 1e5
    %                   (FluxNorm = (Flux/1e5)^BS_PL). Default is 1.5.
    %
    %            --- Catalog column names ---
    %            'ColRA' - Output catalog column name for J2000 RA.
    %                   Default is 'RA'.
    %            'ColDec' - Output catalog column name for J2000 Dec.
    %                   Default is 'Dec'.
    %            'ColPITER' - Column name storing the extraction
    %                   iteration index of each source.
    %                   Default is 'MITER'.
    %            'RedoUpIter' - If non-empty, run an additional bright-
    %                   sources-only refinement step using iterations up
    %                   to this index. The brightest catalog is then
    %                   updated with aperture & PSF photometry on the
    %                   faint-subtracted image. Default is [].
    %            'ColPsfFlux' - Catalog column for PSF flux (updated by
    %                   the RedoUpIter refinement). Default is 'FLUX_PSF'.
    %            'ColPsfFluxErr' - As above for PSF flux error.
    %                   Default is 'FLUXERR_PSF'.
    %            'ColPsfMag' - PSF magnitude column. Default is 'MAG_PSF'.
    %            'ColPsfMagErr' - PSF magnitude-error column.
    %                   Default is 'MAGERR_PSF'.
    %            'ColPsfSN' - PSF-fit S/N column. Default is 'SN'.
    %            'ColPsfChi2' - Reduced chi-square column of the PSF fit
    %                   (chi^2/d.o.f.). Default is 'PSF_CHI2DOF'.
    %            'ColFlux' - Base name for aperture-flux columns;
    %                   numeric suffixes are appended per aperture radius
    %                   (e.g. FLUX_APER_1, ...). Default is 'FLUX_APER'.
    %            'ColFluxErr' - Base name for aperture-flux-error columns.
    %                   Default is 'FLUXERR_APER'.
    %            'ColMag' - Base name for aperture-magnitude columns.
    %                   Default is 'MAG_APER'.
    %            'ColMagErr' - Base name for aperture-magnitude-error
    %                   columns. Default is 'MAGERR_APER'.
    %
    %            --- Photometric calibration ---
    %            'ZP' - Photometric zero point used for converting fluxes
    %                   to luptitude-like magnitudes in PSF and aperture
    %                   photometry. Default is 25.
    %
    %            --- Miscellaneous ---
    %            'AddSkyCoo' - Add RA, Dec sky coordinates to the final
    %                   catalog using the AstroImage WCS, if present.
    %                   Default is true.
    %            'CreateNewObj' - If true, operate on a deep copy of the
    %                   input AstroImage stack; if false, modify the
    %                   input in-place. Default is false.
    %            'Verbose' - Print progress / diagnostics to the console.
    %                   Default is false.
    %            'WriteDs9Regions' - Write DS9 region files with the
    %                   extracted sources at each iteration.
    %                   Default is false.
    %            'AddSrcStat2Header' - Write source-extraction summary
    %                   keywords (NSTARS, M_CHI2D) into the image header.
    %                   Default is true.
    %            'KeyNsrc' - Header keyword used for the number of
    %                   extracted sources when AddSrcStat2Header=true.
    %                   Default is 'NSTARS'.
    %            'KeyMedChi2Dof' - Header keyword used for the median
    %                   PSF-fit reduced chi-square. Default is 'M_CHI2D'.
    %
    %            --- Streak detection ---
    %            'SearchStreaks' - Search for streaks in the first
    %                   iteration. Default is false.
    %            'detectStreaksLSDArgs' - Extra args to
    %                   imUtil.streaks.detectStreaksLSD. Default is {}.
    %
    %            --- Performance ---
    %            'UseMex' - Use MEX routines where available.
    %                   Default is false.
    %
    % Output : - (Result) An AstroImage array, same size as the input
    %            Obj, in which the Image, Back, Var, PSF and CatData
    %            properties are updated by the multi-iteration PSF source
    %            extraction.
    %          - (SourceLess) An AstroImage array containing copies of
    %            the input images with the final (last-iteration)
    %            source-subtracted images stored in the Image property.
    %            Returned only if requested (nargout>1).
    %          - (SubtractedImage) A numeric cube of size [Ny, Nx, Niter]
    %            containing, for each processed AstroImage, the
    %            subtracted image after each iteration of the algorithm.
    %            Returned only if requested (nargout>2).
    %
    % Author : Eran Ofek (2025 Nov)
    % Example: [AI1,AI2] = imProc.sources.multiIterExtractor(AI);
    %          AI1 = imProc.sources.multiIterExtractor(AI, 'Threshold',[300 30 5]);
    %          AI1 = imProc.sources.multiIterExtractor(AI, 'MethodBS','old', 'AddBackNoise',true);

    arguments
        Obj AstroImage

        Args.SumMethodPSF              = 'median';
        Args.MethodPSF                 = 'new';
        Args.ShiftMethod               = 'lanczos3'; % 'lanczos3' | 'fft'

        Args.PsfPhotMethod             = 'legacy';

        % pre subtraction treatment
        Args.ExcludeEmpty              = false;  % if true, will not keep the shape
        Args.BitDict                   = BitDictionary('BitMask.Image.Default');
        Args.JD                        = [];
        Args.KeyJD                     = [];
        Args.KeyGain                   = 'GAIN';
        Args.KeyNcoadd                 = 'NCOADD';

        % background and variance measurement:
        Args.backVarArgs               = {'Block',[256 256], 'Method',@imUtil.background.modeVar_LogHist, 'MethodArgs',{{'MinVal',10, 'MaxVal',6000},{}}};
        Args.UpdateHeaderDataBkgVar    = false;
        Args.ReCalcBackIter            = []; % list of iterations in which to re-calc the background. If 1, recalc also in the begining.

        % measure PSF
        Args.ReCalcPsfIter             = [];  % Index of iterations in which to re-calc PSF; if UseOriginalPSF=true, then no need to set this to 1.
        Args.UseOriginalPSF logical    = true;   % use the PSF already attached to the input AstroImage
        Args.populatePSFArgs cell      = {'CropByQuantile',false, 'SuppressWidth',3, 'SmoothWings',false}; % {'CropByQuantile',true,'Quantile',0.5}
        Args.RadiusPSF                 = 12;
        Args.AperRadius                = [2, 4, 6];
        Args.Annulus                   = [10 12];
        Args.MomentsMethod             = 'mex';  %'legacy'|'mex'
        Args.AperPhotMethod            = 'interp';  % 'simple'|'interp'
        Args.MomPar                    = {};
        Args.MomRadius                 = [4];  % [pix] for each iteration % recommended MomRadius = 1.7 * FWHM ~ 3.8 (for LAST!) - used in MomentsMethod='legacy'

        Args.ThresholdPSF              = 100;
        Args.RangeSN                   = [50 1000];
        Args.InitPsf                   = @imUtil.kernel2.gauss
        Args.InitPsfArgs cell          = {[0.1; 1.5]}; %{[0.1;1.0;1.5]};  
        Args.ConvFunExtendedPSF        = @imUtil.kernel2.sersic;
        Args.ConvFunExtendedPSF_Args   = {[1 2 1]}; 
        
        
        % PSF fitting
        
        Args.psfFitPhotArgs            = {};
        %Args.suppressEdgesArgs         = {'Fun',@imUtil.kernel2.cosbell, 'FunPars', [9, 10], 'Norm', true};
        Args.UsePSFInterpolant         = false;
        Args.FitRadius                 = [3];% PSF fit radius at each iteration
        Args.MaxIter                   = 8;
        Args.mexCutout                 = true;
        Args.CleanSN                   = 4;  % remove sources below this SN (PSF fitting S/N).
        Args.KeyCleanSN                = 'SN';

        % source detection:        
        Args.FindWithEmpiricalPSF logical = true;
        Args.PsfFunPar cell            = {[0.1;1.0;1.5]};  % search for sources                 
        Args.Threshold                 = [500 50 5]; % [50 16.5 5]; % in sigma, this also specifies the # of iterations   
        Args.ColCell cell              = {'XPEAK','YPEAK',...
                                          'X1', 'Y1',...
                                          'X2','Y2','XY',...
                                          'SN','BACK_IM','VAR_IM',...
                                          'BACK_ANNULUS', 'STD_ANNULUS', ...
                                          'FLUX_APER', 'FLUXERR_APER',...
                                          'MAG_APER', 'MAGERR_APER',...
                                          'FLUX_XYPEAK'};
        Args.ColNamesX                 = AstroCatalog.DefNamesX;
        Args.ColNamesY                 = AstroCatalog.DefNamesY;
        

        % source cleaning and mask
        Args.RemoveEdgeDist            = 0;  % NaN for non removal
        Args.FlagCR logical            = true;
        Args.maskCR_Args cell          = {};
        Args.FlagDiffXY logical        = true;
        Args.maskDiffXY_Args cell      = {};

        % add back/var noise -  Bright stars increase back/var
        Args.AddBackNoise              = true;
        Args.ScatteredLightFrac = 0.05;

        
        % Column names to add the catalog
        Args.ColRA                     = 'RA';
        Args.ColDec                    = 'Dec';
        Args.ColPITER                  = 'MITER';  % column name for the iteration index of the PSF multi-iteration

        % cleaning of the subtracted image:        
        %Args.RemoveMasked              = false;  % the input AI.Mask should be filled, but seems like this filter does not influence the result much ? 
        %Args.RemovePSFCore             = false;  % not decided if this is useful and correct

        Args.RedoUpIter = [];
        

        Args.ColPsfFlux        = 'FLUX_PSF';
        Args.ColPsfFluxErr     = 'FLUXERR_PSF';
        Args.ColPsfMag         = 'MAG_PSF';
        Args.ColPsfMagErr      = 'MAGERR_PSF';
        Args.ColPsfSN          = 'SN';
        Args.ColPsfChi2        = 'PSF_CHI2DOF';
        Args.ColFlux           = 'FLUX_APER';
        Args.ColFluxErr        = 'FLUXERR_APER';
        Args.ColMag            = 'MAG_APER';
        Args.ColMagErr         = 'MAGERR_APER'
        
        Args.ZP                = 25;

       
        % miscellaneous:
        %Args.DeleteInputCatalog        = true;  % delete the catalog property from the input AI stack 
        Args.AddSkyCoo                 = true;  % add RA, Dec from the AstroImage WCS if it is present 
        Args.CreateNewObj              = false;   
        %Args.SaveSourcelessImage logical= false; % save the cleaned sourceless image as the second result
        Args.Verbose                   = false;  
        Args.WriteDs9Regions           = false;
        Args.AddSrcStat2Header         = true;
        Args.KeyNsrc                   = 'NSTARS';
        Args.KeyMedChi2Dof             = 'M_CHI2D';  % median of CHI2DOF over all stars 
        

        Args.SearchStreaks                 = false;
        Args.detectStreaksLSDArgs          = {};

        % Bright stars back/var adjustment:
        Args.BS_R     = (0:1:1500)+0.1;
        Args.BS_BackMaxR  = 1501;
        Args.BS_Par   = [0.57111      -4.1984       4.7473];
        Args.BS_Prof  = @(Par, R) 10.^polyval(Par,log10(R));
        Args.BS_PL    = 1.0;
        Args.MethodBS = 'prof';
        Args.BS_ColFlux = 'FLUX_APER_4';
        Args.IsBackSub   = false;   % If true, will not estimate the VarFactor empirically.
        Args.AddExtraBack   = true;
        Args.AddExtraVar    = true;
        Args.NcoaddFactor   = 1;

        Args.UseMex                        = false;


        %--- Extednded PSF ---
        Args.PopExtended               = false;
        Args.ExtendedSize              = [1501 1501];
        Args.Alpha                     = 1;
    end

    %Args.BS_R = Args.BS_R(Args.BS_R<Args.BS_MaxR);

    if isa(Args.BS_Prof, 'function_handle')
        BS_RadProf = Args.BS_Prof(Args.BS_Par, Args.BS_R);
        %loglog(R,10.^polyval([Par],log10(R)))
        
        Ibsr = find(Args.BS_R<1, 1, 'last'); %min(Args.AperRadius),1,'last');
        BS_RadProf(1:Ibsr-1) = BS_RadProf(Ibsr);
        %Fbs = BS_RadProf>1e6;
        %BS_RadProf(Fbs) = 1e6;
    else
        BS_RadProf = Args.BS_Prof;
    end
    MaxRadius = ceil(max(Args.BS_R));
    
    % check consistency
    % if numel(Args.Threshold) > numel(Args.MomRadius) || numel(Args.Threshold) > numel(Args.FitRadius)
    %     error('The length of Args.Threshold does must comply with that of Args.MomRadius');
    % end
    Niter = numel(Args.Threshold);
    Nobj  = numel(Obj);

    % repair some parameters if needed: 
    Args.MomRadius = Args.MomRadius(:).*ones(Niter,1);
    Args.FitRadius = Args.FitRadius(:).*ones(Niter,1);
    
    % create a new object if requested  
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
     
    % exclude objects with empty images
    if Args.ExcludeEmpty
        FlagEmptyImage = Result.isemptyProperty('Image');
        Result         = Result(~FlagEmptyImage);
    end

    if isempty(Args.JD)
        JD = Result.julday('KeyJD',Args.KeyJD);
    else
        JD = Args.JD;
    end
    
    % measure background and variance if it is missing or if the object is new
    ReCalcBackIter1 = any(Args.ReCalcBackIter==1); % re-calc backgroun in 1st iteration
    FlagBack = ReCalcBackIter1 | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
    if any(FlagBack)
        % redo everything - will keep the shape
        Result = imProc.background.backVar(Result, 'AddHeaderInfo', Args.UpdateHeaderDataBkgVar, Args.backVarArgs{:});
    end
    
    % measure PSF if it does not exist or if the user requested to re-calc
    % NB: if the input catalog is empty, the catalog struct need for PSF measurements
    % will be generated inside imUtil.psf.constructPSF by imUtil.sources.findSources 
    % at Threshold > 20 sigma, but the object's catalog property will not be populated
    FlagPSF = Result.isemptyPSF | ~Args.UseOriginalPSF; 
    if any(FlagPSF)
        % redo everything - keep the shape:
        [Result] = imProc.psf.populatePSF(Result,...
                                                   Args.populatePSFArgs{:},...
                                                   'Method',Args.MethodPSF,...
                                                   'SumMethod',Args.SumMethodPSF,...
                                                   'ShiftMethod', Args.ShiftMethod,...
                                                   'RadiusPSF',Args.RadiusPSF,...
                                                   'Annulus',Args.Annulus,...
                                                   'ThresholdPSF',Args.ThresholdPSF,...
                                                   'RangeSN',Args.RangeSN,...
                                                   'InitPsf',Args.InitPsf,...
                                                   'InitPsfArgs',Args.InitPsfArgs,...
                                                   'RePopulatePSF',true,...
                                                   'PopExtended',Args.PopExtended,...
                                                   'ExtendedSize',Args.ExtendedSize,...
                                                   'Alpha',Args.Alpha);
    end



    % delete the object's input catalog 
    % if the catalog is not removed, it may conflict with the new ones 
    FlagPopCat = Result.sizeCatalog>0;
    if any(FlagPopCat)
        Result(FlagPopCat).deleteProp('CatData');
    end
    
    % Define AstroImage of subtracted sources
    if nargout>1
        ExtraOutput = true;
    else
        ExtraOutput = false;
    end
    if ExtraOutput
        SourceLess = AstroImage(size(Result));   
    end

    % get GAIN and NCOADD
    Keys = Result.getStructKey({Args.KeyGain, Args.KeyNcoadd});

    % find and measure sources using multi-iteration PSF fitting    
    FWHM = nan(Nobj,1);
    for Iobj=1:1:Nobj
        if Args.Verbose
            fprintf('Image %d of %d \n',Iobj,Nobj);
        end    
        %Result(Iobj).Table = [];
        FWHM(Iobj) = Result(Iobj).PSFData.fwhm;
      
        if isnan(Keys(Iobj).(Args.KeyGain))
            Gain = 1;
        else
            Gain = Keys(Iobj).(Args.KeyGain);
        end
        if isnan(Keys(Iobj).(Args.KeyNcoadd))
            Ncoadd = 1;
        else
            Ncoadd = Keys(Iobj).(Args.KeyNcoadd);
        end
        


        % PSFTemplate = Args.InitPsf(Args.InitPsfArgs{:});
        % PSFTemplate = repmat(single(0), )

        
        % we need a deep copy here, otherwise, the initial image is not kept in the AI!
        AI              = Result(Iobj).copy;                                    % this AI will be iterated for each Obj 
        AI.CatData      = [];
        %AI.Table        = [];
        Cat             = AstroCatalog([1 Niter]);                              % catalogs produced at each iter, merged afterwards 
        
        SizeImage       = size(AI.ImageData.Image);

        if Args.UseMex
            % SourceImage     = zeros(SizeImage(1), SizeImage(2), Niter, 'like',AI.ImageData.Data);    % source image after each iteration
            % SumSourceImage  = zerso(SizeImage(1), SizeImage(2), 'like',AI.ImageData.Data);    % source image after each iteration
            % if ExtraOutput
            %     SubtractedImage = zeros(SizeImage(1), SizeImage(2), Niter, 'like',AI.ImageData.Data);    % subtracted image after each iteration
            % end

            %ClassI = class(AI.ImageData.Data);
            % FROM SOME reasons using zeros or allocateUninit fails the
            % function.
            SourceImage     = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % source image after each iteration
            SumSourceImage  = repmat(single(0), SizeImage(1), SizeImage(2));    % source image after each iteration
            if ExtraOutput
                SubtractedImage = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % subtracted image after each iteration
            end 

            
            %SourceImage     = tools.array.mex.allocateUninit([SizeImage(1), SizeImage(2), Niter], ClassI);    % source image after each iteration
            %SumSourceImage  = tools.array.mex.allocateUninit([SizeImage(1), SizeImage(2)], ClassI);    % source image after each iteration
            %if ExtraOutput
            %    SubtractedImage = tools.array.mex.allocateUninit([SizeImage(1), SizeImage(2), Niter], ClassI);    % subtracted image after each iteration
            %end
        else
            %ClassI = class(AI.ImageData.Data);
            SourceImage     = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % source image after each iteration
            SumSourceImage  = repmat(single(0), SizeImage(1), SizeImage(2));    % source image after each iteration
            if ExtraOutput
                SubtractedImage = repmat(single(0), SizeImage(1), SizeImage(2), Niter);    % subtracted image after each iteration
            end 

            % SourceImage     = zeros(SizeImage(1), SizeImage(2), Niter, 'like',AI.ImageData.Data);    % source image after each iteration
            % SumSourceImage  = zerso(SizeImage(1), SizeImage(2), 'like',AI.ImageData.Data);    % source image after each iteration
            % if ExtraOutput
            %     SubtractedImage = zeros(SizeImage(1), SizeImage(2), Niter, 'like',AI.ImageData.Data);    % subtracted image after each iteration
            % end
        end

        SizePSF = size(AI.PSFData.DataPSF);
        % if isempty(size(AI.PSFData.DataPSF))
        %     % no PSF / revert to anal;ytical PSF
        %     FindWithEmpiricalPSF = false;
        % else
        %     FindWithEmpiricalPSF = Args.FindWithEmpiricalPSF;
        % end
        if isempty(AI.PSFData.DataPSF)
            % No PSF - do not look for stars!
            % See issue #963 - consider calling findMeasureSources

        else
            for Iiter=1:1:Niter     
    
                if Iiter==1
                    SearchStreaks = Args.SearchStreaks;
                else
                    SearchStreaks = false;
                end
                % find sources (without background recalculation) with the empirical PSF or with a set of Gaussians                     
                % in each case the sources identified as CRs are removed from the catalog
                % NB: 1. If 'Psf' is provided, this parameter overrides the PsfFun input argument
                %     2. When a PSF stamp is used for source detection, the output catalog does not contain SN_3, just SN_1 and SN_2!                
                if Args.FindWithEmpiricalPSF                   
                    
                    if Iiter==1
                        
                        if ~isempty(Args.ConvFunExtendedPSF)
                            ConvExtended = Args.ConvFunExtendedPSF(Args.ConvFunExtendedPSF_Args{:}, SizePSF);
                            PSFTemplate = repmat(single(0), [SizePSF, 3]);
                            PSFTemplate(:,:,3) = conv2(AI.PSFData.DataPSF, ConvExtended, 'same');
                        else
                            PSFTemplate = repmat(single(0), [SizePSF, 2]);
                        end
                        
                        PSFTemplate(:,:,1) = Args.InitPsf(Args.InitPsfArgs{1}(1),size(AI.PSF)); % a narrow delta-like PSF for CR rejection                 
                        PSFTemplate(:,:,2) = AI.PSFData.DataPSF; % the empirical PSF
                        
                        % check the information content overlap between the PSF
                        % and extended PSF:
                        % tools.math.filter.infoOverlapFilters(squeeze(PSFTemplate(:,:,3)),squeeze(PSFTemplate(:,:,2)))
    
                        %PSFTemplate = AI.PSF; % the empirical PSF
                        
                    end
    
                    [AI,Streaks] = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                              'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                              'MomPar',{'MomRadius',Args.MomRadius(Iiter)},...
                                                              'AperRadius',Args.AperRadius,...
                                                              'Annulus',Args.Annulus,...
                                                              'MomentsMethod',Args.MomentsMethod,...
                                                              'AperPhotMethod',Args.AperPhotMethod,...
                                                              'Psf',PSFTemplate,...
                                                              'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                              'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                              'ColCell',Args.ColCell,...
                                                              'ColNamesX',Args.ColNamesX,...
                                                              'ColNamesY',Args.ColNamesY,...
                                                              'BitDict',Args.BitDict,...
                                                              'JD',JD,...
                                                              'ZP',Args.ZP,...
                                                              'SearchStreaks',SearchStreaks,...
                                                              'detectStreaksLSDArgs',Args.detectStreaksLSDArgs);
                   
                    ColSN = 'SN_2';   
                    if Iiter==1
                        Result(Iobj).Streaks = Streaks;
                    end
                    %clear PSFTemplate
                else
                    [AI,Streaks] = imProc.sources.findMeasureSources(AI,'Threshold', Args.Threshold(Iiter),'ReCalcBack',false,...
                                                              'RemoveEdgeDist',Args.RemoveEdgeDist,...
                                                              'MomPar',{'MomRadius',Args.MomRadius(Iiter)},...
                                                              'AperRadius',Args.AperRadius,...
                                                              'Annulus',Args.Annulus,...
                                                              'MomentsMethod',Args.MomentsMethod,...
                                                              'AperPhotMethod',Args.AperPhotMethod,...
                                                              'PsfFunPar',Args.PsfFunPar,...
                                                              'FlagCR',Args.FlagCR,'maskCR_Args',Args.maskCR_Args,...
                                                              'FlagDiffXY',Args.FlagDiffXY, 'maskDiffXY_Args',Args.maskDiffXY_Args,...
                                                              'ColCell',Args.ColCell,...
                                                              'BitDict',Args.BitDict,...
                                                              'JD',JD,...
                                                              'ZP',Args.ZP,...
                                                              'SearchStreaks',SearchStreaks,...
                                                              'detectStreaksLSDArgs',Args.detectStreaksLSDArgs);
                   
                    if Iiter==1
                        Result(Iobj).Streaks = Streaks;
                    end
                    ColSN = 'SN_2';
                end % if Args.FindWithEmpiricalPSF                             
                
                NumSrc = height(AI.CatData.Catalog);
                
                if Args.Verbose
                    fprintf('Iter. %d: S/N > %d, mean bkg = %.0f, mean var = %.0f, Nobj: %d\n',...
                                        Iiter,Args.Threshold(Iiter),mean(AI.Back,'all','omitnan'),mean(AI.Var,'all','omitnan'),NumSrc);
                end            
                % insert a column with iteration number into the source catalog
                %AI.CatData = insertCol(AI.CatData, repmat(Iiter,1,NumSrc)', Inf, 'ITER', {''});
                
                % measure the PSF (if we believe that the PSF is flux-dependent?) or use the previous one 
                ReCalcPSF = any(Args.ReCalcPsfIter==Iiter);
                if ReCalcPSF 
                    %|| isempty(AI.PSF)
                    AI = imProc.psf.populatePSF(AI,Args.populatePSFArgs{:});                
                end
                
                % PSF photometry
                %[Iobj Nobj]
                [AI, Res] = imProc.sources.psfFitPhot(AI,'ColSN',ColSN,'FitRadius',Args.FitRadius(Iiter), 'MaxIter',Args.MaxIter, 'ZP',Args.ZP, 'UseMex',Args.UseMex,...
                                                         'PsfPhotMethod',Args.PsfPhotMethod,...
                                                         'ShiftMethod',Args.ShiftMethod,...
                                                         Args.psfFitPhotArgs{:});  % produces PSFs shifted to RoundX, RoundY, so there is no need to Recenter
    
                
                % use either a) interpolation (experimental) or b) FFT shift (obtained above as Res.ShiftedPSF) + edge suppression
                if Args.UsePSFInterpolant
                    ShiftedPSF = imUtil.trans.shift_interp(AI.PSFData.Data, Res.DX, Res.DY, 'Norm',true);
                    
                else
                    if isempty(Res)
                        ShiftedPSF = [];
                    else
                        ShiftedPSF = Res.ShiftedPSF;
                    end
                    % already done in PSF construction
                    %ShiftedPSF = imUtil.psf.suppressEdges(Res.ShiftedPSF, Args.suppressEdgesArgs{:}); 
                end            
        
                % subtract the newly found and measured sources:
                % 1. construct a source image
                % 2. subtract the source image from the current image
                if isempty(ShiftedPSF)
                    % deals with no stars found in iteration
                    SourceImage(:,:,Iiter) = zeros(SizeImage, 'single');
                else
    
                    [CubePSF, XY]                = imUtil.art.createSourceCube(ShiftedPSF, [Res.RoundY Res.RoundX], Res.Flux, ...
                                                                                'Recenter', false,'PositivePSF',false, 'FunEdge',[]);
                   
                    %CubePSF = imUtil.psf.mex.cosbellTaper(CubePSF,[9 11]);
                    %SourceImage(:,:,Iiter)       = imUtil.art.addSources(zeros(SizeImage, 'single'), permute(CubePSF,[2,1,3]),XY,...
                    %                                                            'Oversample',[],'Subtract',false);  
                    SourceImage(:,:,Iiter)       = imUtil.art.addSources(zeros(SizeImage, 'single'), CubePSF, XY,...
                                                                                'Oversample',[],'Subtract',false);

                    % add wings around bright stars
                    % if Iiter==1
                    %     ColData = AI.CatData.getColMulti({'XPEAK','YPEAK','FLUX_APER_3'});
                    %     % 
                    %     MinFluxFlag = ColData(:,3)>1e5;
                    %     X = ColData(MinFluxFlag,1);
                    %     Y = ColData(MinFluxFlag,2);
                    %     FluxAtR = imUtil.sources.mex.fluxAtRadius(AI.ImageData.Data, [X, Y], [10 12]);
                    %     [~,Ir]  = min(abs(Args.BS_R-10));
                    %     FluxNorm = FluxAtR./BS_RadProf(Ir);
                    %     SourceImage(:,:,Iiter) = imUtil.art.mex.addBrightSourceProfile(SourceImage(:,:,Iiter), X, Y, FluxNorm, Args.BS_BackMaxR.*ones(size(FluxNorm)), BS_RadProf);
                    % 
                    % end

                    SumSourceImage = SumSourceImage + SourceImage(:,:,Iiter);
                end
                Subtracted                   = AI.ImageData.Image - SourceImage(:,:,Iiter);  
                
                
                % optionaly set pixels with Mask > 0 to the background values (in practice this does not influence the result?)
                %if Args.RemoveMasked
                %    Ind = AI.Mask > 0;                
                %    Subtracted(Ind) = AI.Back(Ind);
                %end
    
                % NOT CLEAR WHAT IS THIS FOR?
                % optionaly set pixels with reconstructed source PSFs to the background values 
                %if Args.RemovePSFCore
                %    Ind = SourceImage(:,:,Iiter) > 0;
                %    Subtracted(Ind) = AI.Back(Ind); % need to be tested and improved to operate only on a 3x3 (5x5?) pixel core
                %end              
                            
                Cat(Iiter)                   = AI.CatData; 
                
                % add PITER to catalog
                Nsrc = size(Cat(Iiter).Catalog, 1);
                Cat(Iiter).insertCol(Iiter.*ones(Nsrc,1),Inf, Args.ColPITER,'');
                
                AI.Image                     = Subtracted; % replace the image with the subtracted image
                
                if ExtraOutput
                    SubtractedImage(:,:,Iiter)   = Subtracted; % populate the array of subtracted images 
                end
    
                % re-measure background at each Iter > 1 if Args.ReCalcBack = true and add source noise to the variance                
                ReCalcBackIterI = any(Args.ReCalcBackIter==Iiter); % re-calc backgroun in Iiter iteration
                if ReCalcBackIterI
                    FlagBack         = ReCalcBackIterI | Result.isemptyProperty('Back') | Result.isemptyProperty('Var');
                    Result(FlagBack) = imProc.background.backVar(Result(FlagBack), Args.backVarArgs{:}, 'ReCalc',true);
                    %Result(FlagBack) = imProc.background.backVar(Result, 'AddHeaderInfo', Args.UpdateHeaderDataBkgVar, Args.backVarArgs{:});
                end
                
                % add local variance from the sources revealed at all the previous iterations
                % This is not enough - for bright stars the PSF is more
                % extended and the star edges are not subtracted
                AI.VarData.Image  = AI.VarData.Image  + SumSourceImage./(Ncoadd.*Gain);
                AI.BackData.Image = AI.BackData.Image + SumSourceImage;  
                % if Iiter==1
                %     if Args.IsBackSub
                %         VarFactor = 1./(Ncoadd.*Args.NcoaddFactor);
                %     else
                %         % Image is NOT background subtracted
                %         % can estimate the VarFactor empirically
                %         VarFactor   = (AI.VarData.Data(1)./AI.BackData.Data(1)).^2; %   <1./(Ncoadd.*Gain) or use AI.VarData.Data(1)./AI.BackData.Data(1)>
                %     end
                %     AI.VarData.Image  = imUtil.art.mex.addBrightSourceProfile(AI.VarData.Data, X, Y, FluxNorm.*VarFactor, 1501.*ones(size(FluxNorm)), BS_RadProf); %./Args.BS_Ncoadd;
                % end
    
                if Iiter==1 && Args.AddBackNoise
                    % Add noise/back around bright sources
                    %GK = imUtil.kernel2.gauss(FWHM);
                    %AI.Var  = conv2(AI.Var, GK, 'same'); 

                    
                    switch Args.MethodBS
                        case 'prof'


                            % Add noise/back around bright sources
                            %tic;
                            % normalize such that its correct for a source
                            % with flux of 10^5:
                            %BS_RadProf = io.files.load2('/home/eran/LAST_BrightStar_RadialProfile.mat');
        
                            ColData = AI.CatData.getColMulti({'XPEAK','YPEAK','FLUX_APER_3'});
                            % 
                            MinFluxFlag = ColData(:,3)>1e5;
                            X = ColData(MinFluxFlag,1);
                            Y = ColData(MinFluxFlag,2);
                            Flux = ColData(MinFluxFlag,3);
                            FluxNorm = (Flux./1e5).^Args.BS_PL;
                            % 
                            MaxRadiusF = repmat(MaxRadius,size(X));
                            

                            %When the image is background subtracted this
                            %has no meaning
                            % This has meaning only when gain=1.
                            if Args.IsBackSub
                                VarFactor = 1./(Ncoadd.*Args.NcoaddFactor);
                            else
                                % Image is NOT background subtracted
                                % can estimate the VarFactor empirically
                                VarFactor   = (AI.VarData.Data(1)./AI.BackData.Data(1)).^2; %   <1./(Ncoadd.*Gain) or use AI.VarData.Data(1)./AI.BackData.Data(1)>
                            end
                            %VarFactor   = 1./(20.*Ncoadd.*Gain);
                            if Args.AddExtraBack
                                AI.BackData.Data = imUtil.art.mex.addBrightSourceProfile(AI.BackData.Data, X, Y, FluxNorm, Args.BS_BackMaxR.*ones(size(MaxRadiusF)), BS_RadProf);
                            end
                            if Args.AddExtraVar
                                AI.VarData.Data  = imUtil.art.mex.addBrightSourceProfile(AI.VarData.Data, X, Y, FluxNorm.*VarFactor, MaxRadiusF, BS_RadProf); %./Args.BS_Ncoadd;
                            end
        
                            %toc

                        case 'old'
                            % tic;
                            % hueristic algorithm /problematic and requires
                            % fine tunning
                            LK = imUtil.kernel2.lorentzian(4,[101 101]);
                            CK = imUtil.kernel2.circ(ceil(2.*FWHM(Iobj)),[15 15]);
                            CK = CK./max(CK,[],'all');
                            EdgesVarMap = repmat(single(0), SizeImage);
                            ColData = AI.CatData.getCol({'XPEAK','YPEAK',Args.BS_ColFlux});
                            LinIndex = imUtil.image.sub2ind_fast(SizeImage,ColData(:,2), ColData(:,1));
                            %LinIndex = imUtil.image.mex.sub2ind_mex(SizeImage, ColData(:,2), ColData(:,1));
                            %LinIndex = sub2ind(SizeImage, AI.CatData.Table.YPEAK, AI.CatData.Table.XPEAK);
                            %EdgesVarMap(LinIndex) = AI.CatData.Table.FLUX_APER_3;

                            MinFluxFlag = ColData(:,3)>1e5;
                            EdgesVarMap(LinIndex) = ColData(:,3).*Args.ScatteredLightFrac.*max(1, log10(ColData(:,3)./1e5)).*MinFluxFlag;
                            %AI.Back(AI.Image>5000) = 5000;
                            ConvBright = conv2(EdgesVarMap, LK, 'same');
                            ConvCore   = conv2(EdgesVarMap, CK, 'same')./Args.ScatteredLightFrac;
                            AI.VarData.Image  = AI.VarData.Image  + ConvBright./(Ncoadd.*Gain) + ConvCore;
                            AI.BackData.Image = AI.BackData.Image + ConvBright + ConvCore;
                            % toc
                        case 'none'
                            % do nothing
                        
                        otherwise
                            error('Uknown BrightStarsAlgo option');
                    end

                end % if Iiter==1 && Args.AddBackNoise
                
    
                % write region files with extracted objects 
                if Args.WriteDs9Regions
                    writeDS9region(AI, Args);
                end 
    
    
                % Yes - this is needed
                % Should be replaced with a cleaner way...
                AI.CatData = []; 
    
            end % for Iiter=1:1:Niter    
        end % if isempty(AI.PSFData.DataPSF)
        
        if ~isempty(Args.RedoUpIter)
            if Niter<=Args.RedoUpIter
                error('Niter is <= RedoUpIter - does not make sense');
            end
            % yet another iteration for the bright sources only
            SubImageFaint = Result(Iobj).ImageData.Image - sum(SourceImage(:,:,(Args.RedoUpIter+1):end),3) - Result(Iobj).BackData.Image;
            
            % merge the Catalogs
            CatBright = Cat(1:Args.RedoUpIter).merge;
            CatFaint  = Cat((Args.RedoUpIter+1):end).merge;
            % do aper phot only on bright stars
            BrightXY = CatBright.getXY;

            % See alos:
            % R=imProc.sources.aperPhot(AI);

            % perform only aperture photometry on brightest sources
            [Cube] = imUtil.cut.image2cutouts(SubImageFaint, BrightXY(:,1), BrightXY(:,2), Args.RadiusPSF, 'mexCutout',Args.mexCutout, 'Circle',false);
            
            %!!! need to replace this with a new version of AperPhot:
            ResAperBright = imUtil.sources.aperPhotCube(Cube, 'AperRad',Args.AperRadius, 'AnnulusRad',Args.Annulus);

            PsfHalfSize = (size(Result(Iobj).PSFData.Data,1)-1)./2;
            [Cube] = imUtil.cut.image2cutouts(SubImageFaint, BrightXY(:,1), BrightXY(:,2), PsfHalfSize, 'mexCutout',Args.mexCutout, 'Circle',false);
            ResPsfBright  = imUtil.sources.psfPhotCube(Cube, 'FitRadius',Args.FitRadius(1),...
                                                               'PSF',Result(Iobj).PSFData.Data,...
                                                               'MaxIter',Args.MaxIter,...
                                                               'ZP',Args.ZP,...
                                                               Args.psfFitPhotArgs{:});


            % insert aper phot data to catalog of Cat(1:Args.RedoUpIter)
             
            Naper      = numel(Args.AperRadius);
            ColFlux    = tools.cell.cellNumericSuffix(Args.ColFlux, (1:Naper));
            ColFluxErr = tools.cell.cellNumericSuffix(Args.ColFluxErr, (1:Naper));
            ColMag     = tools.cell.cellNumericSuffix(Args.ColMag, (1:Naper));
            ColMagErr  = tools.cell.cellNumericSuffix(Args.ColMagErr, (1:Naper));

            % order of elements in ColsToAdd and FluxMagData must be
            % consistent!
            ColsToAdd  = [ColFlux, ColFluxErr, ColMag, ColMagErr, Args.ColPsfFlux, Args.ColPsfMag, Args.ColPsfMagErr, Args.ColPsfSN, Args.ColPsfChi2];
            %[C1{1:Naper.*2}] = deal('');
            %[C2{1:Naper.*2}] = deal('mag');
            %ColUnits         = [C1, C2];

            FluxMagData = [ResAperBright.AperPhot,...
                           ResAperBright.AperPhotErr,...
                           convert.luptitude(ResAperBright.AperPhot, 10.^(0.4.*Args.ZP)),...
                           1.086.*ResAperBright.AperPhotErr./ResAperBright.AperPhot,...
                           ResPsfBright.Flux,...
                           convert.luptitude(ResPsfBright.Flux, 10.^(0.4.*Args.ZP)),...
                           1.086./ResPsfBright.SNm,...
                           ResPsfBright.SNm,...
                           ResPsfBright.Chi2./ResPsfBright.Dof];


            % update also Chi2?
             
            CatBright.replaceCol(FluxMagData, ColsToAdd);
            
            Result(Iobj).CatData.Catalog = [CatBright.Catalog; CatFaint.Catalog];
            Result(Iobj).CatData.ColNames = CatBright.ColNames;
        else
            % merge the catalogs of objects extracted at all the iterations
            Result(Iobj).CatData = merge(Cat);
        end % if ~isempty(Args.RedoUpIter)




        

        
        

        % remove sources on edge
        % mask CR
        % remove CR

        % remove bad sources
            % % works only for Gaussian PSF
            % if Args.FlagCR && ~isemptyImage(Obj(Iobj), 'Mask')
            %     Result(Iobj) = imProc.mask.maskCR(Result(Iobj), Args.maskCR_Args{:});
            % end
            % if Args.FlagDiffXY
            %     Result(Iobj) = imProc.mask.xpeak_x1_diff(Result(Iobj), Args.maskDiffXY_Args{:});
            % end
            % 
            % if Args.RemoveBadSources
            %     [Result(Iobj)] = imProc.sources.cleanSources(Result(Iobj), 'SigmaPSF',Args.PsfFunPar{1}(1:2),...
            %                                                                'ColNamsSN',{'SN_1','SN_2'},...
            %                                                                'RemoveBadSources',Args.RemoveBadSources,...
            %                                                                'CreateNewObj',false);
            % end
               

        % The following block should be done only if there is a PSF and
        % star was found
        if ~isempty(Result(Iobj).CatData.ColNames)
            % Clean sources with low S/N
            if ~isempty(Args.CleanSN)
                SNclean = Result(Iobj).CatData.getCol(Args.KeyCleanSN);
                FlagSN = SNclean>Args.CleanSN;
                Result(Iobj).CatData.Catalog = Result(Iobj).CatData.Catalog(FlagSN,:);
            end

            % add RA, Dec from the object's WCS if it is present
            if Args.AddSkyCoo && ~isempty(Result(Iobj).WCS) && Result(Iobj).WCS.Success
                XY        = Result(Iobj).CatData.getXY();
                [RA, Dec] = Result(Iobj).WCS.xy2sky(XY(:,1), XY(:,2));
                Result(Iobj).CatData = insertCol(Result(Iobj).CatData, RA, Inf, Args.ColRA, {''});
                Result(Iobj).CatData = insertCol(Result(Iobj).CatData, Dec, Inf, Args.ColDec, {''});
                Result(Iobj).CatData.sortrows(Args.ColDec);    
            end        
    
            % add header keywords
            % This is done by imProc.header.writeStat2Header
            % if ~isempty(Args.AddSrcStat2Header)
            %     Nsrc = Result(Iobj).CatData.sizeCatalog;
            %     Result(Iobj).HeaderData.insertKey({Args.KeyNsrc, Nsrc, ''});
            % 
            %     % median CHI2_DOF
            %     Chi2Dof = Result(Iobj).CatData.getCol('PSF_CHI2DOF');
            % 
            %     MedChi2Dof = median(Chi2Dof,'all','omitnan');
            %     Result(Iobj).HeaderData.insertKey({Args.KeyMedChi2Dof, MedChi2Dof, ''});
            % end
    
            % save a copy of the AI object with the image replaced by the final subtracted image
            if ExtraOutput
                SourceLess(Iobj)       = Result(Iobj).copy;
                SourceLess(Iobj).Image = SubtractedImage(:,:,Niter); % or just  = Subtracted ?
            end        


            
        else
            % no catalog - skip
        end % if ~isempty(Result(Obj).CatData.ColNames)

        if Args.Verbose
            fprintf('Total %d objects extracted \n',height(Result(Iobj).CatData.Catalog));
        end
    end  % for Iobj=1:1:Nobj
    % Find diffraction spikes?    
    % Cleaning?     
end

%------------------------------------------------

function writeDS9region(AI, Args)
    % help function for writing ds9 region file

    RegName = sprintf('~/%s_it%d.reg',AI.getStructKey('OBJECT').OBJECT,Iiter);
    if     Iiter == 1
        Clr = 'blue';
    elseif Iiter == 2
        Clr = 'red';
    elseif Iiter == 3
        Clr = 'green';
    end
    DS9_new.regionWrite([AI.CatData.getCol('X') AI.CatData.getCol('Y')],...
                        'FileName',RegName,'Color',Clr,'Marker','o','Size',1,'Width',4,'Precision','%.2f','PrintIndividualProp',0);

end
