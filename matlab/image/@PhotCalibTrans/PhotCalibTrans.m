classdef PhotCalibTrans < Component
    % PhotCalibTrans - Transmission-based absolute photometric calibration
    % Description: Performs photometric calibration using atmospheric and instrumental
    %              transmission models. Fits multi-component transmission functions to
    %              calibrator stars with known spectra (default: Gaia DR3 XP). 
    %              Supports position-dependent field corrections via Tran2D polynomials.
    % Author : D. Kovaleva (Dec 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50 (transmission-based calibration)
    %
    % Constant Properties (Hidden):
    %   TransWvl  - Transmission wavelength grid [Angstrom] (3000:20:11000, 20 Angstrom step, 401 points)
    %
    % Inherited Properties (Hidden, from Component):
    %   Logger    - MsgLogger object for status logging via msgLog(LogLevel, message, ...)
    %               Supports LogLevel: Error, Warning, Info, Verbose, Debug
    %
    % Properties:
    %   TransModel - CompositeFun object with fitted transmission model
    %   SpecData   - Structure with reference spectral data (calibrator spectra)
    %   SourceData - AstroCatalog with observed calibrator sources (after calibration: Used, Residuals columns)
    %   CalFound   - Flag indicating whether calibrators were found (set by selectCalibrators)
    %   (No explicit Success flag — non-empty TransModel implies calibration succeeded.)
    %   DeltaZP_CB - Constant-band delta ZP [mag] (set by applyConstBand, written to header as PT_DZP)
    %   LimMag     - Limiting magnitude at SN=LimMagSN [mag] (set by evaluateLimMag, header keyword LIMMAG)
    %   BackMag    - Sky background surface brightness [mag/arcsec^2] (set by evaluateBackMag, header keyword BACKMAG)
    %   MagColPrefix - Prefix for calibrated MAG column names (default 'MAG_AB_';
    %                set 'MAG_' to overwrite instrumental MAG_<suffix> in place)
    %   AirMass, ExpTime, NCoadd, NFramesPerCoadd, Temp, Pressure, Humidity, Aperture
    %                - Observation metadata. NFramesPerCoadd>1 only for
    %                  coadd-of-coadds (e.g. output of pipeline.last.coadd.coaddVisits).
    %
    % Dependent Properties:
    %   ExpTime_eff - Effective per-frame exposure time [s] =
    %                 ExpTime / (NCoadd * NFramesPerCoadd). Recomputed on
    %                 every access; never stored. Used as the time-base for
    %                 every method that applies ZP (calibrate, addMag,
    %                 evaluateMag, evaluateBackMag, applyPhotCalibShifts).
    %
    % Example:
    %{
     % Create calibration object and perform calibration on AstroImage
     PC = PhotCalibTrans();
     PC = PC.calibrate(AI);  % metadata read from AI.HeaderData

     % Check calibration success — non-empty TransModel means fit completed
     if ~isempty(PC.TransModel)
         fprintf('Calibration successful! RMS = %.4f mag\n', PC.TransModel.RMS);
     end

     % Evaluate transmission and zero point
     Trans = PC.evaluateTransmission();
     ZP = PC.evaluateZP();

     % Add calibrated magnitudes to catalog
     Cat = PC.addMag(Cat);

     % Legacy LIMMAG/BACKMAG (set Obj.LimMag, Obj.BackMag; written to header)
     PC = PC.evaluateLimMag(Cat);
     PC = PC.evaluateBackMag(AI);    % needs AI.Back populated (or AI.Image fallback) and AI.WCS

     % Write results to header (PT_*, APCOR*, LIMMAG, BACKMAG)
     PC.photCalibTransToHeader(AI.HeaderData);

     % Propagate coadd calibration to per-epoch images (incl. BACKMAG/LIMMAG)
     %   [Nepoch x Ncrop] EpochAIs, [1 x Ncrop] PC array, DeltaZP from zp_meddiff
     [EpochAIs, NormPerEpoch, DeltaZP] = PC.applyPhotCalibShifts(EpochAIs, ...
         'DeltaZP',          DZPmat, ...
         'PropagateBackMag', true, ...   % requires PC.BackMag finite per crop
         'PropagateLimMag',  true);      % requires PC.LimMag  finite per crop

     % Diagnostic plots
     PC.plotTransmission();
     PC.plotCalibrators();
     PC.plotResiduals();
    %}
    %
    % Methods:
    %   Constructor:
    %     PhotCalibTrans - Constructor for PhotCalibTrans class
    %   Core Calibration Methods:
    %     calibrate - Perform transmission-based photometric calibration
    %     selectCalibrators - Select calibrators with reference spectra
    %   Evaluation Methods:
    %     evaluateTransmission - Evaluate transmission at specific positions
    %     integralTransmission - Mean transmission as fraction of 100% throughput
    %     evaluateZP - Evaluate photometric zero point at specific positions
    %     evaluateMag - Evaluate calibrated magnitudes from observed flux (AB or Vega)
    %     evaluatePredictedFlux - Evaluate model-predicted flux for calibrators
    %   Pre-computation Methods:
    %     propagateCalibratorMagErr - Propagate calibrator spectral and flux errors into per-star magnitude uncertainties
    %     resampleCalibratorSpectra - Resample calibrator reference spectra onto the transmission model wavelength grid
    %   Header I/O Methods:
    %     photCalibTransToHeader - Write calibration results to AstroHeader
    %     photCalibTransFromHeader - Read calibration data from AstroHeader
    %   Catalog Operations:
    %     calcAperCorr - Calculate aperture corrections vs reference flux/mag column.
    %                    Stores AperCorr, AperCorrColNames, AperCorrNStars on object.
    %                    Results written to header by photCalibTransToHeader (APCOR_A1/A2/A3/PS/N).
    %                    Applied to MAG_AB_* columns by orchestrator (fitPhotCalibTrans).
    %                    On failure: AperCorr set to NaN, warning via msgLog.
    %     addMag - Add calibrated magnitude columns to catalog (AB or Vega)
    %     evaluateLimMag - Empirical limiting magnitude from polyfit of MAG_AB_* vs log10(SN)
    %                      in window [MinSN, MaxSN], evaluated at SN=LimMagSN. Stores Obj.LimMag.
    %                      NaN on failure (column missing, <3 points, fit error).
    %     evaluateBackMag - Sky surface brightness in mag/arcsec^2 via
    %                       ZP - 2.5*log10(MedBack/ExpTime_eff) + 5*log10(PixScale),
    %                       with ZP=evaluateZP() (scalar, field centre),
    %                       MedBack=fast_median(AI.Back(:)) (or AI.Image fallback
    %                       if Back empty) and PixScale from AI.WCS.getScale.
    %                       Stores Obj.BackMag. NaN on failure (logged via msgLog).
    %     applyConstBand - Apply constant-band correction to AB magnitudes.
    %                      Replaces fitted atmospheric params with global ConstBandParams,
    %                      computes scalar ΔZP per crop. Called by addMag when
    %                      ApplyConstBand=true, or standalone. Stores ΔZP in
    %                      Obj.DeltaZP_CB, written to header as PT_DZP.
    %     addZP - Add position-dependent ZP column to catalog
    %   Static Methods:
    %     buildConstBandParams - Build ConstBandParams struct from fitted
    %                      PhotCalibTrans objects. Extracts fitted atmospheric params
    %                      (excluding Norm, ZenithAngle, Temperature).
    %                      Source='aggregate' (default): robust median/mean across objects.
    %                      Source='single': extract from one object directly.
    %                      Usage: CBP = PCArray.buildConstBandParams();
    %                             CBP = PC.buildConstBandParams('Source', 'single');
    %     applyPhotCalibShifts - Apply coadd calibration to individual epoch
    %                      images using pre-computed DeltaZP (from zp_meddiff)
    %                      or MatchedSources. Evaluates ZP once per crop,
    %                      shifts per epoch. Accepts [Nepoch × Ncrop] layout.
    %                      Optionally propagates BACKMAG and LIMMAG to each
    %                      epoch header (PropagateBackMag / PropagateLimMag);
    %                      both require the corresponding coadd-level value
    %                      to have been Evaluate'd at fit time. Per-crop
    %                      graceful degradation on NaN coadd value (Warning).
    %                      Usage: [AIs, Norms, DZP] = PC.applyPhotCalibShifts(AIs, 'DeltaZP', DZP);
    %                             [AIs, Norms, DZP] = PC.applyPhotCalibShifts(AIs, 'MS', MScell);
    %                             PC.applyPhotCalibShifts(AIs, 'DeltaZP', DZP, ...
    %                                 'PropagateBackMag', true, 'PropagateLimMag', true);
    %   Display/Output Methods:
    %     summary - Display photometric calibration summary
    %   Plotting Methods:
    %     plotTransmission - Plot transmission curves (overlay or subplots, with integral T)
    %     plotResiduals - Plot calibration residuals (magnitude and spatial)
    %     plotZPMap - Plot 2D map of position-dependent zero point corrections
    %     plotCalibrators - Plot observed vs predicted magnitudes for calibrators
    %     plotFitQuality - Plot RMS/Chi2 evolution across optimization stages

    properties

        % Transmission model (empty until calibration)
        TransModel = []         % CompositeFun transmission model object containing:
                                %   Before calibration: .Funs (function list with initial parameters), .FunOperator ('*'),
                                %                        .Tran2DObj (position-dependent correction object), .UseTran2D (true/false)
                                %   After calibration:  .Funs.Par (fitted parameters), .RMS (fit RMS [mag]), .Chi2 (chi-squared), .DOF (degrees of freedom)

        % Calibration metadata (read from header, defaults for missing values)
        AirMass = 1.2           % Airmass
        Temp = 15              % Temperature [C]
        Pressure = 965          % Atmospheric pressure [mbar] (default: typical at observatory altitude)
        Humidity = NaN          % Relative humidity [%]
        Co2_ppm = 395           % CO2 abundance [ppm] (default matches SMARTS/Simone reference)
        Aperture = pi * (0.1397)^2  % Telescope aperture area [m^2] (default: LAST telescope)
        ExpTime = 1             % Exposure time [s]
        NCoadd = 1              % Number of coadded images (default: single image)
        NFramesPerCoadd = 1     % Number of proc frames per individual input coadd.
                                % Stays 1 for ordinary single-level coadds. Set to
                                % e.g. 20 when calibrating an image built by
                                % stacking already-coadded frames (a coadd-of-coadds,
                                % such as the output of pipeline.last.coadd.coaddVisits):
                                % EXPTIME is the sum of EXPTIMEs of NCOADD input
                                % coadds, and each of those is itself a stack of
                                % NFramesPerCoadd proc frames. Used to scale flux
                                % to a per-frame rate:
                                %    ExpTime_eff = ExpTime / (NCoadd * NFramesPerCoadd)

        % Calibrator information (empty until calibration)
        SpecData = []           % Structure with reference spectral data from selectCalibrators:
                                %   .CalData - struct with .RA, .Dec (catalog positions)
                                %   .SpecWvl [N_wvl x 1] - Wavelength grid [Angstrom] (e.g., 3360:20:10200 for Gaia DR3 XP)
                                %   .Spec [N_calib x N_wvl] - Calibrator spectra flux (Gaia DR3 XP)
                                %   .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
                                %   .SpecFluxMatrix [N_TransWvl x N_calib] - Pre-computed interpolated spectra
                                %        (set by calibrate, computed by resampleCalibratorSpectra)

        SourceData = []         % AstroCatalog with observed calibrator sources from selectCalibrators:
                                %   Catalog table columns: Flux, FluxErr, X, Y, RA, Dec, MatchDistance, NumMatches
                                %   When PerSourceAirmass=true: + AIRMASS
                                %   When AttachBP_RP=true (calibrate default): + BP_RP, MAG_BP, MAG_RP
                                %       (from extra catsHTM match against AuditCatName; NaN where unmatched)
                                %   After calibration: + Used, Residuals, MAG_<System>, PredictedFlux, MagErr

        CalFound = false        % Flag indicating whether calibrators were found (set by selectCalibrators)
        NoRADec = false         % Flag indicating RA/Dec columns missing (set by selectCalibrators)

        % Per-source airmass
        AirmassColName = 'AIRMASS'          % Column name for per-source airmass in catalog
        PerSourceAirmass logical = false    % Whether per-source airmass was actually used

        % Calibrated-magnitude column naming. Prefix applied as
        % FLUX_<suffix> -> <MagColPrefix><suffix> by addMag, applyPhotCalibShifts,
        % calcAperCorr, evaluateLimMag and applyConstBand. Set 'MAG_' to drop the
        % _AB token and overwrite the instrumental MAG_<suffix> columns in place.
        % fitPhotCalibTrans stamps this from its MagColPrefix argument.
        MagColPrefix = 'MAG_AB_'            % Prefix for calibrated MAG column names

        % Reference spectrum slope for target-mag conversion. The transmission
        % is integrated against F_nu(lambda) = (lambda / RefSpecPivot)^RefSpecSlope
        % when evaluateZP / evaluateMag compute calibrated mags. Default is the
        % AB-flat reference (slope = 0); negative values lean blue, positive red.
        % Only the *target-mag conversion* is affected — the calibration fit
        % itself uses the calibrators' true Gaia DR3 spectra (SpecData).
        % fitPhotCalibTrans stamps both from its RefSpecSlope / RefSpecPivot args.
        RefSpecSlope = 0                    % Slope alpha for F_nu reference spectrum
        RefSpecPivot = 5500                 % Pivot wavelength [Angstrom]

        % Aperture corrections
        AperCorr = []           % [1 x N_aper] aperture corrections in mag; NaN if calculation failed
        AperCorrColNames = {}   % Cell array of column names where AperCorr applies
                                %   ('mag' mode: MAG_<prefix>_*; 'flux' mode: FLUX_*)
        AperCorrNStars = 0      % Number of stars used for aperture correction calculation

        % Constant band
        DeltaZP_CB = NaN        % Constant-band delta ZP [mag] (set by applyConstBand)

        % Bright-star RMS
        ARMS = NaN              % sqrt(median(R²)) of N brightest calibrators [mag] (set by calibrate)

        % Limiting magnitude and sky surface brightness (legacy compat keywords)
        LimMag  = NaN           % Limiting magnitude at SN=LimMagSN [mag] (set by evaluateLimMag)
        BackMag = NaN           % Sky background surface brightness [mag/arcsec^2] (set by evaluateBackMag)

        % (Success flag removed — callers check ~isempty(TransModel) instead.)

        % Fit results by stage (stored after calibration for diagnostics)
        FitResults = []         % Struct array from CompositeFun.fitPar() with per-stage results:
                                %   Single-stage: FitResults.Cost, .RMS, .Residuals, .NumObs, .NumClipped,
                                %                 .KeepMask, .ConvergedSigmaClip, .Chi2, .DOF
                                %   Multi-stage:  FitResults(i).StageName, .Method, .Cost, .RMS, .Residuals,
                                %                 .NumObs, .NumClipped, .KeepMask, .IsFieldCorrection, .Chi2, .DOF

    end

    properties
        % Wavelength grid for transmission evaluation (20 Angstrom step)
        TransWvl = (3000:20:11000)'   % Transmission wavelength grid [Angstrom] for model evaluation (401 points)

        % Per-inner-sigma-clip-iteration calibrator snapshots, opt-in via
        % calibrate's CollectCalibTrajectory arg. Struct array; each entry
        % carries StageIndex, StageName, IterIndex, OuterIter, NumClipped
        % (this iter), NumRemaining, RMS, and SourceData — an AstroCatalog
        % holding ALL original calibrator rows with a Used column tracking
        % survivors at that iter and Residuals/PredictedFlux/MagErr
        % populated where Used=true (NaN where Used=false). The snap's
        % SourceData table inherits every column of the live
        % Obj.SourceData.Table at snap time, including BP_RP/MAG_BP/MAG_RP
        % when AttachBP_RP=true. Default empty.
        CalibTrajectory = []
    end

    properties (Dependent)
        % Effective exposure time per single proc frame [s]:
        %    ExpTime_eff = ExpTime / (NCoadd * NFramesPerCoadd)
        % This is the time-base the calibration ZP is referenced to: every
        % method that applies ZP uses MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP.
        % Computed on access from ExpTime, NCoadd and NFramesPerCoadd; never
        % stored, so it cannot go stale.
        ExpTime_eff
    end

    methods % Constructor
        function Obj = PhotCalibTrans(varargin)
            % Constructor for PhotCalibTrans class
            % Input  : 
            %            * ...,key,val,...
            %            Metadata describing conditions of observations:
            %            'AirMass' - Airmass. Default is 1.2.
            %            'Temp' - Temperature [C]. Default is 15.
            %            'Pressure' - Atmospheric pressure [mbar]. Default is 965.
            %            'Humidity' - Relative humidity [%]. Default is NaN.
            %            'ExpTime' - Exposure time [s]. Default is 1.
            %            'NCoadd' - Number of coadded images. Default is 1.
            %            Instrument Configuration:
            %            'Aperture' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2 (LAST telescope).
            %            Calibration Data (set by calibrate() method):
            %            'TransModel' - CompositeFun transmission model object. Default is [].
            %            'CalFound' - Flag indicating if calibrators were found. Default is false.
            %
            % Output : - PhotCalibTrans object
            % Author : D. Kovaleva (Dec 2025)
            % Example: % Create with default values
            %          PC = PhotCalibTrans();
            %
            %          % Create with custom pressure and aperture
            %          PC = PhotCalibTrans('Pressure', 970, 'Aperture', 0.05);
            %
            %          % Create with observation metadata
            %          PC = PhotCalibTrans('AirMass', 1.5, 'ExpTime', 20, ...
            %                              'NCoadd', 1, 'Temp', 15, 'Pressure', 965);

            % Dedicated logger: warnings go to log file only, not terminal
            Obj.Logger.SuppressDispLevel = LogLevel.Warning;

            % Parse name-value pairs and set properties if they exist
            for I = 1:2:length(varargin)
                if I+1 <= length(varargin)
                    PropName = varargin{I};
                    if isprop(Obj, PropName)
                        Obj.(PropName) = varargin{I+1};
                    else
                        Obj.msgLog(LogLevel.Warning, ...
                            'Property "%s" does not exist and will be ignored.', PropName);
                    end
                end
            end
        end
    end

    methods % Dependent-property getters
        function v = get.ExpTime_eff(Obj)
            % Effective exposure time per single proc frame [s].
            % Recomputed on every access from ExpTime, NCoadd and NFramesPerCoadd.
            v = Obj.ExpTime / (Obj.NCoadd * Obj.NFramesPerCoadd);
        end
    end

    methods % Core calibration methods
        function Obj = calibrate(Obj, Cat, Args)
            % Perform transmission-based photometric calibration
            % Input  : - PhotCalibTrans object 
            %          - AstroImage or AstroCatalog object with observed sources 
            %                  Metadata source is determined automatically:
            %                    AstroImage: metadata from Cat.HeaderData
            %                    AstroCatalog: metadata from 'Metadata' argument (if provided)
            %          * ...,key,val,...
            %            'Metadata' - Metadata source (for AstroCatalog only). Can be:
            %                         AstroHeader object, cell array {key1, val1, ...}, or [].
            %                         Default is [].
            %            'Lambda'         - Transmission wavelength grid [Angstrom]. Default is (3000:20:11000)'.
            %            'SearchRadius'   - Gaia matching radius [arcsec]. Default is 2.
            %            'MagRange'       - Calibrator magnitude range [min max]. Default is [11.5 16.0].
            %            'FunListName'    - Transmission function list name. Default is 'DefaultLASTFunList'.
            %            'CustomFunList'  - Custom function list. Default is [].
            %            'OptSeqName'     - Optimization sequence name. Default is 'LAST_NormLin'.
            %            'CustomOptSeq'   - Custom optimization sequence. Default is [].
            %            'Tran2DType'     - Position-dependent correction type. Default is 'cheby1_4_xt'.
            %            'UseTran2D'      - Enable position-dependent correction. Default is true.
            %            'XPixel'         - Detector X size in pixels (sets Tran2D
            %                               normalisation, ParNX = [XPixel/2, XPixel/2]).
            %                               Default is o.
            %            'YPixel'         - Detector Y size in pixels. Default is 1716.
            %            'CalibCatName'   - catsHTM catalog with reference spectra
            %                               (forwarded to selectCalibrators).
            %                               Default is 'GAIADR3spec'.
            %            'MinSN'          - Lower S/N gate on calibrator candidates.
            %                               Default is 5.
            %            'MaxSN'          - Upper S/N gate. Default is 1000.
            %            'FilterBadFlags' - Apply the FLAGS bitmask filter.
            %                               Default is true.
            %            'MagColName'     - Mag column used for MagRange filter
            %                               and audit delta-mag. Default 'MAG_APER_3'.
            %            'SpFluxCol'      - Spectral flux column indices in
            %                               CalibCatName as [flux_start, flux_end,
            %                               err_start, err_end]. Default
            %                               [7, 349, 350, 692] for GAIADR3spec.
            %            'BadBitNames'    - Cell of bit-name strings flagged as
            %                               bad. Default {'Saturated','NaN',
            %                               'Negative','CR_DeltaHT','NearEdge'}.
            %            'AuditCalibrators' - Toggle the step-0 calibrator audit in
            %                               selectCalibrators. Default is false.
            %            'AuditCatName'   - Gaia photometric catalog for the audit.
            %                               Default is 'GAIADR3'.
            %            'AuditBPRPExcessFactorMax' - BPRP-excess-factor rejection cap.
            %                               Default is 1.3.
            %            'AuditBPRPMax'   - BP-RP rejection cap. Default is 1.5.
            %            'AuditLASTNearestDist' - LAST nearest-neighbour distance
            %                               rejection threshold [arcsec]. Default is 20.
            %            'AuditLASTDeltaMag' - LAST nearest-neighbour |delta-mag|
            %                               rejection threshold. Default is 2.
            %            'AttachBP_RP'    - Attach Gaia BP_RP, MAG_BP, MAG_RP
            %                               columns to SourceData via one extra
            %                               catsHTM match against AuditCatName
            %                               (default 'GAIADR3'). Independent of
            %                               AuditCalibrators - the match runs on
            %                               the post-filter calibrator pool.
            %                               Failure-safe: NaN-fills the three
            %                               columns on match error and logs a
            %                               Warning. The columns are inherited
            %                               by every CalibTrajectory snapshot's
            %                               SourceData (no extra plumbing).
            %                               Default is true.
            %            'Tran2DPerturbStd' - Std-dev for one-shot N(0,std)
            %                               randn-seeding of Tran2D ParX
            %                               before fitPar. Affects stages 1-3
            %                               only; stage 4 overwrites ParX with
            %                               the linear LS fit. 0 disables.
            %                               Default is 0.
            %            'WeightingMode'  - Weighting mode. Default is 'combined'.
            %            'FluxErrColName' - Flux error column name. Default is 'FluxErr'.
            %            'SigmaClipMethod'- Sigma-clipping method forwarded to
            %                               tools.math.stat.sigmaClip via
            %                               CompositeFun.fitPar / fitMultiStage.
            %                               Default 'median'. Three options:
            %                                 'median'        - astropy iteration
            %                                                   on abs(residuals).
            %                                                   Matches LAST/
            %                                                   Python production
            %                                                   (Simone feeds
            %                                                   np.abs(data-
            %                                                   model) into
            %                                                   astropy.stats.
            %                                                   sigma_clip).
            %                                                   More aggressive
            %                                                   than its nominal
            %                                                   threshold: at
            %                                                   SigmaThresh=3
            %                                                   the effective
            %                                                   single-sided cut
            %                                                   is ~2.48 sigma
            %                                                   on the signed
            %                                                   scale (because
            %                                                   median(|r|) ≈
            %                                                   0.6745 sigma,
            %                                                   std(|r|) ≈
            %                                                   0.6028 sigma).
            %                                 'median_signed' - astropy iteration
            %                                                   on signed
            %                                                   residuals;
            %                                                   canonical
            %                                                   N-sigma clip
            %                                                   where
            %                                                   SigmaThresh
            %                                                   literally means
            %                                                   N sigma on the
            %                                                   signed residual
            %                                                   distribution.
            %                                                   Equivalent to
            %                                                   astropy.stats.
            %                                                   sigma_clip(r,
            %                                                   cenfunc='median',
            %                                                   stdfunc='std',
            %                                                   maxiters=
            %                                                   MaxIter) on the
            %                                                   SIGNED r.
            %                                 'weighted'      - single-shot
            %                                                   test |r_i /
            %                                                   sigma_i| >
            %                                                   SigmaThresh.
            %                               See tools.math.stat.sigmaClip header
            %                               for the math and the StdFunc
            %                               ('mad_std' vs 'std') option.
            %            'FluxErrorNorm'  - Flux error normalization. Default is 0.5.
            %            'AirmassSource'  - How to obtain the calibration airmass:
            %                               'header'  -> read AIRMASS keyword
            %                                            (Obj.AirMass from getStructKey;
            %                                            current default behaviour).
            %                               'compute' -> compute the field-centre
            %                                            airmass via Hardie (1962)
            %                                            from header RA/DEC/time +
            %                                            observer location. Mirrors
            %                                            the Python production
            %                                            LastCatUtils.get_airmass_from_cat
            %                                            path (uses celestial.coo.radec2azalt
            %                                            and celestial.coo.hardie).
            %                               Default is 'header'.
            %            'AirmassTimeKey' - Header key used as observation time when
            %                               AirmassSource='compute':
            %                               'DATE-OBS' (ISO string, parsed via
            %                               datetime -> juliandate) | 'JD' | 'MIDJD'.
            %                               Default is 'DATE-OBS' (matches production).
            %            'ObsLat'         - Observer latitude [deg]. Default 30.053072
            %                               (LAST Neot Semadar, per LastCatUtils).
            %            'ObsLon'         - Observer longitude [deg]. Default 35.040858.
            %            'ObsHeight'      - Observer height [m]. Default 415.4 (kept
            %                               for provenance; celestial.coo.radec2azalt
            %                               does not consume it).
            %            'WriteComputedAirmass' - When true AND AirmassSource='compute'
            %                               produced a finite airmass, overwrite the
            %                               AIRMASS keyword on the source AstroHeader
            %                               (Cat.HeaderData or Args.Metadata). The
            %                               header handle is shared with the caller,
            %                               so downstream FITS writes carry the new
            %                               value. Inert when AirmassSource='header'.
            %                               Default is false.
            %            'NFramesPerCoadd'- Number of proc frames per individual
            %                               input coadd. Stays at 1 for ordinary
            %                               single-level coadds. For a coadd-of-
            %                               coadds (an image built by stacking
            %                               already-coadded frames; e.g. the
            %                               output of pipeline.last.coadd.coaddVisits)
            %                               header EXPTIME sums NCOADD input coadds,
            %                               each itself made of NFramesPerCoadd
            %                               procs, so set to e.g. 20. The downstream
            %                               per-frame effective exposure is
            %                               ExpTime_eff = ExpTime/(NCoadd*NFramesPerCoadd).
            %                               Usually injected by fitPhotCalibTrans's
            %                               IsMeanImages/NProcsPerCoadd args. Default is 1.
            %            'MagSystem' - Magnitude system ('AB' or 'Vega'). Default is 'AB'.
            %            'CollectCalibTrajectory' - Opt-in: record one
            %                               calibrator-list snapshot per inner
            %                               sigma-clip iteration of every stage
            %                               (single-stage fitPar branches plus
            %                               the inline IsNormOnlyLinear /
            %                               IsJointFCStage / IsNonlinFCStage
            %                               handlers in fitMultiStage). When
            %                               true, after the fit completes the
            %                               trajectory is assembled from
            %                               FitResult(:).IterSnapshots into a
            %                               struct array stored on
            %                               Obj.CalibTrajectory. Each entry:
            %                               .StageIndex, .StageName, .IterIndex
            %                               (inner-iter, from 1), .OuterIter
            %                               (outer sigma-clip iter, 1 when
            %                               OuterSigmaClip=false), .NumClipped
            %                               (this iter), .NumRemaining, .RMS,
            %                               and .SourceData — an AstroCatalog
            %                               with ALL original calibrator rows
            %                               preserved. Per-row columns:
            %                                  Used  - true iff currently in
            %                                          the survivor set.
            %                                  Residuals - current residual
            %                                          for survivors; the
            %                                          LAST-KNOWN residual
            %                                          (i.e. residual at the
            %                                          moment of discard) for
            %                                          calibrators discarded
            %                                          earlier in THIS stage;
            %                                          NaN for calibrators
            %                                          discarded in earlier
            %                                          stages (never entered
            %                                          this stage's pool).
            %                                  PredictedFlux / MagErr - same
            %                                          three-way semantics.
            %                               Plus all columns from the live
            %                               Obj.SourceData.Table (which the
            %                               SnapTable is built from): Flux,
            %                               FluxErr, X, Y, RA, Dec,
            %                               MatchDistance, NumMatches, optional
            %                               AIRMASS, and BP_RP/MAG_BP/MAG_RP
            %                               when AttachBP_RP=true.
            %                               Default false (no extra work;
            %                               CalibTrajectory stays empty).
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - PhotCalibTrans object with calibration results.
            %                  SourceData catalog includes: Used, Residuals,
            %                                   MAG_<System>, PredictedFlux, MagErr.
            %                  When AttachBP_RP=true (default), SourceData also
            %                  carries Gaia BP_RP, MAG_BP, MAG_RP columns (NaN
            %                  for sources unmatched in AuditCatName); these are
            %                  also inherited by every CalibTrajectory snapshot.
            %                  When CollectCalibTrajectory=true, also populates
            %                  Obj.CalibTrajectory (struct array, one entry per
            %                  inner sigma-clip iter; see arg description).
            % Author : D. Kovaleva (Jan 2026)
            % Reference: Garrappa et al. 2025, A&A 699, A50.
            % Example: PC = PhotCalibTrans();
            %          PC = PC.calibrate(AI);
            %          % With custom settings:
            %          PC = PC.calibrate(AI, 'UseTran2D', false, 'SearchRadius', 3);
            %          % Capture per-inner-iter calibrator trajectory:
            %          PC = PC.calibrate(AI, 'CollectCalibTrajectory', true);
            %          Snaps = PC.CalibTrajectory;
            %          fprintf('captured %d snapshots\n', numel(Snaps));
            %          % Survivors at final inner iter of stage 4:
            %          Mask = [Snaps.StageIndex] == 4;
            %          last4 = find(Mask, 1, 'last');
            %          Tab = Snaps(last4).SourceData.Table;
            %          Survivors = Tab(Tab.Used, :);
            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog
                
                % Select calibrators via match_catsHTM
                Args.match_catsHTMArgs = {};

                % Metadata argument (for AstroCatalog only)
                Args.Metadata = []     % AstroHeader object or cell array {key1, val1, key2, val2, ...}

                % Calibration settings (individual NV pairs with defaults)
                Args.Lambda           = (3000:20:11000)'
                Args.SearchRadius     = 2
                Args.MagRange         = [11.5 16.0]
                Args.FilterNegFlux logical = true    % LAST default (matches predefCalibArgs)
                Args.MinSN2           = 10           % LAST default (matches predefCalibArgs)
                Args.FunListName      = 'DefaultLASTFunList'
                Args.CustomFunList    = []
                Args.OptSeqName       = 'LAST_NormLin'
                Args.CustomOptSeq     = []
                Args.Tran2DType       = 'cheby1_4_xt'
                Args.UseTran2D logical = true
                Args.XPixel           = 1716   % Detector X size [pix]; Tran2D centre = XPixel/2
                Args.YPixel           = 1716   % Detector Y size [pix]; Tran2D centre = YPixel/2
                % Initial PWV/AOD/QE-Center from AIRMASS-conditioned
                % polynomial medians (astro.transmission.atmParFromAirmass)
                % instead of the flat predefSeqCompositeFun class defaults.
                % Only affects the INITIAL guess into lsqnonlin - the fit
                % still updates each parameter freely per its FitPar flag.
                % OPT-IN: default false. Pass true to enable.
                Args.InitFromAirmass logical = false
                % Per-source FIXED atmospheric parameters, computed from
                % each calibrator's own AIRMASS via the polynomial fit
                % in astro.transmission.atmParFromAirmass. Passes vectors
                % PerSourcePWV_cm, PerSourceTauAod500, PerSourceCenter_Ang
                % into costFun; those override individual columns of the
                % per-source parameter matrix while still holding scalar
                % transmission parameters fixed globally. Requires
                % PerSourceAirmass=true so per-source ZA (and hence AM)
                % is available. OPT-IN: default false.
                Args.PerSourceAtmFromAirmass logical = false
                Args.Tran2DPerturbStd = 0      % Std-dev for randn-seed of Tran2D ParX (one shot before stage 1); 0 disables
                Args.Tran2DRngSeed (1,1) double = 6   % rng seed mirrored from Simone's np.random.seed(6); used when OptSeq stage method is NONLIN_FC
                % (Co2_ppm retired from calibrate June 2026 — lives on
                %  imUtil.calib.predefSeqCompositeFun as an atmospheric-
                %  constant arg (default 395). Overriders build a FunList
                %  via predefSeqCompositeFun('Co2_ppm', X) and pass through
                %  CustomFunList.)
                % Opt-in: record one snapshot per inner sigma-clip iteration
                % across every stage. Snapshots are assembled from the
                % fit's per-stage IterSnapshots (length NCalibTotal masks
                % plus current Residuals/PredictedFlux/MagErr on the
                % survivors) into a full-row SourceData AstroCatalog per
                % snap, and the struct array is stored on
                % Obj.CalibTrajectory after the fit completes. Default
                % false (no extra work, no memory cost).
                Args.CollectCalibTrajectory logical = false
                % Calibrator selection knobs forwarded to selectCalibrators
                Args.CalibCatName     = 'GAIADR3spec'
                Args.MinSN            = 5                  % Lower S/N gate on calibrator candidates
                Args.MaxSN            = 1000               % Upper S/N gate
                Args.FilterBadFlags logical = true         % Apply FLAGS bitmask filter
                Args.MagColName       = 'MAG_APER_3'       % Mag column for MagRange + audit deltaMag
                Args.SpFluxCol        = [7, 349, 350, 692] % [flux_start, flux_end, err_start, err_end]
                Args.BadBitNames      = {'Saturated', 'NaN', 'Negative', 'CR_DeltaHT', 'NearEdge'}
                Args.AuditCalibrators logical = false
                Args.AuditCatName     = 'GAIADR3'
                Args.AuditBPRPExcessFactorMax = 1.3
                Args.AuditBPRPMax     = 1.5
                Args.AuditLASTNearestDist = 20      % arcsec
                Args.AuditLASTDeltaMag = 2          % mag
                % Attach Gaia BP_RP, MAG_BP, MAG_RP to SourceData (and to
                % every CalibTrajectory snapshot's SourceData, which inherits
                % the base table). One extra catsHTM match against
                % Args.AuditCatName. Default true; pass false to skip.
                Args.AttachBP_RP logical = true
                Args.WeightingMode    = 'combined'
                Args.FluxErrColName   = 'FluxErr'
                Args.SigmaClipMethod  = 'median'    % 'median' | 'median_signed' | 'weighted' — see header doc + tools.math.stat.sigmaClip
                % Outer clip-and-refit loop (passed through to fitPar/fitMultiStage).
                % When OuterSigmaClip=true, full stage loop is run repeatedly,
                % applying a single sigma clip on the final residuals between
                % runs (StdFunc='mad_std' robust by default).
                Args.OuterSigmaClip logical = false
                Args.OuterSigmaThresh = 3.0
                Args.OuterStdFunc     = 'mad_std'   % 'mad_std' (robust) | 'std'
                Args.OuterMaxIter     = 5
                Args.OuterMinNewClipped = 1
                % Per-outer-iter weighting toggle (forwarded to fitPar).
                % Empty (default) leaves every iter weighted. When non-empty,
                % must be a logical vector of length OuterMaxIter.
                Args.WeightedOuterIters = []
                Args.FluxErrorNorm    = 0.5
                % Forward to CompositeFun.fitPar -> lsqnonlin TypicalX.
                % Scales finite-diff steps + stopping tolerances by each
                % free parameter's natural magnitude. Default false
                % preserves current optimizer behaviour.
                Args.UseTypicalX logical = false
                Args.AirmassColName   = 'AIRMASS'
                Args.PerSourceAirmass logical = false

                % --- Norm convention (post-fit gauge fix) ---
                % Selects the reported meaning of Norm and the Tran2D DC
                % offset kx0. The (Norm, kx0) pair is a pure gauge freedom:
                % any bijective reparameterisation preserves every model
                % prediction. Options:
                %   'raw'    - report the fit's raw values (default; every
                %              historical run uses this).
                %   'center' - after the fit completes, rotate the pair so
                %              that Tran2D(field-centre) = 0 and Norm
                %              carries the full field-centre ZP. Applied
                %              via absorbTran2DCenterIntoNorm(). Predictions
                %              are bit-identical; only the (Norm, ParX(1))
                %              split changes.
                Args.NormConvention (1,:) char {mustBeMember(Args.NormConvention, {'raw','center'})} = 'raw'

                % Systematic-error floor applied element-wise to the
                % combined MagErr used as fit weight. See
                % propagateCalibratorMagErr's docstring. Default 0.001 mag.
                Args.SystematicErr (1,1) double {mustBeNonnegative} = 0.001

                % Number of proc frames per individual input coadd. Stays at 1
                % for ordinary single-level coadds (where EXPTIME / NCOADD already
                % gives the per-frame exposure). Set to e.g. 20 when calibrating
                % a coadd-of-coadds (an image built by stacking already-coadded
                % frames): EXPTIME sums NCOADD input coadds, each itself a stack
                % of NFramesPerCoadd procs, so
                %    ExpTime_eff = ExpTime / (NCOADD * NFramesPerCoadd).
                % Usually injected by fitPhotCalibTrans's IsMeanImages /
                % NProcsPerCoadd args.
                Args.NFramesPerCoadd (1,1) double {mustBePositive, mustBeInteger} = 1

                % --- Airmass override: compute from field-centre (RA, Dec, time)
                % via Hardie (1962), mirroring the Python AbsoluteCalibration /
                % LastCatUtils.get_airmass_from_cat path. When AirmassSource is
                % 'compute', the value read from the header AIRMASS key is
                % discarded and Obj.AirMass is set to the Hardie airmass.
                % Defaults match the production LAST observatory at Neot Semadar.
                Args.AirmassSource   (1,:) char = 'header'   % 'header' | 'compute'
                Args.AirmassTimeKey  (1,:) char = 'DATE-OBS' % 'DATE-OBS' | 'JD' | 'MIDJD'
                Args.ObsLat          (1,1) double = 30.053072   % deg
                Args.ObsLon          (1,1) double = 35.040858   % deg
                Args.ObsHeight       (1,1) double = 415.4       % m (ignored by AstroPack airmass calc)
                % Apply the ICRS->apparent-of-date transformation to header RA/Dec
                % before computing Hardie airmass. Precession (mean equinox of date)
                % is run by celestial.coo.radec2azalt via its InEquinoxJD/OutEquinoxJD
                % args. Matches the pipeline imProc.header.addAirMass precession path.
                % Set false to recover the legacy pure-spherical-trig behaviour.
                % Only consulted when AirmassSource='compute'.
                Args.ApplyApparentPlace logical = true
                % Apply annual aberration (Ron & Vondrak 1986) before
                % radec2azalt's precession. ~20 arcsec max; closes ~0.3 milli-
                % airmass gap vs Python at sec(z)~1.7 on top of precession.
                % Only consulted when ApplyApparentPlace=true.
                Args.ApplyAberration logical = true
                % Include nutation. When true (and ApplyApparentPlace=true),
                % the precession step is performed by celestial.convert.precessCoo
                % with OutMean=false (mean->TRUE equinox of date, i.e.
                % precession+nutation). When false, radec2azalt's
                % InEquinoxJD/OutEquinoxJD path is used (mean->mean equinox,
                % precession only, matches the upstream imProc.header.addAirMass
                % path). Nutation adds ~9 arcsec maximum (a fraction of a
                % milli-airmass at sec(z)~2). Only consulted when
                % ApplyApparentPlace=true.
                Args.ApplyNutation logical = false
                % When true AND AirmassSource='compute' actually yielded a finite
                % Hardie airmass, overwrite the AIRMASS keyword on the source
                % AstroHeader (Cat.HeaderData for AstroImage input, or the
                % Metadata AstroHeader otherwise). Lets downstream FITS writes
                % carry the Hardie value instead of the original LAST AIRMASS.
                Args.WriteComputedAirmass logical = false

                % Aperture correction
                Args.CalcAperCorr logical = true
                Args.AperCorrMethod   = 'median'    % 'median' or 'weighted'
                Args.AperCorrSNColName = 'SN'       % S/N column for filtering
                Args.AperCorrMinSN    = 30           % Minimum S/N for aperture correction stars

                Args.MagSystem char   = 'AB'
                % ARMS sample-size selection.
                %   ARMSMode = 'percent' (default) - use ARMS_Percent% of
                %              the brightest survivors. Sample size scales
                %              with the calibrator pool.
                %   ARMSMode = 'count'   - use the fixed N_ARMS brightest
                %              (legacy behaviour; small pools no longer
                %              hit the min(N_ARMS, Nvalid) safety floor
                %              unexpectedly).
                % Common: pool is filtered by Used=true AND finite Flux
                % AND finite Residuals; sorted by Flux descending; the
                % top-K rows contribute to sqrt(median(R^2)).
                Args.ARMSMode        char   {mustBeMember(Args.ARMSMode, {'percent','count'})} = 'percent'
                Args.ARMS_Percent    (1,1) double {mustBeNonnegative, mustBeLessThanOrEqual(Args.ARMS_Percent, 100)} = 20
                Args.N_ARMS          (1,1) double = 20   % legacy count for ARMSMode='count' (0 skips ARMS in either mode)
                Args.Verbose logical  = false

                % --- Alternate calibrator selection (forwarded to selectCalibrators) ---
                Args.SelectionMethod char {mustBeMember(Args.SelectionMethod, ...
                    {'catsHTM','pythonLike'})} = 'catsHTM'
                Args.UseTAPClassprob logical = false
                % Position columns for the Tran2D fit. Default 'X','Y'
                % (per-crop calibrate behaviour unchanged). Set to
                % 'XFULL','YFULL' when calibrating a joint whole-image
                % AstroCatalog from imProc.cat.joinCropsToCatalog so
                % Tran2D operates in the field frame.
                Args.PosColNameX      char    = 'X'
                Args.PosColNameY      char    = 'Y'
            end

            % Save Metadata argument separately
            Metadata = Args.Metadata;

            % Set wavelength grid
            Obj.TransWvl = Args.Lambda(:);

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:calibrate:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n');
            end

            % ====================================================================
            % STEPS 1-3: Metadata + FunList/OptSeq + TransModel
            % ====================================================================

            Metadata     = Args.Metadata;
            IsAstroImage = isa(Cat, 'AstroImage');

            % ----------------------------------------------------------------
            % STEP 1: Extract metadata
            % ----------------------------------------------------------------
            if Args.Verbose
                fprintf('Step 1: Extracting observation metadata...\n');
            end

            HeaderRef = [];
            if IsAstroImage
                HeaderRef = Cat.HeaderData;
            elseif isa(Metadata, 'AstroHeader')
                HeaderRef = Metadata;
            end

            if iscell(Metadata)
                % AstroCatalog with cell array: use directly
            elseif IsAstroImage || isa(Metadata, 'AstroHeader')
                Keys      = {'MNTTEMP', 'EXPTIME', 'NCOADD', 'AIRMASS', 'PRESSURE'};
                PropNames = {'Temp',    'ExpTime', 'NCoadd', 'AirMass', 'Pressure'};
                if IsAstroImage
                    Res = Cat.HeaderData.getStructKey(Keys);
                else
                    Res = Metadata.getStructKey(Keys);
                end
                Metadata = cell(1, 2*length(Keys));
                Idx = 1;
                for I = 1:length(Keys)
                    if isfield(Res, Keys{I})
                        Val = Res.(Keys{I});
                        if ~isempty(Val) && isnumeric(Val) && ~any(isnan(Val))
                            Metadata{Idx}   = PropNames{I};
                            Metadata{Idx+1} = Val;
                            Idx = Idx + 2;
                        end
                    end
                end
                Metadata = Metadata(1:Idx-1);
            else
                Metadata = {};
            end

            if ~isempty(Metadata)
                Obj.setProps(struct(Metadata{:}));
            end

            Obj.NFramesPerCoadd = Args.NFramesPerCoadd;

            % Optional airmass override: compute from field-centre using
            % Hardie polynomial via celestial.coo.radec2azalt (matches
            % Python's get_hardie_airmass). Downstream zenith conversion
            % (ZenithAngle = acosd(1/AirMass)) mirrors get_zenith_from_airmass.
            if strcmpi(Args.AirmassSource, 'compute')
                if isempty(HeaderRef)
                    Obj.msgLog(LogLevel.Warning, ...
                        'calibrate: AirmassSource=compute but no AstroHeader available - keeping header AIRMASS');
                else
                    RAhdr  = HeaderRef.getVal('RA');
                    Dechdr = HeaderRef.getVal('DEC');
                    JDhdr  = readAirmassTime(HeaderRef, Args.AirmassTimeKey);

                    if isfinite(RAhdr) && isfinite(Dechdr) && isfinite(JDhdr)
                        try
                            RA_in  = RAhdr;
                            Dec_in = Dechdr;
                            if Args.ApplyApparentPlace && Args.ApplyAberration
                                Aber = celestial.coo.aberration( ...
                                    [RAhdr*pi/180, Dechdr*pi/180], JDhdr);
                                RA_in  = Aber(1,1) * 180/pi;
                                Dec_in = Aber(1,2) * 180/pi;
                            end
                            if Args.ApplyApparentPlace && Args.ApplyNutation
                                [RA_app, Dec_app] = celestial.convert.precessCoo( ...
                                    RA_in, Dec_in, ...
                                    'InEquinox',  2451545.5, 'InMean',  true, ...
                                    'OutEquinox', JDhdr,     'OutMean', false, ...
                                    'InUnits',    'deg',     'OutUnits','deg');
                                [~, ~, AM] = celestial.coo.radec2azalt( ...
                                    JDhdr, RA_app, Dec_app, ...
                                    'GeoCoo', [Args.ObsLon, Args.ObsLat]);
                            elseif Args.ApplyApparentPlace
                                [~, ~, AM] = celestial.coo.radec2azalt( ...
                                    JDhdr, RA_in, Dec_in, ...
                                    'GeoCoo',       [Args.ObsLon, Args.ObsLat], ...
                                    'InEquinoxJD',  2451545, ...
                                    'OutEquinoxJD', JDhdr);
                            else
                                [~, ~, AM] = celestial.coo.radec2azalt( ...
                                    JDhdr, RA_in, Dec_in, ...
                                    'GeoCoo', [Args.ObsLon, Args.ObsLat]);
                            end
                        catch ME
                            Obj.msgLog(LogLevel.Warning, sprintf( ...
                                'calibrate: airmass apparent-place chain failed (%s) - falling back to legacy radec2azalt', ...
                                ME.message));
                            [~, ~, AM] = celestial.coo.radec2azalt( ...
                                JDhdr, RAhdr, Dechdr, ...
                                'GeoCoo', [Args.ObsLon, Args.ObsLat]);
                        end
                        Obj.AirMass = AM;
                        if Args.Verbose
                            fprintf('  AirMass overridden (Hardie) = %.4f (ApparentPlace=%d, Aberration=%d, Nutation=%d)\n', ...
                                    AM, Args.ApplyApparentPlace, Args.ApplyAberration, Args.ApplyNutation);
                        end
                        if Args.WriteComputedAirmass
                            HeaderRef.replaceVal('AIRMASS', AM);
                            if Args.Verbose
                                fprintf('  AIRMASS header keyword updated to %.4f (Hardie)\n', AM);
                            end
                        end
                    else
                        Obj.msgLog(LogLevel.Warning, sprintf( ...
                            'calibrate: AirmassSource=compute but RA/DEC/%s missing or NaN - keeping header AIRMASS', ...
                            Args.AirmassTimeKey));
                    end
                end
            end

            if IsAstroImage
                CurrentCat = Cat.CatData;
            else
                CurrentCat = Cat;
            end

            if Args.Verbose
                fprintf('  AirMass  = %.2f\n', Obj.AirMass);
                fprintf('  ExpTime  = %.1f s\n', Obj.ExpTime);
                fprintf('  NCoadd   = %d\n', Obj.NCoadd);
                if Obj.NFramesPerCoadd ~= 1
                    fprintf('  NFramesPerCoadd = %d  ->  ExpTime_eff = %.4f s (coadd-of-coadds mode)\n', ...
                        Obj.NFramesPerCoadd, Obj.ExpTime_eff);
                end
                fprintf('  Temp     = %.1f C\n', Obj.Temp);
                fprintf('  Pressure = %.1f mbar\n', Obj.Pressure);
            end

            % ----------------------------------------------------------------
            % STEP 2: Build TransModel structure with observation metadata
            % ----------------------------------------------------------------
            if Args.Verbose
                fprintf('\nStep 2: Building transmission model structure...\n');
            end

            ZenithAngle = acosd(1 / max(Obj.AirMass, 1.0));

            % Co2_ppm is NOT threaded through anymore — predefSeqCompositeFun
            % owns the atmospheric-constant default (395, matches class
            % Obj.Co2_ppm default so PT_CO2PPM provenance stays consistent).
            % When Args.InitFromAirmass is true, the observation-airmass
            % is threaded down so predefSeqCompositeFun replaces the
            % class-default PWV_cm / TauAod500 / QE_Center_Ang with
            % median LAST polynomial values from astro.transmission.
            % atmParFromAirmass - better initial guesses for lsqnonlin
            % at high airmass.
            if Args.InitFromAirmass && isfinite(Obj.AirMass) && Obj.AirMass >= 1
                InitAM = Obj.AirMass;
            else
                InitAM = NaN;
            end
            [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun( ...
                'ZenithAngle_deg',   ZenithAngle, ...
                'Pressure_mbar',     Obj.Pressure, ...
                'Temperature_C',     Obj.Temp, ...
                'InitFromAirmass',   InitAM);

            FunList = FunCat.(Args.FunListName);
            OptSeq  = StageCat.(Args.OptSeqName);

            if Args.Verbose
                if ~isempty(Args.CustomFunList)
                    fprintf('  Using custom function list (%d functions)\n', length(FunList));
                else
                    fprintf('  Using function list: %s (%d functions)\n', Args.FunListName, length(FunList));
                end
                if ~isempty(Args.CustomOptSeq)
                    fprintf('  Using custom optimization sequence (%d stages)\n', numel(OptSeq));
                else
                    fprintf('  Using optimization sequence: %s (%d stages)\n', Args.OptSeqName, numel(OptSeq));
                end
                fprintf('  ZenithAngle = %.1f deg (from AirMass = %.2f)\n', ZenithAngle, Obj.AirMass);
                fprintf('  Transmission functions and optimization sequence configured\n\n');
            end

            % ----------------------------------------------------------------
            % STEP 3: Build TransModel with real metadata
            % ----------------------------------------------------------------
            MetaValues = {'ZenithAngle_deg', ZenithAngle, ...
                          'Pressure_mbar',   Obj.Pressure, ...
                          'Temperature_C',   Obj.Temp};

            Obj.TransModel = tools.math.fun.CompositeFun.model(FunList, ...
                'MetadataValues',       MetaValues, ...
                'OptimizationSequence', OptSeq, ...
                'UseTran2D',            Args.UseTran2D, ...
                'Tran2DType',           Args.Tran2DType, ...
                'XPixel',               Args.XPixel, ...
                'YPixel',               Args.YPixel);

            % ====================================================================
            % STEP 4: Select calibrators
            % ====================================================================

            if Args.Verbose
                fprintf('Selecting calibrators...\n');
            end

            % Obs JD for pythonLike PM propagation: read from HeaderRef when
            % available; NaN otherwise (selectCalibratorsPythonLike will warn
            % and skip PM). Cheap when SelectionMethod='catsHTM' (unused).
            ObsJD = NaN;
            if ~isempty(HeaderRef) && isa(HeaderRef, 'AstroHeader')
                try
                    Tmp = HeaderRef.getVal('JD');
                    if isnumeric(Tmp) && isscalar(Tmp) && isfinite(Tmp)
                        ObsJD = Tmp;
                    end
                catch
                    % JD not in header — leave as NaN
                end
            end

            % Select calibrators (populates Obj.SpecData, Obj.SourceData, Obj.CalFound)
            Obj.selectCalibrators(CurrentCat, ...
                'SearchRadius', Args.SearchRadius, ...
                'MagRange', Args.MagRange, ...
                'FilterNegFlux', Args.FilterNegFlux, ...
                'MinSN2', Args.MinSN2, ...
                'CalibCatName', Args.CalibCatName, ...
                'MinSN', Args.MinSN, ...
                'MaxSN', Args.MaxSN, ...
                'FilterBadFlags', Args.FilterBadFlags, ...
                'MagColName', Args.MagColName, ...
                'SpFluxCol', Args.SpFluxCol, ...
                'BadBitNames', Args.BadBitNames, ...
                'AuditCalibrators', Args.AuditCalibrators, ...
                'AuditCatName', Args.AuditCatName, ...
                'AuditBPRPExcessFactorMax', Args.AuditBPRPExcessFactorMax, ...
                'AuditBPRPMax', Args.AuditBPRPMax, ...
                'AuditLASTNearestDist', Args.AuditLASTNearestDist, ...
                'AuditLASTDeltaMag', Args.AuditLASTDeltaMag, ...
                'AttachBP_RP', Args.AttachBP_RP, ...
                'SelectionMethod', Args.SelectionMethod, ...
                'UseTAPClassprob', Args.UseTAPClassprob, ...
                'ObsJD', ObsJD, ...
                'Verbose', Args.Verbose, ...
                'match_catsHTMArgs',Args.match_catsHTMArgs, ...
                'PosColNameX', Args.PosColNameX, ...
                'PosColNameY', Args.PosColNameY);

            % selectCalibrators populates Obj.SpecData, Obj.SourceData, and Obj.CalFound

            % Store AirmassColName on object for post-fit use by addMag/addZP
            Obj.AirmassColName = Args.AirmassColName;

            % ====================================================================
            % STEP 4b: Extract per-source airmass if requested
            % ====================================================================

            PerSourceZenithAngles = [];
            if Args.PerSourceAirmass && Obj.CalFound
                CalibColNames = Obj.SourceData.Table.Properties.VariableNames;
                if ismember(Args.AirmassColName, CalibColNames)
                    PerSourceAirmassVec = Obj.SourceData.getCol(Args.AirmassColName);
                    ValidAM = ~isnan(PerSourceAirmassVec) & PerSourceAirmassVec >= 1.0;
                    if all(ValidAM)
                        PerSourceZenithAngles = acosd(1 ./ PerSourceAirmassVec);
                        Obj.PerSourceAirmass = true;
                        if Args.Verbose
                            fprintf('  Per-source airmass: range %.3f - %.3f (from %s)\n', ...
                                min(PerSourceAirmassVec), max(PerSourceAirmassVec), Args.AirmassColName);
                        end
                    else
                        Obj.msgLog(LogLevel.Debug, ...
                            'calibrate: %d/%d calibrators have invalid airmass in %s - falling back to header airmass', ...
                            sum(~ValidAM), length(PerSourceAirmassVec), Args.AirmassColName);
                        Obj.PerSourceAirmass = false;
                    end
                else
                    Obj.msgLog(LogLevel.Debug, ...
                        'calibrate: Column %s not found in calibrator catalog - falling back to header airmass', ...
                        Args.AirmassColName);
                    Obj.PerSourceAirmass = false;
                end
            else
                Obj.PerSourceAirmass = false;
            end

            % ====================================================================
            % STEP 5: Fit transmission if calibrators found
            % ====================================================================

            if ~Obj.CalFound
                if Args.Verbose
                    fprintf('  No calibrators found - skipping transmission fitting.\n\n');
                end
                % Object already has CalFound = false
                % TransModel is present but not fitted
            else
                % Calibrators found - proceed with fitting

                if Args.Verbose
                    fprintf('Fitting transmission parameters...\n');
                end

                % Extract data for fitting from SourceData
                Flux = Obj.SourceData.getCol('Flux');
                X = Obj.SourceData.getCol('X');
                Y = Obj.SourceData.getCol('Y');

                % Extract flux errors if using flux-based weighting
                FluxErrVector = [];
                if ismember(lower(Args.WeightingMode), {'flux', 'combined'})
                    try
                        FluxErrVector = Obj.SourceData.getCol(Args.FluxErrColName);
                        if Args.Verbose
                            fprintf('  Extracted flux errors from %s column\n', Args.FluxErrColName);
                        end
                    catch
                        Obj.msgLog(LogLevel.Warning, ...
                            'Could not extract flux errors from %s. Falling back to spectral weighting.', ...
                            Args.FluxErrColName);
                        if strcmpi(Args.WeightingMode, 'flux')
                            Args.WeightingMode = 'none';
                        else  % 'combined'
                            Args.WeightingMode = 'spectral';
                        end
                    end
                end

                % Calculate effective exposure time (accounting for coadding).
                % NFramesPerCoadd > 1 only for coadd-of-coadds inputs.
                ExpTime_eff = Obj.ExpTime_eff;

                % Pre-compute MagErr for all calibrators (expensive, do once)
                % This avoids recalculating error propagation on every costFun call.
                % Both component vectors (spectral only, flux only) come back
                % alongside the WeightingMode-combined fit-weight MagErr so
                % they can be inspected in Cal snapshots.
                [PrecomputedMagErr, PrecomputedMagErr_spectral, PrecomputedMagErr_flux] = ...
                    Obj.propagateCalibratorMagErr(Flux, FluxErrVector, ...
                        'WeightingMode', Args.WeightingMode, ...
                        'ExpTime', ExpTime_eff, ...
                        'FluxErrorNorm', Args.FluxErrorNorm, ...
                        'SystematicErr', Args.SystematicErr);

                % Store pre-computed MagErr (and its two components) in
                % SourceData so every CalibTrajectory snapshot inherits them
                % via SnapTable = BaseTable copy.
                if istable(Obj.SourceData.Catalog)
                    Obj.SourceData.Catalog.MagErr = PrecomputedMagErr;
                    if ~isempty(PrecomputedMagErr_spectral)
                        Obj.SourceData.Catalog.MagErr_spectral = PrecomputedMagErr_spectral(:);
                    end
                    if ~isempty(PrecomputedMagErr_flux)
                        Obj.SourceData.Catalog.MagErr_flux = PrecomputedMagErr_flux(:);
                    end
                else
                    Tab = Obj.SourceData.Table;
                    Tab.MagErr = PrecomputedMagErr;
                    if ~isempty(PrecomputedMagErr_spectral)
                        Tab.MagErr_spectral = PrecomputedMagErr_spectral(:);
                    end
                    if ~isempty(PrecomputedMagErr_flux)
                        Tab.MagErr_flux = PrecomputedMagErr_flux(:);
                    end
                    Obj.SourceData.Catalog = Tab;
                end

                % Pre-compute interpolated spectra matrix (expensive, do once)
                % This avoids recalculating interpolation on every costFun call
                Obj.SpecData.SpecFluxMatrix = Obj.resampleCalibratorSpectra();

                % Setup CostArgs for TransmissionMode
                % Optional per-source FIXED atmospheric parameters from
                % airmass. When enabled (and PerSourceZenithAngles is
                % available), evaluate astro.transmission.atmParFromAirmass
                % element-wise on each calibrator's own AIRMASS. The
                % resulting per-source (PWV, AOD, Center) vectors flow
                % into costFun via CostArgs and enter each source's row
                % of the PerSourceParams matrix - fixed data, not fit
                % parameters. Small effect (LAST FoV airmass spread is
                % ~0.005-0.01), but zero cost when disabled.
                PerSourcePWV_cm     = [];
                PerSourceTauAod500  = [];
                PerSourceCenter_Ang = [];
                if Args.PerSourceAtmFromAirmass && ~isempty(PerSourceZenithAngles)
                    AM_vec = 1 ./ cosd(PerSourceZenithAngles(:));
                    [PerSourcePWV_cm, PerSourceTauAod500, PerSourceCenter_Ang, ~] = ...
                        astro.transmission.atmParFromAirmass(AM_vec);
                    if Args.Verbose
                        fprintf('  PerSourceAtmFromAirmass: PWV [%.2f, %.2f] cm, AOD [%.4f, %.4f], Center [%.0f, %.0f] A\n', ...
                            min(PerSourcePWV_cm), max(PerSourcePWV_cm), ...
                            min(PerSourceTauAod500), max(PerSourceTauAod500), ...
                            min(PerSourceCenter_Ang), max(PerSourceCenter_Ang));
                    end
                end

                % MagErr and SpecFluxMatrix pre-computed to avoid repeated calculations
                CostArgs = {...
                    'WeightMatrix', Obj.SpecData.Spec', ...
                    'PrecomputedMagErr', PrecomputedMagErr, ...
                    'PrecomputedSpecFluxMatrix', Obj.SpecData.SpecFluxMatrix, ...
                    'TransmissionMode', true, ...
                    'CalibWavelength', Obj.SpecData.SpecWvl, ...
                    'ExpTime', ExpTime_eff, ...
                    'Aperture_area_m2', Obj.Aperture, ...
                    'PerSourceZenithAngles', PerSourceZenithAngles, ...
                    'PerSourcePWV_cm',       PerSourcePWV_cm, ...
                    'PerSourceTauAod500',    PerSourceTauAod500, ...
                    'PerSourceCenter_Ang',   PerSourceCenter_Ang};

                % One-shot Tran2D ParX seeding (before stage 1 of the OptSeq).
                % Stages 1-3 see the perturbed coeffs; stage 4 (FieldCorrection)
                % overwrites ParX with the linear LS fit, so the final Tran2D
                % is determined by the data. The perturbation propagates only
                % via the calibrator subset (sigma-clipping in stages 1 & 3) and
                % via Norm / QE_Center fitted on shifted residuals.
                %
                % EXCEPTION: when the OptSeq contains a NONLIN_FC stage (joint
                % nonlinear FC fit, Simone-style), the perturbation must NOT
                % affect stages 1-3 — it is applied locally inside the NONLIN_FC
                % handler so it only seeds the LM initial guess for stage 4.
                OptSeqHasNonlinFC = false;
                if ~isempty(Obj.TransModel.OptSeq)
                    % Recipe shape (scalar struct with .Stages / .NumRepeats /
                    % .IterOverrides) vs legacy stage-array. Normalise so the
                    % perturbation-guard loop iterates the actual stages.
                    if isscalar(Obj.TransModel.OptSeq) && ...
                            isstruct(Obj.TransModel.OptSeq) && ...
                            isfield(Obj.TransModel.OptSeq, 'Stages')
                        StagesForCheck = Obj.TransModel.OptSeq.Stages;
                    else
                        StagesForCheck = Obj.TransModel.OptSeq;
                    end
                    for IStageCheck = 1:numel(StagesForCheck)
                        FP = StagesForCheck(IStageCheck).FreeParams;
                        if ischar(FP) && strcmpi(FP, 'NONLIN_FC')
                            OptSeqHasNonlinFC = true;
                            break;
                        end
                    end
                end
                if Args.Tran2DPerturbStd > 0 && Obj.TransModel.UseTran2D && ...
                        ~isempty(Obj.TransModel.Tran2DObj) && ~OptSeqHasNonlinFC
                    Nparams = numel(Obj.TransModel.Tran2DObj.ParX);
                    Obj.TransModel.Tran2DObj.ParX = randn(1, Nparams) * Args.Tran2DPerturbStd;
                    if Args.Verbose
                        fprintf('Tran2D ParX seeded with N(0, %.3f), %d coeffs\n', ...
                            Args.Tran2DPerturbStd, Nparams);
                    end
                end

                % Fit transmission parameters
                [Model, FitResult] = Obj.TransModel.fitPar(Obj.TransWvl, Flux, ...
                    'X', X, 'Y', Y, ...
                    'CostArgs', CostArgs, ...
                    'SigmaClipMethod', Args.SigmaClipMethod, ...
                    'OuterSigmaClip',     Args.OuterSigmaClip, ...
                    'OuterSigmaThresh',   Args.OuterSigmaThresh, ...
                    'OuterStdFunc',       Args.OuterStdFunc, ...
                    'OuterMaxIter',       Args.OuterMaxIter, ...
                    'OuterMinNewClipped', Args.OuterMinNewClipped, ...
                    'WeightedOuterIters', Args.WeightedOuterIters, ...
                    'UseTypicalX',        Args.UseTypicalX, ...
                    'Tran2DPerturbStd',   Args.Tran2DPerturbStd, ...
                    'Tran2DRngSeed',      Args.Tran2DRngSeed, ...
                    'CollectCalibTrajectory', Args.CollectCalibTrajectory, ...
                    'Verbose', Args.Verbose);

                % Store fitted model and fit results
                Obj.TransModel = Model;
                Obj.FitResults = FitResult;

                % Assemble per-inner-iter calibrator-trajectory snapshots
                % from FitResult(IStage).IterSnapshots into a flat struct
                % array on Obj.CalibTrajectory. Each entry carries a full
                % SourceData AstroCatalog with Used/Residuals/PredictedFlux
                % populated at that iteration. Opt-in: only runs when the
                % flag is true AND the fit produced IterSnapshots.
                Obj.CalibTrajectory = [];
                if Args.CollectCalibTrajectory && isstruct(FitResult) && ~isempty(FitResult)
                    if ~isempty(Obj.SourceData) && ~isempty(Obj.SourceData.Table)
                        BaseTable = Obj.SourceData.Table;
                        NCalibTot = height(BaseTable);
                        TrajAccum = repmat(struct( ...
                            'StageIndex', 0, 'StageName', '', 'IterIndex', 0, ...
                            'OuterIter', 0, 'NumClipped', 0, 'NumRemaining', 0, ...
                            'RMS', NaN, 'Scatter', NaN, 'RobustStd', NaN, ...
                            'ARMS', NaN, 'SourceData', AstroCatalog), 1, 0);
                        % When the fit ran multiple outer iters (Recipe
                        % NumRepeats > 1 or OuterSigmaClip), FitResult(1)
                        % .AllOuterStages is a flat struct array of every
                        % iter's stages in order, each carrying its own
                        % IterSnapshots and stamped with .OuterIter.
                        % Walk it if present so a caller sees the trajectory
                        % for every stage of every outer iter — not just
                        % the last iter (which is what FitResult itself
                        % holds). Falls back to FitResult when the outer
                        % loop ran only once.
                        if isfield(FitResult(1), 'AllOuterStages') && ~isempty(FitResult(1).AllOuterStages)
                            StagesToWalk = FitResult(1).AllOuterStages;
                        else
                            StagesToWalk = FitResult;
                        end
                        for IS = 1:numel(StagesToWalk)
                            if ~isfield(StagesToWalk(IS), 'IterSnapshots') || isempty(StagesToWalk(IS).IterSnapshots)
                                continue;
                            end
                            for IK = 1:numel(StagesToWalk(IS).IterSnapshots)
                                Snap = StagesToWalk(IS).IterSnapshots(IK);
                                if numel(Snap.KeepMask) ~= NCalibTot
                                    Obj.msgLog(LogLevel.Warning, sprintf( ...
                                        'CalibTrajectory: stage %d iter %d KeepMask length %d != NCalib %d — skipping', ...
                                        IS, IK, numel(Snap.KeepMask), NCalibTot));
                                    continue;
                                end
                                SnapTable = BaseTable;
                                UsedCol = logical(Snap.KeepMask(:));
                                SnapTable.Used = UsedCol;
                                % Snap.Residuals/PredictedFlux now arrive
                                % already length NCalibTot from
                                % fitMultiStage's post-stage scatter:
                                %   - current survivors carry this iter's value
                                %   - calibrators discarded EARLIER in this
                                %     stage carry their last-known residual
                                %     (the "residual at discard" diagnostic)
                                %   - calibrators clipped in EARLIER stages
                                %     stay NaN (they never entered this stage)
                                SnapTable.Residuals     = Snap.Residuals(:);
                                SnapTable.PredictedFlux = Snap.PredictedFlux(:);
                                if isfield(Snap, 'MagErr') && ~isempty(Snap.MagErr) && numel(Snap.MagErr) == NCalibTot
                                    SnapTable.MagErr = Snap.MagErr(:);
                                end
                                Entry = struct();
                                Entry.StageIndex   = Snap.StageIndex;
                                Entry.StageName    = Snap.StageName;
                                Entry.IterIndex    = Snap.IterIndex;
                                Entry.OuterIter    = Snap.OuterIter;
                                Entry.NumClipped   = Snap.NumClipped;
                                Entry.NumRemaining = Snap.NumRemaining;
                                Entry.RMS          = Snap.RMS;
                                if isfield(Snap, 'Scatter')
                                    Entry.Scatter  = Snap.Scatter;
                                else
                                    Entry.Scatter  = NaN;
                                end
                                if isfield(Snap, 'RobustStd')
                                    Entry.RobustStd = Snap.RobustStd;
                                else
                                    Entry.RobustStd = NaN;
                                end
                                % ARMS: bright-end sqrt(median(R^2)) on the
                                % ARMSMode-selected sub-pool of this snap's
                                % survivors. Same recipe as Obj.ARMS (line
                                % 1450+), applied per-iter to SnapTable.
                                Entry.ARMS  = NaN;
                                ARMSEnabled = strcmp(Args.ARMSMode, 'percent') || Args.N_ARMS > 0;
                                if ARMSEnabled
                                    UsedSnap   = logical(SnapTable.Used);
                                    FluxSnap   = SnapTable.Flux(UsedSnap);
                                    ResSnap    = SnapTable.Residuals(UsedSnap);
                                    ValidSnap  = isfinite(FluxSnap) & isfinite(ResSnap);
                                    FluxVSnap  = FluxSnap(ValidSnap);
                                    ResVSnap   = ResSnap(ValidSnap);
                                    Ksnap = armsSampleSize(numel(FluxVSnap), ...
                                        Args.ARMSMode, Args.ARMS_Percent, Args.N_ARMS);
                                    if Ksnap > 0 && Ksnap <= numel(FluxVSnap)
                                        [~, SortIdxSnap] = sort(FluxVSnap, 'descend');
                                        R2SnapSort  = ResVSnap(SortIdxSnap).^2;
                                        WindowMedS  = movmedian(R2SnapSort, Ksnap, 'Endpoints', 'discard');
                                        Entry.ARMS  = sqrt(min(WindowMedS));
                                    end
                                end
                                Entry.SourceData   = AstroCatalog(SnapTable);
                                TrajAccum(end+1) = Entry; %#ok<AGROW>
                            end
                        end
                        Obj.CalibTrajectory = TrajAccum;
                    end
                end

                % Add Used and Residuals columns to SourceData
                % Get final KeepMask and Residuals (from last stage if multi-stage)
                if isstruct(FitResult) && ~isempty(FitResult)
                    if numel(FitResult) > 1
                        % Multi-stage: use last stage result
                        FinalResult = FitResult(end);
                    else
                        FinalResult = FitResult;
                    end

                    NCalib = height(Obj.SourceData.Table);
                    Used = FinalResult.KeepMask(:);
                    Residuals = nan(NCalib, 1);
                    Residuals(Used) = FinalResult.Residuals(:);

                    % MagErr was pre-computed and stored in SourceData before fitting
                    % Keep the original pre-computed values for all calibrators

                    % Calculate calibrated magnitudes for calibrators
                    % MAG = -2.5*log10(Flux/ExpTime_eff) + ZP(X,Y)
                    MagCalib = Obj.evaluateMag(Flux, 'X', X, 'Y', Y, ...
                                               'MagSystem', Args.MagSystem);

                    % Dynamic column name: MAG_AB or MAG_VEGA
                    MagColName = ['MAG_', Args.MagSystem];

                    % Get predicted flux from FitResult (calculated by costFun during optimization)
                    PredictedFlux = nan(NCalib, 1);
                    PredictedFlux(Used) = FinalResult.PredictedFlux(:);

                    % Add columns directly to the catalog (MagErr already present from pre-computation)
                    AMPerSourceUsed = double(Obj.PerSourceAirmass) * ones(NCalib, 1);
                    if istable(Obj.SourceData.Catalog)
                        Obj.SourceData.Catalog.Used = Used;
                        Obj.SourceData.Catalog.Residuals = Residuals;
                        Obj.SourceData.Catalog.(MagColName) = MagCalib;
                        Obj.SourceData.Catalog.PredictedFlux = PredictedFlux;
                        Obj.SourceData.Catalog.AMPerSourceUsed = AMPerSourceUsed;
                    else
                        % Convert to table, add columns, convert back
                        Tab = Obj.SourceData.Table;
                        Tab.Used = Used;
                        Tab.Residuals = Residuals;
                        Tab.(MagColName) = MagCalib;
                        Tab.PredictedFlux = PredictedFlux;
                        Tab.AMPerSourceUsed = AMPerSourceUsed;
                        Obj.SourceData.Catalog = Tab;
                    end

                    % Recalculate DOF correctly for multi-stage optimization
                    % DOF = Ncalib(final) - Ntot(free params)
                    NCalibFinal = sum(Used);
                    NFreeParams = 0;

                    % Count unique free function parameters across all stages
                    if ~isempty(Obj.TransModel.OptSeq)
                        % Collect unique parameter names from all stages
                        FittedParamNames = {};
                        HasFieldCorrection = false;

                        % Recipe (scalar struct with .Stages) vs legacy
                        % struct-array — same normalisation pattern as the
                        % perturbation-guard loop above.
                        if isscalar(Obj.TransModel.OptSeq) && ...
                                isstruct(Obj.TransModel.OptSeq) && ...
                                isfield(Obj.TransModel.OptSeq, 'Stages')
                            StagesForCount = Obj.TransModel.OptSeq.Stages;
                        else
                            StagesForCount = Obj.TransModel.OptSeq;
                        end

                        for IStage = 1:length(StagesForCount)
                            Stage = StagesForCount(IStage);
                            if ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'JOINT_FC')
                                % Joint Norm + Tran2D linear stage: counts as
                                % Norm plus the Tran2D ParX (10 coeffs added
                                % below via HasFieldCorrection).
                                if ~any(strcmp(FittedParamNames, 'Norm'))
                                    FittedParamNames{end+1} = 'Norm'; %#ok<AGROW>
                                end
                                if ~isempty(Obj.TransModel.Tran2DObj)
                                    HasFieldCorrection = true;
                                end
                            elseif ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'NONLIN_FC')
                                % Joint nonlinear Tran2D stage (Simone-style):
                                % fits only the 10 Tran2D ParX coeffs (added
                                % below via HasFieldCorrection). Norm is held
                                % fixed at the pre-stage value, so no Norm
                                % contribution to DOF here.
                                if ~isempty(Obj.TransModel.Tran2DObj)
                                    HasFieldCorrection = true;
                                end
                            elseif ~isempty(Stage.FreeParams)
                                for IFree = 1:length(Stage.FreeParams)
                                    ParamName = Stage.FreeParams(IFree).Parameter;
                                    if ~any(strcmp(FittedParamNames, ParamName))
                                        FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                                    end
                                end
                            else
                                % Empty FreeParams indicates field correction stage
                                if ~isempty(Obj.TransModel.Tran2DObj)
                                    HasFieldCorrection = true;
                                end
                            end
                        end

                        NFreeParams = length(FittedParamNames);

                        % Count position correction parameters if fitted
                        if HasFieldCorrection
                            NFreeParams = NFreeParams + length(Obj.TransModel.Tran2DObj.ParX);
                        end
                    else
                        % No OptSeq, use initial FitPar configuration
                        for IFun = 1:length(Obj.TransModel.Funs)
                            NFreeParams = NFreeParams + sum(Obj.TransModel.Funs(IFun).FitPar);
                        end
                    end

                    % Set DOF
                    Obj.TransModel.DOF = NCalibFinal - NFreeParams;

                    if Args.Verbose
                        fprintf('  Calibrators (final): %d\n', NCalibFinal);
                        fprintf('  Free parameters: %d\n', NFreeParams);
                        fprintf('  DOF: %d\n', Obj.TransModel.DOF);
                    end
                end

                if Args.Verbose
                    fprintf('  Calibrators (initial): %d\n', size(Obj.SpecData.Spec, 1));
                    if ~isnan(Obj.TransModel.RMS)
                        fprintf('  RMS: %.4f mag\n', Obj.TransModel.RMS);
                    end
                    if ~isnan(Obj.TransModel.Chi2) && ~isnan(Obj.TransModel.DOF) && Obj.TransModel.DOF > 0
                        fprintf('  Chi2/DOF: %.2f / %d = %.3f\n', ...
                                Obj.TransModel.Chi2, Obj.TransModel.DOF, Obj.TransModel.Chi2/Obj.TransModel.DOF);
                    end
                end

            % Compute ARMS (bright-star RMS) if requested. ARMSMode picks
            % between fixed-count and percent-of-pool selection.
            ARMSEnabled = ~isempty(Obj.TransModel) && ~isempty(Obj.SourceData) && ...
                (strcmp(Args.ARMSMode, 'percent') || Args.N_ARMS > 0);
            if ARMSEnabled
                Tab = Obj.SourceData.Table;
                UsedMask = logical(Tab.Used);
                FluxUsed = Tab.Flux(UsedMask);
                ResUsed  = Tab.Residuals(UsedMask);
                ValidMask = isfinite(FluxUsed) & isfinite(ResUsed);
                FluxValid = FluxUsed(ValidMask);
                ResValid  = ResUsed(ValidMask);
                K = armsSampleSize(numel(FluxValid), Args.ARMSMode, ...
                    Args.ARMS_Percent, Args.N_ARMS);
                if K > 0 && K <= numel(FluxValid)
                    % Sliding-window-min-median: sort survivors by Flux
                    % descending, slide a K-wide window across the sorted
                    % list, take the median of R^2 in each window, and
                    % pick the minimum across all windows. sqrt gives
                    % the "best-behaving-bin's" root-median-square
                    % residual - a robust bright-to-mid-mag noise floor.
                    [~, SortIdx] = sort(FluxValid, 'descend');
                    R2Sorted     = ResValid(SortIdx).^2;
                    WindowMed    = movmedian(R2Sorted, K, 'Endpoints', 'discard');
                    Obj.ARMS     = sqrt(min(WindowMed));
                end
            end

            % NOTE: calcAperCorr is no longer called here. It runs in
            % fitPhotCalibTrans AFTER addMag creates MAG_AB_* columns,
            % so 'mag' mode can use AB magnitudes.

            % Post-fit gauge fix (opt-in). Reifies the reported (Norm, kx0)
            % split according to NormConvention. Runs after CalibTrajectory
            % has already been assembled, so the trajectory records the raw
            % fit evolution and only the final PC.TransModel is
            % gauge-canonicalised. Predictions unchanged either way.
            if strcmpi(Args.NormConvention, 'center')
                Obj = Obj.absorbTran2DCenterIntoNorm('Verbose', Args.Verbose);
            end

            if Args.Verbose
                fprintf('=== Calibration Complete ===\n');
            end
            end  % Close if ~Obj.CalFound ... else block
        end

        function Obj = selectCalibrators(Obj, Cat, Args)
            % Select calibrators with reference spectra for photometric calibration
            % Input  : - PhotCalibTrans object
            %          - AstroCatalog object with observed sources (single element)
            %          * ...,key,val,...
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 2.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [11.5 16.0].
            %            'MinSN' - Minimum S/N for calibrators. Default is 5.
            %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
            %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
            %            'FluxColName' - Flux column name to compare with. Default is 'FLUX_APER_3'.
            %            'FilterNegFlux' - Remove sources with negative flux in
            %                        FluxColName. Default is true.
            %            'MinSN2'   - Minimum SN_2 value for calibrators. Set to 0
            %                        to skip this filter. Default is 10.
            %            'CalibCatName' - catsHTM catalog containing reference spectra
            %                        for calibration. Default is 'GAIADR3spec'.
            %            'SpFluxCol' - Spectral flux column indices [flux_start, flux_end, error_start, error_end].
            %                          Default is [7, 349, 350, 692] for Gaia DR3 XP spectra.
            %            'BadBitNames' - A cell array of bad bit mask
            %                   names. Sources with one of these bits are not used
            %                   as calibrators.
            %                   Default is {'Saturated', 'NaN', 'Negative', 'CR_DeltaHT', 'NearEdge'}
            %            'AuditCalibrators' - Run "step 0" audit that rejects doubtful
            %                        calibrators after the calibrator-catalog match,
            %                        before the standard quality filters. Default is false.
            %            'AuditCatName' - catsHTM Gaia photometric catalog used to fetch
            %                        BP-RP and BP-RP excess factor for the audit.
            %                        Default is 'GAIADR3'.
            %            'AuditBPRPExcessFactorMax' - Reject if the matched Gaia source has
            %                        phot_bp_rp_excess_factor above this value. Default is 1.3.
            %            'AuditBPRPMax' - Reject if the matched Gaia source has bp_rp above
            %                        this value. Default is 1.5.
            %            'AuditLASTNearestDist' - Reject if the nearest LAST neighbour
            %                        (self-excluded) lies within this distance [arcsec].
            %                        Default is 20.
            %            'AuditLASTDeltaMag' - Reject if the nearest LAST neighbour has
            %                        |delta-mag| (using MagColName) below this value.
            %                        Default is 2.
            %            'AttachBP_RP' - Attach Gaia BP_RP, MAG_BP, MAG_RP columns
            %                        to SourceData after the calibrator pool is
            %                        finalized. Costs one extra catsHTM match
            %                        against AuditCatName (default 'GAIADR3').
            %                        Independent of AuditCalibrators - the match
            %                        is on the post-filter pool, not the pre-
            %                        filter candidates. Failure-safe: on any
            %                        match error, logs a Warning and fills the
            %                        three columns with NaN. Inherited by every
            %                        CalibTrajectory snapshot's SourceData
            %                        (snap SnapTable is built from the live
            %                        Obj.SourceData.Table). Default is true.
            %            'SelectionMethod' - Calibrator-selection recipe:
            %                        'catsHTM'    - existing path: match against
            %                                       CalibCatName, apply quality
            %                                       filters, optional AuditCalibrators.
            %                                       (Default; preserves status quo.)
            %                        'pythonLike' - mirror the Python prototype
            %                                       (matlab/.../Drafts-* GaiaQuery):
            %                                       parallel matches to
            %                                       GAIADR3spec + GAIADR3,
            %                                       proper-motion propagation
            %                                       from J2016 to ObsJD, Python's
            %                                       MagRange [12,16], flag set,
            %                                       SN window (5,1000), and
            %                                       FLUX_APER_3 / FLUX_PSF > 0.
            %            'UseTAPClassprob' - Only consulted in 'pythonLike'. When
            %                        true, keeps only candidates whose matched
            %                        GAIADR3spec row has
            %                        classprob_dsc_combmod_star > 0.9.
            %                        (Pre-Jun-2026 this triggered a VO.TopCat /
            %                        STILTS TAP query against gaiadr3.gaia_source.
            %                        After the GAIADR3spec regen the column lives
            %                        at position 700, so the filter is now a
            %                        direct column read with no network call.
            %                        Arg name preserved for backwards compat.)
            %                        Default is false.
            %            'ObsJD' - Observation Julian Date for PM propagation in
            %                        'pythonLike'. Default is NaN (skip PM with a
            %                        Warning); calibrate() fills this from the
            %                        image/Metadata header automatically.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - PhotCalibTrans object with populated properties:
            %                  .SpecData - Structure with reference spectral data:
            %                    .CalData - struct with .RA, .Dec (catalog positions)
            %                    .SpecWvl [N_wvl x 1] - Wavelength grid [Angstrom]
            %                    .Spec [N_calib x N_wvl] - Calibrator spectra flux
            %                    .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
            %                  .SourceData - AstroCatalog with observed calibrator sources.
            %                    Columns:
            %                      Flux, FluxErr, X, Y, RA, Dec  - per-source
            %                                                      observed
            %                                                      quantities
            %                      MatchDistance, NumMatches    - catalog-match
            %                                                      diagnostics
            %                      AIRMASS (if PerSourceAirmass) - per-source AM
            %                      BP_RP, MAG_BP, MAG_RP (if AttachBP_RP=true)
            %                                                    - Gaia colour
            %                                                      from extra
            %                                                      AuditCatName
            %                                                      match; NaN
            %                                                      where the
            %                                                      Gaia match
            %                                                      missed
            %                  .CalFound - true if length(SourceData) > 0
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.selectCalibrators(Cat);
            %          PC = PC.selectCalibrators(Cat, 'SearchRadius', 2, 'MagRange', [11.5 16.0]);
            %          PC = PC.selectCalibrators(Cat, 'SpFluxCol', [7, 349, 350, 692]);
            % Note: Default implementation uses Gaia DR3 XP spectra from GAIADR3spec catalog.
            %       Default telescope/instrument configuration is for LAST.
            %       Input must be single-element AstroCatalog (extracted in calibrate()).

            arguments
                Obj
                Cat  % AstroCatalog
                Args.SearchRadius = 2  % arcsec
                Args.MagRange = [11.5 16.0]
                Args.MinSN = 5
                Args.MaxSN = 1000
                Args.FilterBadFlags logical = true
                Args.FluxColName = 'FLUX_APER_3'
                Args.MagColName = 'MAG_APER_3'
                Args.FilterNegFlux logical = true     % Remove sources with negative flux (LAST default)
                Args.MinSN2 = 10                      % Minimum SN_2 for calibrators, 0 to skip (LAST default)
                Args.CalibCatName = 'GAIADR3spec'    % catsHTM catalog with reference spectra
                Args.SpFluxCol = [7, 349, 350, 692]   % [flux_start, flux_end, error_start, error_end]
                Args.BadBitNames     = {'Saturated', 'NaN', 'Negative', 'CR_DeltaHT', 'NearEdge'};
                Args.match_catsHTMArgs = {};
                % Step-0 audit (off by default; status quo preserved)
                Args.AuditCalibrators logical = false   % Toggle the audit step
                Args.AuditCatName = 'GAIADR3'           % Gaia catalog for audit (photometric, not spec)
                Args.AuditBPRPExcessFactorMax = 1.3
                Args.AuditBPRPMax = 1.5
                Args.AuditLASTNearestDist = 20          % arcsec
                Args.AuditLASTDeltaMag = 2              % mag
                % Attach BP_RP, MAG_BP, MAG_RP columns to SourceData via a
                % cheap extra match against AuditCatName (default 'GAIADR3').
                % Inherited automatically by every CalibTrajectory snapshot
                % (which builds its SnapTable from Obj.SourceData.Table).
                % Default true; pass false to skip the extra catsHTM call.
                Args.AttachBP_RP logical = true
                Args.Verbose logical = false
                % Alternate selection recipe (off by default; preserves status quo)
                Args.SelectionMethod char {mustBeMember(Args.SelectionMethod, ...
                    {'catsHTM','pythonLike'})} = 'catsHTM'
                Args.UseTAPClassprob logical = false    % only consulted in 'pythonLike'
                Args.ObsJD            double  = NaN     % obs JD, drives PM propagation in 'pythonLike'
                % Position columns read off Cands when building SourceData.
                % Default 'X','Y' preserves per-crop calibrate byte-for-byte.
                % Set to 'XFULL','YFULL' when calibrating a joint (whole-image)
                % AstroCatalog produced by imProc.cat.joinCropsToCatalog so
                % downstream Tran2D evaluation uses the field-frame coords.
                Args.PosColNameX      char    = 'X'
                Args.PosColNameY      char    = 'Y'
            end

            RAD = constant.RAD;

            % --- Dispatcher ---
            % 'pythonLike' mirrors the Python prototype (LastCatUtils + GaiaQuery):
            %   match LAST -> GAIADR3spec AND GAIADR3 in parallel, apply proper-motion
            %   propagation from J2016 to Args.ObsJD, then apply Python's filter cascade.
            % Default 'catsHTM' path runs the existing logic unchanged below.
            if ~strcmpi(Args.SelectionMethod, 'catsHTM')
                Obj = selectCalibratorsPythonLike(Obj, Cat, Args);
                return
            end

            % ====================================================================
            % STEP 1: FIND CALIBRATOR CANDIDATES (cone match against CalibCatName)
            % ====================================================================
            [Cands, FieldTab, CatH] = PhotCalibTrans.findCalibCandidates(Cat, ...
                'CalibCatName',      Args.CalibCatName, ...
                'SearchRadius',      Args.SearchRadius, ...
                'match_catsHTMArgs', Args.match_catsHTMArgs, ...
                'Verbose',           Args.Verbose, ...
                'Logger',            Obj);

            HasRADec = ismember('RA', Cat.Table.Properties.VariableNames) && ...
                       ismember('Dec', Cat.Table.Properties.VariableNames);
            if ~HasRADec
                Obj.NoRADec = true;
            end

            % ====================================================================
            % STEP 2 (OPTIONAL): AUDIT CALIBRATORS
            % ====================================================================
            if Args.AuditCalibrators && ~isempty(Cands) && height(Cands) > 0
                Doubtful = PhotCalibTrans.auditCalibCandidates(Cands, FieldTab, ...
                    'AuditCatName',             Args.AuditCatName, ...
                    'SearchRadius',             Args.SearchRadius, ...
                    'AuditBPRPMax',             Args.AuditBPRPMax, ...
                    'AuditBPRPExcessFactorMax', Args.AuditBPRPExcessFactorMax, ...
                    'AuditLASTNearestDist',     Args.AuditLASTNearestDist, ...
                    'AuditLASTDeltaMag',        Args.AuditLASTDeltaMag, ...
                    'MagColName',               Args.MagColName, ...
                    'Verbose',                  Args.Verbose, ...
                    'Logger',                   Obj);
                NumDoubtful = sum(Doubtful);
                Cands = Cands(~Doubtful, :);
                if Args.Verbose
                    fprintf('  Audit step 0: %d flagged doubtful, %d remain\n', ...
                            NumDoubtful, height(Cands));
                end
            end

            % ====================================================================
            % STEP 3: APPLY QUALITY FILTERS
            % ====================================================================
            if ~isempty(Cands) && height(Cands) > 0
                KeepMask = PhotCalibTrans.applyCalibQuality(Cands, ...
                    'MagRange',       Args.MagRange, ...
                    'MagColName',     Args.MagColName, ...
                    'FilterBadFlags', Args.FilterBadFlags, ...
                    'BadBitNames',    Args.BadBitNames, ...
                    'MinSN',          Args.MinSN, ...
                    'MaxSN',          Args.MaxSN, ...
                    'FluxColName',    Args.FluxColName, ...
                    'FilterNegFlux',  Args.FilterNegFlux, ...
                    'MinSN2',         Args.MinSN2, ...
                    'Verbose',        Args.Verbose);
                Cands = Cands(KeepMask, :);
            end

            HasGoodMatches = ~isempty(Cands) && height(Cands) > 0;
            if HasRADec && ~HasGoodMatches
                Obj.msgLog(LogLevel.Warning, ...
                    'selectCalibrators: No sources passed quality filters and have calibrator matches');
            end

            % ====================================================================
            % STEP 4: EXTRACT CALIBRATOR DATA (if matches found)
            % ====================================================================
            if HasRADec && HasGoodMatches
                CalIdx     = double(Cands.CalibInd);
                Nmatch     = Cands.Nmatch;
                DistArcsec = convert.angular('rad', 'arcsec', Cands.MatchDistRad);

                CalArr = CatH.Catalog;
                CalTab = CalArr(CalIdx, :);

                FluxIni  = Args.SpFluxCol(1);
                FluxEnd  = Args.SpFluxCol(2);
                EFluxIni = Args.SpFluxCol(3);
                EFluxEnd = Args.SpFluxCol(4);

                SpecFlux = double(CalTab(:, FluxIni:FluxEnd));
                SpecErr  = double(CalTab(:, EFluxIni:EFluxEnd));

                Cal_RA  = double(CalTab(:, 1)) * RAD;
                Cal_Dec = double(CalTab(:, 2)) * RAD;

                Obs_X    = Cands.(Args.PosColNameX);
                Obs_Y    = Cands.(Args.PosColNameY);
                Obs_RA   = Cands.RA;
                Obs_Dec  = Cands.Dec;
                Obs_Flux = Cands.(Args.FluxColName);

                HasAirmassCol = ismember('AIRMASS', Cands.Properties.VariableNames);
                if HasAirmassCol
                    Obs_Airmass = Cands.AIRMASS;
                end

                FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
                if ismember(FluxErrColName, Cands.Properties.VariableNames)
                    Obs_FluxErr = Cands.(FluxErrColName);
                else
                    Obs_FluxErr = sqrt(abs(Obs_Flux));
                    Obj.msgLog(LogLevel.Warning, sprintf( ...
                        'selectCalibrators: %s not found, using sqrt(flux) for errors', FluxErrColName));
                end

                Nsources_before = length(Obs_Flux);
                InvalidFlux  = isnan(Obs_Flux) | isinf(Obs_Flux) | (Obs_Flux <= 0);
                InvalidXY    = isnan(Obs_X) | isinf(Obs_X) | isnan(Obs_Y) | isinf(Obs_Y);
                InvalidRADec = isnan(Obs_RA) | isinf(Obs_RA) | isnan(Obs_Dec) | isinf(Obs_Dec);
                if any(InvalidFlux)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Flux validation: %d/%d sources have invalid Flux (NaN/Inf/<=0) - excluded', ...
                        sum(InvalidFlux), Nsources_before);
                end
                if any(InvalidXY)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Position validation: %d/%d sources have invalid X/Y (NaN/Inf) - excluded', ...
                        sum(InvalidXY), Nsources_before);
                end
                if any(InvalidRADec)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Coordinate validation: %d/%d sources have invalid RA/Dec (NaN/Inf) - excluded', ...
                        sum(InvalidRADec), Nsources_before);
                end
                ValidCalibMask = ~InvalidFlux & ~InvalidXY & ~InvalidRADec;
                Nvalid = sum(ValidCalibMask);

                if Nvalid < Nsources_before
                    Obs_X       = Obs_X(ValidCalibMask);
                    Obs_Y       = Obs_Y(ValidCalibMask);
                    Obs_RA      = Obs_RA(ValidCalibMask);
                    Obs_Dec     = Obs_Dec(ValidCalibMask);
                    Obs_Flux    = Obs_Flux(ValidCalibMask);
                    Obs_FluxErr = Obs_FluxErr(ValidCalibMask);
                    DistArcsec  = DistArcsec(ValidCalibMask);
                    Nmatch      = Nmatch(ValidCalibMask);
                    Cal_RA      = Cal_RA(ValidCalibMask);
                    Cal_Dec     = Cal_Dec(ValidCalibMask);
                    SpecFlux    = SpecFlux(ValidCalibMask, :);
                    SpecErr     = SpecErr(ValidCalibMask, :);
                    if HasAirmassCol
                        Obs_Airmass = Obs_Airmass(ValidCalibMask);
                    end
                    if Args.Verbose
                        fprintf('  Data validation: %d/%d calibrators have valid data\n', Nvalid, Nsources_before);
                    end
                end

                NmatchTotal = Nvalid;
                if NmatchTotal == 0
                    Obj.msgLog(LogLevel.Error, 'selectCalibrators: No valid calibrators remain after data validation');
                    Obj.SourceData = [];
                    Obj.SpecData   = [];
                    Obj.CalFound   = false;
                    return;
                end

                Obj.SpecData = struct();
                Obj.SpecData.CalData = struct('RA', Cal_RA, 'Dec', Cal_Dec);
                Obj.SpecData.SpecWvl = (3360:20:10200)';
                Obj.SpecData.Spec    = SpecFlux;
                Obj.SpecData.SpecErr = SpecErr;

                SourceTable = table(Obs_Flux, Obs_FluxErr, Obs_X, Obs_Y, Obs_RA, Obs_Dec, DistArcsec, Nmatch, ...
                                    'VariableNames', {'Flux', 'FluxErr', 'X', 'Y', 'RA', 'Dec', 'MatchDistance', 'NumMatches'});
                if HasAirmassCol
                    SourceTable.AIRMASS = Obs_Airmass;
                end
                if Args.AttachBP_RP
                    % Read Gaia tail cols straight off the candidate row
                    % (attached by findCalibCandidates after the Jun 2026
                    % GAIADR3spec regen). Apply ValidCalibMask the same way
                    % the other per-row vectors were subset. Missing column
                    % => NaN-padded so the SourceData schema stays stable.
                    CandsVN = Cands.Properties.VariableNames;
                    % Raw Gaia col name in Cands -> user-facing col name in SourceData
                    GaiaMap = { ...
                        'bp_rp',                         'BP_RP'; ...
                        'phot_bp_mean_mag',              'MAG_BP'; ...
                        'phot_rp_mean_mag',              'MAG_RP'; ...
                        'phot_g_mean_mag',               'MAG_G'; ...
                        'PMRA',                          'PMRA'; ...
                        'PMDec',                         'PMDec'; ...
                        'classprob_dsc_combmod_star',    'CLASSPROB'; ...
                        'phot_bp_rp_excess_factor',      'BPRP_EXCESS'};
                    for GK = 1:size(GaiaMap, 1)
                        RawName  = GaiaMap{GK, 1};
                        NiceName = GaiaMap{GK, 2};
                        Val = nan(Nvalid, 1);
                        if ismember(RawName, CandsVN)
                            Tmp = Cands.(RawName); Val = Tmp(ValidCalibMask);
                        end
                        SourceTable.(NiceName) = Val;
                    end
                end
                Obj.SourceData = AstroCatalog(SourceTable);
                Obj.CalFound   = true;

                if Args.Verbose
                    fprintf('Calibrator selection complete: %d matched calibrators.\n\n', NmatchTotal);
                end
            else
                Obj.SourceData = [];
                Obj.SpecData   = [];
                Obj.CalFound   = false;
            end

            % Clean up temporary columns added during the match step
            if HasRADec
                CatTab = Cat.Table;
                if ismember('Dist', CatTab.Properties.VariableNames)
                    Cat = Cat.deleteCol('Dist');
                end
                if ismember('Nmatch', CatTab.Properties.VariableNames)
                    Cat = Cat.deleteCol('Nmatch');
                end
            end
        end
    end

    methods % Evaluation methods
        function Trans = evaluateTransmission(Obj, Args)
            % Evaluate transmission at specific positions (with position-dependent corrections)
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Lambda' - Wavelength grid [Angstrom] [N_lambda x 1]. Default is Obj.TransWvl (constant property).
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            % Output : - Transmission values [N_pos x N_lambda] or [N_lambda x 1]
            %                    If X, Y provided: matrix where Trans(i,j) = transmission for position i at wavelength j
            %                    If X, Y empty: vector of base transmission at field center
            % Author : D. Kovaleva (Dec 2025)
            % Example: Trans = PC.evaluateTransmission();  % Transmission at field center using Obj.TransWvl
            %          Trans = PC.evaluateTransmission('Lambda', CustomLambda, 'X', X, 'Y', Y);

            arguments
                Obj
                Args.Lambda = []
                Args.X = []
                Args.Y = []
            end

            % Use default Lambda if not provided
            if isempty(Args.Lambda)
                Lambda = Obj.TransWvl;
            else
                Lambda = Args.Lambda;
            end

            Lambda = Lambda(:);  % Ensure column vector

            % If no positions provided, evaluate at field center (no position correction)
            if isempty(Args.X) || isempty(Args.Y)
                if ~isempty(Obj.TransModel.Tran2DObj)
                    % With Tran2D: evaluate at field center (reference point)
                    Xc = Obj.TransModel.Tran2DObj.ParNX(1);
                    Yc = Obj.TransModel.Tran2DObj.ParNY(1);
                    Trans = Obj.TransModel.evaluateWithPosition(Lambda, Xc, Yc);
                else
                    % Without Tran2D: evaluate base transmission directly
                    Trans = Obj.TransModel.evaluateAllFunParInput(Lambda);
                end
                Trans = Trans(:);  % Return as column vector [N_lambda x 1]
                return;
            end

            % Evaluate transmission at specified positions
            X = Args.X(:);  % Ensure column vectors
            Y = Args.Y(:);

            if ~isempty(Obj.TransModel.Tran2DObj)
                % With Tran2D: evaluate with position-dependent corrections
                % evaluateWithPosition returns [N_pos x N_lambda]
                Trans = Obj.TransModel.evaluateWithPosition(Lambda, X, Y);
            else
                % Without Tran2D: same transmission for all positions
                % Evaluate once and replicate
                TransBase = Obj.TransModel.evaluateAllFunParInput(Lambda);
                Trans = repmat(TransBase(:)', length(X), 1);  % [N_pos x N_lambda]
            end
        end

        function IntTrans = integralTransmission(Obj, Args)
            % Integral (mean) transmission as fraction of perfect 100% throughput
            % Input  : - PhotCalibTrans object.
            %          * ...,key,val,...
            %            'WvlRange' - [min, max] wavelength range [Angstrom].
            %                         Default is [] (use full TransWvl range).
            % Output : - Scalar in [0,1]: trapz(T) / (wvl_max - wvl_min).
            % Author : D. Kovaleva (Feb 2026)
            % Example: T = PC.integralTransmission();
            %          T = PC.integralTransmission('WvlRange', [4000, 9000]);

            arguments
                Obj
                Args.WvlRange = []
            end

            Lambda = Obj.TransWvl;
            Trans = Obj.evaluateTransmission('Lambda', Lambda);
            Trans = Trans(:);
            Lambda = Lambda(:);

            % Apply wavelength range if specified
            if ~isempty(Args.WvlRange)
                Mask = Lambda >= Args.WvlRange(1) & Lambda <= Args.WvlRange(2);
                Lambda = Lambda(Mask);
                Trans = Trans(Mask);
            end

            IntTrans = trapz(Lambda, Trans) / (Lambda(end) - Lambda(1));
        end

        function PCnew = derivePC(Obj, RefTransParams, Args)
            % Derive a modified PhotCalibTrans from reference transmission parameters.
            %   Creates a deep copy of this object with its transmission
            %   parameters replaced by RefTransParams, optionally keeping
            %   this crop's own Norm and absorbing the Tran2D center offset.
            %   Used by orchestrators for non-percrop photometry modes
            %   (refshape, refzp, refzp_raw, etc.) to produce a PhotCalibTrans
            %   object with the desired state baked in, so plain addMag/
            %   evaluateZP calls work without mode-aware overrides.
            % Input  : - PhotCalibTrans object (this crop's fit).
            %          - RefTransParams — full parameter vector (same length
            %            as Obj.TransModel.getAllFunPar().Val), typically
            %            visit-averaged. Position params (ZenithAngle_deg,
            %            Temperature_C) are preserved from this crop.
            %          * ...,key,val,...
            %            'UseRefNorm'  - If true, use Norm from RefTransParams.
            %                       If false (default), keep this crop's own Norm.
            %            'NormTran2DToCenter' - If true (default when UseRefNorm=true),
            %                       absorb Tran2D(center) into Norm so that Tran2D
            %                       captures only relative shape. Default is true.
            %            'PreserveObsParams' - Cell array of param names to keep
            %                       from this crop (observation-dependent params).
            %                       Default is {'ZenithAngle_deg', 'Temperature_C'}.
            % Output : - PCnew — modified deep-copied PhotCalibTrans.
            % Author : D. Kovaleva (Apr 2026)
            % Example: PCref = PC.derivePC(RefParamVec);                 % refshape
            %          PCref = PC.derivePC(RefParamVec, 'UseRefNorm', true);  % refzp
            %          PCref = PC.derivePC(RefParamVec, 'UseRefNorm', true, ...
            %                     'NormTran2DToCenter', false);                % refzp_raw

            arguments
                Obj
                RefTransParams
                Args.UseRefNorm logical = false
                Args.NormTran2DToCenter logical = true
                Args.PreserveObsParams cell = {'ZenithAngle_deg', 'Temperature_C'}
            end

            PCnew = Obj.copy();
            AllFunPar = PCnew.TransModel.getAllFunPar();

            % Start from reference params
            NewVal = RefTransParams(:);

            % Preserve observation-dependent params from this crop
            for Ip = 1:numel(Args.PreserveObsParams)
                Idx = find(strcmp(AllFunPar.Name, Args.PreserveObsParams{Ip}), 1);
                if ~isempty(Idx)
                    NewVal(Idx) = AllFunPar.Val(Idx);
                end
            end

            % Optionally keep this crop's Norm (refshape mode)
            NormIdx = find(strcmp(AllFunPar.Name, 'Norm'), 1);
            if ~Args.UseRefNorm && ~isempty(NormIdx)
                NewVal(NormIdx) = AllFunPar.Val(NormIdx);
            end

            % Optionally absorb Tran2D(center) into Norm (refzp mode)
            HasTran2D = ~isempty(PCnew.TransModel.Tran2DObj) && PCnew.TransModel.UseTran2D;
            if Args.UseRefNorm && Args.NormTran2DToCenter && HasTran2D && ~isempty(NormIdx)
                Xc = PCnew.TransModel.Tran2DObj.ParNX(1);
                Yc = PCnew.TransModel.Tran2DObj.ParNY(1);
                [CenterCorr, ~] = PCnew.TransModel.Tran2DObj.forward([Xc, Yc]);
                % Adjust Norm so that ZP at (Xc,Yc) equals the un-corrected ZP
                NewVal(NormIdx) = NewVal(NormIdx) * 10^(CenterCorr / 2.5);
            end

            AllFunPar.Val = NewVal;
            PCnew.TransModel.setAllFunPar(AllFunPar);
        end

        function Obj = absorbTran2DCenterIntoNorm(Obj, Args)
            % Rotate the (Norm, Tran2D-DC) gauge freedom so Tran2D(centre)=0
            % Description: Post-fit relabelling of two degenerate parameters
            %              that carry the same information: the fit's Norm
            %              and the Tran2D polynomial's value at the field
            %              centre. This method absorbs Tran2D(centre) into
            %              Norm so that after the call Tran2D(x_c, y_c) = 0
            %              exactly and Norm carries the full ZP at the
            %              field centre. Every model prediction (ZP at any
            %              (x,y), predicted flux, residuals, RMS, chi^2)
            %              is bit-identical before and after — this is a
            %              gauge fix, not a refit.
            %
            %              Math (multiplicative-in-flux ZP model, see
            %              evaluateZP): the ZP at position (x,y) is
            %                ZP(x,y) = 2.5*log10(Norm * OtherFactors)
            %                          - Tran2D(x,y).
            %              Let PolyC = Tran2D(x_c, y_c). To zero the
            %              polynomial at the centre while preserving
            %              ZP(x,y) everywhere:
            %                Norm_new    = Norm_old * 10^(-PolyC/2.5)
            %                ParX(1)_new = ParX(1)_old - PolyC     (uniform
            %                              DC shift of the polynomial via
            %                              the constant-basis coefficient)
            %
            %              Idempotent: running it a second time is a no-op
            %              because Tran2D(centre) is already zero.
            % Input  : - PhotCalibTrans object with a populated TransModel
            %            and Tran2DObj.
            %          * ...,key,val,...
            %            'Tol'      - Threshold on |PolyC| below which the
            %                         call is treated as a no-op.
            %                         Default 1e-12.
            %            'Verbose'  - Print the (Norm_old -> Norm_new,
            %                         PolyC) trio on non-trivial calls.
            %                         Default false.
            % Output : - Same PhotCalibTrans (handle class; mutated in
            %                     place). Returned for method chaining.
            % Author : D. Kovaleva (Jul 2026)
            % See also: derivePC (uses a related absorption for the
            %           reference-vs-per-crop reshape/refzp modes).
            % Example: PC = PC.calibrate(AI, 'NormConvention', 'center');
            %          % or explicitly, after a raw calibrate:
            %          PC = PC.absorbTran2DCenterIntoNorm();

            arguments
                Obj
                Args.Tol     (1,1) double  = 1e-12
                Args.Verbose (1,1) logical = false
            end

            if isempty(Obj.TransModel) || isempty(Obj.TransModel.Tran2DObj) ...
                    || ~Obj.TransModel.UseTran2D
                return;
            end

            T2D = Obj.TransModel.Tran2DObj;

            % Field centre in the pixel coordinates the basis is
            % normalised against. ParNX / ParNY hold [Xc, Xc] and
            % [Yc, Yc] (basis is normalised to [-1,1] using these
            % centres and half-widths, see Tran2D constructor).
            Xc = T2D.ParNX(1);
            Yc = T2D.ParNY(1);
            [PolyC, ~] = T2D.forward([Xc, Yc]);

            if ~isfinite(PolyC) || abs(PolyC) < Args.Tol
                return;                                     % already canonical
            end

            AllFunPar = Obj.TransModel.getAllFunPar();
            NormIdx   = find(strcmp(AllFunPar.Name, 'Norm'), 1);
            if isempty(NormIdx)
                Obj.addStatus('absorbTran2DCenterIntoNorm', 'warning', ...
                    'TransModel has no Norm parameter — nothing to absorb into.', ...
                    'PhotCalibTrans:absorbTran2DCenterIntoNorm:NoNorm');
                return;
            end

            NormOld = AllFunPar.Val(NormIdx);
            NormNew = NormOld * 10^(-PolyC / 2.5);

            AllFunPar.Val(NormIdx) = NormNew;
            Obj.TransModel.setAllFunPar(AllFunPar);

            % Uniformly shift the polynomial by -PolyC via the
            % constant-basis coefficient. For cheby1_*_xt the first
            % basis function is ones(size(x)) (see Tran2D.m case
            % 'cheby1_4_xt' / 'cheby1_2'), so ParX(1) is the DC term.
            T2D.ParX(1) = T2D.ParX(1) - PolyC;

            if Args.Verbose
                fprintf('  absorbTran2DCenterIntoNorm: PolyC=%+.6g mag, Norm %g -> %g\n', ...
                        PolyC, NormOld, NormNew);
            end
        end

        function ZP = evaluateZP(Obj, Args)
            % Evaluate photometric zero point at specific positions
            % Input  : - PhotCalibTrans object.
            %          * ...,key,val,...
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            %            'PerSourceZenithAngles' - Per-source zenith angles [deg]
            %                         [N_pos x 1]. When non-empty, evaluates per-source
            %                         atmospheric transmission for each source.
            %                         Default is [] (use single fitted airmass).
            % Output : - Zero point(s) [N_pos x 1] or scalar.
            %                 If X, Y provided: vector with ZP for each position.
            %                 If X, Y empty: scalar ZP at field center.
            % Author : D. Kovaleva (Dec 2025)
            % Example: ZP = PC.evaluateZP();  % ZP at field center
            %          ZP = PC.evaluateZP('X', X, 'Y', Y);  % ZP at specific positions
            %          ZP = PC.evaluateZP('MagSystem', 'AB');

            arguments
                Obj
                Args.X = []
                Args.Y = []
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
                Args.PerSourceZenithAngles = []  % [N_pos x 1] per-source zenith angles [deg]
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:evaluateZP:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            Fnu = constant.Fnu('SI');  % AB system reference flux density [W/m^2/Hz]
            H = 6.62607015e-34;         % SI 2019 Plank constant

            % Use constant wavelength grid
            Lambda = Obj.TransWvl;

            if ~isempty(Args.PerSourceZenithAngles)
                % === Per-source airmass mode ===
                % Each source gets its own atmospheric transmission based on its zenith angle
                N_pos = length(Args.PerSourceZenithAngles);

                % Build per-source AllFunPar matrix
                AllFunPar = Obj.TransModel.getAllFunPar();
                AllNames = AllFunPar.Name;
                BaseParams = AllFunPar.Val(:)';

                ZenithIdx = find(strcmp(AllNames, 'ZenithAngle_deg'));
                PerSourceParams = repmat(BaseParams, N_pos, 1);  % [N_pos x N_params]
                PerSourceParams(:, ZenithIdx) = Args.PerSourceZenithAngles(:);

                % Evaluate per-source transmission: [N_wvl x N_pos]
                TransPerSource = Obj.TransModel.evaluateAllFunParInput(Lambda, PerSourceParams);

                % Reference F_nu spectrum: power law (lambda/pivot)^slope.
                % Slope = 0 (default) reduces to the AB-flat reference.
                RefSpectrum = Fnu * (Lambda(:) / Obj.RefSpecPivot) .^ Obj.RefSpecSlope;  % [N_lambda x 1]

                % Apply transmission per source: [N_wvl x N_pos] .* [N_wvl x 1]
                SpecTrans = TransPerSource .* RefSpectrum;  % [N_wvl x N_pos]

                % Divide by Lambda for integration
                Integrand = SpecTrans ./ Lambda;  % [N_wvl x N_pos]

                % Integrate along wavelength dimension (dim 1 = along rows/wavelengths)
                A = tools.math.integral.trapzmat(Lambda(:), Integrand, 1);  % [1 x N_pos]

                % Calculate per-source zero-point flux
                TotalFlux_ZP = Obj.Aperture * A / H;  % [1 x N_pos]

                % Convert to per-source magnitude ZP
                ZP = 2.5 * log10(TotalFlux_ZP);  % [1 x N_pos]
                ZP = ZP(:);  % [N_pos x 1]

                % Add position-dependent Tran2D correction if available
                if ~isempty(Args.X) && ~isempty(Args.Y) && ...
                   ~isempty(Obj.TransModel.Tran2DObj) && Obj.TransModel.UseTran2D
                    [FieldCorrectionMag, ~] = Obj.TransModel.Tran2DObj.forward([Args.X(:), Args.Y(:)]);
                    ZP = ZP - FieldCorrectionMag(:);
                end
            else
                % === Single airmass mode (original path) ===
                TransBase = Obj.TransModel.evaluateAllFunParInput(Lambda);
                TransBase = TransBase(:)';  % Row vector [1 x N_lambda]

                % Reference F_nu spectrum: power law (lambda/pivot)^slope.
                % Slope = 0 (default) reduces to the AB-flat reference.
                RefSpectrum = Fnu * (Lambda(:) / Obj.RefSpecPivot) .^ Obj.RefSpecSlope;  % [N_lambda x 1]

                % Apply transmission: multiply by reference spectrum
                SpecTrans = TransBase .* RefSpectrum';  % [1 x N_lambda]

                % Multiply by Lambda for integration
                Integrand = SpecTrans ./ Lambda';  % [1 x N_lambda]

                % Integrate along wavelength dimension
                A = tools.math.integral.trapzmat(Lambda(:)', Integrand, 2);  % scalar

                % Calculate base zero-point flux
                TotalFlux_ZP = Obj.Aperture * A / H;  % scalar

                % Convert to base magnitude ZP
                ZP_base = 2.5 * log10(TotalFlux_ZP);  % scalar

                % Add position-dependent correction if positions provided and Tran2D exists
                if ~isempty(Args.X) && ~isempty(Args.Y) && ...
                   ~isempty(Obj.TransModel.Tran2DObj) && Obj.TransModel.UseTran2D
                    X = Args.X(:);
                    Y = Args.Y(:);

                    % Get field correction in magnitude space from Tran2D
                    [FieldCorrectionMag, ~] = Obj.TransModel.Tran2DObj.forward([X, Y]);
                    FieldCorrectionMag = FieldCorrectionMag(:);  % [N_pos x 1]

                    % ZP at each position = base ZP + field correction
                    ZP = ZP_base - FieldCorrectionMag;
                else
                    ZP = ZP_base;
                end
            end

            % If single position, return scalar
            if length(ZP) == 1
                ZP = ZP(1);
            end
        end

        function [Mag, MagErr] = evaluateMag(Obj, Flux, Args)
            % Evaluate calibrated magnitudes from observed flux
            % Input  : - PhotCalibTrans object.
            %          - Observed flux values [photons] [N x 1].
            %          * ...,key,val,...
            %            'X' - X coordinates [N x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N x 1]. Default is [] (field center).
            %            'MagErr' - Magnitude errors [N x 1]. Default is [].
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            % Output : - Calibrated magnitudes [N x 1].
            %          - Calibrated magnitude errors [N x 1] (optional).
            % Author : D. Kovaleva (Jan 2026)
            % Example: Mag = PC.evaluateMag(Flux);
            %          [Mag, MagErr] = PC.evaluateMag(Flux, 'X', X, 'Y', Y, 'MagErr', MagErr);
            %          Mag = PC.evaluateMag(Flux, 'MagSystem', 'AB');
            % Description: Converts observed flux to calibrated magnitudes.
            %              MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              Uses evaluateZP to calculate position-dependent zero points.
            %              Errors are provided directly (e.g., from MAGERR columns).

            arguments
                Obj
                Flux                 % Observed flux [photons] [N x 1]
                Args.X = []          % X coordinates [N x 1]
                Args.Y = []          % Y coordinates [N x 1]
                Args.MagErr = []     % Magnitude errors [N x 1]
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:evaluateMag:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.ExpTime_eff;

            % Ensure column vectors
            Flux = Flux(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP('X', Args.X, 'Y', Args.Y, ...
                                'MagSystem', Args.MagSystem);
            ZP = ZP(:);  % Ensure column vector

            % Calculate calibrated magnitudes
            % MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            Mag = convert.luptitude(Flux/ExpTime_eff, 10.^(0.4.*ZP));

            % Return magnitude errors if requested
            if nargout > 1
                if isempty(Args.MagErr)
                    % No errors provided
                    MagErr = [];
                else
                    % Use provided magnitude errors directly
                    MagErr = Args.MagErr(:);
                end
            end
        end

        function PredictedFlux = evaluatePredictedFlux(Obj, Args)
            % Evaluate predicted flux for calibrators using fitted transmission model
            % Input  : - PhotCalibTrans object (must have TransModel and SpecData)
            %          * ...,key,val,...
            %            'CostArgs' - Cell array of costFun arguments. Default uses stored data.
            % Output : - Predicted photon counts [N_calib x 1]
            % Author : D. Kovaleva (Jan 2026)
            % Description: Calls costFun with stored or provided CostArgs to calculate predicted flux.
            % Example: PredictedFlux = PC.evaluatePredictedFlux();
            %          PredictedFlux = PC.evaluatePredictedFlux('CostArgs', CustomCostArgs);

            arguments
                Obj
                Args.CostArgs = []
            end

            if isempty(Obj.TransModel) || isempty(Obj.SpecData)
                error('PhotCalibTrans:evaluatePredictedFlux:NoModel', ...
                    'TransModel and SpecData must be populated');
            end

            % Get stored data
            Flux = Obj.SourceData.getCol('Flux');

            % Build default CostArgs if not provided
            if isempty(Args.CostArgs)
                X = Obj.SourceData.getCol('X');
                Y = Obj.SourceData.getCol('Y');
                ExpTime_eff = Obj.ExpTime_eff;
                CostArgs = {'WeightMatrix', Obj.SpecData.Spec', 'TransmissionMode', true, ...
                            'CalibWavelength', Obj.SpecData.SpecWvl, 'ExpTime', ExpTime_eff, ...
                            'Aperture_area_m2', Obj.Aperture, 'X', X, 'Y', Y};
            else
                CostArgs = Args.CostArgs;
            end

            [~, ~, PredictedFlux] = Obj.TransModel.costFun(Obj.TransWvl, Flux, CostArgs{:});
        end

        function ParamsInfo = getMCMCParamsInfo(Obj, Args)
            % Get parameter information for MCMC sampling
            % Description: Extracts parameters that were fitted in ANY optimization
            %              stage (not just current FitPar flags). This ensures MCMC
            %              samples all physically relevant parameters.
            % Input  : - PhotCalibTrans object (must be calibrated)
            %          * ...,key,val,...
            %            'IncludeTran2D' - Include position coefficients. Default is false.
            %            'PosBounds' - Bounds for position coefficients [min, max].
            %                   Default is [-10, 10].
            % Output : - Structure with fields:
            %                   .Names - Cell array of parameter names
            %                   .Values - Current parameter values [N x 1]
            %                   .Min - Lower bounds [N x 1]
            %                   .Max - Upper bounds [N x 1]
            %                   .NumTrans - Number of transmission parameters
            %                   .NumPos - Number of position parameters
            %                   .TransIndices - Indices into Funs structure (for setFreeParamVector)
            %                   .WasFitted - Logical array indicating which were fitted
            % Author : D. Kovaleva (Jan 2026)
            % Example: Info = PC.getMCMCParamsInfo('IncludeTran2D', true);

            arguments
                Obj
                Args.IncludeTran2D logical = false
                Args.PosBounds = [-10, 10]
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:getMCMCParamsInfo:NoModel', ...
                    'TransModel is empty. Run calibration first.');
            end

            OptSeq = Obj.TransModel.OptSeq;

            % Get all parameters via getAllFunPar (consistent with optimization code)
            AllFunPar = Obj.TransModel.getAllFunPar();
            NumAllParams = length(AllFunPar.Val);

            % Collect parameters that were fitted in ANY stage
            Names = {};
            Values = [];
            MinVals = [];
            MaxVals = [];
            TransIndices = [];  % Global indices for setFreeParamVector
            WasFitted = [];

            if ~isempty(OptSeq)
                % Use OptSeq to determine fitted parameters
                % Look up parameters by name directly (same approach as optimization code)
                FittedParamNames = {};

                for IStage = 1:length(OptSeq)
                    Stage = OptSeq(IStage);
                    if ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'JOINT_FC')
                        % Joint Norm + Tran2D linear stage: contributes 'Norm'
                        % to the named-parameter list (Tran2D ParX handled
                        % separately via Tran2DObj.ParX).
                        if ~any(strcmp(FittedParamNames, 'Norm'))
                            FittedParamNames{end+1} = 'Norm'; %#ok<AGROW>
                        end
                    elseif ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'NONLIN_FC')
                        % Joint nonlinear Tran2D stage (Simone-style): fits
                        % only the 10 Tran2D ParX coeffs (handled separately
                        % via Tran2DObj.ParX). No named-parameter contribution.
                    elseif ~isempty(Stage.FreeParams)
                        for IFree = 1:length(Stage.FreeParams)
                            ParamName = Stage.FreeParams(IFree).Parameter;
                            % Add to list if not already present
                            if ~any(strcmp(FittedParamNames, ParamName))
                                FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                            end
                        end
                    end
                end

                % Now find each fitted parameter in AllFunPar by name
                for IParam = 1:length(FittedParamNames)
                    ParamName = FittedParamNames{IParam};
                    % Look up parameter by name (same as CompositeFun.runTransmissionOptimization)
                    Idx = find(strcmp(AllFunPar.Name, ParamName), 1);
                    if ~isempty(Idx)
                        Names{end+1} = ParamName; %#ok<AGROW>
                        Values(end+1) = AllFunPar.Val(Idx); %#ok<AGROW>
                        MinVals(end+1) = AllFunPar.Min(Idx); %#ok<AGROW>
                        MaxVals(end+1) = AllFunPar.Max(Idx); %#ok<AGROW>
                        TransIndices(end+1) = Idx; %#ok<AGROW>
                        WasFitted(end+1) = true; %#ok<AGROW>
                    end
                end
            else
                % No OptSeq, use FitPar flags (fallback)
                for Idx = 1:NumAllParams
                    if AllFunPar.FitPar(Idx)
                        Names{end+1} = AllFunPar.Name{Idx}; %#ok<AGROW>
                        Values(end+1) = AllFunPar.Val(Idx); %#ok<AGROW>
                        MinVals(end+1) = AllFunPar.Min(Idx); %#ok<AGROW>
                        MaxVals(end+1) = AllFunPar.Max(Idx); %#ok<AGROW>
                        TransIndices(end+1) = Idx; %#ok<AGROW>
                        WasFitted(end+1) = true; %#ok<AGROW>
                    end
                end
            end

            NumTrans = length(Names);

            % Add position parameters if requested
            NumPos = 0;
            if Args.IncludeTran2D && Obj.TransModel.UseTran2D && ~isempty(Obj.TransModel.Tran2DObj)
                ParX = Obj.TransModel.Tran2DObj.ParX;
                NCoeff = length(ParX);

                for ICoeff = 1:NCoeff
                    Names{end+1} = sprintf('PosCoeff_%d', ICoeff);  %#ok<AGROW>
                    Values(end+1) = ParX(ICoeff);  %#ok<AGROW>
                    MinVals(end+1) = Args.PosBounds(1);  %#ok<AGROW>
                    MaxVals(end+1) = Args.PosBounds(2);  %#ok<AGROW>
                    % Position parameter indices are offset from transmission parameters
                    TransIndices(end+1) = NumAllParams + ICoeff;  %#ok<AGROW>
                    WasFitted(end+1) = true;  %#ok<AGROW>
                end
                NumPos = NCoeff;
            end

            % Build output structure
            ParamsInfo.Names = Names(:);
            ParamsInfo.Values = Values(:);
            ParamsInfo.Min = MinVals(:);
            ParamsInfo.Max = MaxVals(:);
            ParamsInfo.NumTrans = NumTrans;
            ParamsInfo.NumPos = NumPos;
            ParamsInfo.TransIndices = TransIndices(:);
            ParamsInfo.WasFitted = WasFitted(:);
        end

        function [MagErr, MagErr_spectral, MagErr_flux] = propagateCalibratorMagErr(Obj, Flux, FluxErrVector, Args)
            % Propagate calibrator spectral and flux errors into per-star magnitude uncertainties
            % Description: Combines Gaia XP spectral errors (through reference
            %              transmission) and observed flux errors into a single
            %              MagErr vector, used as weights in the cost function
            %              during optimization. Called once before fitting to
            %              avoid repeated error propagation. Both component
            %              vectors are always computed when the requisite data
            %              are available (independent of WeightingMode) so that
            %              they can be inspected in Cal snapshots.
            % Input  : - PhotCalibTrans object (must have SpecData populated)
            %          - Observed flux values [photons] [N_calib x 1]
            %          - Relative flux errors [N_calib x 1] (can be [])
            %          * ...,key,val,...
            %            'WeightingMode' - Error sources to include in the
            %                   combined fit-weight MagErr:
            %                   'spectral' - Gaia XP spectral errors only
            %                   'flux'     - Observed flux errors only
            %                   'combined' - Quadrature sum of both (default)
            %                   'none'     - No weighting (returns [])
            %            'ExpTime' - Effective exposure time [s]. Default uses Obj.ExpTime/Obj.NCoadd.
            %            'RefTransmissionFun' - Function handle for reference transmission.
            %                   Default is @telescope.optics.refTransmissionLAST.
            %            'FluxErrorNorm' - Effective area scaling for synthetic flux
            %                   in error calculation [dimensionless]. Default is 0.5.
            %            'SystematicErr' - Floor on the returned combined MagErr
            %                   in magnitude units. Applied element-wise as
            %                   MagErr = max(MagErr, SystematicErr). Prevents
            %                   a handful of bright, tiny-formal-error stars
            %                   from dominating the chi^2 when photon-noise
            %                   propagation underestimates the true
            %                   calibration floor (per-star systematics like
            %                   flat-field errors, aperture correction
            %                   residuals, colour-term mismatch). Applied to
            %                   the WeightingMode-combined MagErr only; the
            %                   MagErr_spectral / MagErr_flux component
            %                   vectors are returned unfloored so they still
            %                   reflect the raw propagated errors.
            %                   Default 0.001 mag.
            % Output : - Combined per-calibrator magnitude uncertainties
            %                     [N_calib x 1], selected by WeightingMode,
            %                     or [] if WeightingMode is 'none'.
            %          - MagErr_spectral [N_calib x 1] - spectral-error
            %                     component only, or [] if SpecData unavailable
            %                     or WeightingMode is 'none'.
            %          - MagErr_flux [N_calib x 1] - flux-error component only
            %                     (1.086*FluxErr for 'spectral'/'combined'/no-mode,
            %                     bandpass-propagated for legacy 'flux' mode),
            %                     or [] if FluxErrVector unavailable or
            %                     WeightingMode is 'none'.
            % Author : D. Kovaleva (Jan 2026)
            % Example: [MagErr, MagErr_spec, MagErr_fx] = PC.propagateCalibratorMagErr(Flux, FluxErrVector, 'WeightingMode', 'combined');

            arguments
                Obj
                Flux
                FluxErrVector = []
                Args.WeightingMode = 'combined'
                Args.ExpTime = []
                Args.RefTransmissionFun = @telescope.optics.refTransmissionLAST
                Args.FluxErrorNorm = 0.5
                Args.SystematicErr (1,1) double {mustBeNonnegative} = 0.001
            end

            % Get effective exposure time
            if isempty(Args.ExpTime)
                ExpTime_eff = Obj.ExpTime_eff;
            else
                ExpTime_eff = Args.ExpTime;
            end

            % Ensure column vectors
            Flux = Flux(:);
            N_calib = length(Flux);

            % Initialize outputs
            MagErr = zeros(N_calib, 1);
            MagErr_spectral = [];
            MagErr_flux = [];

            % Check weighting mode
            UseSpectralWeighting = ismember(lower(Args.WeightingMode), {'spectral', 'combined'});
            UseFluxWeighting = ismember(lower(Args.WeightingMode), {'flux', 'combined'});

            if ~UseSpectralWeighting && ~UseFluxWeighting
                % No weighting, return three empties (fast path)
                MagErr = [];
                return;
            end

            % Constants
            H = 6.62607015e-34;  % Planck constant [J*s]
            C = constant.c('SI');  % Speed of light [m/s]
            B = H * C * 1e10;  % H*C with Angstrom to m conversion

            % Get wavelength grid from TransModel or use default
            if ~isempty(Obj.TransWvl)
                SpecWvl_Integration = Obj.TransWvl(:);
            else
                SpecWvl_Integration = (3000:20:11000)';  % Default LAST grid
            end
            SpecWvl_nm = SpecWvl_Integration / 10;  % Convert to nm

            % Compute dLambda for each wavelength bin
            dLambda = diff(SpecWvl_Integration(:));
            dLambda = [dLambda(1); (dLambda(1:end-1) + dLambda(2:end)) / 2; dLambda(end)];

            % Get reference transmission (for error propagation)
            T_ref_vec = Args.RefTransmissionFun(SpecWvl_Integration);  % [N_wvl x 1]

            % Scaling factor 
            NSigma = 3;

            % Spectral error propagation. Computed whenever SpecData is
            % available (independent of WeightingMode) so the component is
            % accessible in Cal snapshots even when only flux weighting is
            % active in the fit weight.
            if ~isempty(Obj.SpecData) && ~isempty(Obj.SpecData.SpecErr)
                SpecErrMatrix = Obj.SpecData.SpecErr';  % [N_wvl x N_calib]
                SpecWvl = Obj.SpecData.SpecWvl(:);

                % Interpolate spectral errors onto integration grid (same as costFun)
                N_integration = length(SpecWvl_Integration);
                SpecWvl_min = min(SpecWvl);
                SpecWvl_max = max(SpecWvl);

                MaskGaia = (SpecWvl_Integration >= SpecWvl_min) & (SpecWvl_Integration <= SpecWvl_max);
                MaskUV = (SpecWvl_Integration <= SpecWvl_min);
                MaskIR = (SpecWvl_Integration >= SpecWvl_max);

                SpecErrInterp = zeros(N_integration, N_calib);
                WvlGaiaRegion = SpecWvl_Integration(MaskGaia);

                for IObs = 1:N_calib
                    SpecErrInterp(MaskGaia, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), WvlGaiaRegion, 'linear');
                    if any(MaskUV)
                        SpecErrInterp(MaskUV, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), SpecWvl_min, 'linear');
                    end
                    if any(MaskIR)
                        SpecErrInterp(MaskIR, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), SpecWvl_max, 'linear');
                    end
                end

                % Error propagation: sigma_Spec * T_ref * Lambda
                T_ref = repmat(T_ref_vec(:), 1, N_calib);
                TransmittedSpectraErr = SpecErrInterp .* T_ref;  % [N_wvl x N_calib]
                TransmittedSpectraErrT = TransmittedSpectraErr';  % [N_calib x N_wvl]
                ErrIntegrand = TransmittedSpectraErrT .* SpecWvl_nm(:)';  % [N_calib x N_wvl]

                % Quadrature sum (scaled by FluxErrorNorm to match model normalization)
                Dt = ExpTime_eff;
                Ageom = Obj.Aperture;
                PredictedFlux_err = Args.FluxErrorNorm * Dt * Ageom * sqrt(sum((NSigma * ErrIntegrand .* dLambda(:)').^2, 2)) / B;
              
                % Convert to magnitude error
                MagErr_spectral = 2.5 * log10(1 + PredictedFlux_err ./ Flux);
                MagErr_spectral(isinf(MagErr_spectral)) = 100;
                MagErr_spectral(isnan(MagErr_spectral)) = 100;
            end

            % Flux error propagation. Computed whenever FluxErrVector is
            % available (independent of WeightingMode). Default form is the
            % simple per-source instrumental MagErr = 1.086 * FluxErr (FluxErr
            % is relative dF/F, see [[fluxerr_relative_convention]]). Legacy
            % 'flux' mode keeps the older bandpass-propagated formula so the
            % fit-weight semantics for that mode are unchanged.
            if ~isempty(FluxErrVector)
                FluxErrVector = FluxErrVector(:);
                if length(FluxErrVector) == N_calib
                    if strcmpi(Args.WeightingMode, 'flux')
                        T_lambda_dlambda = T_ref_vec(:) .* SpecWvl_nm(:) .* dLambda(:);
                        BandpassNorm = sum(T_lambda_dlambda);
                        BandpassQuad = sqrt(sum(T_lambda_dlambda.^2));
                        BandpassFactor = BandpassQuad / BandpassNorm;
                        FluxErrPropagated = NSigma * FluxErrVector .* BandpassFactor;
                        MagErr_flux = 2.5 * log10(1 + FluxErrPropagated);
                    else
                        MagErr_flux = 1.086 * FluxErrVector;
                    end
                    MagErr_flux(~isfinite(MagErr_flux) | MagErr_flux <= 0) = 100;
                end
            end

            % Combine components into fit-weight MagErr according to the
            % requested WeightingMode. The two component vectors above are
            % returned in full regardless of this selection.
            if UseSpectralWeighting && UseFluxWeighting && ~isempty(MagErr_spectral) && ~isempty(MagErr_flux)
                MagErr = sqrt(MagErr_spectral.^2 + MagErr_flux.^2);
            elseif UseSpectralWeighting && ~isempty(MagErr_spectral)
                MagErr = MagErr_spectral;
            elseif UseFluxWeighting && ~isempty(MagErr_flux)
                MagErr = MagErr_flux;
            else
                MagErr = [];
            end

            % Systematic-error floor on the returned combined MagErr.
            % Skipped when MagErr is empty ('none' mode).
            if ~isempty(MagErr) && Args.SystematicErr > 0
                MagErr = max(MagErr, Args.SystematicErr);
            end
        end

        function SpecFluxMatrix = resampleCalibratorSpectra(Obj, Args)
            % Resample calibrator reference spectra onto the transmission model wavelength grid
            % Description: Interpolates Gaia XP spectra within their native range
            %              (3360-10200 Angstrom) and extrapolates with constant
            %              boundary values outside. Called once before fitting to
            %              avoid repeated interpolation in costFun.
            % Input  : - PhotCalibTrans object (must have SpecData populated)
            %          * ...,key,val,...
            %            'TransWvl' - Transmission wavelength grid [Angstrom]. Default uses Obj.TransWvl.
            % Output : - Resampled spectra [N_TransWvl x N_calib]
            %                             on the transmission model wavelength grid
            % Author : D. Kovaleva (Feb 2026)
            % Example: SpecFluxMatrix = PC.resampleCalibratorSpectra();
            %          % Pass to costFun via CostArgs:
            %          CostArgs = {..., 'PrecomputedSpecFluxMatrix', SpecFluxMatrix, ...};

            arguments
                Obj
                Args.TransWvl = []
            end

            % Get transmission wavelength grid
            if isempty(Args.TransWvl)
                TransWvl = Obj.TransWvl(:);
            else
                TransWvl = Args.TransWvl(:);
            end

            % Check that SpecData is populated
            if isempty(Obj.SpecData) || isempty(Obj.SpecData.Spec)
                Obj.msgLog(LogLevel.Error, 'resampleCalibratorSpectra: SpecData.Spec is empty - run selectCalibrators first');
                SpecFluxMatrix = [];
                return;
            end

            % Get calibrator spectra and wavelength grid
            Spec = Obj.SpecData.Spec';  % [N_SpecWvl x N_calib] (transpose from [N_calib x N_SpecWvl])
            SpecWvl = Obj.SpecData.SpecWvl(:);

            Ninput = length(TransWvl);
            NCalib = size(Spec, 2);

            % Calibrator spectral boundaries (e.g., Gaia XP: 3360-10200 Angstrom)
            SpecWvlMin = min(SpecWvl);
            SpecWvlMax = max(SpecWvl);

            % Wavelength region masks for extrapolation
            MaskGaia = (TransWvl >= SpecWvlMin) & (TransWvl <= SpecWvlMax);
            MaskUV = (TransWvl < SpecWvlMin);
            MaskIR = (TransWvl > SpecWvlMax);
            WvlGaiaRegion = TransWvl(MaskGaia);

            % Interpolate calibrator spectra onto transmission grid (vectorized)
            SpecFluxMatrix = zeros(Ninput, NCalib);
            SpecFluxMatrix(MaskGaia, :) = interp1(SpecWvl, Spec, WvlGaiaRegion, 'linear');

            % UV/IR extrapolation: constant boundary values
            if any(MaskUV)
                EdgeValuesUV = interp1(SpecWvl, Spec, SpecWvlMin, 'linear');
                SpecFluxMatrix(MaskUV, :) = repmat(EdgeValuesUV, sum(MaskUV), 1);
            end
            if any(MaskIR)
                EdgeValuesIR = interp1(SpecWvl, Spec, SpecWvlMax, 'linear');
                SpecFluxMatrix(MaskIR, :) = repmat(EdgeValuesIR, sum(MaskIR), 1);
            end
        end
    end


    methods % Header I/O methods
        function HeaderObj = photCalibTransToHeader(Obj, HeaderObj, Args)
            % Write calibration data to AstroHeader
            % Input  : - PhotCalibTrans object
            %          - AstroHeader object
            %          * ...,key,val,...
            %            'WriteComments' - Add explanatory comments to keywords. Default is false.
            % Output : - Updated AstroHeader object with PT_* keywords
            % Author : D. Kovaleva (Jan 2026)
            % Example: Header = PC.photCalibTransToHeader(Header);
            %          Header = PC.photCalibTransToHeader(Header, 'WriteComments', true);
            % Description: Writes calibration results and fitted parameters to header.
            %              Keywords: PT_RMS, PT_ARMS, PT_CHI2, PT_DOF, PT_NCALIB,
            %                        PT_AREF, PT_SPEC,
            %                        PT_X_N, PT_X_VY, PT_X_FY (function parameters),
            %                        PT_P_N, PT_P_VY, PT_P_FY (position corrections if UseTran2D=true),
            %                        APCOR_* (aperture corrections, if AperCorr populated),
            %                        PT_DZP (constant-band delta ZP, if DeltaZP_CB finite),
            %                        LIMMAG (if LimMag finite), BACKMAG (if BackMag finite).

            arguments
                Obj
                HeaderObj
                Args.WriteComments logical = false
            end

            % Preallocate history comments array if requested
            if Args.WriteComments
                % Estimate max size: 8 (general) + 10*11 (functions) + 101 (position) = ~220
                HistoryComments = cell(1, 300);
                IComment = 0;
            end

            % Remove all existing PT_* and APCOR_* keywords to ensure clean ordering
            HeaderObj = HeaderObj.deleteKey({'PT_.*', 'APCOR.*'});

            % Pre-extract scalar values with NaN fallbacks. An uncalibrated PC
            % (e.g. selectCalibrators failed with missing RA/Dec) has an empty
            % TransModel — dot-indexing it would error. Header keys still get
            % written so downstream consumers find them; values are NaN.
            if isempty(Obj.TransModel)
                RMSval  = NaN;
                Chi2val = NaN;
                DOFval  = NaN;
            else
                RMSval  = Obj.TransModel.RMS;
                Chi2val = Obj.TransModel.Chi2;
                DOFval  = Obj.TransModel.DOF;
            end

            % General results
            HeaderObj = HeaderObj.replaceVal('PT_RMS',  RMSval);
            HeaderObj = HeaderObj.replaceVal('PT_ARMS', Obj.ARMS);
            HeaderObj = HeaderObj.replaceVal('PT_CHI2', Chi2val);
            HeaderObj = HeaderObj.replaceVal('PT_DOF',  DOFval);
            % Use final calibrator count (after sigma clipping) from last stage.
            % NaN when nothing was fit.
            if ~isempty(Obj.FitResults)
                if numel(Obj.FitResults) > 1
                    NCalFinal = Obj.FitResults(end).NCalUsed;
                else
                    NCalFinal = Obj.FitResults.NCalUsed;
                end
            elseif ~isempty(Obj.SpecData) && ~isempty(Obj.SpecData.Spec)
                NCalFinal = size(Obj.SpecData.Spec, 1);  % Fallback to initial
            else
                NCalFinal = NaN;
            end
            HeaderObj = HeaderObj.replaceVal('PT_NCALIB', NCalFinal);
            HeaderObj = HeaderObj.replaceVal('PT_AREF', 'SMART v2.9.8');
            HeaderObj = HeaderObj.replaceVal('PT_SPEC', 'GaiaDR3');
            HeaderObj = HeaderObj.replaceVal('PT_REFSL', Obj.RefSpecSlope);
            HeaderObj = HeaderObj.replaceVal('PT_REFPV', Obj.RefSpecPivot);
            HeaderObj = HeaderObj.replaceVal('PT_CO2PPM', Obj.Co2_ppm);

            if Args.WriteComments
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_RMS: RMS of calibration fit [mag]';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_ARMS: sqrt(median(R^2)) of N brightest calibrators [mag]';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_CHI2: Chi-squared of fit';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_DOF: Degrees of freedom';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_NCALIB: Number of calibrators';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_AREF: Atmospheric model reference';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_SPEC: Spectra reference';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_REFSL: Ref spectrum F_nu slope (lambda/PT_REFPV)^slope';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_REFPV: Ref spectrum pivot wavelength [Angstrom]';
            end

            % Function parameters — only writable when TransModel is populated.
            % An uncalibrated PC writes only the scalar PT_* keys above (as NaN);
            % the per-function PT_<I>_<V|F><P> keys are skipped because we have
            % no model structure to enumerate.
            if isempty(Obj.TransModel)
                Funs = [];
                NFuns = 0;
            else
                Funs = Obj.TransModel.Funs;
                NFuns = length(Funs);
            end

            % Pre-compute fitted parameter names from OptSeq
            FittedParamNames = {};
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.OptSeq)
                % Recipe (scalar struct with .Stages) vs legacy struct-array.
                if isscalar(Obj.TransModel.OptSeq) && ...
                        isstruct(Obj.TransModel.OptSeq) && ...
                        isfield(Obj.TransModel.OptSeq, 'Stages')
                    StagesForNames = Obj.TransModel.OptSeq.Stages;
                else
                    StagesForNames = Obj.TransModel.OptSeq;
                end
                for IStage = 1:length(StagesForNames)
                    Stage = StagesForNames(IStage);
                    if ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'JOINT_FC')
                        % Joint Norm + Tran2D linear stage: contributes 'Norm'.
                        if ~any(strcmp(FittedParamNames, 'Norm'))
                            FittedParamNames{end+1} = 'Norm'; %#ok<AGROW>
                        end
                    elseif ischar(Stage.FreeParams) && strcmpi(Stage.FreeParams, 'NONLIN_FC')
                        % Joint nonlinear Tran2D stage: only the 10 Tran2D
                        % ParX coeffs are fitted; no named parameter.
                    elseif ~isempty(Stage.FreeParams)
                        for IFree = 1:length(Stage.FreeParams)
                            ParamName = Stage.FreeParams(IFree).Parameter;
                            if ~any(strcmp(FittedParamNames, ParamName))
                                FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                            end
                        end
                    end
                end
            end

            for IFun = 1:NFuns
                Fun = Funs(IFun);

                % Function reference
                FunRef = func2str(Fun.Handle);
                KeyName = sprintf('PT_%d_N', IFun);
                HeaderObj = HeaderObj.replaceVal(KeyName, FunRef);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = sprintf('%s: %s function', KeyName, Fun.Desc);
                end

                % Parameters
                NPar = length(Fun.Par);
                for IPar = 1:NPar
                    % Value
                    KeyName = sprintf('PT_%d_V%d', IFun, IPar);
                    HeaderObj = HeaderObj.replaceVal(KeyName, Fun.Par(IPar));

                    % Get parameter name from ArgNames if available
                    if ~isempty(Fun.ArgNames) && IPar <= length(Fun.ArgNames)
                        ParName = Fun.ArgNames(IPar).Description;
                    else
                        ParName = sprintf('%s_Par%d', Fun.Desc, IPar);
                    end

                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: %s [%s]', KeyName, Fun.Desc, ParName);
                    end

                    % Fit flag - check if parameter name is in fitted list
                    KeyName = sprintf('PT_%d_F%d', IFun, IPar);

                    % Determine if parameter was ever freed during optimization
                    if ~isempty(Obj.TransModel.OptSeq)
                        % Check if parameter name is in the fitted list
                        WasFitted = any(strcmp(FittedParamNames, ParName));
                    else
                        % No OptSeq defined, use initial FitPar configuration
                        WasFitted = Fun.FitPar(IPar);
                    end

                    FitFlag = double(WasFitted);
                    HeaderObj = HeaderObj.replaceVal(KeyName, FitFlag);
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Fit flag (1=fitted in any stage, 0=always fixed)', KeyName);
                    end
                end
            end

            % Position-dependent corrections (only if UseTran2D = true and
            % a TransModel exists — uncalibrated PCs skip this block).
            if ~isempty(Obj.TransModel) && Obj.TransModel.UseTran2D
                % Type
                HeaderObj = HeaderObj.replaceVal('PT_P_N', Obj.TransModel.NameTran2D);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = 'PT_P_N: Position correction type';
                end

                % Coefficients
                ParX = Obj.TransModel.Tran2DObj.ParX;
                NCoeff = length(ParX);

                for ICoeff = 1:NCoeff
                    % Value
                    KeyName = sprintf('PT_P_V%d', ICoeff);
                    HeaderObj = HeaderObj.replaceVal(KeyName, ParX(ICoeff));
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Coefficient %d of position-dependent correction', KeyName, ICoeff);
                    end

                    % Fit flag (all coefficients of position-dependent correction are fitted if UseTran2D=true)
                    KeyName = sprintf('PT_P_F%d', ICoeff);
                    HeaderObj = HeaderObj.replaceVal(KeyName, 1);
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Fit flag (1=fitted, 0=fixed)', KeyName);
                    end
                end
            end

            % Aperture corrections (NaN written when calculation failed)
            if ~isempty(Obj.AperCorr)
                for Iaper = 1:length(Obj.AperCorr)
                    KeyName = PhotCalibTrans.fluxCol2AperCorrKey(Obj.AperCorrColNames{Iaper});
                    HeaderObj = HeaderObj.replaceVal(KeyName, Obj.AperCorr(Iaper));
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Aperture correction for %s [mag]', KeyName, Obj.AperCorrColNames{Iaper});
                    end
                end
                HeaderObj = HeaderObj.replaceVal('APCOR_N', Obj.AperCorrNStars);
            end

            % Constant-band delta ZP
            if isfinite(Obj.DeltaZP_CB)
                HeaderObj = HeaderObj.replaceVal('PT_DZP', Obj.DeltaZP_CB);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = 'PT_DZP: Constant-band delta ZP [mag]';
                end
            end

            % Limiting magnitude and sky surface brightness (legacy keyword names)
            if isfinite(Obj.LimMag)
                HeaderObj = HeaderObj.replaceVal('LIMMAG', Obj.LimMag);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = 'LIMMAG: Limiting magnitude at SN=5 [mag]';
                end
            end
            if isfinite(Obj.BackMag)
                HeaderObj = HeaderObj.replaceVal('BACKMAG', Obj.BackMag);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = 'BACKMAG: Sky background surface brightness [mag/arcsec^2]';
                end
            end

            % Write HISTORY comments at the end if requested
            if Args.WriteComments
                % Trim to actual size
                HistoryComments = HistoryComments(1:IComment);
                for I = 1:IComment
                    HeaderObj = HeaderObj.insertKey({'HISTORY', HistoryComments{I}}, Inf);
                end
            end
        end

        function Obj = photCalibTransFromHeader(Obj, HeaderObj, Args)
            % Populate PhotCalibTrans object from AstroHeader
            % Input  : - PhotCalibTrans object (existing)
            %          - HeaderObj - AstroHeader object with PT_* keywords
            %          * ...,key,val,...
            % Output : - PhotCalibTrans object populated from header
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.photCalibTransFromHeader(Header);
            % Description: Reads calibration results and fitted parameters from header.
            %              Populates existing PhotCalibTrans object with stored data.

            arguments
                Obj
                HeaderObj
                Args.Dummy = []  % Reserved for future arguments
            end

            % Create TransModel if empty
            if isempty(Obj.TransModel)
                Obj.TransModel = tools.math.fun.CompositeFun();
            end

            % General results
            if HeaderObj.isKeyExist('PT_RMS')
                Obj.TransModel.RMS = HeaderObj.getVal('PT_RMS');
            end
            if HeaderObj.isKeyExist('PT_CHI2')
                Obj.TransModel.Chi2 = HeaderObj.getVal('PT_CHI2');
            end
            if HeaderObj.isKeyExist('PT_DOF')
                Obj.TransModel.DOF = HeaderObj.getVal('PT_DOF');
            end

            % Limiting magnitude and sky surface brightness (legacy keywords)
            if HeaderObj.isKeyExist('LIMMAG')
                Obj.LimMag = HeaderObj.getVal('LIMMAG');
            end
            if HeaderObj.isKeyExist('BACKMAG')
                Obj.BackMag = HeaderObj.getVal('BACKMAG');
            end

            % Reference-spectrum slope / pivot (default to AB-flat if absent
            % so headers written before this convention still load cleanly).
            if HeaderObj.isKeyExist('PT_REFSL')
                Obj.RefSpecSlope = HeaderObj.getVal('PT_REFSL');
            end
            if HeaderObj.isKeyExist('PT_REFPV')
                Obj.RefSpecPivot = HeaderObj.getVal('PT_REFPV');
            end

            % CO2 abundance (Simone-parity default 395 ppm when absent so
            % FITS written before step-1 UMG ParamMatrix expansion still
            % load cleanly).
            if HeaderObj.isKeyExist('PT_CO2PPM')
                Val = HeaderObj.getVal('PT_CO2PPM');
                if isnumeric(Val) && ~isnan(Val)
                    Obj.Co2_ppm = Val;
                end
            end

            % Observation metadata - read from standard FITS keywords if available
            % Override defaults only if value exists and is not NaN
            if HeaderObj.isKeyExist('AIRMASS')
                Val = HeaderObj.getVal('AIRMASS');
                if ~isnan(Val)
                    Obj.AirMass = Val;
                end
            end

            if HeaderObj.isKeyExist('MNTTEMP')
                Val = HeaderObj.getVal('MNTTEMP');
                if ~isnan(Val)
                    Obj.Temp = Val;
                end
            end

            if HeaderObj.isKeyExist('PRESSURE')
                Val = HeaderObj.getVal('PRESSURE');
                if ~isnan(Val)
                    Obj.Pressure = Val;
                end
            end

            if HeaderObj.isKeyExist('HUMIDITY')
                Val = HeaderObj.getVal('HUMIDITY');
                if ~isnan(Val)
                    Obj.Humidity = Val;
                end
            end

            if HeaderObj.isKeyExist('APERTURE')
                Val = HeaderObj.getVal('APERTURE');
                if ~isnan(Val)
                    Obj.Aperture = Val;
                end
            end

            if HeaderObj.isKeyExist('EXPTIME')
                Val = HeaderObj.getVal('EXPTIME');
                if ~isnan(Val)
                    Obj.ExpTime = Val;
                end
            end

            if HeaderObj.isKeyExist('NCOADD')
                Val = HeaderObj.getVal('NCOADD');
                if ~isnan(Val)
                    Obj.NCoadd = Val;
                end
            end

            % Function parameters - read function list
            IFun = 1;
            while true
                KeyName = sprintf('PT_%d_N', IFun);
                if ~HeaderObj.isKeyExist(KeyName)
                    break;
                end

                FunRef = HeaderObj.getVal(KeyName);

                % Initialize function entry
                Obj.TransModel.Funs(IFun).Name = IFun;
                Obj.TransModel.Funs(IFun).Desc = '';
                Obj.TransModel.Funs(IFun).Handle = str2func(FunRef);
                Obj.TransModel.Funs(IFun).Par = [];
                Obj.TransModel.Funs(IFun).FitPar = [];
                Obj.TransModel.Funs(IFun).OptionalArgs = {};
                Obj.TransModel.Funs(IFun).ArgNames = [];
                Obj.TransModel.Funs(IFun).ArgMapping = [];
                Obj.TransModel.Funs(IFun).PreCalc = [];

                % Read parameters (build arrays from scratch)
                % Preallocate for max expected parameters per function (e.g., 20)
                ParValues = zeros(1, 20);
                FitFlags = false(1, 20);
                IPar = 1;
                while true
                    KeyNameV = sprintf('PT_%d_V%d', IFun, IPar);
                    if ~HeaderObj.isKeyExist(KeyNameV)
                        break;
                    end

                    % Read parameter value
                    ParValues(IPar) = HeaderObj.getVal(KeyNameV);

                    % Read fit flag
                    KeyNameF = sprintf('PT_%d_F%d', IFun, IPar);
                    if HeaderObj.isKeyExist(KeyNameF)
                        FitFlags(IPar) = logical(HeaderObj.getVal(KeyNameF));
                    else
                        FitFlags(IPar) = false;  % Default to fixed if not specified
                    end

                    IPar = IPar + 1;
                end

                % Trim to actual size and store parameters and fit flags
                if IPar > 1
                    Obj.TransModel.Funs(IFun).Par = ParValues(1:IPar-1);
                    Obj.TransModel.Funs(IFun).FitPar = FitFlags(1:IPar-1);
                else
                    Obj.TransModel.Funs(IFun).Par = [];
                    Obj.TransModel.Funs(IFun).FitPar = [];
                end

                IFun = IFun + 1;
            end

            % Position-dependent corrections
            if HeaderObj.isKeyExist('PT_P_N')
                Tran2DType = HeaderObj.getVal('PT_P_N');
                if ~isempty(Tran2DType)
                    Obj.TransModel.UseTran2D = true;
                    Obj.TransModel.NameTran2D = Tran2DType;

                    % Create Tran2D object
                    Obj.TransModel.Tran2DObj = Tran2D(Tran2DType);

                    % Read coefficients
                    ICoeff = 1;
                    % Preallocate for max expected coefficients (e.g., 100)
                    ParX = zeros(1, 100);
                    while true
                        KeyName = sprintf('PT_P_V%d', ICoeff);
                        if ~HeaderObj.isKeyExist(KeyName)
                            break;
                        end
                        ParX(ICoeff) = HeaderObj.getVal(KeyName);
                        ICoeff = ICoeff + 1;
                    end
                    % Trim to actual size
                    if ICoeff > 1
                        ParX = ParX(1:ICoeff-1);
                    else
                        ParX = [];
                    end

                    % Set coefficients
                    if ~isempty(ParX)
                        Obj.TransModel.Tran2DObj.ParX = ParX;
                        % Seed ParY = zeros(size(ParX)) so isParKnown(TC)
                        % returns true on subsequent forward() calls. The
                        % writer (photCalibTransToHeader) only serialises
                        % ParX — for cheby1_4_xt / cheby1_4_xt_constrainedxy
                        % the LAST calibration uses ParX exclusively and
                        % evaluateZP discards forward()'s Yf=Hy*ParY output,
                        % so a zero seed is numerically identical to the
                        % pre-serialisation state and only needed to satisfy
                        % the (NparY==NfunY) gate inside isParKnown.
                        Obj.TransModel.Tran2DObj.ParY = zeros(size(ParX));
                    end
                else
                    Obj.TransModel.UseTran2D = false;
                end
            else
                Obj.TransModel.UseTran2D = false;
            end

            % Set CalFound based on PT_NCALIB
            if HeaderObj.isKeyExist('PT_NCALIB')
                Val = HeaderObj.getVal('PT_NCALIB');
                if ~isnan(Val) && Val > 0
                    Obj.CalFound = true;
                else
                    Obj.CalFound = false;
                end
            end
        end
    end

    methods % Catalog operations
        function Obj = calcAperCorr(Obj, CatObj, Args)
            % Calculate aperture corrections vs a reference flux/magnitude column.
            %   For each flux column matching AperFluxPrefix, computes the
            %   magnitude offset relative to RefFluxCol. The reference column
            %   itself gets AperCorr = 0.
            %   Results are stored on the object (AperCorr, AperCorrColNames,
            %   AperCorrNStars). Orchestrators (e.g., fitPhotCalibTrans)
            %   apply the corrections to existing MAG_<System>_* columns
            %   after this method runs. photCalibTransToHeader writes
            %   APCOR_A1, APCOR_A2, APCOR_A3, APCOR_PS, APCOR_N keywords
            %   to the FITS header. NaN is written when calculation failed.
            %   On failure (missing columns, too few stars), AperCorr is set
            %   to NaN and a warning is issued via msgLog (no stdout).
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object with flux/magnitude columns.
            %          * ...,key,val,...
            %            'RefFluxCol'     - Reference flux column name.
            %                        Correction for this column is 0 by definition.
            %                        Default is 'FLUX_APER_3'.
            %            'AperFluxPrefix' - Prefix for identifying flux columns
            %                        in the catalog. Default is 'FLUX_'.
            %                        Note: when CalcCorrType='mag', the magnitude
            %                        columns are located by replacing 'FLUX_'
            %                        with the object's MagColPrefix property
            %                        (so it matches whatever prefix addMag used).
            %            'SNColName'      - S/N column name for filtering. Default is 'SN'.
            %            'MinSN'          - Minimum S/N for star selection. Default is 30.
            %            'MaxSN'          - Maximum S/N. Default is Inf.
            %            'Method'         - 'median' or 'weighted'. Default is 'median'.
            %            'CalcCorrType'   - 'flux' (default) computes
            %                        -2.5*log10(median(FLUX_i / FLUX_ref)) from
            %                        pristine FLUX columns; 'mag' reads MAG_*
            %                        columns (may be pre-corrected by prior
            %                        pipeline steps — use with caution).
            %                        Default is 'flux'.
            %            'UpdateMagIfFail' - If true, NaN corrections propagate
            %                        to magnitudes in addMag. Default is true.
            %            'Verbose'        - Enable verbose output. Default is false.
            % Output : - PhotCalibTrans object with AperCorr, AperCorrColNames,
            %            and AperCorrNStars populated.
            % Author : D. Kovaleva (Mar 2026)
            % Example: PC = PC.calcAperCorr(Cat);
            %          PC = PC.calcAperCorr(Cat, 'RefFluxCol', 'FLUX_PSF');
            %          PC = PC.calcAperCorr(Cat, 'CalcCorrType', 'flux');
            %          PC = PC.calcAperCorr(Cat, 'Method', 'weighted', 'MinSN', 50);

            arguments
                Obj
                CatObj
                Args.RefFluxCol = 'FLUX_APER_3'
                Args.AperFluxPrefix = 'FLUX_'
                Args.SNColName = 'SN'
                Args.MinSN = 30
                Args.MaxSN = Inf
                Args.Method = 'median'
                Args.CalcCorrType = 'mag'            % 'mag' or 'flux'
                Args.UpdateMagIfFail logical = true  % If true, NaN correction propagates to magnitudes
                Args.Verbose logical = false
            end

            % Get column names
            AllColNames = CatObj.Table.Properties.VariableNames;

            % Find aperture flux columns (exclude FLUX_XYPEAK — not photometric)
            AperCols = AllColNames(startsWith(AllColNames, Args.AperFluxPrefix));
            AperCols = AperCols(~strcmp(AperCols, 'FLUX_XYPEAK'));
            AperCols = sort(AperCols);
            % Keep reference in the list (correction = 0 by definition)
            Naper = numel(AperCols);

            % Helper: build NaN vector with 0 at reference position
            RefIdx = find(strcmp(AperCols, Args.RefFluxCol), 1);
            function V = nanVecWithRefZero(N)
                V = nan(1, N);
                if ~isempty(RefIdx); V(RefIdx) = 0; end
            end

            % Check that reference flux column exists
            if ~ismember(Args.RefFluxCol, AllColNames)
                Msg = sprintf('calcAperCorr: %s column not found - aperture corrections set to NaN', Args.RefFluxCol);
                Obj.msgLog(LogLevel.Warning, Msg);
                Obj.AperCorr = nanVecWithRefZero(Naper);
                Obj.AperCorrColNames = AperCols;
                Obj.AperCorrNStars = 0;
                return;
            end

            if Naper == 0
                Msg = sprintf('calcAperCorr: No %s* columns found - aperture corrections not computed', Args.AperFluxPrefix);
                Obj.msgLog(LogLevel.Warning, Msg);
                Obj.AperCorr = [];
                Obj.AperCorrColNames = {};
                Obj.AperCorrNStars = 0;
                return;
            end

            % Filter by S/N
            if ismember(Args.SNColName, AllColNames)
                SN = CatObj.getCol(Args.SNColName);
                Mask = SN > Args.MinSN & SN < Args.MaxSN;
            else
                Msg = sprintf('calcAperCorr: S/N column %s not found - using all sources', Args.SNColName);
                Obj.msgLog(LogLevel.Warning, Msg);
                Mask = true(CatObj.sizeCatalog, 1);
            end

            NStars = sum(Mask);
            if NStars < 5
                Msg = sprintf('calcAperCorr: Only %d high-S/N stars - aperture corrections set to NaN', NStars);
                Obj.msgLog(LogLevel.Warning, Msg);
                Obj.AperCorr = nanVecWithRefZero(Naper);
                Obj.AperCorrColNames = AperCols;
                Obj.AperCorrNStars = NStars;
                return;
            end

            % Calculate aperture correction for each aperture
            AperCorrVec = zeros(1, Naper);
            UseMag = strcmpi(Args.CalcCorrType, 'mag');

            if UseMag
                % Derive reference magnitude column from flux column name
                RefMagCol = strrep(Args.RefFluxCol, 'FLUX_', Obj.MagColPrefix);
                if ~ismember(RefMagCol, AllColNames)
                    Msg = sprintf('calcAperCorr: %s column not found for mag mode - aperture corrections set to NaN', RefMagCol);
                    Obj.msgLog(LogLevel.Warning, Msg);
                    Obj.AperCorr = nanVecWithRefZero(Naper);
                    Obj.AperCorrColNames = AperCols;
                    Obj.AperCorrNStars = 0;
                    return;
                end
                MagRef = CatObj.getCol(RefMagCol);
                MagRef = MagRef(Mask);
            else
                FluxRef = CatObj.getCol(Args.RefFluxCol);
                FluxRef = FluxRef(Mask);
            end

            for Iaper = 1:Naper
                % Reference column: correction is 0 by definition
                if strcmp(AperCols{Iaper}, Args.RefFluxCol)
                    AperCorrVec(Iaper) = 0;
                    continue;
                end

                if UseMag
                    % Read magnitude columns directly
                    AperMagCol = strrep(AperCols{Iaper}, 'FLUX_', Obj.MagColPrefix);
                    if ~ismember(AperMagCol, AllColNames)
                        AperCorrVec(Iaper) = NaN;
                        continue;
                    end
                    MagAper = CatObj.getCol(AperMagCol);
                    MagAper = MagAper(Mask);

                    % Sign: AperCorr = MagRef - MagAper
                    % (negative for smaller apertures; applied as Mag + AperCorr)
                    MagDiff = MagRef - MagAper;
                    Valid = isfinite(MagDiff);

                    if sum(Valid) < 5
                        AperCorrVec(Iaper) = NaN;
                        continue;
                    end

                    switch lower(Args.Method)
                        case 'median'
                            AperCorrVec(Iaper) = median(MagDiff(Valid), 'omitnan');
                        case 'weighted'
                            MagErrColName = strrep(AperMagCol, 'MAG_', 'MAGERR_');
                            RefErrColName = strrep(RefMagCol, 'MAG_', 'MAGERR_');
                            if ismember(MagErrColName, AllColNames) && ismember(RefErrColName, AllColNames)
                                MagAperErr = CatObj.getCol(MagErrColName);
                                MagRefErr  = CatObj.getCol(RefErrColName);
                                MagAperErr = MagAperErr(Mask); MagAperErr = MagAperErr(Valid);
                                MagRefErr  = MagRefErr(Mask);  MagRefErr  = MagRefErr(Valid);
                                MagDiffErr = sqrt(MagAperErr.^2 + MagRefErr.^2);
                                AperCorrVec(Iaper) = tools.math.stat.wmedian(MagDiff(Valid), MagDiffErr, 1);
                            else
                                AperCorrVec(Iaper) = median(MagDiff(Valid), 'omitnan');
                            end
                    end

                else
                    % Flux-based calculation
                    FluxAper = CatObj.getCol(AperCols{Iaper});
                    FluxAper = FluxAper(Mask);

                    Ratio = FluxAper ./ FluxRef;
                    ValidRatio = Ratio > 0 & isfinite(Ratio);

                    if sum(ValidRatio) < 5
                        AperCorrVec(Iaper) = NaN;
                        continue;
                    end

                    Ratio = Ratio(ValidRatio);

                    switch lower(Args.Method)
                        case 'median'
                            % Sign: AperCorr = 2.5*log10(<FluxAper/FluxRef>)
                            % (negative for smaller apertures; applied as Mag + AperCorr)
                            AperCorrVec(Iaper) = 2.5 * log10(median(Ratio, 'omitnan'));
                        case 'weighted'
                            MagDiff = 2.5 * log10(Ratio);
                            FluxErrColName = strrep(AperCols{Iaper}, 'FLUX_', 'FLUXERR_');
                            RefErrColName = strrep(Args.RefFluxCol, 'FLUX_', 'FLUXERR_');
                            if ismember(FluxErrColName, AllColNames) && ismember(RefErrColName, AllColNames)
                                FluxAperErr = CatObj.getCol(FluxErrColName);
                                FluxRefErr  = CatObj.getCol(RefErrColName);
                                FluxAperErr = FluxAperErr(Mask); FluxAperErr = FluxAperErr(ValidRatio);
                                FluxRefErr  = FluxRefErr(Mask);  FluxRefErr  = FluxRefErr(ValidRatio);
                                % FLUXERR is the relative flux uncertainty (FluxErr/Flux),
                                % so the relative error of the ratio adds them in quadrature directly.
                                RelErr = sqrt(FluxAperErr.^2 + FluxRefErr.^2);
                                MagErr = 1.086 .* RelErr;
                                AperCorrVec(Iaper) = tools.math.stat.wmedian(MagDiff, MagErr, 1);
                            else
                                AperCorrVec(Iaper) = median(MagDiff, 'omitnan');
                            end
                    end
                end
            end

            % Store results — column names reflect mode:
            % <MagColPrefix>* in 'mag' mode, FLUX_* in 'flux' mode.
            Obj.AperCorr = AperCorrVec;
            if UseMag
                Obj.AperCorrColNames = cellfun(@(C) strrep(C, 'FLUX_', Obj.MagColPrefix), ...
                    AperCols, 'UniformOutput', false);
            else
                Obj.AperCorrColNames = AperCols;
            end
            Obj.AperCorrNStars = NStars;

            if Args.Verbose
                fprintf('  Aperture corrections vs %s (N=%d stars):\n', Args.RefFluxCol, NStars);
                for Iaper = 1:Naper
                    fprintf('    %s: %+.4f mag\n', AperCols{Iaper}, AperCorrVec(Iaper));
                end
            end
        end

        function CatObj = addMag(Obj, CatObj, Args)
            % Add calibrated magnitude columns to catalog
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object with flux measurements.
            %          * ...,key,val,...
            %            'FluxColNames' - Flux column names to calibrate.
            %                             Default is all FLUX_* columns.
            %            'ApplyPosCorrection' - Apply position-dependent
            %                                   corrections. Default is true.
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            %            'AddMagErr' - Add magnitude error columns. Default is true.
            %                         Error formula: MagErr = 1.086 * FluxErr
            %                         (FLUXERR is the relative flux uncertainty FluxErr/Flux).
            %                         Requires FLUXERR_<suffix> columns in catalog.
            %                         Column naming: leading 'MAG_' of the
            %                         calibrated mag column name is replaced
            %                         with 'MAGERR_' (e.g. MAG_AB_APER_3 ->
            %                         MAGERR_AB_APER_3, MAG_PSF -> MAGERR_PSF).
            %            'PropagateCalibratedErr' - Propagate calibrated magnitude
            %                         errors. Default is false. Not yet implemented.
            % Output : - AstroCatalog with added calibrated magnitude columns.
            %                     Column naming: FLUX_<suffix> -> <prefix><suffix>,
            %                     where <prefix> is the object's MagColPrefix
            %                     property (default 'MAG_AB_'; e.g.
            %                     FLUX_APER_3 -> MAG_AB_APER_3). If MagColPrefix
            %                     is 'MAG_', the calibrated mags overwrite the
            %                     instrumental MAG_<suffix> columns in place.
            %                     If AddMagErr=true, also: MAGERR_<rest> where
            %                     <rest> is the calibrated mag column name with
            %                     its leading 'MAG_' stripped (e.g.
            %                     MAG_AB_APER_3 -> MAGERR_AB_APER_3).
            % Author : D. Kovaleva (Jan 2026)
            % Example: Cat = PC.addMag(Cat);
            %          Cat = PC.addMag(Cat, 'FluxColNames', {'FLUX_APER_3', 'FLUX_PSF'});
            %          Cat = PC.addMag(Cat, 'MagSystem', 'AB');
            %          Cat = PC.addMag(Cat, 'AddMagErr', false);
            % Description: Creates new columns with calibrated magnitudes from flux measurements.
            %              Formula: MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              For each FLUX_<suffix> column, creates MAG_<System>_<suffix> column.
            %              If AddMagErr=true, also creates MAGERR_<System>_<suffix>
            %              column with error = 1.086 * FLUXERR_<suffix>
            %              (FLUXERR is the relative flux uncertainty).
            %              Preserves original flux columns.
            %              Applies position-dependent corrections if available.

            arguments
                Obj
                CatObj
                Args.FluxColNames = []
                Args.ApplyPosCorrection logical = true
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
                Args.AddMagErr logical = true  % Add magnitude error columns
                Args.AddZP logical = false  % Also insert ZP column (avoids recomputing)
                Args.PropagateCalibratedErr logical = false  % Propagate calibrated errors (placeholder)
                Args.ApplyConstBand logical = false  % Apply constant-band correction after AB mags
                Args.ConstBandParams = []            % Struct or .mat path for constant band params
                Args.ConstBandOutputMode = 'newcol'  % 'newcol' or 'replace'
                Args.ConstBandPrefix = 'MAG_CB_'     % Prefix for constant-band mag columns
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:addMag:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            % Column-name prefix for output magnitudes, from the object's
            % MagColPrefix property. Naming convention only; the magnitude
            % *system* is set by MagSystem. When MagColPrefix='MAG_' the
            % calibrated mags overwrite the instrumental MAG_<suffix> columns
            % (insertCol deletes the existing column first).
            MagPrefix = Obj.MagColPrefix;

            % Get catalog table
            Tab = CatObj.Table;

            if isempty(Tab) || height(Tab) == 0
                Obj.msgLog(LogLevel.Warning, 'addMag: Catalog is empty. No columns added.');
                return;
            end

            % Determine which flux columns to calibrate
            AllColNames = Tab.Properties.VariableNames;
            if isempty(Args.FluxColNames)
                % Find all flux columns (FLUX_*), excluding FLUX_XYPEAK
                FluxColNames = AllColNames(startsWith(AllColNames, 'FLUX_'));
                FluxColNames = FluxColNames(~strcmp(FluxColNames, 'FLUX_XYPEAK'));
            else
                % Use specified columns
                if ischar(Args.FluxColNames)
                    FluxColNames = {Args.FluxColNames};
                else
                    FluxColNames = Args.FluxColNames;
                end
            end

            if isempty(FluxColNames)
                Obj.msgLog(LogLevel.Warning, 'addMag: No FLUX_* columns found in catalog.');
                return;
            end

            % Extract X, Y coordinates if position corrections requested
            X = [];
            Y = [];
            if Args.ApplyPosCorrection
                if ismember('X', AllColNames) && ismember('Y', AllColNames)
                    X = Tab.X;
                    Y = Tab.Y;
                else
                    Obj.msgLog(LogLevel.Warning, ...
                            'addMag: X, Y columns not found. Position corrections disabled.');
                end
            end

            % Compute ZP once for all flux columns
            Nrows = height(Tab);
            ZP = nan(Nrows, 1);
            ExpTime_eff = Obj.ExpTime_eff;
            ValidPosMask = true(Nrows, 1);
            if ~isempty(X)
                InvalidPos = isnan(X) | isinf(X) | isnan(Y) | isinf(Y);
                if any(InvalidPos)
                    Obj.msgLog(LogLevel.Debug, 'addMag: Position validation: %d/%d sources have invalid X/Y - magnitude and ZP will be NaN', ...
                        sum(InvalidPos), Nrows);
                    ValidPosMask = ~InvalidPos;
                end
            end

            % Extract per-source zenith angles if per-source airmass was used
            PerSourceZenithAngles = [];
            if Obj.PerSourceAirmass && ~isempty(Obj.AirmassColName) && ...
               ismember(Obj.AirmassColName, AllColNames)
                Airmass = Tab.(Obj.AirmassColName);
                ValidAM = Airmass >= 1 & isfinite(Airmass);
                ValidPosMask = ValidPosMask & ValidAM;
                PerSourceZenithAngles = nan(Nrows, 1);
                PerSourceZenithAngles(ValidAM) = acosd(1 ./ Airmass(ValidAM));
            end

            if any(ValidPosMask)
                ZPArgs = {'MagSystem', Args.MagSystem};
                if ~isempty(X)
                    ZPArgs = [ZPArgs, 'X', X(ValidPosMask), 'Y', Y(ValidPosMask)];
                end
                if ~isempty(PerSourceZenithAngles)
                    ZPArgs = [ZPArgs, 'PerSourceZenithAngles', PerSourceZenithAngles(ValidPosMask)];
                end
                ZP_valid = Obj.evaluateZP(ZPArgs{:});
                ZP(ValidPosMask) = ZP_valid(:);
            end

            % Insert ZP column if requested
            if Args.AddZP
                ZPColName = [Args.MagSystem, '_ZP'];
                CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
            end

            % Process each flux column
            for I = 1:length(FluxColNames)
                FluxColName = FluxColNames{I};

                % Get flux values [photons]
                Flux = Tab.(FluxColName);

                % Calibrated magnitude using pre-computed ZP
                % MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP  (via luptitude)
                Mag = convert.luptitude(Flux/ExpTime_eff, 10.^(0.4.*ZP));

                % Create new calibrated magnitude column name
                % e.g., FLUX_APER_3 -> MAG_AB_APER_3
                NewMagColName = strrep(FluxColName, 'FLUX_', MagPrefix);

                % Insert magnitude column into catalog
                CatObj = CatObj.insertCol(Mag, Inf, {NewMagColName});

                % Add magnitude error column if requested. Two error sources
                % accepted: FLUXERR_<suffix> (preferred), or (for FLUX_PSF
                % specifically) SN via MagErr = 1.086 / SN. If neither is
                % present, no MAGERR column is written.
                if Args.AddMagErr
                    FluxErrColName = strrep(FluxColName, 'FLUX_', 'FLUXERR_');
                    MagErrColName  = regexprep(NewMagColName, '^MAG_', 'MAGERR_');

                    if ismember(FluxErrColName, AllColNames)
                        % FLUXERR is the relative flux uncertainty (dF/F per
                        % LAST extractor), so MagErr = 1.086 * FLUXERR.
                        FluxErr   = Tab.(FluxErrColName);
                        MagErr    = nan(Nrows, 1);
                        ValidFlux = Flux > 0 & ~isnan(Flux) & ~isnan(FluxErr);
                        MagErr(ValidFlux) = 1.086 .* FluxErr(ValidFlux);
                        CatObj = CatObj.insertCol(MagErr, Inf, {MagErrColName});
                    elseif strcmp(FluxColName, 'FLUX_PSF') && ismember('SN', AllColNames)
                        % PSF special case: LAST has no FLUXERR_PSF; derive
                        % from SN as MagErr = 1.086 / SN.
                        SN     = Tab.SN;
                        MagErr = nan(Nrows, 1);
                        ValidSN = isfinite(SN) & SN > 0;
                        MagErr(ValidSN) = 1.086 ./ SN(ValidSN);
                        CatObj = CatObj.insertCol(MagErr, Inf, {MagErrColName});
                    else
                        Obj.msgLog(LogLevel.Debug, ...
                            'addMag: no FLUXERR/SN error source for %s - no %s written', ...
                            FluxColName, MagErrColName);
                    end
                end

                % Propagate calibrated magnitude error if requested (placeholder)
                if Args.PropagateCalibratedErr
                    % TODO: call dedicated method for calibrated error propagation
                    error('PhotCalibTrans:addMag:PropagateCalibratedErrNotImplemented', ...
                          'Calibrated magnitude error propagation is not yet implemented.');
                end
            end

            % Apply constant-band correction if requested
            if Args.ApplyConstBand
                CatObj = Obj.applyConstBand(CatObj, ...
                    'ConstBandParams', Args.ConstBandParams, ...
                    'OutputMode', Args.ConstBandOutputMode, ...
                    'OutputPrefix', Args.ConstBandPrefix);
            end

        end

        function CatObj = applyConstBand(Obj, CatObj, Args)
            % Apply constant-band correction to calibrated magnitudes.
            %   Converts per-crop AB magnitudes to a standardized bandpass by
            %   replacing fitted atmospheric parameters with global constant
            %   values. ZenithAngle and Temperature are taken from the
            %   observation (Obj.AirMass, Obj.Temp). Norm and Tran2D cancel
            %   (they are wavelength-independent and already in MAG_AB).
            %   The correction is a scalar delta ZP per crop:
            %     MAG_CB = MAG_AB + delta ZP
            % Input  : - PhotCalibTrans object (must have fitted TransModel).
            %          - AstroCatalog object with MAG_AB columns from addMag.
            %          * ...,key,val,...
            %            'ConstBandParams' - Struct with global atmospheric
            %                        parameter values. Field names must match
            %                        parameter Descriptions in CompositeFun
            %                        (e.g., Pressure_mbar, DobsonUnits,
            %                        TauAod500, AngstromExponent, PWV_cm).
            %                        Or path to .mat file containing the struct.
            %            'OutputMode' - 'newcol' creates new columns with
            %                        OutputPrefix; 'replace' overwrites the
            %                        original MAG columns. Default is 'newcol'.
            %                        The magnitude columns to convert are
            %                        identified by the object's MagColPrefix
            %                        property.
            %            'OutputPrefix' - Prefix for new columns when
            %                        OutputMode='newcol'. Default is 'MAG_CB_'.
            % Output : - AstroCatalog with constant-band magnitude columns.
            % Author : D. Kovaleva (Mar 2026)
            % Example: Cat = PC.applyConstBand(Cat, 'ConstBandParams', CBP);
            %          Cat = PC.applyConstBand(Cat, 'ConstBandParams', 'ConstBand_LAST.mat');
            %          Cat = PC.applyConstBand(Cat, 'ConstBandParams', CBP, 'OutputMode', 'replace');

            arguments
                Obj
                CatObj
                Args.ConstBandParams = []       % Struct or .mat path
                Args.OutputMode = 'newcol'      % 'newcol' or 'replace'
                Args.OutputPrefix = 'MAG_CB_'
            end

            if isempty(Args.ConstBandParams)
                Obj.msgLog(LogLevel.Warning, ...
                    'applyConstBand: ConstBandParams not provided — skipping constant band correction');
                return;
            end

            % Load ConstBandParams from .mat if path given
            if ischar(Args.ConstBandParams) || isstring(Args.ConstBandParams)
                S = load(Args.ConstBandParams);
                Fn = fieldnames(S);
                CBP = S.(Fn{1});
            else
                CBP = Args.ConstBandParams;
            end

            Lambda = Obj.TransWvl;

            % TODO: PerSourceAirmass mode requires per-source delta ZP (each source
            %       has its own ZenithAngle and thus its own T_atm_crop). Currently
            %       computes a single scalar delta ZP per crop assuming shared airmass.
            if Obj.PerSourceAirmass
                Obj.msgLog(LogLevel.Warning, ...
                    'applyConstBand: PerSourceAirmass mode not yet supported — using single-airmass delta ZP');
            end

            % --- Evaluate T_atm_crop (fitted atmospheric, Norm=1, no Tran2D) ---
            AllPar = Obj.TransModel.getAllFunPar();

            % Build crop parameter vector with Norm=1
            CropParVec = AllPar.Val(:)';
            NormIdx = find(strcmp(AllPar.Name, 'Norm'), 1);
            if ~isempty(NormIdx)
                CropParVec(NormIdx) = 1.0;
            end
            T_crop = Obj.TransModel.evaluateAllFunParInput(Lambda, CropParVec);

            % --- Build T_const (replace atmospheric params with global values) ---
            ConstParVec = CropParVec;
            Fields = fieldnames(CBP);
            for If = 1:numel(Fields)
                Idx = find(strcmp(AllPar.Name, Fields{If}));
                if ~isempty(Idx)
                    ConstParVec(Idx) = CBP.(Fields{If});
                end
            end
            T_const = Obj.TransModel.evaluateAllFunParInput(Lambda, ConstParVec);

            % --- Compute delta ZP ---
            IntCrop  = trapz(Lambda, T_crop(:) .* Lambda(:));
            IntConst = trapz(Lambda, T_const(:) .* Lambda(:));

            if IntCrop <= 0 || IntConst <= 0
                Obj.msgLog(LogLevel.Warning, ...
                    'applyConstBand: Non-positive transmission integral — skipping constant band');
                return;
            end

            DeltaZP = 2.5 * log10(IntConst / IntCrop);
            Obj.DeltaZP_CB = DeltaZP;

            % --- Apply to magnitude columns ---
            % Magnitude columns identified by the object's MagColPrefix.
            % Exclude OutputPrefix columns: when MagColPrefix='MAG_' it would
            % otherwise also match the 'MAG_CB_' constant-band outputs.
            AllColNames = CatObj.ColNames;
            MagCols = AllColNames(startsWith(AllColNames, Obj.MagColPrefix) & ...
                                  ~startsWith(AllColNames, Args.OutputPrefix));

            for Ic = 1:numel(MagCols)
                ColIdx = CatObj.colname2ind(MagCols{Ic});
                MagVals = CatObj.Catalog(:, ColIdx) + DeltaZP;

                if strcmpi(Args.OutputMode, 'replace')
                    CatObj.Catalog(:, ColIdx) = MagVals;
                else
                    NewColName = strrep(MagCols{Ic}, Obj.MagColPrefix, Args.OutputPrefix);
                    CatObj = CatObj.insertCol(MagVals, Inf, NewColName, {});
                end
            end
        end

        function CatObj = addZP(Obj, CatObj, Args)
            % Add position-dependent ZP column to catalog
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object.
            %          * ...,key,val,...
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            % Output : - AstroCatalog with added ZP column
            %                     (AB_ZP or VEGA_ZP depending on MagSystem).
            % Author : D. Kovaleva (Jan 2026)
            % Example: Cat = PC.addZP(Cat);
            %          Cat = PC.addZP(Cat, 'MagSystem', 'AB');

            arguments
                Obj
                CatObj
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
            end

            Tab = CatObj.Table;
            if isempty(Tab) || height(Tab) == 0
                Obj.msgLog(LogLevel.Debug, 'addZP: Catalog is empty. No columns added.');
                return;
            end

            Nrows = height(Tab);

            % Extract X, Y coordinates
            AllColNames = Tab.Properties.VariableNames;
            % Dynamic column name: AB_ZP or VEGA_ZP
            ZPColName = [Args.MagSystem, '_ZP'];

            if ~ismember('X', AllColNames) || ~ismember('Y', AllColNames)
                Obj.msgLog(LogLevel.Error, 'addZP: X, Y columns not found in catalog. ZP column set to NaN.');
                ZP = nan(Nrows, 1);
                CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
                return;
            end

            X = Tab.X(:);
            Y = Tab.Y(:);

            % Validate X, Y coordinates
            InvalidPos = isnan(X) | isinf(X) | isnan(Y) | isinf(Y);
            if any(InvalidPos)
                Obj.msgLog(LogLevel.Debug, 'addZP: Position validation: %d/%d sources have invalid X/Y - ZP set to NaN', ...
                    sum(InvalidPos), Nrows);
            end

            % Initialize ZP as NaN
            ZP = nan(Nrows, 1);

            % Extract per-source zenith angles if per-source airmass was used
            ValidMask = ~InvalidPos;
            PerSourceZenithAngles = [];
            AllColNames = Tab.Properties.VariableNames;
            if Obj.PerSourceAirmass && ~isempty(Obj.AirmassColName) && ...
               ismember(Obj.AirmassColName, AllColNames)
                Airmass = Tab.(Obj.AirmassColName);
                ValidAM = Airmass >= 1 & isfinite(Airmass);
                ValidMask = ValidMask & ValidAM;
                PerSourceZenithAngles = nan(Nrows, 1);
                PerSourceZenithAngles(ValidAM) = acosd(1 ./ Airmass(ValidAM));
            end

            % Evaluate ZP only for valid positions
            if any(ValidMask)
                ZPArgs = {'X', X(ValidMask), 'Y', Y(ValidMask), ...
                          'MagSystem', Args.MagSystem};
                if ~isempty(PerSourceZenithAngles)
                    ZPArgs = [ZPArgs, 'PerSourceZenithAngles', PerSourceZenithAngles(ValidMask)];
                end
                ZP_valid = Obj.evaluateZP(ZPArgs{:});
                ZP(ValidMask) = ZP_valid(:);
            end

            % Insert column
            CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
        end

        function Obj = evaluateLimMag(Obj, CatObj, Args)
            % Compute limiting magnitude from calibrated catalog (legacy LIMMAG)
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object after addMag (must contain the
            %            matching FLUXERR_<suffix> and <MagColPrefix><suffix>
            %            columns derived from FluxColName; <MagColPrefix> is the
            %            object's MagColPrefix property).
            %          * ...,key,val,...
            %            'FluxColName' - Flux column name; the matching
            %                            FLUXERR_<suffix> drives the SN ratio
            %                            and <MagColPrefix><suffix> is fit.
            %                            Default is 'FLUX_APER_3'.
            %            'MagSystem'   - Magnitude system ('AB' or 'Vega').
            %                            Default is 'AB'.
            %            'MinSN'       - Lower SN bound for fit window. Default is 5.
            %            'MaxSN'       - Upper SN bound for fit window. Default is 50.
            %            'LimMagSN'    - SN at which to evaluate limiting magnitude.
            %                            Default is 5.
            % Output : - PhotCalibTrans object with Obj.LimMag set (NaN on failure).
            % Author : D. Kovaleva (May 2026)
            % Example: PC = PC.evaluateLimMag(Cat);
            %          PC = PC.evaluateLimMag(Cat, 'FluxColName', 'FLUX_PSF');
            % Description: Empirical limiting magnitude via straight-line fit of
            %              MAG_<system>_<suffix> vs log10(SN) in window
            %              [MinSN, MaxSN], evaluated at SN=LimMagSN. SN is taken
            %              as 1./FLUXERR_<suffix> because FLUXERR is the relative
            %              flux uncertainty (FluxErr/Flux); equivalent to
            %              1.086/MagErr in the Gaussian limit. Color term omitted
            %              because MAG_<system>_* already absorbs color.

            arguments
                Obj
                CatObj
                Args.FluxColName char = 'FLUX_APER_3'
                Args.MagSystem   char = 'AB'
                Args.MinSN       double = 5
                Args.MaxSN       double = 50
                Args.LimMagSN    double = 5
            end

            Obj.LimMag = NaN;

            if isempty(CatObj) || isempty(CatObj.Table) || height(CatObj.Table) == 0
                Obj.msgLog(LogLevel.Warning, 'evaluateLimMag: Catalog is empty - LimMag set to NaN.');
                return;
            end

            % Derive FLUXERR / MAG column names from FLUX_<suffix>
            Tokens = regexp(Args.FluxColName, '^FLUX_(.+)$', 'tokens', 'once');
            if isempty(Tokens)
                Obj.msgLog(LogLevel.Warning, ...
                    'evaluateLimMag: FluxColName "%s" must start with FLUX_ - LimMag set to NaN.', ...
                    Args.FluxColName);
                return;
            end
            Suffix         = Tokens{1};
            FluxErrColName = ['FLUXERR_', Suffix];
            MagColName     = [Obj.MagColPrefix, Suffix];

            AllCols = CatObj.Table.Properties.VariableNames;
            if ~ismember(FluxErrColName, AllCols) || ~ismember(MagColName, AllCols)
                Obj.msgLog(LogLevel.Warning, ...
                    'evaluateLimMag: Required columns %s / %s not found - LimMag set to NaN.', ...
                    FluxErrColName, MagColName);
                return;
            end

            try
                FluxErr = CatObj.getCol(FluxErrColName);
                Mag     = CatObj.getCol(MagColName);
                FluxErr = FluxErr(:);
                Mag     = Mag(:);

                % FLUXERR is relative (FluxErr/Flux), so SN = 1/FLUXERR
                % (equivalent to 1.086/MagErr in the Gaussian limit).
                SN = 1 ./ FluxErr;

                Valid = isfinite(Mag) & isfinite(SN) & SN > Args.MinSN & SN < Args.MaxSN;
                if nnz(Valid) < 3
                    Obj.msgLog(LogLevel.Warning, ...
                        'evaluateLimMag: Only %d points in SN window [%.1f, %.1f] - LimMag set to NaN.', ...
                        nnz(Valid), Args.MinSN, Args.MaxSN);
                    return;
                end

                ParLimMagFit = polyfit(log10(SN(Valid)), Mag(Valid), 1);
                Obj.LimMag = polyval(ParLimMagFit, log10(Args.LimMagSN));
            catch ME
                Obj.msgLog(LogLevel.Warning, ...
                    'evaluateLimMag: Fit failed (%s) - LimMag set to NaN.', ME.message);
                Obj.LimMag = NaN;
            end
        end

        function Obj = evaluateBackMag(Obj, AI, Args)
            % Compute sky surface brightness from image background (legacy BACKMAG)
            % Input  : - PhotCalibTrans object.
            %          - AstroImage with populated WCS.
            %          * ...,key,val,...
            %            'PixScale' - Pixel scale [arcsec/pixel]. 
            %                         (read from AI.WCS via getScale('arcsec')).
            %            'backVarArgs' - argument to pass to the background 
            %            measurement function for the case AI comes w/o AI.Back  
            % Output : - PhotCalibTrans object with Obj.BackMag set (NaN on failure).
            % Author : D. Kovaleva (May 2026)
            % Example: PC = PC.evaluateBackMag(AI);
            % Description: Sky surface brightness in mag/arcsec^2 using legacy formula
            %              BackMag = ZP - 2.5*log10(MedBack) + 5*log10(PixScale),
            %              where ZP = Obj.evaluateZP() (scalar at field centre) and
            %              MedBack = fast_median(AI.Back(:)).

            arguments
                Obj
                AI
                Args.PixScale = []        % [] => read from AI.WCS.getScale('arcsec'); otherwise must be a finite positive double [arcsec/pix]
                Args.backVarArgs = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[256 256], 'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}};
            end

            Obj.BackMag = NaN;

            try
                % Fast path: MEDBCK already in header (written by imProc.background.backVar).
                MedBack = NaN;
                if ~isempty(AI.HeaderData) && AI.HeaderData.isKeyExist('MEDBCK')
                    HVal = AI.HeaderData.getVal('MEDBCK');
                    if isnumeric(HVal) && isscalar(HVal) && isfinite(HVal) && HVal > 0
                        MedBack = HVal;
                    end
                end
                % Fallback: compute background image and take its median.
                if ~isfinite(MedBack)
                    if isempty(AI.Back)
                        AI = imProc.background.backVar(AI, Args.backVarArgs{:});
                    end
                    MedBack = fast_median(AI.Back(:));
                end
                if ~isfinite(MedBack) || MedBack <= 0
                    Obj.msgLog(LogLevel.Warning, ...
                        'evaluateBackMag: Non-positive median background (%.3g) - BackMag set to NaN.', MedBack);
                    return;
                end

                if isempty(Args.PixScale)
                    PixScale = AI.WCS.getScale('arcsec');
                else
                    PixScale = Args.PixScale;
                end
                if ~isfinite(PixScale) || PixScale <= 0
                    Obj.msgLog(LogLevel.Warning, ...
                        'evaluateBackMag: Invalid pixel scale (%.3g) - BackMag set to NaN.', PixScale);
                    return;
                end

                ZP          = Obj.evaluateZP();        % scalar at field centre (on per-frame-rate scale)
                Obj.BackMag = ZP - 2.5*log10(MedBack/Obj.ExpTime_eff) + 5*log10(PixScale);
            catch ME
                Obj.msgLog(LogLevel.Warning, ...
                    'evaluateBackMag: Computation failed (%s) - BackMag set to NaN.', ME.message);
                Obj.BackMag = NaN;
            end
        end
    end

    methods % Display/Output methods
        function summary(Obj, Args)
            % Display photometric calibration summary
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : None
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.summary();

            arguments
                Obj
                Args.Verbose logical = false
            end

            if ~Args.Verbose
                return;
            end

            fprintf('\n=== PhotCalibTrans Object ===\n');
            fprintf('Calibrated: %s\n', mat2str(~isempty(Obj.TransModel)));

            if ~isempty(Obj.SpecData)
                fprintf('Calibrators: %d (min required: %d)\n', size(Obj.SpecData.Spec, 1), Obj.NCalibMin);
            else
                fprintf('Calibrators: 0 (min required: %d)\n', Obj.NCalibMin);
            end

            if ~isempty(Obj.TransModel)
                fprintf('Transmission Model: Available\n');

                if ~isempty(Obj.TransModel.RMS)
                    fprintf('RMS: %.4f mag (max allowed: %.4f mag)\n', Obj.TransModel.RMS, Obj.RMSMax);
                end

                if ~isempty(Obj.TransModel.Chi2) && ~isempty(Obj.TransModel.DOF)
                    fprintf('Chi2/DOF: %.2f / %d = %.3f\n', ...
                            Obj.TransModel.Chi2, Obj.TransModel.DOF, ...
                            Obj.TransModel.Chi2/Obj.TransModel.DOF);
                end
            else
                fprintf('Transmission Model: Not available\n');
            end

            if ~isnan(Obj.AirMass)
                fprintf('Airmass: %.3f\n', Obj.AirMass);
            end

            % Check for position-dependent corrections
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.Tran2DObj) && ~isempty(Obj.TransModel.Tran2DObj.ParX)
                PosParams = Obj.TransModel.Tran2DObj.ParX;
                if any(PosParams(:) ~= 0)
                    fprintf('Position-dependent Corrections: Available (max: %.4f mag)\n', max(abs(PosParams(:))));
                end
            end

            fprintf('========================\n\n');
        end
    end

    
    methods % Plotting methods
        function [Fig, IntTrans] = plotTransmission(Obj, Args)
            % Plot total system transmission curves for scalar or array of objects
            % Input  : - PhotCalibTrans object (scalar or array).
            %          * ...,key,val,...
            %            'Layout'    - 'overlay' (all on one axes, default) or
            %                          'subplots' (grid of individual plots).
            %            'Labels'    - Cell array of legend labels {N x 1}. Default
            %                          auto-generates '1', '2', etc.
            %            'NewFigure' - Create new figure. Default is true.
            %            'RefCrop'   - Reference crop index for relative integral
            %                          transmission. When set, integral T is normalized
            %                          so that T(RefCrop)=1. Default is [] (absolute).
            % Output : - Figure handle.
            %          - [N x 1] integral transmission values (one per object),
            %            in absolute units (same as Obj.integralTransmission()).
            %            For relative integrals divide by IntTrans(RefCrop)
            %            externally.
            % Author : D. Kovaleva (Feb 2026)
            % Example: PC.plotTransmission();
            %          PC.plotTransmission('Layout', 'subplots');
            %          [Fig, IntTrans] = PC.plotTransmission('RefCrop', 10);

            arguments
                Obj
                Args.Layout    = 'overlay'
                Args.Labels cell = {}
                Args.NewFigure logical = true
                Args.RefCrop = []
            end

            N = numel(Obj);

            % Generate default labels
            if isempty(Args.Labels)
                Labels = cell(N, 1);
                for Ipc = 1:N
                    Labels{Ipc} = sprintf('%d', Ipc);
                end
            else
                Labels = Args.Labels(:);
            end

            % Evaluate transmission and integral transmission for each object
            Lambda = Obj(1).TransWvl;
            Nlambda = numel(Lambda);
            TransAll = zeros(Nlambda, N);
            IntTrans = zeros(N, 1);
            for Ipc = 1:N
                TransAll(:, Ipc) = Obj(Ipc).evaluateTransmission('Lambda', Lambda);
                IntTrans(Ipc) = Obj(Ipc).integralTransmission();
            end

            % Normalize to reference crop if requested
            if ~isempty(Args.RefCrop)
                RefIdx = Args.RefCrop;
                if RefIdx < 1 || RefIdx > N
                    error('PhotCalibTrans:plotTransmission:BadRefCrop', ...
                          'RefCrop=%d is out of range [1, %d].', RefIdx, N);
                end
                RelTrans = IntTrans ./ IntTrans(RefIdx);
                for Ipc = 1:N
                    Labels{Ipc} = sprintf('%s (T=%.3f)', Labels{Ipc}, RelTrans(Ipc));
                end
            else
                for Ipc = 1:N
                    Labels{Ipc} = sprintf('%s (T=%.3f)', Labels{Ipc}, IntTrans(Ipc));
                end
            end

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            switch lower(Args.Layout)
                case 'overlay'
                    plot(Lambda, TransAll, 'LineWidth', 1.5);
                    grid on;
                    xlabel('Wavelength [Angstrom]');
                    ylabel('Transmission');
                    title('Total System Transmission');
                    ylim([0, max(TransAll(:)) * 1.1]);
                    legend(Labels, 'Location', 'best');

                case 'subplots'
                    Ncols = ceil(sqrt(N));
                    Nrows = ceil(N / Ncols);
                    for Ipc = 1:N
                        subplot(Nrows, Ncols, Ipc);
                        plot(Lambda, TransAll(:, Ipc), 'LineWidth', 1.5);
                        grid on;
                        title(Labels{Ipc});
                        xlabel('Wavelength [Angstrom]');
                        ylabel('Transmission');
                        ylim([0, max(TransAll(:, Ipc)) * 1.1]);
                    end
                    sgtitle('Total System Transmission');

                otherwise
                    error('PhotCalibTrans:plotTransmission:UnknownLayout', ...
                          'Unknown Layout ''%s''. Use ''overlay'' or ''subplots''.', Args.Layout);
            end
        end

        function Fig = plotResiduals(Obj, Args)
            % Plot calibration residuals
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Type' - Plot type: 'magnitude' (residuals vs mag),
            %                     'spatial' (2D spatial distribution), 'both'. Default is 'both'.
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle or array of handles
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotResiduals();
            %          PC.plotResiduals('Type', 'spatial');
            % Description: Plots magnitude residuals from last fit stage.
            %              Shows spatial patterns and magnitude-dependent systematics.

            arguments
                Obj
                Args.Type = 'both'
                Args.NewFigure logical = true
            end

            % Get residuals and calibrator data from SourceData
            % (Residuals and Used columns are added by calibrate() after fitting)
            Tab = Obj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;

            if ~ismember('Residuals', ColNames)
                error('PhotCalibTrans:plotResiduals:NoResiduals', ...
                      'No residuals available. Run calibrate() first.');
            end

            AllResiduals = Tab.Residuals;
            X_all = Tab.X;
            Y_all = Tab.Y;
            Flux_all = Tab.Flux;

            % Filter to used calibrators only (not sigma-clipped)
            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(size(AllResiduals));
            end

            Residuals = AllResiduals(UsedMask);
            X = X_all(UsedMask);
            Y = Y_all(UsedMask);
            Flux = Flux_all(UsedMask);
            MagInst = -2.5 * log10(Flux);  % Convert flux to instrumental magnitude

            % Determine what to plot
            switch lower(Args.Type)
                case 'magnitude'
                    Nplots = 1;
                case 'spatial'
                    Nplots = 1;
                case 'both'
                    Nplots = 2;
            end

            % Create figure(s)
            if Args.NewFigure
                if Nplots == 1
                    Fig = figure;
                else
                    Fig = [figure, figure];
                end
            else
                Fig = gcf;
            end

            % Plot 1: Residuals vs Magnitude
            if strcmpi(Args.Type, 'magnitude') || strcmpi(Args.Type, 'both')
                if Nplots == 2
                    figure(Fig(1));
                end

                scatter(MagInst, Residuals, 30, 'filled', 'MarkerFaceAlpha', 0.6);
                hold on;
                yline(0, 'k--', 'LineWidth', 1.5);
                grid on;
                xlabel('Instrumental Magnitude');
                ylabel('Residual [mag]');
                title(sprintf('Calibration Residuals (RMS=%.4f mag)', Obj.TransModel.RMS));

                % Add RMS lines
                yline(Obj.TransModel.RMS, 'r--', 'RMS');
                yline(-Obj.TransModel.RMS, 'r--', 'RMS');
            end

            % Plot 2: Spatial distribution
            if strcmpi(Args.Type, 'spatial') || strcmpi(Args.Type, 'both')
                if Nplots == 2
                    figure(Fig(2));
                end

                scatter(X, Y, 50, Residuals, 'filled');
                colorbar;
                colormap(jet);
                caxis([-3*Obj.TransModel.RMS, 3*Obj.TransModel.RMS]);
                xlabel('X [pixels]');
                ylabel('Y [pixels]');
                title(sprintf('Spatial Distribution of Residuals (RMS=%.4f mag)', Obj.TransModel.RMS));
                axis equal;
                grid on;
            end
        end

        function [Fig, ZPTable] = plotZPMap(Obj, Args)
            % Plot 2D map of position-dependent zero point corrections
            % For scalar input: plots single crop ZP map.
            % For array input: plots seamless mosaic across all crops
            %   with scattered interpolation and optional Gaussian smoothing.
            % Input  : - PhotCalibTrans object (scalar or array)
            %          * ...,key,val,...
            %            'GridSize' - Grid resolution [Nx, Ny]. Default is [50, 50].
            %            'NewFigure' - Create new figure. Default is true.
            %            'CLim' - Color limits [min max]. Default is [] (auto).
            %            --- Mosaic-only arguments (array input) ---
            %            'CropIDs' - [1 x Nobj] crop IDs. Default is 1:Nobj.
            %            'Ncols' - Number of columns in grid. Default is 4.
            %            'Nrows' - Number of rows in grid. Default is 6.
            %            'SubImgSize' - Subimage size [Nx, Ny] pixels. Default
            %                        is [] (auto-detect from the first crop's
            %                        Tran2DObj as [2*ParNX(2), 2*ParNY(2)] in
            %                        per-crop frame; defaults to [1726, 1726]
            %                        in field frame, where Tran2D ParNX covers
            %                        the whole mosaic and is not a per-crop
            %                        extent). Pass an explicit [Nx, Ny] to
            %                        override.
            %            'Tran2DFrame' - 'auto' | 'percrop' | 'field'.
            %                        Controls how plotZPMap calls evaluateZP
            %                        per crop:
            %                          'percrop' - each PC's Tran2DObj covers
            %                                      that crop only; evaluateZP
            %                                      sees local (X, Y) = 1..Nx.
            %                                      Matches single-crop calibrate
            %                                      output.
            %                          'field'   - all PCs' Tran2DObjs share a
            %                                      field-frame ParNX (cover
            %                                      whole mosaic); evaluateZP
            %                                      sees global (X, Y) =
            %                                      X + (Col-1)*Nx, Y + (Row-1)*Ny.
            %                                      Matches joint-fit output
            %                                      (imProc.calib.fitPhotCalibTrans
            %                                      with 'JointVisit', true).
            %                          'auto'    - default: peek at the first
            %                                      PC's ParNX(2). If
            %                                      2*ParNX(2) > 3000 px treat
            %                                      as field; else per-crop.
            %            'SmoothSigma' - Gaussian smoothing sigma [grid units].
            %                        Applied via NaN-aware conv2 with Gaussian kernel.
            %                        Set to 0 to disable. Default is 3.
            %            'PhotSys' - Photometry system mode for ZP evaluation:
            %                        'percrop' (default) - each crop uses own model.
            %                        'refshape' - reference spectral shape, per-crop Norm.
            %                        'refzp' - full reference params incl. Norm, center-normalized Tran2D.
            %                        'refzp_raw' - full reference params, no Tran2D normalization.
            %            'RefCrop' - Reference crop index for refshape/refzp.
            %                        0 = weighted mean over all crops. Default is 10.
            %            'TileOrder' - Crop tiling order in mosaic:
            %                        'colmajor' (old pipeline) - bottom-to-top, column by column.
            %                        'rowmajor' (new pipeline) - left-to-right, row by row.
            %                        Default is 'rowmajor'.
            %            'OverlayCalibrators' - Overlay SourceData (X,Y) on the
            %                        ZP map. Accepts:
            %                          'both' (default) - Used calibrators as
            %                              white dots, clipped (Used=false) as
            %                              grey dots ([0.6 0.6 0.6]).
            %                          'used' - only survivors (white dots).
            %                          'all'  - every SourceData entry (white).
            %                          'none' - no overlay.
            %                        Logical true/false accepted as 'all'/'none'
            %                        for back-compat. In mosaic mode each crop's
            %                        local coords are offset by
            %                        (Col-1)*Nx, (Row-1)*Ny. If the 'Used' column
            %                        is absent (pre-calibration), 'both'/'used'
            %                        fall back to 'all'.
            % Output : - Fig: figure handle.
            %          - ZPTable: the (X, Y, ZP) sample table that backs the
            %                     plot. Single-crop mode returns columns
            %                     {X, Y, ZP} (one row per grid point in the
            %                     local crop coordinate system); mosaic mode
            %                     returns {CropID, LocalX, LocalY, X, Y, ZP}
            %                     where (LocalX, LocalY) is the in-crop
            %                     position used to call evaluateZP and
            %                     (X, Y) is the global mosaic-frame position
            %                     (LocalX + (Col-1)*Nx, LocalY + (Row-1)*Ny).
            %                     The ZP column carries the same values fed
            %                     to scatteredInterpolant — the imagesc grid
            %                     is then derived from interpolating this
            %                     table onto (XvecG, YvecG). NaN ZP rows are
            %                     dropped, matching the plot's Valid filter.
            %                     Request as `[~, ZPTable] = PC.plotZPMap()`
            %                     to skip the plot side-effect via NewFigure.
            % Author : D. Kovaleva (Dec 2025, Mar 2026)
            % Example: PC(5).plotZPMap();                          % single crop
            %          PC.plotZPMap();                              % mosaic, percrop
            %          PC.plotZPMap('PhotSys', 'refzp', 'RefCrop', 10);  % mosaic, refzp
            %          PC.plotZPMap('PhotSys', 'refzp_raw', 'RefCrop', 0); % mosaic, weighted mean, raw
            %          PC.plotZPMap('SmoothSigma', 0);             % mosaic, no smoothing
            %          PC.plotZPMap('TileOrder', 'rowmajor');       % mosaic, new pipeline tiling
            %          PC.plotZPMap('OverlayCalibrators', 'none');  % mosaic, no calibrator dots
            %          PC.plotZPMap('OverlayCalibrators', 'used');  % only survivors of clipping
            %          % Capture the underlying (X,Y,ZP) sample table:
            %          [Fig, T] = PC.plotZPMap();
            %          fprintf('Median mosaic ZP = %.3f mag\n', median(T.ZP));

            arguments
                Obj
                Args.GridSize = [50, 50]
                Args.NewFigure logical = true
                Args.CLim = []
                Args.CropIDs = []
                Args.Ncols = 4
                Args.Nrows = 6
                Args.SubImgSize = []     % [] = auto-detect from Tran2DObj.ParNX/ParNY of first valid crop (per-crop frame) or fall back to [1726, 1726] (field frame)
                Args.Tran2DFrame char {mustBeMember(Args.Tran2DFrame, {'auto','percrop','field'})} = 'auto'
                Args.SmoothSigma = 3
                Args.PhotSys = 'percrop'
                Args.RefCrop = 10
                Args.TileOrder = 'rowmajor'  % 'colmajor' (old: bottom-up columns) | 'rowmajor' (new: left-right rows)
                Args.OverlayCalibrators = 'both'   % 'both' | 'used' | 'all' | 'none' (logical true/false accepted as 'all'/'none')
                % Optional AstroImage array with UNIQSEC/ORIGUSEC (fallback
                % CCDSEC/ORIGSEC) headers. When passed, every real-detector
                % pixel gets exactly one crop's ZP (no duplication in
                % overlap strips). Empty (default) preserves the legacy
                % edge-to-edge layout that draws overlap strips twice.
                Args.AI = []
            end

            % Normalise OverlayCalibrators to canonical string
            if islogical(Args.OverlayCalibrators)
                if Args.OverlayCalibrators
                    Args.OverlayCalibrators = 'all';
                else
                    Args.OverlayCalibrators = 'none';
                end
            end
            Args.OverlayCalibrators = lower(string(Args.OverlayCalibrators));
            if ~ismember(Args.OverlayCalibrators, ["both", "used", "all", "none"])
                error('PhotCalibTrans:plotZPMap:OverlayMode', ...
                    'OverlayCalibrators must be ''both''|''used''|''all''|''none'' (or logical).');
            end

            Nobj = numel(Obj);

            if Nobj == 1
                % === Single crop mode ===
                % Get field boundaries from Tran2D. ParNX/ParNY are
                % [Center, HalfRange] — the normalization (X-Center)/HalfRange
                % maps [Xc-HalfRange, Xc+HalfRange] onto [-1, 1].
                Xc = Obj.TransModel.Tran2DObj.ParNX(1);
                Yc = Obj.TransModel.Tran2DObj.ParNY(1);
                HalfX = Obj.TransModel.Tran2DObj.ParNX(2);
                HalfY = Obj.TransModel.Tran2DObj.ParNY(2);

                Xmin = Xc - HalfX;
                Xmax = Xc + HalfX;
                Ymin = Yc - HalfY;
                Ymax = Yc + HalfY;

                Xvec = linspace(Xmin, Xmax, Args.GridSize(1));
                Yvec = linspace(Ymin, Ymax, Args.GridSize(2));
                [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);

                ZP = Obj.evaluateZP('X', Xgrid(:), 'Y', Ygrid(:));
                ZPgrid = reshape(ZP, Args.GridSize(2), Args.GridSize(1));

                % Sample table backing the imagesc grid (single-crop)
                ZPTable = table(Xgrid(:), Ygrid(:), ZP(:), ...
                    'VariableNames', {'X', 'Y', 'ZP'});

                if Args.NewFigure
                    Fig = figure;
                else
                    Fig = gcf;
                end

                imagesc(Xvec, Yvec, ZPgrid);
                if ~isempty(Args.CLim)
                    caxis(Args.CLim);
                end
                colorbar;
                colormap(jet);
                xlabel('X [pixels]');
                ylabel('Y [pixels]');
                title('Zero Point Map Across Field');
                axis xy equal tight;

                if Args.OverlayCalibrators ~= "none" && ~isempty(Obj.SourceData)
                    [Xall, Yall, UsedFlag, Mode] = PhotCalibTrans.resolveOverlay(...
                        Obj.SourceData, Args.OverlayCalibrators);
                    hold on;
                    switch Mode
                        case "all"
                            plot(Xall, Yall, 'w.', 'MarkerSize', 8);
                        case "used"
                            plot(Xall(UsedFlag), Yall(UsedFlag), 'w.', 'MarkerSize', 8);
                        case "both"
                            plot(Xall( UsedFlag), Yall( UsedFlag), 'w.', 'MarkerSize', 8);
                            plot(Xall(~UsedFlag), Yall(~UsedFlag), '.', 'Color', [0.6 0.6 0.6], 'MarkerSize', 8);
                    end
                end
            else
                % === Mosaic mode ===
                % Auto-detect Tran2DFrame + SubImgSize from the first valid
                % Tran2DObj. ParNX/ParNY are [Center, HalfRange]:
                %   - per-crop frame: 2*HalfRange ~ 1726 px (one crop)
                %   - field frame   : 2*HalfRange ~ 6912 px (LAST mosaic)
                % Threshold: 2*HalfRange > 3000 px => field-frame Tran2D.
                ProbeHalfX = []; ProbeHalfY = [];
                for Itmp = 1:Nobj
                    if ~isempty(Obj(Itmp).TransModel) && ...
                            ~isempty(Obj(Itmp).TransModel.Tran2DObj)
                        ParNX = Obj(Itmp).TransModel.Tran2DObj.ParNX;
                        ParNY = Obj(Itmp).TransModel.Tran2DObj.ParNY;
                        if numel(ParNX) >= 2 && numel(ParNY) >= 2
                            ProbeHalfX = ParNX(2);
                            ProbeHalfY = ParNY(2);
                        end
                        break;
                    end
                end
                if strcmp(Args.Tran2DFrame, 'auto')
                    if ~isempty(ProbeHalfX) && 2*ProbeHalfX > 3000
                        Args.Tran2DFrame = 'field';
                    else
                        Args.Tran2DFrame = 'percrop';
                    end
                end
                if isempty(Args.SubImgSize)
                    if strcmp(Args.Tran2DFrame, 'percrop') && ~isempty(ProbeHalfX)
                        Args.SubImgSize = [2*ProbeHalfX, 2*ProbeHalfY];
                    else
                        Args.SubImgSize = [1726, 1726];   % LAST default for field frame
                    end
                end

                GridRes = max(Args.SubImgSize(1) / Args.GridSize(1), 1);

                % Determine crop IDs
                if ~isempty(Args.CropIDs)
                    CropIDs = Args.CropIDs(:)';
                else
                    CropIDs = 1:Nobj;
                end

                % Build reference parameters for non-percrop modes
                RefParamVec = [];
                UseRefNorm = false;
                NormTran2D = true;
                if ~strcmp(Args.PhotSys, 'percrop')
                    RefIdx = Args.RefCrop;
                    if RefIdx == 0
                        % Weighted mean of all successful crops
                        AllParams = [];
                        AllWeights = [];
                        for Itmp = 1:Nobj
                            if ~isempty(Obj(Itmp).TransModel) && Obj(Itmp).TransModel.RMS > 0
                                P = Obj(Itmp).TransModel.getAllFunPar();
                                AllParams = [AllParams; P.Val(:)'];
                                AllWeights = [AllWeights; 1 ./ Obj(Itmp).TransModel.RMS.^2];
                            end
                        end
                        if ~isempty(AllParams)
                            W = AllWeights / sum(AllWeights);
                            RefParamVec = (W' * AllParams)';
                        end
                    elseif RefIdx >= 1 && RefIdx <= Nobj && ~isempty(Obj(RefIdx).TransModel)
                        RefTransParams = Obj(RefIdx).TransModel.getAllFunPar();
                        RefParamVec = RefTransParams.Val;
                    else
                        Obj(1).msgLog(LogLevel.Warning, ...
                                'plotZPMap: RefCrop=%d invalid or failed. Falling back to percrop.', RefIdx);
                    end
                    if ~isempty(RefParamVec)
                        UseRefNorm = ismember(Args.PhotSys, {'refzp', 'refzp_raw'});
                        NormTran2D = strcmp(Args.PhotSys, 'refzp');
                    end
                end

                % Collect all ZP data with global coordinates
                allX = [];
                allY = [];
                allZP = [];
                allCropID = [];
                allLocalX = [];
                allLocalY = [];

                % Per-crop geometry cache: UNIQSEC (local) + ORIGUSEC (global)
                % boxes for each crop. Filled from headers if AI is passed,
                % otherwise the legacy edge-to-edge boxes are stored so
                % labels / grid / overlay code below can share a single
                % linear (local -> global) mapping.
                CropUniqLocal  = nan(Nobj, 4);   % [XloL, XhiL, YloL, YhiL]
                CropUniqGlobal = nan(Nobj, 4);   % [XloG, XhiG, YloG, YhiG]

                % Preflight: if AI is passed, note whether every crop has
                % UNIQSEC + ORIGUSEC. If any crop lacks them we drop back
                % to CCDSEC/ORIGSEC (overlap-carrying) with a warning.
                UseAIGeom  = ~isempty(Args.AI);
                UsedUniq   = false;
                if UseAIGeom
                    if numel(Args.AI) < Nobj
                        Obj(1).msgLog(LogLevel.Warning, ...
                            'plotZPMap: AI array shorter (%d) than PC array (%d) - dropping AI-driven geometry', ...
                            numel(Args.AI), Nobj);
                        UseAIGeom = false;
                    end
                end

                for Iobj = 1:Nobj
                    if isempty(Obj(Iobj).TransModel)
                        continue;
                    end

                    CropID = CropIDs(Iobj);
                    [Row, Col] = PhotCalibTrans.cropID2RowCol(CropID, Args.Nrows, Args.Ncols, Args.TileOrder);

                    % Sample-grid bounds. When AI is available and carries
                    % UNIQSEC + ORIGUSEC, sample only within the crop's
                    % non-overlap partition (local coords = UNIQSEC) and
                    % place at its footprint in the detector (global coords
                    % = ORIGUSEC). Otherwise fall back to the legacy
                    % edge-to-edge placement.
                    UsedUniqThisCrop = false;
                    if UseAIGeom
                        UniqLocal = [];
                        UniqGlobal = [];
                        try
                            UniqLocal  = Args.AI(Iobj).HeaderData.getVal('UNIQSEC',  'ReadCCDSEC', true);
                            UniqGlobal = Args.AI(Iobj).HeaderData.getVal('ORIGUSEC', 'ReadCCDSEC', true);
                        catch
                        end
                        if isempty(UniqLocal) || any(~isfinite(UniqLocal(:))) || ...
                                isempty(UniqGlobal) || any(~isfinite(UniqGlobal(:)))
                            try
                                UniqLocal  = Args.AI(Iobj).HeaderData.getVal('CCDSEC',  'ReadCCDSEC', true);
                                UniqGlobal = Args.AI(Iobj).HeaderData.getVal('ORIGSEC', 'ReadCCDSEC', true);
                                if Iobj == 1
                                    Obj(1).msgLog(LogLevel.Warning, ...
                                        'plotZPMap: UNIQSEC/ORIGUSEC absent; falling back to CCDSEC/ORIGSEC (overlap will be rendered twice)');
                                end
                            catch
                                UniqLocal = []; UniqGlobal = [];
                            end
                        else
                            UsedUniqThisCrop = true;
                        end
                        if ~isempty(UniqLocal) && ~isempty(UniqGlobal)
                            XloL = UniqLocal(1);  XhiL = UniqLocal(2);
                            YloL = UniqLocal(3);  YhiL = UniqLocal(4);
                            XloG = UniqGlobal(1); XhiG = UniqGlobal(2);
                            YloG = UniqGlobal(3); YhiG = UniqGlobal(4);
                            Xvec_l = linspace(XloL, XhiL, Args.GridSize(1));
                            Yvec_l = linspace(YloL, YhiL, Args.GridSize(2));
                            [Xgrid, Ygrid] = meshgrid(Xvec_l, Yvec_l);
                            Xvec_g = linspace(XloG, XhiG, Args.GridSize(1));
                            Yvec_g = linspace(YloG, YhiG, Args.GridSize(2));
                            [XgridG, YgridG] = meshgrid(Xvec_g, Yvec_g);
                            X_global = XgridG(:);
                            Y_global = YgridG(:);
                            CropUniqLocal(Iobj,  :) = [XloL XhiL YloL YhiL];
                            CropUniqGlobal(Iobj, :) = [XloG XhiG YloG YhiG];
                            UsedUniq = UsedUniq || UsedUniqThisCrop;
                        else
                            % Fully failed to read AI geometry -> legacy path.
                            Nx = Args.SubImgSize(1);
                            Ny = Args.SubImgSize(2);
                            Xvec = linspace(1, Nx, Args.GridSize(1));
                            Yvec = linspace(1, Ny, Args.GridSize(2));
                            [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);
                            X_global = Xgrid(:) + (Col - 1) * Nx;
                            Y_global = Ygrid(:) + (Row - 1) * Ny;
                            CropUniqLocal(Iobj,  :) = [1 Nx 1 Ny];
                            CropUniqGlobal(Iobj, :) = ...
                                [(Col-1)*Nx  Col*Nx  (Row-1)*Ny  Row*Ny];
                        end
                    else
                        Nx = Args.SubImgSize(1);
                        Ny = Args.SubImgSize(2);
                        Xvec = linspace(1, Nx, Args.GridSize(1));
                        Yvec = linspace(1, Ny, Args.GridSize(2));
                        [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);
                        X_global = Xgrid(:) + (Col - 1) * Nx;
                        Y_global = Ygrid(:) + (Row - 1) * Ny;
                        CropUniqLocal(Iobj,  :) = [1 Nx 1 Ny];
                        CropUniqGlobal(Iobj, :) = ...
                            [(Col-1)*Nx  Col*Nx  (Row-1)*Ny  Row*Ny];
                    end

                    if ~isempty(RefParamVec)
                        PCeval = Obj(Iobj).derivePC(RefParamVec, ...
                            'UseRefNorm', UseRefNorm, ...
                            'NormTran2DToCenter', NormTran2D);
                    else
                        PCeval = Obj(Iobj);
                    end
                    % Field-frame Tran2D needs the global mosaic-frame (X,Y)
                    % for normalisation against ParNX/ParNY. Per-crop frame
                    % wants the crop-local (X,Y).
                    if strcmp(Args.Tran2DFrame, 'field')
                        ZP = PCeval.evaluateZP('X', X_global, 'Y', Y_global);
                    else
                        ZP = PCeval.evaluateZP('X', Xgrid(:), 'Y', Ygrid(:));
                    end
                    Npts = numel(ZP);

                    allX = [allX; X_global];
                    allY = [allY; Y_global];
                    allZP = [allZP; ZP(:)];
                    allCropID = [allCropID; repmat(CropID, Npts, 1)];
                    allLocalX = [allLocalX; Xgrid(:)];
                    allLocalY = [allLocalY; Ygrid(:)];
                end

                % Safety-net dedup: after collecting, if two samples fall
                % in the same 1-px cell in global coords, average their
                % ZP. Under UNIQSEC-based geometry this is effectively a
                % no-op (partition -> zero collisions); under legacy /
                % CCDSEC fallback it collapses overlap strips.
                if ~isempty(allX)
                    % Use int64 - int32 saturates at 2.15e9 which is
                    % well below Y*1e6 = 9576*1e6 = 9.6e9 for LAST.
                    Key = int64(round(allX)) + int64(round(allY)) * int64(1e6);
                    [KeyUniq, ~, ic] = unique(Key);
                    if numel(KeyUniq) < numel(Key)
                        Kcount = accumarray(ic, 1);
                        allZP_avg = accumarray(ic, allZP, [], @mean);
                        allX_avg  = accumarray(ic, allX,  [], @mean);
                        allY_avg  = accumarray(ic, allY,  [], @mean);
                        % Represent the merged cell with the modal CropID
                        % (only used for the sample-table provenance).
                        allCropID_agg = accumarray(ic, double(allCropID), [], @(v) mode(v));
                        allLocalX_agg = accumarray(ic, allLocalX,         [], @mean);
                        allLocalY_agg = accumarray(ic, allLocalY,         [], @mean);
                        allX      = allX_avg;
                        allY      = allY_avg;
                        allZP     = allZP_avg;
                        allCropID = allCropID_agg;
                        allLocalX = allLocalX_agg;
                        allLocalY = allLocalY_agg;
                        if any(Kcount > 1)
                            Obj(1).msgLog(LogLevel.Info, ...
                                'plotZPMap: %d overlap samples averaged across duplicate cells', ...
                                sum(Kcount) - numel(Kcount));
                        end
                    end
                end

                if isempty(allZP)
                    error('PhotCalibTrans:plotZPMap:NoData', ...
                          'No successful calibrations found.');
                end

                Valid = isfinite(allZP);
                allX = allX(Valid);
                allY = allY(Valid);
                allZP = allZP(Valid);
                allCropID = allCropID(Valid);
                allLocalX = allLocalX(Valid);
                allLocalY = allLocalY(Valid);

                % Sample table backing the scatteredInterpolant (mosaic)
                ZPTable = table(allCropID, allLocalX, allLocalY, allX, allY, allZP, ...
                    'VariableNames', {'CropID', 'LocalX', 'LocalY', 'X', 'Y', 'ZP'});

                if isempty(Args.CLim)
                    Args.CLim = [prctile(allZP, 1), prctile(allZP, 99)];
                end

                % Global grid. When AI-driven UNIQSEC geometry is in use,
                % the sample extent is the real-detector footprint and
                % Ncols*SubImgSize would over-shoot it; take the data
                % extent instead.
                if UsedUniq
                    XmaxG = ceil(max(allX));
                    YmaxG = ceil(max(allY));
                else
                    XmaxG = Args.Ncols * Args.SubImgSize(1);
                    YmaxG = Args.Nrows * Args.SubImgSize(2);
                end
                XvecG = 0:GridRes:XmaxG;
                YvecG = 0:GridRes:YmaxG;
                [XgridG, YgridG] = meshgrid(XvecG, YvecG);

                F = scatteredInterpolant(allX, allY, allZP, 'natural', 'none');
                ZPgrid = F(XgridG, YgridG);

                % Gaussian smoothing (NaN-aware)
                if Args.SmoothSigma > 0
                    KernelSize = ceil(Args.SmoothSigma * 6);
                    if mod(KernelSize, 2) == 0
                        KernelSize = KernelSize + 1;
                    end
                    Kernel = fspecial('gaussian', KernelSize, Args.SmoothSigma);

                    NanMask = isnan(ZPgrid);
                    ZPtemp = ZPgrid;
                    ZPtemp(NanMask) = 0;
                    ZPsmooth = conv2(ZPtemp, Kernel, 'same');
                    Weights = conv2(double(~NanMask), Kernel, 'same');
                    ZPgrid = ZPsmooth ./ Weights;
                    ZPgrid(NanMask) = NaN;
                end

                if Args.NewFigure
                    Fig = figure('Position', [100, 100, 800, 1000]);
                else
                    Fig = gcf;
                end

                imagesc(XvecG, YvecG, ZPgrid);
                caxis(Args.CLim);
                axis xy equal tight;
                colormap(jet);
                cb = colorbar;
                ylabel(cb, 'ZP [mag]');
                xlabel('X [pixels]');
                ylabel('Y [pixels]');

                % Draw crop boundaries. The interior lines sit at the
                % shared edges between adjacent crops' ORIGUSEC boxes:
                % gather all distinct low/high edges, drop the outermost
                % ones (that's the image frame, not a divider).
                hold on;
                XEdges = unique([CropUniqGlobal(:,1); CropUniqGlobal(:,2)]);
                YEdges = unique([CropUniqGlobal(:,3); CropUniqGlobal(:,4)]);
                XEdges = XEdges(XEdges > min(CropUniqGlobal(:,1)) & ...
                                XEdges < max(CropUniqGlobal(:,2)));
                YEdges = YEdges(YEdges > min(CropUniqGlobal(:,3)) & ...
                                YEdges < max(CropUniqGlobal(:,4)));
                for kk = 1:numel(XEdges)
                    plot([XEdges(kk), XEdges(kk)], [0, YmaxG], 'w-', 'LineWidth', 0.5);
                end
                for kk = 1:numel(YEdges)
                    plot([0, XmaxG], [YEdges(kk), YEdges(kk)], 'w-', 'LineWidth', 0.5);
                end

                % Label crop IDs at each ORIGUSEC centre.
                for Iobj = 1:Nobj
                    if any(isnan(CropUniqGlobal(Iobj, :))); continue; end
                    CropID = CropIDs(Iobj);
                    Xc = mean(CropUniqGlobal(Iobj, 1:2));
                    Yc = mean(CropUniqGlobal(Iobj, 3:4));
                    text(Xc, Yc, sprintf('%d', CropID), ...
                        'HorizontalAlignment', 'center', 'Color', 'w', ...
                        'FontSize', 8, 'FontWeight', 'bold');
                end

                % Overlay calibrator positions per crop, mapped from the
                % SourceData's local (X,Y) to global via linear rescaling
                % of the crop's UNIQSEC(local) -> ORIGUSEC(global) box.
                % In the legacy path both boxes reduce to the tile so the
                % result is identical to the old (Col-1)*SubImgSize offset.
                if Args.OverlayCalibrators ~= "none"
                    for Iobj = 1:Nobj
                        if isempty(Obj(Iobj).SourceData); continue; end
                        if any(isnan(CropUniqGlobal(Iobj, :))); continue; end
                        [Xall, Yall, UsedFlag, Mode] = PhotCalibTrans.resolveOverlay(...
                            Obj(Iobj).SourceData, Args.OverlayCalibrators);
                        XloL = CropUniqLocal(Iobj, 1);  XhiL = CropUniqLocal(Iobj, 2);
                        YloL = CropUniqLocal(Iobj, 3);  YhiL = CropUniqLocal(Iobj, 4);
                        XloG = CropUniqGlobal(Iobj, 1); XhiG = CropUniqGlobal(Iobj, 2);
                        YloG = CropUniqGlobal(Iobj, 3); YhiG = CropUniqGlobal(Iobj, 4);
                        Xg = XloG + (Xall - XloL) .* (XhiG - XloG) ./ (XhiL - XloL);
                        Yg = YloG + (Yall - YloL) .* (YhiG - YloG) ./ (YhiL - YloL);
                        switch Mode
                            case "all"
                                plot(Xg, Yg, 'w.', 'MarkerSize', 4);
                            case "used"
                                plot(Xg(UsedFlag), Yg(UsedFlag), 'w.', 'MarkerSize', 4);
                            case "both"
                                plot(Xg( UsedFlag), Yg( UsedFlag), 'w.', 'MarkerSize', 4);
                                plot(Xg(~UsedFlag), Yg(~UsedFlag), '.', 'Color', [0.6 0.6 0.6], 'MarkerSize', 4);
                        end
                    end
                end
                hold off;

                title(sprintf('ZP Mosaic — %s (%d crops, range %.3f mag)', ...
                    Args.PhotSys, Nobj, Args.CLim(2) - Args.CLim(1)));
            end
        end

        function [Fig, S] = plotCalibratorWeightHist(Obj, Args)
            % Histogram of calibrator weights aggregated across crops,
            % split into used (KeepMask survivors) vs rejected (clipped).
            % Pulls SourceData.MagErr / Used columns from every PC in Obj.
            % Input  : - PhotCalibTrans object array (e.g., 24 crops of one image).
            %          * ...,key,val,...
            %            'NewFigure' - logical. Default true.
            %            'Quantity'  - 'Weight' (= 1/MagErr^2) | 'MagErr' | 'LogMagErr'.
            %                          Default 'Weight'.
            %            'NBins'     - integer. Default 50.
            %            'Scale'     - 'log' | 'linear' x-axis. Default 'log'
            %                          for Weight, 'linear' otherwise.
            %            'MagErrCol' - column name for per-source MagErr.
            %                          Default 'MagErr'.
            %            'UsedCol'   - column name for the kept-flag.
            %                          Default 'Used'. Missing column → all
            %                          rows treated as used.
            % Output : - Fig: figure handle.
            %          - S: struct with fields .Quantity, .Used, .Rejected
            %               (raw vectors used for the histograms).
            % Example: PC.plotCalibratorWeightHist();
            %          PC.plotCalibratorWeightHist('Quantity','MagErr','Scale','log');
            arguments
                Obj
                Args.NewFigure logical = true
                Args.Quantity   = 'Weight'
                Args.NBins      = 50
                Args.Scale      = ''
                Args.MagErrCol  = 'MagErr'
                Args.UsedCol    = 'Used'
            end

            Nobj = numel(Obj);
            AllMagErr = [];
            AllUsed   = [];
            NCrops    = 0;
            for I = 1:Nobj
                if isempty(Obj(I).SourceData)
                    continue;
                end
                SD = Obj(I).SourceData;
                ME = [];
                try
                    ME = SD.getCol(Args.MagErrCol);
                catch
                    if istable(SD.Catalog) && ismember(Args.MagErrCol, ...
                            SD.Catalog.Properties.VariableNames)
                        ME = SD.Catalog.(Args.MagErrCol);
                    end
                end
                if isempty(ME)
                    continue;
                end
                US = [];
                try
                    US = logical(SD.getCol(Args.UsedCol));
                catch
                    if istable(SD.Catalog) && ismember(Args.UsedCol, ...
                            SD.Catalog.Properties.VariableNames)
                        US = logical(SD.Catalog.(Args.UsedCol));
                    end
                end
                if isempty(US)
                    US = true(numel(ME), 1);
                end
                AllMagErr = [AllMagErr; ME(:)];
                AllUsed   = [AllUsed;   US(:)];
                NCrops    = NCrops + 1;
            end

            if isempty(AllMagErr)
                error('PhotCalibTrans:plotCalibratorWeightHist:NoData', ...
                    'No SourceData.%s found across PC array.', Args.MagErrCol);
            end

            switch lower(string(Args.Quantity))
                case "weight"
                    Q = 1 ./ AllMagErr.^2;
                    XLab = sprintf('Weight = 1/%s^2', Args.MagErrCol);
                    if isempty(Args.Scale), Args.Scale = 'log'; end
                case "magerr"
                    Q = AllMagErr;
                    XLab = Args.MagErrCol;
                    if isempty(Args.Scale), Args.Scale = 'linear'; end
                case "logmagerr"
                    Q = log10(AllMagErr);
                    XLab = sprintf('log_{10}(%s)', Args.MagErrCol);
                    if isempty(Args.Scale), Args.Scale = 'linear'; end
                otherwise
                    error('PhotCalibTrans:plotCalibratorWeightHist:BadQuantity', ...
                        'Quantity must be ''Weight''|''MagErr''|''LogMagErr''.');
            end

            Valid = isfinite(Q);
            if strcmpi(Args.Scale, 'log')
                Valid = Valid & Q > 0;
            end
            Q       = Q(Valid);
            UsedV   = logical(AllUsed(Valid));

            if strcmpi(Args.Scale, 'log')
                Edges = logspace(log10(min(Q)), log10(max(Q)), Args.NBins+1);
            else
                Edges = linspace(min(Q), max(Q), Args.NBins+1);
            end

            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            histogram(Q(UsedV), Edges, 'FaceColor', [0.2 0.4 0.8], ...
                'EdgeColor', 'none', 'FaceAlpha', 0.6, ...
                'DisplayName', sprintf('Used (n=%d)', sum(UsedV)));
            hold on;
            histogram(Q(~UsedV), Edges, 'FaceColor', [0.85 0.33 0.1], ...
                'EdgeColor', 'none', 'FaceAlpha', 0.6, ...
                'DisplayName', sprintf('Rejected (n=%d)', sum(~UsedV)));
            if strcmpi(Args.Scale, 'log')
                set(gca, 'XScale', 'log');
            end
            xlabel(XLab);
            ylabel('Count');
            legend('Location', 'best');
            title(sprintf('Calibrator weight distribution — %d crops', NCrops));
            grid on;
            hold off;

            if nargout > 1
                S = struct('Quantity', Q, 'Used', Q(UsedV), 'Rejected', Q(~UsedV));
            end
        end

        function Fig = plotCalibrators(Obj, Args)
            % Plot observed vs predicted magnitudes for calibrators
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotCalibrators();
            % Description: Shows 1:1 plot of observed vs model-predicted magnitudes.
            %              Includes RMS and Chi2/DOF statistics.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel) || isempty(Obj.SourceData)
                error('PhotCalibTrans:plotCalibrators:NoFitResults', ...
                    'Fit results not available (TransModel or SourceData is empty)');
            end
            % Use the live Table view directly. getCol routes through
            % AstroTable.ColNames, which caches the original column list
            % and doesn't see the Residuals/Used/PredictedFlux columns
            % that calibrate appends to the table-form Catalog.
            SDTab = Obj.SourceData.Table;
            SDCols = SDTab.Properties.VariableNames;
            if ~ismember('Residuals', SDCols) || ~ismember('Flux', SDCols)
                error('PhotCalibTrans:plotCalibrators:NoFitResults', ...
                    'SourceData missing Residuals/Flux columns - run calibrate first');
            end

            % Read residuals AND fluxes from SourceData so the two are
            % index-aligned. Restrict to calibrators kept by sigma
            % clipping (Used==true) when that column is present.
            Residuals = SDTab.Residuals;
            Flux_obs  = SDTab.Flux;
            if ismember('Used', SDCols)
                UsedMask  = logical(SDTab.Used);
                Residuals = Residuals(UsedMask);
                Flux_obs  = Flux_obs(UsedMask);
            end

            MagInst_obs  = -2.5 * log10(Flux_obs);
            % Predicted instrumental magnitude (Residual = observed - predicted, mag).
            MagInst_pred = MagInst_obs - Residuals;

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Plot 1:1 comparison
            scatter(MagInst_pred, MagInst_obs, 40, 'filled', 'MarkerFaceAlpha', 0.6);
            hold on;

            % Add 1:1 line
            Lims = [min([MagInst_pred; MagInst_obs]), max([MagInst_pred; MagInst_obs])];
            plot(Lims, Lims, 'k--', 'LineWidth', 2);

            % Add RMS error bands
            plot(Lims, Lims + Obj.TransModel.RMS, 'r--', 'LineWidth', 1);
            plot(Lims, Lims - Obj.TransModel.RMS, 'r--', 'LineWidth', 1);

            grid on;
            xlabel('Model Predicted Magnitude');
            ylabel('Observed Magnitude');
            axis equal tight;

            % Add statistics to title. N_used is the calibrator count
            % AFTER all filtering and sigma clipping (matches the points
            % being plotted and the DOF denominator). N_initial is the
            % raw GAIADR3spec match count for context.
            NumUsed    = numel(MagInst_obs);
            NumInitial = size(Obj.SpecData.Spec, 1);
            if ~isempty(Obj.TransModel.Chi2) && ~isempty(Obj.TransModel.DOF)
                title(sprintf('Calibrators: N_{used}=%d / N_{init}=%d, RMS=%.4f mag, Chi^2/DOF=%.2f/%d=%.2f', ...
                    NumUsed, NumInitial, Obj.TransModel.RMS, ...
                    Obj.TransModel.Chi2, Obj.TransModel.DOF, ...
                    Obj.TransModel.Chi2/Obj.TransModel.DOF));
            else
                title(sprintf('Calibrators: N_{used}=%d / N_{init}=%d, RMS=%.4f mag', ...
                    NumUsed, NumInitial, Obj.TransModel.RMS));
            end

            % Add legend
            legend('Calibrators', '1:1 line', 'RMS bounds', 'Location', 'best');
        end

        function Fig = plotFitQuality(Obj, Args)
            % Plot RMS/Chi2 evolution across optimization stages
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotFitQuality();
            % Description: Shows convergence of fit across optimization stages.
            %              Displays RMS, Chi2/DOF evolution, and number of calibrators.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.FitResults)
                error('PhotCalibTrans:plotFitQuality:NoFitResults', ...
                      'Fit results not available. Run calibrate() first.');
            end

            % Prefer AllOuterStages (Niter*Nstages flat array) when present,
            % so the plot shows the per-outer-iteration evolution. Falls back
            % to the final outer pass's per-stage results otherwise.
            FitResAll = Obj.FitResults;
            if isfield(FitResAll, 'AllOuterStages') && ~isempty(FitResAll(1).AllOuterStages)
                FitRes = FitResAll(1).AllOuterStages;
            else
                FitRes = FitResAll;
            end
            Npts = length(FitRes);

            % Stage / iteration indices for labeling and boundaries
            HasIter = isfield(FitRes, 'OuterIter');
            if HasIter
                OuterIters = [FitRes.OuterIter];
                Niter = max(OuterIters);
                Nstages = Npts / Niter;
                XLabels = arrayfun(@(K) sprintf('S%d.I%d', ...
                    mod(K-1, Nstages)+1, OuterIters(K)), 1:Npts, ...
                    'UniformOutput', false);
                % Vertical boundaries between iterations (after each block of Nstages)
                IterBoundaries = (Nstages + 0.5):Nstages:(Npts - 0.5);
            else
                Nstages = Npts;
                Niter = 1;
                XLabels = arrayfun(@(K) sprintf('S%d', K), 1:Npts, ...
                    'UniformOutput', false);
                IterBoundaries = [];
            end

            % Extract per-point metrics
            RMS_pts        = nan(Npts, 1);
            WeightedRMS_pts = nan(Npts, 1);
            Chi2_pts       = nan(Npts, 1);
            DOF_pts        = nan(Npts, 1);
            for I = 1:Npts
                if ~isempty(FitRes(I).RMS); RMS_pts(I) = FitRes(I).RMS; end
                if isfield(FitRes(I), 'Chi2') && ~isempty(FitRes(I).Chi2)
                    Chi2_pts(I) = FitRes(I).Chi2;
                end
                if isfield(FitRes(I), 'DOF') && ~isempty(FitRes(I).DOF)
                    DOF_pts(I) = FitRes(I).DOF;
                end
                if isfield(FitRes(I), 'WeightedResiduals') && ~isempty(FitRes(I).WeightedResiduals)
                    WRes = FitRes(I).WeightedResiduals;
                    WeightedRMS_pts(I) = sqrt(mean(WRes(:).^2));
                end
            end

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            X = 1:Npts;
            BoundaryColor = [0.5 0.5 0.5];

            % --- Subplot 1: RMS evolution (unweighted [mag] left, weighted [sigma] right)
            subplot(2, 1, 1); cla;
            yyaxis left;
            plot(X, RMS_pts, 'o-', 'LineWidth', 2, 'MarkerSize', 8); hold on;
            ylabel('RMS [mag]');
            yyaxis right;
            plot(X, WeightedRMS_pts, 's--', 'LineWidth', 1.5, 'MarkerSize', 7);
            ylabel('Weighted RMS [\sigma]');
            % Iteration boundary lines (drawn on right axis; they span both)
            for B = IterBoundaries
                xline(B, '--', 'Color', BoundaryColor, 'LineWidth', 1.0, ...
                    'HandleVisibility', 'off');
            end
            grid on;
            xlabel('Optimization Stage');
            xticks(X); xticklabels(XLabels);
            title(sprintf('Fit Convergence (N=%d initial calibrators, %d outer iter)', ...
                size(Obj.SpecData.Spec, 1), Niter));
            legend({'Unweighted RMS [mag]', 'Weighted RMS [\sigma]'}, 'Location', 'best');

            % --- Subplot 2: Chi2/DOF evolution
            subplot(2, 1, 2); cla;
            if any(isfinite(Chi2_pts) & Chi2_pts ~= 0) && any(isfinite(DOF_pts) & DOF_pts ~= 0)
                Chi2PerDOF = Chi2_pts ./ DOF_pts;
                plot(X, Chi2PerDOF, 's-', 'LineWidth', 2, 'MarkerSize', 8); hold on;
                yline(1, 'r--', 'LineWidth', 1.5);
                ylabel('Chi2/DOF');
                legend('Fit Quality', 'Location', 'best');
            else
                plot(X, Chi2_pts, 's-', 'LineWidth', 2, 'MarkerSize', 8); hold on;
                ylabel('Chi2');
            end
            for B = IterBoundaries
                xline(B, '--', 'Color', BoundaryColor, 'LineWidth', 1.0, ...
                    'HandleVisibility', 'off');
            end
            grid on;
            xlabel('Optimization Stage');
            xticks(X); xticklabels(XLabels);
            title('Goodness of Fit Evolution');
        end
    end

    methods
        function [EpochAIs, NormPerEpoch, DeltaZP] = applyPhotCalibShifts(Obj, EpochAIs, Args)
            % Apply coadd photometric calibration to individual epoch images.
            %   Handles multi-crop layout: EpochAIs is [Nepoch × Ncrop] and
            %   Obj (the PC array) is [1 × Ncrop]. DeltaZP is [Nepoch × Ncrop]
            %   matrix (from lcUtil.zp_meddiff per crop). The expensive
            %   transmission-integral evaluation runs ONCE per crop, with
            %   cheap position-dependent Tran2D correction per epoch. Scalar
            %   Obj / 1-D EpochAIs are accepted (treated as Ncrop = 1).
            % Input  : - [1 × Ncrop] PhotCalibTrans array (one per crop).
            %          - [Nepoch × Ncrop] AstroImage matrix. May be [Nepoch × 1]
            %            when Ncrop = 1.
            %          * ...,key,val,...
            %            'DeltaZP'   - [Nepoch × Ncrop] pre-computed ZP shifts
            %                        [mag]. Same sign as zp_meddiff.FitZP:
            %                        positive when epoch is fainter.
            %                        Default is [] (compute from MS).
            %            'MS'        - Cell array {Ncrop} of MatchedSources,
            %                        one per crop, or scalar MatchedSources for
            %                        Ncrop = 1. Used when DeltaZP is empty.
            %            'FluxField' - Flux field in MS.Data. Default is 'FLUX_APER_3'.
            %            'FluxErrField' - Flux error field. Default is 'FLUXERR_APER_3'.
            %            'RefEpoch'  - Reference epoch for MS-based DeltaZP.
            %                        Default is 1.
            %            'UseWeightedMedian' - Use weighted median for MS-based
            %                        DeltaZP. Default is true.
            %            'AddMag'    - Add MAG columns. Default is true.
            %            'AddZP'     - Add ZP column. Default is true.
            %            'ApplyAperCorr' - Apply inherited aperture corrections
            %                        to MAG_<System>_* columns. Default is true.
            %            'UpdateHeader' - Write PT_* to epoch headers. Default is true.
            %            'MagSystem' - Magnitude system. Default is 'AB'.
            %            'Verbose'   - Print progress. Default is true.
            %            'PropagateBackMag' - Compute & write BACKMAG to each
            %                        epoch header [mag/arcsec^2]. Per-epoch
            %                        formula:
            %                          BACKMAG = (ZP_base + dZP)
            %                                  - 2.5*log10(MedBack_ep / ExpTime_ep)
            %                                  + 5*log10(PixScale_ep)
            %                        Requires the corresponding coadd-level
            %                        value to exist (isfinite(PC_c.BackMag));
            %                        if any crop has NaN BackMag, that crop is
            %                        skipped with a Warning. Set
            %                        EvaluateBackMag=true at calibrate/fit time
            %                        to populate the coadd value first.
            %                        Default is false.
            %            'PropagateLimMag' - Compute & write LIMMAG to each
            %                        epoch header [mag at SN=LimMagSN]. Primary:
            %                        empirical polyfit of MAG vs log10(SN) on
            %                        the freshly-calibrated epoch catalog.
            %                        Fallback when <3 sources in [MinSN, MaxSN]:
            %                          LIMMAG_ep = PC.LimMag + dZP
            %                                    - 2.5*log10(sqrt(PC.NCoadd))
            %                        Requires isfinite(PC_c.LimMag) (i.e. the
            %                        coadd-level evaluateLimMag must have run);
            %                        crops with NaN LimMag are skipped with a
            %                        Warning. Default is false.
            %            'PixScale'  - Override pixel scale [arcsec/pix] for
            %                        BACKMAG. If empty, read per-epoch from
            %                        AIie.WCS.getScale('arcsec'). Default is [].
            %            'LimMagFluxCol' - FLUX column whose matching FLUXERR
            %                        drives SN; <MagColPrefix><suffix> is fit
            %                        for LIMMAG. Default is 'FLUX_APER_3'.
            %            'LimMagSN'  - SN at which to evaluate limiting
            %                        magnitude. Default is 5.
            %            'MinSN'     - Lower SN bound for LIMMAG fit window.
            %                        Default is 5.
            %            'MaxSN'     - Upper SN bound for LIMMAG fit window.
            %                        Default is 50.
            %            'LimMagMethod' - Method for per-epoch LIMMAG:
            %                          'auto'       - empirical polyfit with
            %                                       analytical fallback when
            %                                       polyfit can't run (default).
            %                          'empirical'  - empirical only; NaN if
            %                                       polyfit can't run.
            %                          'analytical' - skip polyfit, always use
            %                                       PC.LimMag + dZP - 2.5*log10(sqrt(NCoadd)).
            % Output : - Updated AstroImages with MAG, ZP, header.
            %            When PropagateBackMag/PropagateLimMag is true, the
            %            epoch headers also carry BACKMAG/LIMMAG keys.
            %          - [Nepoch × Ncrop] per-epoch Norm values.
            %          - [Nepoch × Ncrop] DeltaZP matrix [mag].
            % Author : D. Kovaleva (Apr 2026)
            % Example: % Multi-crop fast path (instance call):
            %          [AIs, Norms, DZP] = PC.applyPhotCalibShifts(AllSI, 'DeltaZP', ZPdif);
            %
            %          % Same plus per-epoch BACKMAG/LIMMAG. Requires the coadd
            %          % calibration to have set PC.BackMag and PC.LimMag (i.e.
            %          % fitPhotCalibTrans called with EvaluateBackMag=true and
            %          % EvaluateLimMag=true). Crops with NaN coadd value are
            %          % skipped with a Warning.
            %          [AIs, Norms, DZP] = PC.applyPhotCalibShifts(AllSI, ...
            %              'DeltaZP',          ZPdif, ...
            %              'PropagateBackMag', true, ...
            %              'PropagateLimMag',  true);
            %
            %          % Programmatic readback per epoch:
            %          AIs(1, 5).HeaderData.getVal('BACKMAG')   % mag/arcsec^2
            %          AIs(1, 5).HeaderData.getVal('LIMMAG')    % mag at SN=5

            arguments
                Obj  PhotCalibTrans
                EpochAIs
                Args.DeltaZP = []
                Args.MS = []
                Args.FluxField = 'FLUX_APER_3'
                Args.FluxErrField = 'FLUXERR_APER_3'
                Args.RefEpoch = 1
                Args.UseWeightedMedian logical = true
                Args.AddMag logical = true
                Args.AddZP logical = true
                Args.ApplyAperCorr logical = true
                Args.UpdateHeader logical = true
                Args.MagSystem char = 'AB'
                Args.Verbose logical = false
                % --- Per-epoch BACKMAG / LIMMAG propagation (optional) ---
                % Each Propagate* flag requires the corresponding coadd-level
                % Evaluate* to have populated PC_c.BackMag / PC_c.LimMag at
                % fit time. Per-crop graceful degradation: a NaN coadd value
                % skips that crop with a Warning instead of erroring out.
                Args.PropagateBackMag logical = false
                Args.PropagateLimMag  logical = false
                Args.PixScale         double  = []           % [arcsec/pix] override; [] => AIie.WCS.getScale
                Args.LimMagFluxCol    char    = 'FLUX_APER_3'   % FLUX_<suffix>; FLUXERR_<suffix> -> SN, <MagColPrefix><suffix> fit
                Args.LimMagSN         double  = 5
                Args.MinSN            double  = 5
                Args.MaxSN            double  = 50
                % LIMMAG method selector:
                %   'auto'       - empirical polyfit, analytical fallback if polyfit fails (default)
                %   'empirical'  - empirical only; NaN if polyfit fails (no fallback)
                %   'analytical' - analytical formula only; skip polyfit entirely
                Args.LimMagMethod char {mustBeMember(Args.LimMagMethod, {'auto', 'empirical', 'analytical'})} = 'auto'
            end

            % ---- Normalize dimensions ----
            % EpochAIs: [Nepoch × Ncrop]. Obj: [1 × Ncrop].
            Ncrop = numel(Obj);
            SizeAI = size(EpochAIs);
            if Ncrop == 1
                EpochAIs = EpochAIs(:);  % force column
                Nepoch   = numel(EpochAIs);
            else
                if SizeAI(2) ~= Ncrop
                    if SizeAI(1) == Ncrop
                        EpochAIs = EpochAIs.';  % transpose to [Nepoch × Ncrop]
                        SizeAI = size(EpochAIs);
                    else
                        error('PhotCalibTrans:applyPhotCalibShifts:BadShape', ...
                            'EpochAIs must be [Nepoch x Ncrop] with Ncrop=%d.', Ncrop);
                    end
                end
                Nepoch = SizeAI(1);
            end

            % =============================================================
            % Step 1: Obtain DeltaZP — from input or from MS
            % =============================================================

            if ~isempty(Args.DeltaZP)
                DeltaZP = Args.DeltaZP;
                % Accept zp_meddiff output struct array: [1 x Ncrop] with .FitZP
                if isstruct(DeltaZP) && isfield(DeltaZP, 'FitZP')
                    DZmat = nan(Nepoch, numel(DeltaZP));
                    for Ic = 1:numel(DeltaZP)
                        DZmat(:, Ic) = DeltaZP(Ic).FitZP(:);
                    end
                    DeltaZP = DZmat;
                end
                if Ncrop == 1
                    DeltaZP = DeltaZP(:);
                end
                if ~isequal(size(DeltaZP), [Nepoch, Ncrop])
                    error('PhotCalibTrans:applyPhotCalibShifts:BadDeltaZP', ...
                        'DeltaZP size must be [%d x %d], got [%d x %d].', ...
                        Nepoch, Ncrop, size(DeltaZP,1), size(DeltaZP,2));
                end
            elseif ~isempty(Args.MS)
                DeltaZP = nan(Nepoch, Ncrop);
                MScell = Args.MS;
                if ~iscell(MScell)
                    MScell = {MScell};
                end
                for Ic = 1:Ncrop
                    DeltaZP(:, Ic) = computeDeltaZPfromMS(MScell{Ic}, Nepoch, ...
                        Args.FluxField, Args.FluxErrField, ...
                        Args.RefEpoch, Args.UseWeightedMedian);
                end
            else
                error('PhotCalibTrans:applyPhotCalibShifts:NoInput', ...
                    'Provide either DeltaZP matrix or MS (MatchedSources).');
            end

            % Mean-center DeltaZP per crop: coadd ZP reflects the pooled
            % data level across epochs, not epoch 1's level. No-op if the
            % input is already centered.
            DeltaZP = DeltaZP - mean(DeltaZP, 1, 'omitnan');

            % =============================================================
            % Step 2: Loop over crops (outer), epochs (inner)
            %         evaluateZP called ONCE per crop (Ncrop× total).
            %         Tran2D.forward called per epoch (cheap).
            % =============================================================

            NormPerEpoch = nan(Nepoch, Ncrop);

            for Ic = 1:Ncrop
                PC_c = Obj(Ic);

                % Guard: crops with no calibration (empty TransModel) — for
                % example when selectCalibrators failed because the catalog
                % was missing RA/Dec or per-source X,Y were null. Log a
                % warning, NaN-fill each epoch's MAG/ZP columns, write NaN
                % PT_* keys to the header, and skip the rest of this crop.
                if isempty(PC_c.TransModel)
                    PC_c.msgLog(LogLevel.Warning, sprintf( ...
                        'applyPhotCalibShifts: crop %d has no calibration (empty TransModel) - writing NaN to MAG/ZP columns', Ic));
                    NormPerEpoch(:, Ic) = NaN;
                    for Ie = 1:Nepoch
                        AIie = EpochAIs(Ie, Ic);
                        if isa(AIie, 'AstroImage')
                            CatObj = AIie.CatData;
                        else
                            CatObj = AIie;
                        end
                        if ~isempty(CatObj) && ~isempty(CatObj.Catalog)
                            Nrows  = size(CatObj.Catalog, 1);
                            NaNcol = nan(Nrows, 1);
                            if Args.AddZP
                                ZPColName = [Args.MagSystem, '_ZP'];
                                CatObj = CatObj.insertCol(NaNcol, Inf, ZPColName, {}, 'OmitValidation', true);
                            end
                            if Args.AddMag
                                AllColNames = CatObj.ColNames;
                                MagPrefix   = PC_c.MagColPrefix;
                                IsFlux      = startsWith(AllColNames, 'FLUX_');
                                FluxColNames = AllColNames(IsFlux);
                                % FLUX_XYPEAK is the pixel peak value, not a
                                % photometric flux — skip it.
                                FluxColNames = FluxColNames(~strcmp(FluxColNames, 'FLUX_XYPEAK'));
                                for I = 1:numel(FluxColNames)
                                    NewMagColName = strrep(FluxColNames{I}, 'FLUX_', MagPrefix);
                                    CatObj = CatObj.insertCol(NaNcol, Inf, NewMagColName, {}, 'OmitValidation', true);
                                    % MAGERR written when an error source
                                    % exists: FLUXERR_<suffix> or, for
                                    % FLUX_PSF, SN.
                                    FluxErrCol = strrep(FluxColNames{I}, 'FLUX_', 'FLUXERR_');
                                    HasErrSource = any(strcmp(AllColNames, FluxErrCol)) || ...
                                        (strcmp(FluxColNames{I}, 'FLUX_PSF') && any(strcmp(AllColNames, 'SN')));
                                    if HasErrSource
                                        MagErrColName = regexprep(NewMagColName, '^MAG_', 'MAGERR_');
                                        CatObj = CatObj.insertCol(NaNcol, Inf, MagErrColName, {}, 'OmitValidation', true);
                                    end
                                end
                            end
                            if isa(AIie, 'AstroImage')
                                EpochAIs(Ie, Ic).CatData = CatObj;
                            else
                                EpochAIs(Ie, Ic) = CatObj;
                            end
                        end
                        % Header NaN-fill: photCalibTransToHeader is robust
                        % to empty TransModel and writes scalar PT_* as NaN.
                        if Args.UpdateHeader && isa(AIie, 'AstroImage') && ~isempty(AIie.HeaderData)
                            EpochAIs(Ie, Ic).HeaderData = PC_c.photCalibTransToHeader(AIie.HeaderData);
                        end
                    end
                    continue;
                end

                AllFunPar = PC_c.TransModel.getAllFunPar();
                NormIdx   = find(strcmp(AllFunPar.Name, 'Norm'), 1);
                NormOrig  = AllFunPar.Val(NormIdx);
                ExpTime_coadd = PC_c.ExpTime / (PC_c.NCoadd * PC_c.NFramesPerCoadd);

                % Per-crop template header (PT_*/APCOR rows only). Cheaper
                % per-epoch path: pre-locate the PT_1_V1 (Norm) row so we can
                % patch it once and append the whole block in a single
                % Data-setter call instead of ~100 replaceVal invocations.
                if Args.UpdateHeader
                    TemplateHeader = AstroHeader();
                    PC_c.photCalibTransToHeader(TemplateHeader);
                    TemplateKeys = TemplateHeader.Data;
                    NormRowIdx = find(strcmp(TemplateKeys(:, 1), 'PT_1_V1'), 1, 'last');
                end

                % Expensive scalar ZP_base (transmission integral) once per crop
                ZP_base   = PC_c.evaluateZP('MagSystem', Args.MagSystem);
                HasTran2D = ~isempty(PC_c.TransModel.Tran2DObj) && PC_c.TransModel.UseTran2D;

                % Per-crop gates for BACKMAG / LIMMAG propagation. A NaN
                % coadd-level value disables propagation for this crop only
                % (graceful degradation in multi-crop runs).
                DoPropBackMag = Args.PropagateBackMag && isfinite(PC_c.BackMag);
                DoPropLimMag  = Args.PropagateLimMag  && isfinite(PC_c.LimMag);
                if Args.PropagateBackMag && ~isfinite(PC_c.BackMag)
                    PC_c.msgLog(LogLevel.Warning, sprintf( ...
                        'applyPhotCalibShifts: crop %d: PropagateBackMag=true but PC.BackMag is NaN (Evaluate it at fit time) - BACKMAG propagation skipped for this crop', Ic));
                end
                if Args.PropagateLimMag && ~isfinite(PC_c.LimMag)
                    PC_c.msgLog(LogLevel.Warning, sprintf( ...
                        'applyPhotCalibShifts: crop %d: PropagateLimMag=true but PC.LimMag is NaN (Evaluate it at fit time) - LIMMAG propagation skipped for this crop', Ic));
                end

                if Args.Verbose
                    fprintf('  Crop %02d: ZP_base=%.4f, Norm=%.6f\n', ...
                        Ic, ZP_base, NormOrig);
                end

                for Ie = 1:Nepoch
                    dZP = DeltaZP(Ie, Ic);
                    NormNew = NormOrig;
                    if isfinite(dZP)
                        NormNew = NormOrig * 10^(dZP / 2.5);
                    end
                    NormPerEpoch(Ie, Ic) = NormNew;

                    AIie = EpochAIs(Ie, Ic);
                    if isa(AIie, 'AstroImage')
                        CatObj = AIie.CatData;
                    else
                        CatObj = AIie;
                    end
                    % Direct numeric access: CatObj.Table getter calls array2table
                    % on numeric Catalog every invocation (AstroTable.m:276).
                    HasCat = ~isempty(CatObj) && ~isempty(CatObj.Catalog) && size(CatObj.Catalog, 1) > 0;

                    ExpTime_epoch = ExpTime_coadd;
                    if isa(AIie, 'AstroImage') && ~isempty(AIie.HeaderData)
                        ExpTimeVal = AIie.HeaderData.getVal('EXPTIME');
                        if ~isempty(ExpTimeVal) && isnumeric(ExpTimeVal)
                            ExpTime_epoch = ExpTimeVal;
                        end
                    end

                    if HasCat && (Args.AddMag || Args.AddZP)
                        % Direct numeric access — avoids per-call array2table
                        % from CatObj.Table getter.
                        AllColNames = CatObj.ColNames;
                        Nrows       = size(CatObj.Catalog, 1);

                        Xidx = find(strcmp(AllColNames, 'X'), 1);
                        Yidx = find(strcmp(AllColNames, 'Y'), 1);
                        if HasTran2D && ~isempty(Xidx) && ~isempty(Yidx)
                            Xcol = CatObj.Catalog(:, Xidx);
                            Ycol = CatObj.Catalog(:, Yidx);
                            [FieldCorr, ~] = PC_c.TransModel.Tran2DObj.forward([Xcol, Ycol]);
                            ZP_epoch = (ZP_base - FieldCorr(:)) + dZP;
                        else
                            ZP_epoch = repmat(ZP_base + dZP, Nrows, 1);
                        end

                        if Args.AddZP
                            ZPColName = [Args.MagSystem, '_ZP'];
                            CatObj = CatObj.insertCol(ZP_epoch, Inf, ZPColName, {}, 'OmitValidation', true);
                        end

                        if Args.AddMag
                            % Naming prefix from the per-crop PC object's
                            % property — set once at calibration time, travels
                            % with the object into this per-epoch path.
                            MagPrefix = PC_c.MagColPrefix;
                            IsFlux    = startsWith(AllColNames, 'FLUX_');
                            FluxColIdx   = find(IsFlux);
                            FluxColNames = AllColNames(IsFlux);
                            % FLUX_XYPEAK is the pixel peak value, not a
                            % photometric flux — skip it.
                            KeepMask     = ~strcmp(FluxColNames, 'FLUX_XYPEAK');
                            FluxColIdx   = FluxColIdx(KeepMask);
                            FluxColNames = FluxColNames(KeepMask);
                            for I = 1:numel(FluxColNames)
                                Flux_col = CatObj.Catalog(:, FluxColIdx(I));
                                Mag = convert.luptitude(Flux_col / ExpTime_epoch, ...
                                    10.^(0.4 .* ZP_epoch));
                                NewMagColName = strrep(FluxColNames{I}, 'FLUX_', MagPrefix);

                                if Args.ApplyAperCorr && ~isempty(PC_c.AperCorr) && ...
                                        ~isempty(PC_c.AperCorrColNames)
                                    % Match AperCorrColNames stored in either
                                    % mag mode (MAG_<sys>_*) or flux mode (FLUX_*).
                                    AperIdx = find(strcmp(PC_c.AperCorrColNames, NewMagColName) | ...
                                                   strcmp(PC_c.AperCorrColNames, FluxColNames{I}), 1);
                                    if ~isempty(AperIdx) && isfinite(PC_c.AperCorr(AperIdx))
                                        % Sign matches fitPhotCalibTrans / calcAperCorr:
                                        % AperCorr = MagRef - MagAper (<=0 for smaller apertures),
                                        % applied as Mag + AperCorr.
                                        Mag = Mag + PC_c.AperCorr(AperIdx);
                                    end
                                end

                                CatObj = CatObj.insertCol(Mag, Inf, NewMagColName, {}, 'OmitValidation', true);

                                % MAGERR source priority:
                                %   (1) FLUXERR_<suffix> -> MagErr = 1.086 * FLUXERR
                                %   (2) FLUX_PSF special case -> SN column,
                                %       MagErr = 1.086 / SN.
                                % If neither is available, no MAGERR column is
                                % written (no NaN-fill, no instrumental copy).
                                FluxErrCol = strrep(FluxColNames{I}, 'FLUX_', 'FLUXERR_');
                                FluxErrIdx = find(strcmp(AllColNames, FluxErrCol), 1);
                                if ~isempty(FluxErrIdx)
                                    MagErrCol = regexprep(NewMagColName, '^MAG_', 'MAGERR_');
                                    FluxErr = CatObj.Catalog(:, FluxErrIdx);
                                    % FLUXERR is the relative flux uncertainty
                                    % (dF/F per LAST source extractor).
                                    MagErr = nan(Nrows, 1);
                                    ValidFlux = Flux_col > 0 & isfinite(Flux_col) & isfinite(FluxErr);
                                    MagErr(ValidFlux) = 1.086 .* FluxErr(ValidFlux);
                                    CatObj = CatObj.insertCol(MagErr, Inf, MagErrCol, {}, 'OmitValidation', true);
                                elseif strcmp(FluxColNames{I}, 'FLUX_PSF')
                                    SnIdx = find(strcmp(AllColNames, 'SN'), 1);
                                    if ~isempty(SnIdx)
                                        MagErrCol = regexprep(NewMagColName, '^MAG_', 'MAGERR_');
                                        SN = CatObj.Catalog(:, SnIdx);
                                        MagErr = nan(Nrows, 1);
                                        ValidSN = isfinite(SN) & SN > 0;
                                        MagErr(ValidSN) = 1.086 ./ SN(ValidSN);
                                        CatObj = CatObj.insertCol(MagErr, Inf, MagErrCol, {}, 'OmitValidation', true);
                                    end
                                end
                            end
                        end
                    end

                    if isa(AIie, 'AstroImage')
                        EpochAIs(Ie, Ic).CatData = CatObj;
                    end

                    if Args.UpdateHeader && isa(AIie, 'AstroImage') && ~isempty(AIie.HeaderData)
                        EpochHeader = EpochAIs(Ie, Ic).HeaderData;
                        EpochHeader.deleteKey({'PT_.*', 'APCOR.*'});
                        EpochTemplate = TemplateKeys;
                        if ~isempty(NormRowIdx)
                            EpochTemplate{NormRowIdx, 2} = NormNew;
                        end
                        EpochHeader.Data = [EpochHeader.Data; EpochTemplate];
                    end

                    % --- BACKMAG: scalar sky surface brightness per epoch ---
                    % Mirrors evaluateBackMag's coadd-level formula but with
                    % the epoch's own ExpTime, MedBack and PixScale; ZP uses
                    % the field-centre ZP_base shifted by dZP (no Tran2D).
                    if DoPropBackMag && isa(AIie, 'AstroImage')
                        % Fast path: MEDBCK already in epoch header (written by backVar).
                        MedBack_ep = NaN;
                        if ~isempty(AIie.HeaderData) && AIie.HeaderData.isKeyExist('MEDBCK')
                            HVal = AIie.HeaderData.getVal('MEDBCK');
                            if isnumeric(HVal) && isscalar(HVal) && isfinite(HVal) && HVal > 0
                                MedBack_ep = HVal;
                            end
                        end
                        % Fallback: take the median over Back image (or Image if Back is empty).
                        if ~isfinite(MedBack_ep)
                            BackPix = AIie.Back;
                            if isempty(BackPix)
                                BackPix = AIie.Image;
                            end
                            if ~isempty(BackPix)
                                MedBack_ep = fast_median(double(BackPix(:)));
                            end
                        end

                        if ~isempty(Args.PixScale)
                            PixScale_ep = Args.PixScale;
                        else
                            try
                                PixScale_ep = AIie.WCS.getScale('arcsec');
                            catch
                                PixScale_ep = NaN;
                            end
                        end

                        ZP_epoch_scalar = ZP_base + dZP;
                        BackMag_ep      = NaN;
                        if isfinite(MedBack_ep) && MedBack_ep > 0 && ...
                                isfinite(PixScale_ep) && PixScale_ep > 0 && ...
                                isfinite(ZP_epoch_scalar) && ...
                                isfinite(ExpTime_epoch) && ExpTime_epoch > 0
                            BackMag_ep = ZP_epoch_scalar ...
                                - 2.5*log10(MedBack_ep / ExpTime_epoch) ...
                                + 5*log10(PixScale_ep);
                        end

                        if Args.UpdateHeader && ~isempty(AIie.HeaderData)
                            EpochAIs(Ie, Ic).HeaderData = ...
                                EpochAIs(Ie, Ic).HeaderData.replaceVal('BACKMAG', BackMag_ep);
                        end
                    end

                    % --- LIMMAG: method selected by Args.LimMagMethod ---
                    % 'auto'       : empirical polyfit (MAG vs log10(SN), same
                    %                fit as evaluateLimMag) with analytical
                    %                fallback when polyfit can't run
                    %                (<3 sources in SN window, missing columns,
                    %                or non-finite output).
                    % 'empirical'  : empirical only; NaN on failure.
                    % 'analytical' : skip polyfit; scale coadd LimMag by
                    %                sqrt(NCoadd) and apply dZP.
                    if DoPropLimMag
                        LimMag_ep = NaN;
                        TryEmpirical = strcmpi(Args.LimMagMethod, 'auto') || ...
                                       strcmpi(Args.LimMagMethod, 'empirical');
                        UseAnalytical = strcmpi(Args.LimMagMethod, 'analytical');
                        DoFallback   = strcmpi(Args.LimMagMethod, 'auto');

                        if TryEmpirical
                            Tokens = regexp(Args.LimMagFluxCol, '^FLUX_(.+)$', 'tokens', 'once');
                            if HasCat && ~isempty(Tokens)
                                Suffix         = Tokens{1};
                                FluxErrColName = ['FLUXERR_', Suffix];
                                MagColName     = [PC_c.MagColPrefix, Suffix];
                                EpochColNames  = CatObj.ColNames;
                                if any(strcmp(EpochColNames, FluxErrColName)) && ...
                                        any(strcmp(EpochColNames, MagColName))
                                    FluxErrIdx = find(strcmp(EpochColNames, FluxErrColName), 1);
                                    MagIdx     = find(strcmp(EpochColNames, MagColName), 1);
                                    FluxErrCol = CatObj.Catalog(:, FluxErrIdx);
                                    MagAll     = CatObj.Catalog(:, MagIdx);
                                    % FLUXERR is relative (dF/F); SN = 1/FLUXERR.
                                    SNall = 1 ./ FluxErrCol;
                                    Valid = isfinite(MagAll) & isfinite(SNall) & ...
                                            SNall > Args.MinSN & SNall < Args.MaxSN;
                                    if nnz(Valid) >= 3
                                        Pfit      = polyfit(log10(SNall(Valid)), MagAll(Valid), 1);
                                        LimMag_ep = polyval(Pfit, log10(Args.LimMagSN));
                                    end
                                end
                            end
                        end
                        if (UseAnalytical || (DoFallback && ~isfinite(LimMag_ep))) && PC_c.NCoadd > 0
                            LimMag_ep = PC_c.LimMag + dZP - 2.5*log10(sqrt(PC_c.NCoadd));
                        end

                        if Args.UpdateHeader && isa(AIie, 'AstroImage') && ~isempty(AIie.HeaderData)
                            EpochAIs(Ie, Ic).HeaderData = ...
                                EpochAIs(Ie, Ic).HeaderData.replaceVal('LIMMAG', LimMag_ep);
                        end
                    end
                end
            end

        end

        function CBP = buildConstBandParams(Obj, Args)
            % Build constant-band parameters from fitted PhotCalibTrans objects.
            %   Extracts fitted atmospheric parameters (excluding Norm,
            %   ZenithAngle_deg, Temperature_C) from each object's TransModel.
            %   Two modes controlled by 'Source':
            %     'aggregate' (default) — robust median/mean across all objects.
            %     'single' — extract from a single object directly (no aggregation).
            %   Optionally saves to .mat file with date in filename.
            % Input  : - Obj — PhotCalibTrans array (flat). Failed objects (empty TransModel)
            %            are skipped. If a cell of arrays, flatten first at the
            %            call site: [PC_cell{:}].buildConstBandParams(...).
            %            For Source='single', the first successful object is used.
            %          * ...,key,val,...
            %            'Source'     - 'aggregate' (median/mean across objects)
            %                        or 'single' (extract from one object).
            %                        Default is 'aggregate'.
            %            'Method'     - 'median' or 'mean' (for aggregate mode).
            %                        Default is 'median'.
            %            'OutputPath' - Directory for output .mat. Default is '' (no save).
            %            'OutputName' - Filename prefix. Default is 'ConstBandParams_LAST'.
            %                        Date suffix appended automatically.
            %            'ExcludeParams' - Parameter names to exclude beyond
            %                        Norm/ZenithAngle/Temperature. Default is {}.
            %            'Verbose'    - Print summary. Default is true.
            % Output : - ConstBandParams struct with one field per parameter.
            %            Also contains .CreatedDate, .NObjects, .Method metadata.
            % Author : D. Kovaleva (Mar 2026)
            % Example: % Aggregate:
            %          CBP = PC_all.buildConstBandParams();
            %          % From cached cell storage:
            %          S = load('results/PC_percrop.mat');
            %          CBP = [S.PC_all{:}].buildConstBandParams('OutputPath', '~/data');
            %          % Single crop:
            %          CBP = PC_all(10).buildConstBandParams('Source', 'single');

            arguments
                Obj  PhotCalibTrans
                Args.Source = 'aggregate'       % 'aggregate' or 'single'
                Args.Method = 'median'
                Args.OutputPath = ''
                Args.OutputName = 'ConstBandParams_LAST'
                Args.ExcludeParams cell = {}
                Args.Verbose logical = false
            end

            PCArray = Obj(:);

            % Parameters to always exclude
            AlwaysExclude = {'Norm', 'ZenithAngle_deg', 'Temperature_C'};
            ExcludeSet = [AlwaysExclude, Args.ExcludeParams(:)'];

            % --- Single mode: extract from one object ---
            if strcmpi(Args.Source, 'single')
                % Find first successful object
                PC = [];
                for Ipc = 1:numel(PCArray)
                    if ~isempty(PCArray(Ipc).TransModel)
                        PC = PCArray(Ipc);
                        break;
                    end
                end
                if isempty(PC)
                    error('PhotCalibTrans:buildConstBandParams:NoFit', ...
                        'No successful fit found in input');
                end

                AllPar = PC.TransModel.getAllFunPar();
                CBP = struct();
                for Ip = 1:numel(AllPar.Name)
                    Name = AllPar.Name{Ip};
                    if AllPar.FitPar(Ip) && ~ismember(Name, ExcludeSet)
                        CBP.(Name) = AllPar.Val(Ip);
                    end
                end

                CBP.CreatedDate = datestr(now, 'yyyy-mm-dd');
                CBP.NObjects = 1;
                CBP.Method = 'single';

                if Args.Verbose
                    ParamNames = fieldnames(CBP);
                    fprintf('ConstBandParams from single object:\n');
                    for Ip = 1:numel(ParamNames)
                        if isnumeric(CBP.(ParamNames{Ip})) && isscalar(CBP.(ParamNames{Ip}))
                            fprintf('  %-25s = %.6g\n', ParamNames{Ip}, CBP.(ParamNames{Ip}));
                        end
                    end
                end

            else
                % --- Aggregate mode: collect from all objects ---
                ParamValues = [];   % [Nobj x Nparams]
                ParamNames = {};
                KeepIdx = [];
                Nobj = 0;

                for Ipc = 1:numel(PCArray)
                    PC = PCArray(Ipc);
                    if isempty(PC.TransModel)
                        continue;
                    end

                    AllPar = PC.TransModel.getAllFunPar();

                    % On first successful object, set up parameter names
                    if isempty(ParamNames)
                        KeepMask = AllPar.FitPar(:);
                        for Ie = 1:numel(ExcludeSet)
                            KeepMask = KeepMask & ~strcmp(AllPar.Name, ExcludeSet{Ie});
                        end
                        KeepIdx = find(KeepMask);
                        ParamNames = AllPar.Name(KeepIdx);
                        ParamValues = nan(numel(PCArray), numel(KeepIdx));
                    end

                    Nobj = Nobj + 1;
                    for Ip = 1:numel(KeepIdx)
                        Idx = find(strcmp(AllPar.Name, ParamNames{Ip}), 1);
                        if ~isempty(Idx)
                            ParamValues(Nobj, Ip) = AllPar.Val(Idx);
                        end
                    end
                end

                ParamValues = ParamValues(1:Nobj, :);

                % Compute robust statistic
                CBP = struct();
                for Ip = 1:numel(ParamNames)
                    Vals = ParamValues(:, Ip);
                    Vals = Vals(isfinite(Vals));
                    switch lower(Args.Method)
                        case 'median'
                            CBP.(ParamNames{Ip}) = median(Vals);
                        case 'mean'
                            CBP.(ParamNames{Ip}) = mean(Vals);
                    end
                end

                CBP.CreatedDate = datestr(now, 'yyyy-mm-dd');
                CBP.NObjects = Nobj;
                CBP.Method = Args.Method;

                if Args.Verbose
                    fprintf('ConstBandParams from %d objects (%s):\n', Nobj, Args.Method);
                    for Ip = 1:numel(ParamNames)
                        fprintf('  %-25s = %.6g\n', ParamNames{Ip}, CBP.(ParamNames{Ip}));
                    end
                end
            end

            % Save if OutputPath specified
            if ~isempty(Args.OutputPath)
                DateStr = datestr(now, 'yyyymmdd');
                FileName = sprintf('%s_%s.mat', Args.OutputName, DateStr);
                FullPath = fullfile(Args.OutputPath, FileName);
                ConstBandParams = CBP; %#ok<NASGU>
                save(FullPath, 'ConstBandParams');
                if Args.Verbose
                    fprintf('Saved: %s\n', FullPath);
                end
            end
        end
    end

    methods (Static)
        % Static methods defined in separate files under @PhotCalibTrans/
        [Cands, FieldTab, CatH]             = findCalibCandidates(Cat, Args)
        [KeepMask, Reason]                  = applyCalibQuality(Cands, Args)
        [DoubtfulMask, Reason]              = auditCalibCandidates(CandTab, FieldTab, Args)

        function [Row, Col] = cropID2RowCol(CropID, Nrows, Ncols, TileOrder)
            % Convert CropID to grid (Row, Col) position
            % Input  : - CropID (scalar integer, 1-based)
            %          - Nrows - number of rows in grid
            %          - Ncols - number of columns in grid
            %          - TileOrder - 'colmajor' or 'rowmajor'
            %            'colmajor' (old pipeline): CropIDs fill bottom-to-top,
            %              then left-to-right. CropID 1..Nrows = column 1, etc.
            %            'rowmajor' (new pipeline): CropIDs fill left-to-right,
            %              then bottom-to-top. CropID 1..Ncols = row 1, etc.
            % Output : - Row (1-based, 1 = bottom)
            %          - Col (1-based, 1 = left)

            switch lower(TileOrder)
                case 'colmajor'
                    Col = ceil(CropID / Nrows);
                    Row = mod(CropID - 1, Nrows) + 1;
                case 'rowmajor'
                    Row = ceil(CropID / Ncols);
                    Col = mod(CropID - 1, Ncols) + 1;
                otherwise
                    error('PhotCalibTrans:cropID2RowCol:BadTileOrder', ...
                          'TileOrder must be ''colmajor'' or ''rowmajor'', got ''%s''.', TileOrder);
            end
        end

        function [Xall, Yall, UsedFlag, Mode] = resolveOverlay(SourceData, Mode)
            % Extract X/Y/UsedFlag from a SourceData catalog for plotZPMap overlay.
            % Falls back to 'all' if the requested mode needs a missing 'Used' column.
            % AstroTable's ColNames property and the underlying Catalog table can
            % drift (ColNames is cached on the property; a column added to Catalog
            % afterwards isn't reflected). Probe via try/getCol to handle either.
            % Input  : - SourceData (AstroCatalog from PhotCalibTrans).
            %          - Mode (string) - 'both'|'used'|'all'.
            % Output : - Xall, Yall column vectors.
            %          - UsedFlag logical vector (true=survivor).
            %          - Mode (string) - possibly downgraded to 'all'.
            Xall = SourceData.getCol('X');
            Yall = SourceData.getCol('Y');
            UsedFlag = [];
            try
                UsedFlag = logical(SourceData.getCol('Used'));
            catch
                if istable(SourceData.Catalog) && ismember('Used', ...
                        SourceData.Catalog.Properties.VariableNames)
                    UsedFlag = logical(SourceData.Catalog.Used);
                end
            end
            if isempty(UsedFlag)
                UsedFlag = true(numel(Xall), 1);
                if Mode == "used" || Mode == "both"
                    Mode = "all";
                end
            end
        end

        function KeyName = fluxCol2AperCorrKey(ColName)
            % Convert flux/mag column name to FITS header keyword (max 8 chars)
            %   FLUX_APER_1 / MAG_AB_APER_1 / MAG_APER_1 -> APCOR_A1
            %   FLUX_PSF    / MAG_AB_PSF                 -> APCOR_PS
            % Input  : - Column name (char)
            % Output : - FITS keyword (char, max 8 chars)

            if contains(ColName, 'APER_')
                Suffix = extractAfter(ColName, 'APER_');
                KeyName = ['APCOR_A', Suffix];
            elseif endsWith(ColName, 'PSF')
                KeyName = 'APCOR_PS';
            else
                % Generic: take last 2 chars of column name
                Tag = ColName(max(1, end-1):end);
                KeyName = ['APCOR_', Tag];
            end
        end
    end

end

% =========================================================================
function DeltaZP = computeDeltaZPfromMS(MS, Nepoch, FluxField, FluxErrField, RefEpoch, UseWeightedMedian)
    % Compute per-epoch DeltaZP from MatchedSources flux comparison (legacy).
    % Same sign as zp_meddiff: positive when epoch is fainter.

    [~, GoodStar] = MS.selectGoodPhotCalibStars();
    Flux = MS.getMatrix(FluxField);
    Flux = Flux(:, GoodStar);

    FluxRef = Flux(RefEpoch, :);
    ValidRef = FluxRef > 0 & isfinite(FluxRef);

    DiffMag = -2.5 * log10(Flux ./ FluxRef);
    ValidMask = isfinite(DiffMag) & ValidRef;

    HasFluxErr = UseWeightedMedian && isfield(MS.Data, FluxErrField);
    if HasFluxErr
        FluxErr = MS.getMatrix(FluxErrField);
        FluxErr = FluxErr(:, GoodStar);
        FluxErrRef = FluxErr(RefEpoch, :);
    end

    DeltaZP = nan(Nepoch, 1);
    for Ie = 1:Nepoch
        if Ie == RefEpoch
            DeltaZP(Ie) = 0;
        elseif sum(ValidMask(Ie, :)) < 5
            DeltaZP(Ie) = NaN;
        elseif HasFluxErr
            Valid = ValidMask(Ie, :);
            DM = DiffMag(Ie, Valid);
            % FLUXERR is already the relative flux uncertainty (dF/F),
            % so the per-DiffMag error is 1.086 * sqrt(eF^2 + eF_ref^2)
            % with no extra /Flux divisions.
            MagErr = 1.086 * sqrt(FluxErr(Ie, Valid).^2 + FluxErrRef(Valid).^2);
            DeltaZP(Ie) = tools.math.stat.wmedian(DM(:), MagErr(:), 1);
        else
            DeltaZP(Ie) = median(DiffMag(Ie, ValidMask(Ie, :)), 'omitnan');
        end
    end
end

% =========================================================================
function JD = readAirmassTime(HeaderObj, TimeKey)
    % Read a time value from an AstroHeader and return it as a JD (double).
    % Supported keys: 'DATE-OBS' (ISO string), 'JD', 'MIDJD' (numeric scalars).
    JD = NaN;
    if isempty(HeaderObj); return; end
    switch upper(TimeKey)
        case {'JD', 'MIDJD'}
            Val = HeaderObj.getVal(TimeKey);
            if isnumeric(Val) && isscalar(Val); JD = double(Val); end
        case 'DATE-OBS'
            Val = HeaderObj.getVal('DATE-OBS');
            if ischar(Val) || isstring(Val)
                try
                    DT = datetime(string(Val), 'InputFormat', ...
                        'yyyy-MM-dd''T''HH:mm:ss.SSS', 'TimeZone', 'UTC');
                    JD = juliandate(DT);
                catch
                    % fall back to MATLAB's lenient parser
                    try
                        DT = datetime(string(Val), 'TimeZone', 'UTC');
                        JD = juliandate(DT);
                    catch
                        % leave JD as NaN
                    end
                end
            elseif isnumeric(Val) && isscalar(Val)
                JD = double(Val);  % tolerate numeric DATE-OBS
            end
        otherwise
            error('PhotCalibTrans:calibrate:BadAirmassTimeKey', ...
                'AirmassTimeKey must be ''DATE-OBS'', ''JD'', or ''MIDJD'' (got ''%s'')', TimeKey);
    end
end

% =========================================================================
function idx = findColIdxLocal(ColNames, Candidates)
    idx = 0;
    for I = 1:numel(Candidates)
        f = find(strcmp(ColNames, Candidates{I}), 1);
        if ~isempty(f); idx = f; return; end
    end
end


% =========================================================================
function Obj = selectCalibratorsPythonLike(Obj, Cat, Args)
    % Mirror the Python prototype's calibrator-selection recipe using
    % only catsHTM (GAIADR3spec + GAIADR3). Public entry point is
    % PhotCalibTrans/selectCalibrators with SelectionMethod='pythonLike'.
    %
    % Pipeline:
    %   1. Cone-match LAST catalogue to GAIADR3spec (spectra) and to
    %      GAIADR3 (PM + photometry) within Args.SearchRadius.
    %   2. Keep sources with a unique 1-1 match in GAIADR3spec AND any
    %      positional hit in GAIADR3. (GAIADR3spec is implicitly the
    %      XP-sampled subset of Gaia DR3, so uniqueness on it acts as
    %      Python's `has_xp_sampled=TRUE AND G in [12,16]` pre-filter +
    %      unique-1-1 check on the filtered set. A faint non-XP blender
    %      in GAIADR3 within SearchRadius does NOT cause rejection.)
    %   3. Propagate Gaia J2016 positions to Args.ObsJD using GAIADR3
    %      PMRA/PMDec, re-check the Args.SearchRadius gate.
    %   4. Apply Python's filters (MagRange via Gaia G mag, FLAGS
    %      bitmask, SN window, FLUX_<col> > 0, FLUX_PSF > 0).
    %   5. Optional Args.UseTAPClassprob
    %   6. Populate Obj.SpecData / Obj.SourceData / Obj.CalFound exactly
    %      as the default catsHTM path, so downstream code is unaffected.
    %
    % MagRange override semantics: respects the caller's value unless it
    % is exactly the catsHTM default [11.5 16.0], in which case Python's
    % [12, 16] applies. MinSN/MaxSN/BadBitNames defaults already match
    % the Python recipe, so they pass through unchanged. SearchRadius=2
    % arcsec matches Python's 2*u.arcsec.

    RAD = constant.RAD;

    if isempty(Cat) || isempty(Cat.Table)
        Obj.msgLog(LogLevel.Warning, ...
            'selectCalibratorsPythonLike: empty input catalogue');
        Obj.SourceData = []; Obj.SpecData = []; Obj.CalFound = false;
        return
    end

    Tab          = Cat.Table;
    Nsources     = height(Tab);
    AllColNames  = Tab.Properties.VariableNames;
    HasRADec     = ismember('RA', AllColNames) && ismember('Dec', AllColNames);

    if ~HasRADec
        Obj.NoRADec = true;
        Obj.msgLog(LogLevel.Warning, ...
            'selectCalibratorsPythonLike: RA/Dec missing - cannot match');
        Obj.SourceData = []; Obj.SpecData = []; Obj.CalFound = false;
        return
    end

    % --- Resolve Python-recipe defaults (override only if at catsHTM default) ---
    if isequal(Args.MagRange, [11.5 16.0])
        MagRange = [12, 16];
    else
        MagRange = Args.MagRange;
    end

    if Args.Verbose
        fprintf('  [pythonLike] match %d sources to GAIADR3spec (radius=%.1f arcsec)...\n', ...
                Nsources, Args.SearchRadius);
    end

    % --- Cone match against GAIADR3spec only (Jun 2026 regen) ---
    % The parallel GAIADR3 cone match that this code used to do (for
    % PMRA/PMDec/phot_g_mean_mag/classprob_dsc_combmod_star) was retired:
    % the GAIADR3spec regen attached cols 693-700 (PM + Gaia photometry +
    % bp_rp + bp_rp_excess + classprob_dsc_combmod_star) to every row, so
    % every value we used to pull from CatH_G now lives on CatH_S.
    [~, ~, ResIndS, CatH_S] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', ...
        'Radius', Args.SearchRadius, 'RadiusUnits', 'arcsec', Args.match_catsHTMArgs{:});

    SIdx = ResIndS.Obj2_IndInObj1;
    SNm  = ResIndS.Obj2_NmatchObj1;

    % Keep LAST sources with unique 1-1 match in GAIADR3spec.
    %   GAIADR3spec is the XP-sampled subset of Gaia DR3, so a "1 in spec"
    %   match is implicitly filtered to XP-available sources -- matching
    %   Python's prototype (`has_xp_sampled = TRUE` pre-filter + unique-1-1
    %   check). The earlier `BLENDED in GAIADR3` second-neighbour rejection
    %   was already disabled before this refactor (it cost spec matches
    %   without gain) so removing the parallel GAIADR3 match changes
    %   nothing semantically.
    HasBoth = ~isnan(SIdx) & (SNm == 1);

    if ~any(HasBoth)
        Obj.msgLog(LogLevel.Warning, sprintf( ...
            'selectCalibratorsPythonLike: no LAST sources matched GAIADR3spec with unique 1-1 hit within %.1f arcsec', ...
            Args.SearchRadius));
        Obj.SourceData = []; Obj.SpecData = []; Obj.CalFound = false;
        return
    end

    if Args.Verbose
        fprintf('  [pythonLike] %d sources with unique GAIADR3spec match\n', sum(HasBoth));
    end

    % --- Locate required GAIADR3spec columns (case-insensitive synonyms) ---
    GColNames = CatH_S.ColNames;
    GRAi      = findColIdxLocal(GColNames, {'RA'});
    GDeci     = findColIdxLocal(GColNames, {'Dec'});
    PMRAi     = findColIdxLocal(GColNames, {'PMRA', 'pmra'});
    PMDeci    = findColIdxLocal(GColNames, {'PMDec', 'pmdec'});
    GMagi     = findColIdxLocal(GColNames, {'phot_g_mean_mag'});

    if any([GRAi, GDeci, PMRAi, PMDeci] == 0)
        Obj.msgLog(LogLevel.Warning, ...
            'selectCalibratorsPythonLike: required GAIADR3spec columns missing (RA/Dec/PMRA/PMDec) - skipping PM propagation');
    end

    % --- PM propagation (always-on per design when ObsJD is available) ---
    CandIdx = find(HasBoth);
    Ncand   = numel(CandIdx);

    GR_2016 = double(CatH_S.Catalog(SIdx(CandIdx), GRAi))  .* RAD;  % rad -> deg
    GD_2016 = double(CatH_S.Catalog(SIdx(CandIdx), GDeci)) .* RAD;
    PMra    = double(CatH_S.Catalog(SIdx(CandIdx), PMRAi));   % mas/yr
    PMdec   = double(CatH_S.Catalog(SIdx(CandIdx), PMDeci));  % mas/yr
    PMra(~isfinite(PMra))   = 0;
    PMdec(~isfinite(PMdec)) = 0;

    HavePM = isfinite(Args.ObsJD) && PMRAi > 0 && PMDeci > 0;
    if HavePM
        % JD -> decimal year via J2000.0 (TT not distinguished from UTC at this precision)
        Yr_obs = 2000.0 + (Args.ObsJD - 2451545.0) / 365.25;
        dt_yr  = Yr_obs - 2016.0;
        % Linear PM propagation - valid to mas precision over decade timescales
        GR_obs = GR_2016 + (PMra  .* dt_yr) ./ (cosd(GD_2016) .* 3.6e6);
        GD_obs = GD_2016 + (PMdec .* dt_yr) ./ 3.6e6;
    else
        if ~isfinite(Args.ObsJD)
            Obj.msgLog(LogLevel.Warning, ...
                'selectCalibratorsPythonLike: ObsJD missing - skipping PM propagation');
        end
        GR_obs = GR_2016;
        GD_obs = GD_2016;
        dt_yr  = NaN;
    end

    % Recompute LAST <-> Gaia distance at obs JD; re-filter by SearchRadius
    LAST_RA  = Tab.RA(CandIdx);
    LAST_Dec = Tab.Dec(CandIdx);
    Dist_rad = celestial.coo.sphere_dist_fast( ...
        deg2rad(GR_obs), deg2rad(GD_obs), ...
        deg2rad(LAST_RA), deg2rad(LAST_Dec));
    Dist_arcsec = Dist_rad .* RAD .* 3600;

    GoodMask = Dist_arcsec < Args.SearchRadius;

    if Args.Verbose && HavePM
        fprintf('  [pythonLike] %d/%d still within %.1f arcsec after PM propagation (dt=%.2f yr)\n', ...
                sum(GoodMask), Ncand, Args.SearchRadius, dt_yr);
    end

    % --- Filter cascade (Python recipe) ---
    %   (a) MagRange via GAIADR3spec phot_g_mean_mag (Python uses Gaia G, not LAST)
    if GMagi > 0
        GMag = double(CatH_S.Catalog(SIdx(CandIdx), GMagi));
        GoodMask = GoodMask & (GMag >= MagRange(1)) & (GMag <= MagRange(2));
    end

    %   (b) Bad FLAGS bitmask
    if Args.FilterBadFlags && ismember('FLAGS', AllColNames)
        Flags    = Tab.FLAGS(CandIdx);
        BadValue = isnan(Flags) | isinf(Flags) | Flags < 0 | Flags ~= floor(Flags);
        Flags(BadValue) = 0;
        BD = BitDictionary('BitMask.Image.Default');
        [~, ~, BadBitMask] = BD.name2bit(Args.BadBitNames);
        BadFlagsMask = BadValue | bitand(uint32(Flags), uint32(BadBitMask)) > 0;
        GoodMask = GoodMask & ~BadFlagsMask;
    end

    %   (c) S/N range from LAST SN column
    if ismember('SN', AllColNames)
        SN = Tab.SN(CandIdx);
        GoodMask = GoodMask & (SN >= Args.MinSN) & (SN <= Args.MaxSN);
    end

    %   (d) FLUX_<col> > 0 and FLUX_PSF > 0 (Python checks both explicitly)
    if ismember(Args.FluxColName, AllColNames)
        F = Tab.(Args.FluxColName)(CandIdx);
        GoodMask = GoodMask & (F > 0);
    end
    if ismember('FLUX_PSF', AllColNames)
        Fpsf = Tab.FLUX_PSF(CandIdx);
        GoodMask = GoodMask & (Fpsf > 0);
    end

    % --- Optional classprob filter (now: direct column read, was: TAP query) ---
    % Pre-Jun-2026, this block ran a VO.TopCat / STILTS TAP query against
    % gaiadr3.gaia_source for `classprob_dsc_combmod_star > 0.9 AND
    % has_xp_sampled = TRUE`, then matched results to candidate J2016
    % positions within 0.5 arcsec. After the GAIADR3spec regen, that
    % column is at position 700 of every matched row and `has_xp_sampled`
    % is implicit (GAIADR3spec only contains XP-sampled sources). One line
    % of column-read replaces the whole TAP + match step. The
    % `UseTAPClassprob` arg name is preserved for backwards compat with
    % existing callers — semantically it now means "apply classprob>0.9
    % filter using the catsHTM column".
    if Args.UseTAPClassprob && any(GoodMask)
        ClassprobI = findColIdxLocal(GColNames, {'classprob_dsc_combmod_star'});
        if ClassprobI > 0
            Classprob = double(CatH_S.Catalog(SIdx(CandIdx), ClassprobI));
            ClassMask = isfinite(Classprob) & Classprob > 0.9;
            Before = sum(GoodMask);
            GoodMask = GoodMask & ClassMask;
            if Args.Verbose
                fprintf('  [pythonLike] classprob filter (catsHTM col): %d/%d candidates retained\n', ...
                        sum(GoodMask), Before);
            end
        else
            Obj.msgLog(LogLevel.Warning, ...
                'selectCalibratorsPythonLike: classprob_dsc_combmod_star not in GAIADR3spec - classprob filter NOT applied');
            if Args.Verbose
                fprintf('  [pythonLike] classprob_dsc_combmod_star missing from CatH_S - filter SKIPPED\n');
            end
        end
    end

    if ~any(GoodMask)
        Obj.msgLog(LogLevel.Warning, ...
            'selectCalibratorsPythonLike: all candidates rejected by filter cascade');
        Obj.SourceData = []; Obj.SpecData = []; Obj.CalFound = false;
        return
    end

    if Args.Verbose
        fprintf('  [pythonLike] %d sources passed all filters\n', sum(GoodMask));
    end

    % --- Build outputs (same shape as catsHTM path) ---
    KeptIdx  = CandIdx(GoodMask);
    SpecRows = SIdx(KeptIdx);

    SpecArr = CatH_S.Catalog;
    SpecTab = SpecArr(SpecRows, :);

    FluxIni  = Args.SpFluxCol(1);
    FluxEnd  = Args.SpFluxCol(2);
    EFluxIni = Args.SpFluxCol(3);
    EFluxEnd = Args.SpFluxCol(4);

    SpecFlux = double(SpecTab(:, FluxIni:FluxEnd));    % [N x N_wvl]
    SpecErr  = double(SpecTab(:, EFluxIni:EFluxEnd));  % [N x N_wvl]
    Cal_RA   = double(SpecTab(:, 1)) .* RAD;           % rad -> deg
    Cal_Dec  = double(SpecTab(:, 2)) .* RAD;

    ObsTab   = Tab(KeptIdx, :);
    Obs_X    = ObsTab.X;
    Obs_Y    = ObsTab.Y;
    Obs_RA   = ObsTab.RA;
    Obs_Dec  = ObsTab.Dec;
    Obs_Flux = ObsTab.(Args.FluxColName);

    FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
    if ismember(FluxErrColName, AllColNames)
        Obs_FluxErr = ObsTab.(FluxErrColName);
    else
        Obs_FluxErr = sqrt(abs(Obs_Flux));
        Obj.msgLog(LogLevel.Warning, sprintf( ...
            'selectCalibratorsPythonLike: %s not found, using sqrt(flux) for errors', ...
            FluxErrColName));
    end

    % MatchDistance is the PM-corrected separation [arcsec]
    DistArcsec = Dist_arcsec(GoodMask);
    Nmatch     = double(SNm(KeptIdx));   % all 1 by construction; preserved for shape parity

    HasAirmassCol = ismember('AIRMASS', AllColNames);
    if HasAirmassCol
        Obs_Airmass = ObsTab.AIRMASS;
    end

    % Final per-source validity guard (mirrors catsHTM path)
    InvalidFlux  = isnan(Obs_Flux) | isinf(Obs_Flux) | (Obs_Flux <= 0);
    InvalidXY    = isnan(Obs_X) | isinf(Obs_X) | isnan(Obs_Y) | isinf(Obs_Y);
    InvalidRADec = isnan(Obs_RA) | isinf(Obs_RA) | isnan(Obs_Dec) | isinf(Obs_Dec);
    ValidMask    = ~InvalidFlux & ~InvalidXY & ~InvalidRADec;
    Nvalid       = sum(ValidMask);

    if Nvalid < numel(Obs_Flux)
        Obs_X       = Obs_X(ValidMask);
        Obs_Y       = Obs_Y(ValidMask);
        Obs_RA      = Obs_RA(ValidMask);
        Obs_Dec     = Obs_Dec(ValidMask);
        Obs_Flux    = Obs_Flux(ValidMask);
        Obs_FluxErr = Obs_FluxErr(ValidMask);
        DistArcsec  = DistArcsec(ValidMask);
        Nmatch      = Nmatch(ValidMask);
        Cal_RA      = Cal_RA(ValidMask);
        Cal_Dec     = Cal_Dec(ValidMask);
        SpecFlux    = SpecFlux(ValidMask, :);
        SpecErr     = SpecErr(ValidMask, :);
        if HasAirmassCol
            Obs_Airmass = Obs_Airmass(ValidMask);
        end
        if Args.Verbose
            fprintf('  [pythonLike] data validation: %d/%d kept\n', Nvalid, numel(ValidMask));
        end
    end

    if Nvalid == 0
        Obj.msgLog(LogLevel.Error, ...
            'selectCalibratorsPythonLike: no valid calibrators remain after data validation');
        Obj.SourceData = []; Obj.SpecData = []; Obj.CalFound = false;
        return
    end

    % Populate SpecData (same struct shape and wavelength grid as catsHTM path)
    Obj.SpecData         = struct();
    Obj.SpecData.CalData = struct('RA', Cal_RA, 'Dec', Cal_Dec);
    Obj.SpecData.SpecWvl = (3360:20:10200)';
    Obj.SpecData.Spec    = SpecFlux;
    Obj.SpecData.SpecErr = SpecErr;

    % Populate SourceData (AstroCatalog)
    SourceTable = table(Obs_Flux, Obs_FluxErr, Obs_X, Obs_Y, Obs_RA, Obs_Dec, ...
                        DistArcsec, Nmatch, ...
        'VariableNames', {'Flux', 'FluxErr', 'X', 'Y', 'RA', 'Dec', 'MatchDistance', 'NumMatches'});
    if HasAirmassCol
        SourceTable.AIRMASS = Obs_Airmass;
    end
    if Args.AttachBP_RP
        % Read Gaia tail cols straight off the matched GAIADR3spec row.
        % SpecTab was indexed before ValidMask filtering, so apply ValidMask
        % the same way SpecFlux/Cal_RA were subset.
        SpecCN     = CatH_S.ColNames;
        BPRPColIdx = find(strcmp(SpecCN, 'bp_rp'),             1);
        BPColIdx   = find(strcmp(SpecCN, 'phot_bp_mean_mag'),  1);
        RPColIdx   = find(strcmp(SpecCN, 'phot_rp_mean_mag'),  1);
        Nrow = size(SpecTab, 1);
        BPRPv = nan(Nrow, 1); BPv = nan(Nrow, 1); RPv = nan(Nrow, 1);
        if ~isempty(BPRPColIdx); BPRPv = double(SpecTab(:, BPRPColIdx)); end
        if ~isempty(BPColIdx);   BPv   = double(SpecTab(:, BPColIdx));   end
        if ~isempty(RPColIdx);   RPv   = double(SpecTab(:, RPColIdx));   end
        if exist('ValidMask','var') && numel(BPRPv) > Nvalid
            BPRPv = BPRPv(ValidMask); BPv = BPv(ValidMask); RPv = RPv(ValidMask);
        end
        SourceTable.BP_RP  = BPRPv;
        SourceTable.MAG_BP = BPv;
        SourceTable.MAG_RP = RPv;
    end
    Obj.SourceData = AstroCatalog(SourceTable);
    Obj.CalFound   = true;

    if Args.Verbose
        fprintf('  [pythonLike] calibrator selection complete: %d matched\n', Nvalid);
    end
end

% =========================================================================
function N = armsSampleSize(Nvalid, Mode, Percent, Count)
    % Pick the number of brightest calibrators contributing to ARMS,
    % dispatched by Mode:
    %   'percent' - ceil(Nvalid * Percent/100), floored at 1 if Nvalid>0.
    %               Small pools collapse gracefully (Nvalid=0 -> N=0).
    %   'count'   - min(Count, Nvalid); N=0 disables ARMS in this mode
    %               (handled by the caller-side ARMSEnabled guard).
    if Nvalid <= 0
        N = 0;
        return;
    end
    switch Mode
        case 'percent'
            N = max(1, ceil(Nvalid * Percent / 100));
            N = min(N, Nvalid);
        case 'count'
            N = min(max(0, floor(Count)), Nvalid);
        otherwise
            N = 0;
    end
end
