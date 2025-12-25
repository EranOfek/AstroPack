function [Result, ResFit] = transmissionZP_skeleton(Obj, Args)
    % Transmission-based absolute photometric calibration using Gaia XP spectra
    % Upper-level wrapper using CompositeFun framework with
    % predefSeqCompositeFun input structures
    %
    % Input  : - Obj - AstroImage or AstroCatalog object with sources.
    %          * ...,key,val,...
    %            'SearchRadius' - Gaia matching radius [arcsec].
    %                   Default is 1.0.
    %            'MagRange' - Calibrator magnitude range [min max].
    %                   Default is [12, 16].
    %            'MinSN' - Minimum S/N for calibrators.
    %                   Default is 5.
    %            'MaxSN' - Maximum S/N for calibrators.
    %                   Default is 1000.
    %            'FilterBadFlags' - Apply FLAGS quality filtering.
    %                   Default is true.
    %            'FluxColName' - LAST flux column name.  %%%% ALL FLUX COLUMNS???
    %                   Default is 'FLUX_APER_3'.
    %            'WvlRange_nm' - Wavelength integration range [nm].
    %                   Default is [300, 1100].
    %            'WvlNumPoints' - Number of wavelengths for transmission calculation
    %            'SpRange_nm' - Spectra wavelength range [nm].
    %                   Default is [336, 1020].
    %            'SPNumPoints' - Number of spectrum wavelength points.
    %                   Default is 343.
    %            'Pressure_mbar' - Atmospheric pressure [mbar].
    %                   Default is 965.
    %            'TransFuns' - Struct array of transmission functions.
    %                   Default is [] (uses predefSeqCompositeFun catalog: Normalization,
    %                   Rayleigh, Ozone, Aerosol, Water, UMG, Mirror, Corrector,
    %                   QE_Legendre, QE_SkewedGaussian).
    %            'OptSeq' - Struct array of optimization stages.
    %                   Default is [] (uses StageCatalog.Default: 5-stage sequence
    %                   from Garrappa et al. 2025).
    %            'FluxIni' - Start column index for Gaia XP flux values.
    %                   Default is 7.
    %            'FluxEnd' - End column index for Gaia XP flux values.
    %                   Default is 349.
    %            'EFluxIni' - Start column index for Gaia XP flux errors.
    %                   Default is 350.
    %            'EFluxEnd' - End column index for Gaia XP flux errors.
    %                   Default is 692.
    %            'ExpTime' - Exposure time [s]. Default is 20.
    %            'ApertureArea' - Telescope aperture area [m^2]. Default (LAST) is pi * (0.1397)^2.
    %            'Temperature' - Default temperature [C].
    %                   Default is 15.
    %            'UpdateHeader' - Update AstroImage header with ZP. Default is true.
    %            'UpdateMagCols' - Update magnitude columns with ZP. Default is true.
    %            'CreateNewObj' - Copy input object. Default is false.
    %            'ValInp' - Validate inputs. Default is true.
    % Output : - Result - Input object, possibly with updated magnitudes.
    %          - ResFit - Structure array with calibration results per object with fields:
    %                     .ZP - Photometric zero point [mag]
    %                     .RMS - RMS of residuals [mag]
    %                     .NumCalibrators - Number of calibrators used
    %                     .Model - Optimized CompositeFun transmission model
    %                     .FieldParams - Field correction parameters [1x10]
    %                     .FitResults - Results from optimization stages
    %                     .CalibratorTable - Table with calibrator diagnostics

    arguments
        Obj  % AstroImage or AstroCatalog

        % Calibrator selection
        Args.SearchRadius = 1.0  % arcsec
        Args.MagRange = [12, 16]
        Args.MinSN = 5
        Args.MaxSN = 1000
        Args.FilterBadFlags logical = true
        Args.FluxColName = 'FLUX_APER_3'

        % Wavelength settings
        Args.WvRange_nm = [300, 1100]
        Args.SpRange_nm = [336, 1020]
        Args.SpNumPoints = 343

        % Atmospheric parameters
        Args.Pressure_mbar = 965

        % Transmission functions and optimization sequence (from predefSeqCompositeFun)
        % These are set to [] and initialized in code below
        Args.TransFuns = []
        Args.OptSeq = []

        % Gaia XP spectra column indices
        Args.FluxIni = 7
        Args.FluxEnd = 349
        Args.EFluxIni = 350
        Args.EFluxEnd = 692

        % Constants
        Args.ExpTime = 20
        Args.ApertureArea = pi * (0.1397)^2
        Args.Temperature = 15

        % Catalog update
        Args.UpdateHeader logical = true
        Args.UpdateMagCols logical = true

        % General
        Args.CreateNewObj logical = false
        Args.ValInp logical = true
        Args.Plot logical = false
    end
    
    RAD = constant.RAD;
    
    [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();

    % Use provided functions/sequence or defaults from catalog
    if isempty(Args.TransFuns)
        % Build default function list
        FunList = [FunCat.Normalization, FunCat.Rayleigh, FunCat.Ozone, ...
                   FunCat.Aerosol, FunCat.Water, FunCat.UMG, ...
                   FunCat.Mirror, FunCat.Corrector, ...
                   FunCat.QE_Legendre, FunCat.QE_SkewedGaussian];
    else
        FunList = Args.TransFuns;
    end

    if isempty(Args.OptSeq)
        OptSeq = StageCat.DefaultLAST;  % 5-stage sequence from Garrappa et al. (2025)
    else
        OptSeq = Args.OptSeq;
    end

    % Pre-load absorption interpolants 
    AbsData = astro.transmission.loadAbsorptionInterpolantsSMARTS();

 
    % ====================================================================
    % STEP 1: LOOP OVER SUBIMAGES (OBJECTS)
    % ====================================================================

    for Iobj = 1:Nobj
    
        % ----------------------------------------------------------------
        % STEP 1.1: Extract observation metadata
        % ----------------------------------------------------------------

        Metadata.AIRMASS = Image.HeaderData.getVal('AIRMASS');
        Metadata.TEMP = Image.HeaderData.getVal('TEMP');
        Metadata.EXPTIME = Image.HeaderData.getVal('EXPTIME');

        % Pre-cache airmassSMARTS (session cache)
        ZenithAngle_deg = acosd(1.0 / Metadata.AIRMASS);
        astro.transmission.airmassSMARTS(ZenithAngle_deg);

        % ----------------------------------------------------------------
        % STEP 1.2: Find calibrators with Gaia XP spectra
        % ----------------------------------------------------------------

            Cat = Result(Iobj).CatData;
     
        % Find and filter Gaia calibrators
        [CalibData] = findGaiaCalibrators(Cat, ...
            'SearchRadius', Args.SearchRadius, ...
            'MagRange', Args.MagRange, ...
            'MinSN', Args.MinSN, ...
            'MaxSN', Args.MaxSN, ...
            'FilterBadFlags', Args.FilterBadFlags, ...
            'FluxColName', Args.FluxColName, ...
            'SpRange_nm', Args.SpRange_nm, ...
            'SpNumPoints', Args.SpNumPoints, ...
            'FluxIni', Args.FluxIni, ...
            'FluxEnd', Args.FluxEnd, ...
            'EFluxIni', Args.EFluxIni, ...
            'EFluxEnd', Args.EFluxEnd);



        % ----------------------------------------------------------------
        % STEP 1.3: Perform calibration using PhotCalib
        % ----------------------------------------------------------------

        % Create PhotCalib object and run calibration
        PC = PhotCalib();
        PC.calibrate(CalibData, FunList, OptSeq, Metadata, ...
            'Pressure_mbar', Args.Pressure_mbar, ...
            'ApertureArea_m2', Args.ApertureArea);

        % ----------------------------------------------------------------
        % STEP 1.4: Update magnitude columns (transmissionZP responsibility)
        % ----------------------------------------------------------------

        if Args.UpdateMagCols
            % Apply ZP to catalog magnitude columns
            % Example: updateCatalogMagnitudes(Result(Iobj).CatData, PC);
        end

        % ----------------------------------------------------------------
        % STEP 1.5: Update header (transmissionZP responsibility)
        % ----------------------------------------------------------------

        if Args.UpdateHeader
            % Write ZP to header
            % Result(Iobj).HeaderData.replaceVal('PHOTZP', PC.ZP);
            % Result(Iobj).HeaderData.replaceVal('PHOTZPER', PC.ZP_Err);
        end

        % ----------------------------------------------------------------
        % STEP 1.6: Store PhotCalib object
        % ----------------------------------------------------------------

        ResFit(Iobj).PhotCalib = PC;  % Store complete calibration object

     

    end %%% [Loop over subimages]



%% ========================================================================
%  HELPER FUNCTION: Find Gaia Calibrators
%  ========================================================================

function [CalibData] = findGaiaCalibrators(Cat, Args)
    % Find and filter Gaia XP calibrators
    %
    % Input  : - Cat - AstroCatalog with source data
    %          * ...,key,val,...
    % Output : - CalibData - Structure with Gaia XP spectra and LAST data
    %          - Lambda - Wavelength grid [nm]
    
    
    arguments
        Cat
        Args.SearchRadius = 1.0
        Args.MagRange = [12, 16]
        Args.MinSN = 5
        Args.MaxSN = 1000
        Args.FilterBadFlags logical = true
        Args.FluxColName = 'FLUX_APER_3'      %%%% All flux columns?
        Args.SpRange_nm = [336, 1020]
        Args.SpNumPoints = 343
        Args.FluxIni = 7
        Args.FluxEnd = 349
        Args.EFluxIni = 350
        Args.EFluxEnd = 692
     end

   
    Lambda = [];
    CalibData = [];

     % ----------------------------------------------------------------
    % STEP 1: Apply quality filters
    % ----------------------------------------------------------------

    % Calculate S/N
    SN = FluxCol ./ FluxErrCol;

    % Apply S/N filter
    SNFilter = (SN >= Args.MinSN) & (SN <= Args.MaxSN);
    
    % Apply magnitude filter
    MagFilter = (Mag >= Args.MagRange(1)) & (Mag <= Args.MagRange(2));

    % Apply FLAGS filter
  

    % Combine filters
    GoodSources = MagFilter & SNFilter & FlagsFilter;
    NumGood = sum(GoodSources);


    % ----------------------------------------------------------------
    % STEP 2: Match with Gaia DR3 XP catalog
    % ----------------------------------------------------------------

  
    % Get RA, Dec
    RA = getCol(CatFiltered, 'RA');
    Dec = getCol(CatFiltered, 'Dec');

    % Match with Gaia XP catalog
    [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', 'Coo', [RA/RAD, Dec/RAD], 'Radius', Args.SearchRadius, 'CooUnits', 'rad', 'RadiusUnits', 'arcsec'); 
                                      
  
    % ----------------------------------------------------------------
    % STEP 3: Extract Gaia XP spectra
    % ----------------------------------------------------------------

  
    % ----------------------------------------------------------------
    % STEP 4: Extract LAST fluxes to matched sources
    % ----------------------------------------------------------------

   

    % ----------------------------------------------------------------
    % STEP 5: Build CalibData structure to save calibrators data
    % ----------------------------------------------------------------

    CalibData = struct();
    CalibData.Lambda = ...             % Wavelength grid from Gaia XP [nm] [WvlPoints x 1]
    CalibData.Spec = ...               % Gaia XP spectra [N x WvlPoints]
    CalibData.SpecErr = ...            % Gaia XP spectra errors [N x WvlPoints]
    CalibData.LASTData = struct();
    CalibData.LASTData.Flux = ...      % LAST observed flux [N x 1]
    CalibData.LASTData.FluxErr = ...   % LAST flux uncertainties [N x 1] (for Chi2 calculation)
    CalibData.LASTData.X = ...         % X pixel coordinates [N x 1]
    CalibData.LASTData.Y = ...         % Y pixel coordinates [N x 1]
    CalibData.LASTData.RA = ...        % Right Ascension [rad] [N x 1]
    CalibData.LASTData.Dec = ...       % Declination [rad] [N x 1] 

  end


