function CalibArgs = predefCalibArgs(Args)
    % Predefined calibration workflow arguments for fitPhotCalibTrans
    % Package: imUtil.calib
    % Description: Returns a struct with default calibration workflow settings
    %              for LAST telescope photometric calibration. Users can
    %              override individual fields via name-value arguments, then
    %              pass the struct as 'CalibArgs' to fitPhotCalibTrans or
    %              PhotCalibTrans.calibrate.
    % Input  : * ...,key,val,...
    %            'Lambda'         - Transmission wavelength grid [Angstrom]. Default is (3000:20:11000)'.
    %            'SearchRadius'   - Gaia matching radius [arcsec]. Default is 2.
    %            'MagRange'       - Calibrator magnitude range [min max]. Default is [11.5 15.5].
    %            'FunListName'    - Transmission function list name. Default is 'DefaultLASTFunList'.
    %            'CustomFunList'  - Custom function list. Default is [].
    %            'OptSeqName'     - Optimization sequence name. Default is 'LAST_NormLin'.
    %            'CustomOptSeq'   - Custom optimization sequence. Default is [].
    %            'Tran2DType'     - Position-dependent correction type. Default is 'cheby1_4_xt'.
    %            'UseTran2D'      - Enable position-dependent correction. Default is true.
    %            'WeightingMode'  - Weighting mode. Default is 'spectral'.
    %            'FluxErrColName' - Flux error column name. Default is 'FluxErr'.
    %            'SigmaClipMethod'- Sigma clipping method. Default is 'median'.
    %            'FluxErrorNorm'  - Flux error normalization. Default is 0.5.
    %            'AddMag'         - Add calibrated magnitude columns. Default is true.
    %            'MagSystem'      - Magnitude system ('AB' or 'Vega'). Default is 'AB'.
    %            'FluxColName'    - Flux column name. Default is 'FLUX_APER_3'.
    %            'AddZP'          - Add ZP column. Default is true.
    %            'UpdateHeader'   - Update header with results. Default is true.
    %            'CreateNewObj'   - Copy input object. Default is false.
    %            'DiffCalibProps' - Properties to calibrate for AstroDiff. Default is {'New', 'Ref'}.
    %            'LASTTelescopeTransmission' - Fixed telescope transmission struct from
    %                              telescope.optics.LASTTransmissionFixed(). Default is struct()
    %                              (computed automatically by fitPhotCalibTrans).
    % Output : - CalibArgs - Struct with all calibration settings.
    % Author : D. Kovaleva (Feb 2026)
    % Example: cfg = imUtil.calib.predefCalibArgs();
    %          cfg = imUtil.calib.predefCalibArgs('SearchRadius', 3, 'Verbose', false);
    %          Result = imProc.calib.fitPhotCalibTrans(AI, 'CalibArgs', cfg);

    arguments
        % Wavelength grid
        Args.Lambda           = (3000:20:11000)'  % Transmission wavelength grid [Angstrom]

        % Calibrator selection
        Args.SearchRadius     = 2         % arcsec
        Args.MagRange         = [11.5 15.5]

        % Transmission model
        Args.FunListName      = 'DefaultLASTFunList'
        Args.CustomFunList    = []
        Args.OptSeqName       = 'LAST_NormLin'
        Args.CustomOptSeq     = []
        Args.Tran2DType       = 'cheby1_4_xt'
        Args.UseTran2D logical = true

        % Weighting
        Args.WeightingMode    = 'spectral'  % 'none', 'spectral', 'flux', 'combined'
        Args.FluxErrColName   = 'FluxErr'
        Args.SigmaClipMethod  = 'median'    % 'median' or 'weighted'
        Args.FluxErrorNorm    = 0.5

        % Output control
        Args.AddMag logical    = true
        Args.MagSystem char    = 'AB'
        Args.FluxColName       = 'FLUX_APER_3'
        Args.AddZP logical     = true
        Args.UpdateHeader logical = true
        Args.CreateNewObj logical = false

        % AstroDiff/AstroZOGY
        Args.DiffCalibProps cell = {'New', 'Ref'}

        % Fixed telescope transmission
        Args.LASTTelescopeTransmission struct = struct()  % From telescope.optics.LASTTransmissionFixed()

        % Note: Verbose and AddMagErr are direct arguments of fitPhotCalibTrans,
        % not part of CalibArgs.
    end

    CalibArgs = Args;
end
