function Result = filterTransmission(Lambda, unusedParam, Args)
    % Tabulated filter transmission of a named mission/survey band
    % Package: astro.transmission
    % Description: CompositeFun-compatible function returning the response
    %              curve of a named astronomical filter, interpolated onto
    %              the requested wavelength grid and zero outside the band.
    %              The curve is read from the AstroPack filter catalogue
    %              (cats.spec.filter.AstFilterCat) through
    %              AstroTransmission.getFilt, so any family/band present
    %              there can be used - e.g. SDSS, ZTF, LSST, PAN-STARRS/PS1,
    %              DES, SkyMapper, GAIA, Johnson.
    %
    %              The curve carries the SHAPE of the combined
    %              instrument+filter response only; the absolute throughput
    %              is carried by the fitted 'Normalization' term of the
    %              composite model, exactly as it is for the LAST components.
    %
    %              The catalogue stores each band AREA-normalized
    %              (integral(T dLambda) = 1, so T ~ 1e-3 for an optical
    %              band), which is the one convention present for every
    %              family - the raw transmission is missing for LSST and
    %              PAN-STARRS and is scaled inconsistently elsewhere (SDSS
    %              as a fraction, ZTF in percent). By default the curve is
    %              therefore rescaled to peak 1, which puts it on the same
    %              0-1 footing as the LAST throughput model so that the
    %              fitted Norm stays within its usual bounds. Leaving the
    %              area normalization in place would demand Norm ~ 1e3 and
    %              rail it against Norm_Max.
    %
    %              Unlike telescope.detector.qeSkewedGaussianLAST this
    %              function has NO fitted parameter (no skewness, no centre
    %              wavelength) and no LAST-specific constant: the band shape
    %              is measured data, not a model.
    % Input  : - Lambda - Wavelength array [Angstrom].
    %                   Default is linspace(3000, 11000, 401)'.
    %          - unusedParam - Dummy parameter for CompositeFun
    %                   compatibility; ignored. Default is 1.
    %          * ...,key,val,...
    %            'Family' - Filter family name, e.g. 'SDSS', 'ZTF', 'LSST'.
    %                   Case-insensitive. Default is '' (no filter -> error).
    %            'Band' - Band name within the family, e.g. 'g'.
    %                   Case-insensitive. Default is '' (no band -> error).
    %            'Normalization' - How to scale the catalogue curve:
    %                   'peak' (default) - rescale to a maximum of 1.
    %                   'area' - leave the catalogue's unit-integral scaling.
    %                   'none' - same as 'area' (the stored convention).
    %                   The fit is invariant to this choice up to the value
    %                   of Norm, but only 'peak' keeps Norm inside the
    %                   default bounds.
    %            'InterpMethod' - interp1 method used to resample the
    %                   catalogue curve onto Lambda. Default is 'linear'.
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'UsePersistentCache' - Enable persistent caching.
    %                   Default is true.
    %            'Tolerance' - Tolerance for cache comparison.
    %                   Default is 1e-12.
    %            'GetArgNames' - Return ArgNames structure instead of
    %                   calculating. Default is false.
    % Output : - Result - Transmission on Lambda, 0 outside the band
    %                   [numel(Lambda) x 1].
    %                   OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2026)
    % Notes  : The reference spectra used by PhotCalibTrans (Gaia DR3 XP,
    %          3360-10200 Angstrom) are extrapolated with a constant beyond
    %          their edges, so a band reaching outside that range - LSST y,
    %          and the blue wing of SDSS/LSST u - rests partly on
    %          extrapolated reference flux. Check the band coverage before
    %          trusting a calibration there.
    % Example: Lambda = (3000:20:11000)';
    %          T = astro.transmission.filterTransmission(Lambda, 1, ...
    %                      'Family','SDSS', 'Band','g');
    %          % Usage with CompositeFun (family/band travel as OptionalArgs,
    %          % not as fitted parameters):
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('MissionFilter', @astro.transmission.filterTransmission, [], ...
    %                       'Par', 1, 'FitPar', false, ...
    %                       'OptionalArgs', {'Family','ZTF', 'Band','r'});

    arguments
        Lambda                         = linspace(3000, 11000, 401)'
        unusedParam                    = 1 %#ok<INUSA>
        Args.Family       (1,:) char   = ''
        Args.Band         (1,:) char   = ''
        Args.Normalization (1,:) char {mustBeMember(Args.Normalization, {'peak','area','none'})} = 'peak'
        Args.InterpMethod (1,:) char   = 'linear'
        Args.Return                    = []
        Args.UsePersistentCache logical = true
        Args.Tolerance                 = 1e-12
        Args.GetArgNames logical       = false
    end

    % Return ArgNames structure if requested (dummy parameter for CompositeFun compatibility)
    if Args.GetArgNames
        Result = struct('Name', {1}, 'Description', {'unusedParam'}, 'Min', {1}, 'Max', {1});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    if isempty(Args.Family) || isempty(Args.Band)
        error('astro:transmission:filterTransmission:NoFilter', ...
              ['No filter specified. Pass ''Family'' and ''Band'', e.g. ', ...
               '''Family'',''SDSS'',''Band'',''g''.']);
    end

    % Persistent cache. The key is the wavelength grid AND the filter
    % identity, so alternating between bands within one session is safe.
    persistent CachedResult CachedLambda CachedFamily CachedBand CachedMethod CachedNorm

    if Args.UsePersistentCache && ~isempty(CachedResult) && ...
            strcmpi(Args.Family, CachedFamily) && ...
            strcmpi(Args.Band, CachedBand) && ...
            strcmp(Args.InterpMethod, CachedMethod) && ...
            strcmp(Args.Normalization, CachedNorm) && ...
            compareParams(Lambda, CachedLambda, Args.Tolerance)
        Result = CachedResult;
        return;
    end

    % Read the catalogue curve. getFilt reports the available families or
    % bands when the name does not resolve.
    Filt = AstroTransmission.getFilt(Args.Family, Args.Band);
    if numel(Filt) > 1
        error('astro:transmission:filterTransmission:Ambiguous', ...
              'Family ''%s'' / band ''%s'' matches %d filters; the pair must be unique.', ...
              Args.Family, Args.Band, numel(Filt));
    end

    Wave = convert.length(Filt.WaveUnits, 'A', Filt.Wave(:));
    Tran = Filt.Tran(:);

    % Rescale on the NATIVE grid, so the peak does not depend on how
    % finely Lambda happens to sample the band.
    if strcmp(Args.Normalization, 'peak')
        PeakTran = max(Tran);
        if ~isfinite(PeakTran) || PeakTran <= 0
            error('astro:transmission:filterTransmission:DegenerateCurve', ...
                  'Filter %s/%s has no positive transmission in the catalogue.', ...
                  Args.Family, Args.Band);
        end
        Tran = Tran ./ PeakTran;
    end

    % Resample onto the model grid. Outside the tabulated support the
    % transmission is zero, not extrapolated.
    Result = interp1(Wave, Tran, Lambda(:), Args.InterpMethod, 0);
    Result(~isfinite(Result)) = 0;
    Result = max(Result, 0);

    % Store in persistent cache if enabled
    if Args.UsePersistentCache
        CachedResult = Result;
        CachedLambda = Lambda;
        CachedFamily = Args.Family;
        CachedBand   = Args.Band;
        CachedMethod = Args.InterpMethod;
        CachedNorm   = Args.Normalization;
    end
end

function isSame = compareParams(ParamsCurrent, ParamsCached, Tolerance)
    % Compare numerical parameters with tolerance
    if nargin < 3
        Tolerance = 1e-12;
    end
    isSame = isequal(size(ParamsCurrent), size(ParamsCached)) && ...
             all(abs(ParamsCurrent(:) - ParamsCached(:)) < Tolerance);
end
