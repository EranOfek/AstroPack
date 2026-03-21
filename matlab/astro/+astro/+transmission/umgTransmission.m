function Result = umgTransmission(Lambda, ParamMatrix, Args)
    % Uniformly Mixed Gases (UMG) transmission of the Earth atmosphere
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %            If GetArgNames flag is true, returns ArgNames structure for parameters.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, Temperature_C, Pressure_mbar].
    %          * ...,key,val,...
    %            'AbsorptionData' - Pre-loaded AbsorptionData object for fast interpolation.
    %                              lsDefault is [].
    %            'Return' - Pre-computed results for caching (external cache pattern).
    %                      Default is [].
    %            'UsePersistentCache' - Enable/disable persistent cache. Default is true.
    %            'Tolerance' - Parameter comparison tolerance. Default is 1e-12.
    %            'GetArgNames' - Return ArgNames structure instead of calculating.
    %                           Default is false.
    % Output : - Result (double matrix): Transmission matrix (wavelengths × parameter_sets) [0-1]
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Reference: Gueymard, C. A. (2019). Solar Energy, 187, 233-253.
    % Example: % Basic usage:
    %          Lambda = linspace(3000, 11000, 401)';
    %          ParamMatrix = [45, 15, 1013; 60, 25, 950; 30, 5, 1020];  % Multiple parameter sets
    %          Result = astro.transmission.umgTransmission(Lambda, ParamMatrix);
    %          AbsData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    %          Result = astro.transmission.umgTransmission(Lambda, ParamMatrix, 'AbsorptionData', AbsData);
    %
    %          % Get parameter information:
    %          ArgNames = astro.transmission.umgTransmission('GetArgNames', true);
    %
    %          % Usage with CompositeFun:
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('UMG gases', @astro.transmission.umgTransmission, [], 'Par', [45, 15, 1013]);

    arguments
        Lambda        = linspace(3000,11000,401)'
        ParamMatrix   = [30, 15, 965]           % [ZenithAngle_deg, Temperature_C, Pressure_mbar]
        Args.AbsorptionData = []
        Args.Return = []
        Args.UsePersistentCache logical = true  % Enable/disable persistent cache
        Args.Tolerance = 1e-12                  % Parameter comparison tolerance
        Args.GetArgNames logical = false        % Return ArgNames structure instead of calculating
    end
    
    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2, 3}, ...
                       'Description', {'ZenithAngle_deg', 'Temperature_C', 'Pressure_mbar'}, ...
                       'Min', {0, -50, 800}, ...
                       'Max', {90, 50, 1100});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Persistent cache
    persistent CachedResult CachedParams CachedLambda

    if Args.UsePersistentCache && ~isempty(CachedResult)
        % Check if parameters and wavelengths match within tolerance
        if compareParams(Lambda, CachedLambda, Args.Tolerance) && ...
           compareParams(ParamMatrix, CachedParams, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Validate input dimensions
    if size(ParamMatrix, 2) ~= 3
        error('ParamMatrix must have 3 columns: [ZenithAngle_deg, Temperature_C, Pressure_mbar]');
    end

    % Fixed constants for UMG calculations
    Co2_ppm = 420;  % Modern atmospheric CO2 concentration
%    With_trace_gases = true;  % Always include trace gases

    % Extract parameters
    ZenithAngles = ParamMatrix(:, 1);  % Column vector
    Temperatures = ParamMatrix(:, 2);  % Column vector (Celsius)
    Pressures = ParamMatrix(:, 3);     % Column vector (mbar)
    NumParamSets = size(ParamMatrix, 1);
    NumWavelengths = length(Lambda);
    
    % Validate zenith angles
    if any(ZenithAngles > 90 | ZenithAngles < 0)
        error('Zenith angles out of range [0, 90] degrees');
    end

    % Validate pressure values
    if any(Pressures <= 0)
        error('Pressure values must be positive');
    end

    % Create or validate AbsorptionData object
    if isempty(Args.AbsorptionData)
        Args.AbsorptionData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    end

    % Calculate airmasses for all zenith angles at once (vectorized)
    Airmasses = astro.transmission.airmassSMARTS(ZenithAngles);

    % Group by unique (Temperature, Pressure) pairs to avoid redundant
    % getInterpolated calls and abundance calculations
    NonZA = [Temperatures, Pressures];
    [UniqNonZA, ~, Ic] = unique(NonZA, 'rows');

    Result = zeros(NumWavelengths, NumParamSets);

    for ig = 1:size(UniqNonZA, 1)
        Mask = (Ic == ig);

        Tair = UniqNonZA(ig, 1);
        Pressure = UniqNonZA(ig, 2);

        % Convert temperature and normalize pressure/temperature
        Tair_kelvin = Tair + 273.15;
        Pp0 = Pressure / 1013.25;
        Tt0 = Tair_kelvin / 273.15;

        % Airmass vectors for this group [1 x Ngroup]
        Am_o2   = Airmasses.o2(Mask)';
        Am_ch4  = Airmasses.ch4(Mask)';
        Am_co   = Airmasses.co(Mask)';
        Am_n2o  = Airmasses.n2o(Mask)';
        Am_co2  = Airmasses.co2(Mask)';
        Am_n2   = Airmasses.n2(Mask)';
        Am_o4   = Am_o2;    % O4 uses O2 airmass
        Am_nh3  = Airmasses.nh3(Mask)';
        Am_no   = Airmasses.no(Mask)';
        Am_no2  = Airmasses.no2(Mask)';
        Am_so2  = Airmasses.so2(Mask)';
        Am_hno3 = Airmasses.hno3(Mask)';
        Am_no3  = Am_no;    % Use NO airmass for NO3
        Am_hno2 = Am_hno3;  % Use HNO3 airmass for HNO2
        Am_ch2o = Am_co;    % Use CO airmass for CH2O
        Am_bro  = Am_o2;    % Use O2 airmass for BrO
        Am_clno = Am_no;    % Use NO airmass for ClNO

        % Pre-compute all abundance factors (scalar, same for all sources in group)
        Abundance_o2  = 1.67766e5 * Pp0;
        Abundance_ch4 = 1.3255 * (Pp0 ^ 1.0574);
        Abundance_co  = 0.29625 * (Pp0^2.4480) * exp(0.54669 - 2.4114 * Pp0 + 0.65756 * (Pp0^2));
        Abundance_n2o = 0.24730 * (Pp0^1.0791);
        Abundance_co2 = 0.802685 * Co2_ppm * Pp0;
        Abundance_n2  = 3.8269 * (Pp0^1.8374);
        Abundance_o4  = 1.8171e4 * (constant.Loschmidt^2) * (Pp0^1.7984) / (Tt0^0.344);

        % Accumulate total optical depth: [Nwave x 1] .* [1 x Ngroup] via implicit expansion
        % Each term: AbsCoeff [Nwave x 1] * Abundance [scalar] .* Am [1 x Ngroup]
        Tau_total = zeros(NumWavelengths, sum(Mask));

        % =====================================================================
        % UNIFORMLY MIXED GASES
        % =====================================================================

        % 1. Oxygen (O2)
        try
            O2_abs = Args.AbsorptionData.getInterpolated('O2', Lambda);
            Tau_total = Tau_total + (O2_abs * Abundance_o2) .* Am_o2;
        catch
        end

        % 2. Methane (CH4)
        try
            Ch4_abs = Args.AbsorptionData.getInterpolated('CH4', Lambda);
            Tau_total = Tau_total + (Ch4_abs * Abundance_ch4) .* Am_ch4;
        catch
        end

        % 3. Carbon Monoxide (CO)
        try
            Co_abs = Args.AbsorptionData.getInterpolated('CO', Lambda);
            Tau_total = Tau_total + (Co_abs * Abundance_co) .* Am_co;
        catch
        end

        % 4. Nitrous Oxide (N2O)
        try
            N2o_abs = Args.AbsorptionData.getInterpolated('N2O', Lambda);
            Tau_total = Tau_total + (N2o_abs * Abundance_n2o) .* Am_n2o;
        catch
        end

        % 5. Carbon Dioxide (CO2)
        try
            Co2_abs = Args.AbsorptionData.getInterpolated('CO2', Lambda);
            Tau_total = Tau_total + (Co2_abs * Abundance_co2) .* Am_co2;
        catch
        end

        % 6. Nitrogen (N2)
        try
            N2_abs = Args.AbsorptionData.getInterpolated('N2', Lambda);
            Tau_total = Tau_total + (N2_abs * Abundance_n2) .* Am_n2;
        catch
        end

        % 7. Oxygen-Oxygen collision complex (O4)
        try
            O4_abs = Args.AbsorptionData.getInterpolated('O4', Lambda) * 1e-46;
            Tau_total = Tau_total + (O4_abs * Abundance_o4) .* Am_o4;
        catch
        end

        % =====================================================================
        % TRACE GASES (always included)
        % =====================================================================

        % 1. Nitric Acid, HNO3
        try
            Hno3_abs = Args.AbsorptionData.getInterpolated('HNO3', Lambda);
            Hno3_abundance = 1e-4 * 3.637 * (Pp0^0.12319);
            Tau_total = Tau_total + (Hno3_abs * Hno3_abundance) .* Am_hno3;
        catch
        end

        % 2. Nitrogen Dioxide, NO2
        try
            No2_abs = Args.AbsorptionData.getInterpolated('NO2', Lambda);
            No2_abundance = 1e-4 * min(1.8599 + 0.18453 * Pp0, 41.771 * Pp0);
            Tau_total = Tau_total + (No2_abs * No2_abundance) .* Am_no2;
        catch
        end

        % 3. Nitrogen Trioxide, NO3
        try
            No3_abs = Args.AbsorptionData.getInterpolated('NO3', Lambda);
            No3_abundance = 5e-5;
            Tau_total = Tau_total + (No3_abs * No3_abundance) .* Am_no3;
        catch
        end

        % 4. Nitric Oxide, NO
        try
            No_abs = Args.AbsorptionData.getInterpolated('NO', Lambda);
            No_abundance = 1e-4 * min(0.74307 + 2.4015 * Pp0, 57.079 * Pp0);
            Tau_total = Tau_total + (No_abs * No_abundance) .* Am_no;
        catch
        end

        % 5. Sulfur Dioxide, SO2 (combination of SO2U and SO2I)
        So2_abs = zeros(NumWavelengths, 1);
        try
            So2u_abs = Args.AbsorptionData.getInterpolated('SO2U', Lambda);
            So2_abs = So2_abs + So2u_abs;
        catch
        end
        try
            So2i_abs = Args.AbsorptionData.getInterpolated('SO2I', Lambda);
            So2_abs = So2_abs + So2i_abs;
        catch
        end
        if any(So2_abs > 0)
            So2_abundance = 1e-4 * 0.11133 * (Pp0^0.812) * exp(0.81319 + 3.0557 * (Pp0^2) - 1.578 * (Pp0^3));
            Tau_total = Tau_total + (So2_abs * So2_abundance) .* Am_so2;
        end

        % 6. Ammonia, NH3
        if Pp0 > 0
            try
                Nh3_abs = Args.AbsorptionData.getInterpolated('NH3', Lambda);
                Log_pp0 = log(Pp0);
                Nh3_abundance = exp(-8.6499 + 2.1947 * Log_pp0 - 2.5936 * (Log_pp0^2) - ...
                                   1.819 * (Log_pp0^3) - 0.65854 * (Log_pp0^4));
                Tau_total = Tau_total + (Nh3_abs * Nh3_abundance) .* Am_nh3;
            catch
            end
        end

        % 7. Bromine Monoxide, BrO
        try
            Bro_abs = Args.AbsorptionData.getInterpolated('BrO', Lambda);
            Bro_abundance = 2.5e-6;
            Tau_total = Tau_total + (Bro_abs * Bro_abundance) .* Am_bro;
        catch
        end

        % 8. Formaldehyde, CH2O
        try
            Ch2o_abs = Args.AbsorptionData.getInterpolated('CH2O', Lambda);
            Ch2o_abundance = 3e-4;
            Tau_total = Tau_total + (Ch2o_abs * Ch2o_abundance) .* Am_ch2o;
        catch
        end

        % 9. Nitrous Acid, HNO2
        try
            Hno2_abs = Args.AbsorptionData.getInterpolated('HNO2', Lambda);
            Hno2_abundance = 1e-4;
            Tau_total = Tau_total + (Hno2_abs * Hno2_abundance) .* Am_hno2;
        catch
        end

        % 10. Chlorine Nitrate, ClNO3
        try
            Clno_abs = Args.AbsorptionData.getInterpolated('ClNO', Lambda);
            Clno_abundance = 1.2e-4;
            Tau_total = Tau_total + (Clno_abs * Clno_abundance) .* Am_clno;
        catch
        end

        % Calculate transmission for this group
        Result(:, Mask) = exp(-Tau_total);
    end

    % Store in persistent cache if enabled
    if Args.UsePersistentCache
        CachedResult = Result;
        CachedParams = ParamMatrix;
        CachedLambda = Lambda;
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

