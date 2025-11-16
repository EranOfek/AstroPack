function Result = umgTransmission(Lambda, ParamMatrix, Args)
    % Uniformly Mixed Gases (UMG) transmission of the Earth atmosphere
    % Input  : - Lambda (double array): Wavelength array in nm.
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
    %          Lambda = linspace(300, 1100, 401)';
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
        Lambda        = linspace(300,1100,401)'
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

    % Initialize result matrix
    Result = zeros(NumWavelengths, NumParamSets);
    
    % Calculate transmission for each parameter set
    for i = 1:NumParamSets
        % Current parameters
        Z_ = ZenithAngles(i);
        Tair = Temperatures(i);
        Pressure = Pressures(i);

        % Convert temperature and normalize pressure/temperature
        Tair_kelvin = Tair + 273.15;
        Pp0 = Pressure / 1013.25;
        Tt0 = Tair_kelvin / 273.15;

        % Pre-allocate total optical depth for this parameter set
        Tau_total = zeros(NumWavelengths, 1);

        % Calculate airmass values using SMARTS model
        Airmasses = astro.transmission.airmassSMARTS(Z_);
        Am_o2 = Airmasses.o2;
        Am_ch4 = Airmasses.ch4;
        Am_co = Airmasses.co;
        Am_n2o = Airmasses.n2o;
        Am_co2 = Airmasses.co2;
        Am_n2 = Airmasses.n2;
        Am_o4 = Am_o2;  % O4 uses O2 airmass

        % Trace gas airmass values (always included)
        % Use available airmass values, fallback to general gas airmass for missing species
        Am_nh3 = Airmasses.nh3;
        Am_no = Airmasses.no;
        Am_no2 = Airmasses.no2;
        Am_so2 = Airmasses.so2;
        Am_hno3 = Airmasses.hno3;
        Am_no3 = Am_no;   % Use NO airmass for NO3 (not in SMARTS table)
        Am_hno2 = Am_hno3; % Use HNO3 airmass for HNO2 (similar compound)
        Am_ch2o = Am_co;   % Use CO airmass for CH2O (carbon compound)
        Am_bro = Am_o2;    % Use O2 airmass for BrO (oxygen compound)
        Am_clno = Am_no;   % Use NO airmass for ClNO (nitrogen compound)
    
        % Pre-compute all abundance factors
        Abundance_o2 = 1.67766e5 * Pp0;
        Abundance_ch4 = 1.3255 * (Pp0 ^ 1.0574);
        Abundance_co = 0.29625 * (Pp0^2.4480) * exp(0.54669 - 2.4114 * Pp0 + 0.65756 * (Pp0^2));
        Abundance_n2o = 0.24730 * (Pp0^1.0791);
        Abundance_co2 = 0.802685 * Co2_ppm * Pp0;
        Abundance_n2 = 3.8269 * (Pp0^1.8374);
        Abundance_o4 = 1.8171e4 * (constant.Loschmidt^2) * (Pp0^1.7984) / (Tt0^0.344);  
    
        % =========================================================================
        % UNIFORMLY MIXED GASES PROCESSING
        % Using interpolants from loadAbsorptionInterpolantsSMARTS
        % =========================================================================

        % 1. Oxygen (O2)
        try
            O2_abs = Args.AbsorptionData.getInterpolated('O2', Lambda);
            tau_o2 = O2_abs .* Abundance_o2 .* Am_o2;
            Tau_total = Tau_total + tau_o2;
        catch
            % Species not available, skip
        end

        % 2. Methane (CH4)
        try
            Ch4_abs = Args.AbsorptionData.getInterpolated('CH4', Lambda);
            tau_ch4 = Ch4_abs .* Abundance_ch4 .* Am_ch4;
            Tau_total = Tau_total + tau_ch4;
        catch
            % Species not available, skip
        end

        % 3. Carbon Monoxide (CO)
        try
            Co_abs = Args.AbsorptionData.getInterpolated('CO', Lambda);
            tau_co = Co_abs .* Abundance_co .* Am_co;
            Tau_total = Tau_total + tau_co;
        catch
            % Species not available, skip
        end

        % 4. Nitrous Oxide (N2O)
        try
            N2o_abs = Args.AbsorptionData.getInterpolated('N2O', Lambda);
            tau_n2o = N2o_abs .* Abundance_n2o .* Am_n2o;
            Tau_total = Tau_total + tau_n2o;
        catch
            % Species not available, skip
        end

        % 5. Carbon Dioxide (CO2)
        try
            Co2_abs = Args.AbsorptionData.getInterpolated('CO2', Lambda);
            tau_co2 = Co2_abs .* Abundance_co2 .* Am_co2;
            Tau_total = Tau_total + tau_co2;
        catch
            % Species not available, skip
        end

        % 6. Nitrogen (N2)
        try
            N2_abs = Args.AbsorptionData.getInterpolated('N2', Lambda);
            tau_n2 = N2_abs .* Abundance_n2 .* Am_n2;
            Tau_total = Tau_total + tau_n2;
        catch
            % Species not available, skip
        end

        % 7. Oxygen-Oxygen collision complex (O4)
        try
            O4_abs = Args.AbsorptionData.getInterpolated('O4', Lambda) * 1e-46;
            tau_o4 = O4_abs .* Abundance_o4 .* Am_o4;
            Tau_total = Tau_total + tau_o4;
        catch
            % Species not available, skip
        end
    
        % =========================================================================
        % TRACE GASES PROCESSING (always included)
        % Temperature corrections already applied in interpolants with fixed reference temperatures
        % =========================================================================

        % 1. Nitric Acid, HNO3
        try
            Hno3_abs = Args.AbsorptionData.getInterpolated('HNO3', Lambda);
            Hno3_abundance = 1e-4 * 3.637 * (Pp0^0.12319);
            tau_hno3 = Hno3_abs .* Hno3_abundance .* Am_hno3;
            Tau_total = Tau_total + tau_hno3;
        catch
            % Species not available, skip
        end

        % 2. Nitrogen Dioxide, NO2
        try
            No2_abs = Args.AbsorptionData.getInterpolated('NO2', Lambda);
            No2_abundance = 1e-4 * min(1.8599 + 0.18453 * Pp0, 41.771 * Pp0);
            tau_no2 = No2_abs .* No2_abundance .* Am_no2;
            Tau_total = Tau_total + tau_no2;
        catch
            % Species not available, skip
        end

        % 3. Nitrogen Trioxide, NO3
        try
            No3_abs = Args.AbsorptionData.getInterpolated('NO3', Lambda);
            No3_abundance = 5e-5;
            tau_no3 = No3_abs .* No3_abundance .* Am_no3;
            Tau_total = Tau_total + tau_no3;
        catch
            % Species not available, skip
        end

        % 4. Nitric Oxide, NO
        try
            No_abs = Args.AbsorptionData.getInterpolated('NO', Lambda);
            No_abundance = 1e-4 * min(0.74307 + 2.4015 * Pp0, 57.079 * Pp0);
            tau_no = No_abs .* No_abundance .* Am_no;
            Tau_total = Tau_total + tau_no;
        catch
            % Species not available, skip
        end

        % 5. Sulfur Dioxide, SO2 (combination of SO2U and SO2I)
        So2_abs = zeros(NumWavelengths, 1);
        try
            So2u_abs = Args.AbsorptionData.getInterpolated('SO2U', Lambda);
            So2_abs = So2_abs + So2u_abs;
        catch
            % SO2U not available, skip
        end
        try
            So2i_abs = Args.AbsorptionData.getInterpolated('SO2I', Lambda);
            So2_abs = So2_abs + So2i_abs;
        catch
            % SO2I not available, skip
        end
        if any(So2_abs > 0)
            So2_abundance = 1e-4 * 0.11133 * (Pp0^0.812) * exp(0.81319 + 3.0557 * (Pp0^2) - 1.578 * (Pp0^3));
            tau_so2 = So2_abs .* So2_abundance .* Am_so2;
            Tau_total = Tau_total + tau_so2;
        end

        % 6. Ammonia, NH3
        if Pp0 > 0
            try
                Nh3_abs = Args.AbsorptionData.getInterpolated('NH3', Lambda);
                Log_pp0 = log(Pp0);
                Nh3_abundance = exp(-8.6499 + 2.1947 * Log_pp0 - 2.5936 * (Log_pp0^2) - ...
                                   1.819 * (Log_pp0^3) - 0.65854 * (Log_pp0^4));
                tau_nh3 = Nh3_abs .* Nh3_abundance .* Am_nh3;
                Tau_total = Tau_total + tau_nh3;
            catch
                % Species not available, skip
            end
        end

        % 7. Bromine Monoxide, BrO
        try
            Bro_abs = Args.AbsorptionData.getInterpolated('BrO', Lambda);
            Bro_abundance = 2.5e-6;
            tau_bro = Bro_abs .* Bro_abundance .* Am_bro;
            Tau_total = Tau_total + tau_bro;
        catch
            % Species not available, skip
        end

        % 8. Formaldehyde, CH2O
        try
            Ch2o_abs = Args.AbsorptionData.getInterpolated('CH2O', Lambda);
            Ch2o_abundance = 3e-4;
            tau_ch2o = Ch2o_abs .* Ch2o_abundance .* Am_ch2o;
            Tau_total = Tau_total + tau_ch2o;
        catch
            % Species not available, skip
        end

        % 9. Nitrous Acid, HNO2
        try
            Hno2_abs = Args.AbsorptionData.getInterpolated('HNO2', Lambda);
            Hno2_abundance = 1e-4;
            tau_hno2 = Hno2_abs .* Hno2_abundance .* Am_hno2;
            Tau_total = Tau_total + tau_hno2;
        catch
            % Species not available, skip
        end

        % 10. Chlorine Nitrate, ClNO3
        try
            Clno_abs = Args.AbsorptionData.getInterpolated('ClNO', Lambda);
            Clno_abundance = 1.2e-4;
            tau_clno = Clno_abs .* Clno_abundance .* Am_clno;
            Tau_total = Tau_total + tau_clno;
        catch
            % Species not available, skip
        end

        % Calculate transmission for this parameter set
        Result(:, i) = exp(-Tau_total);
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

