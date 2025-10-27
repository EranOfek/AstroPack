function Result = umgTransmission(Lambda, ParamMatrix, Args)
    % Uniformly Mixed Gases (UMG) transmission of the Earth atmosphere
    % Input  : - Lambda (double array): Wavelength array in nm.
    %            If GetArgNames flag is true, returns ArgNames structure for parameters.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [ZenithAngle_deg, Temperature_C, Pressure_mbar].
    %          * ...,key,val,...
    %            'AbsorptionData' - Pre-loaded AbsorptionData object for fast interpolation.
    %                              Default is [].
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
    %          AbsData = astro.transmission.loadAbsorptionInterpolants();
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
        Args.AbsorptionData = astro.transmission.loadAbsorptionInterpolants();
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
        % Using interpolants from loadAbsorptionInterpolants
        % =========================================================================

        % 1. Oxygen (O2)
        try
            O2_abs = Args.AbsorptionData.getInterpolated('O2', Lambda);
            Tau_total = Tau_total + O2_abs .* Abundance_o2 .* Am_o2;
        catch
            % Species not available, skip
        end

        % 2. Methane (CH4)
        try
            Ch4_abs = Args.AbsorptionData.getInterpolated('CH4', Lambda);
            Tau_total = Tau_total + Ch4_abs .* Abundance_ch4 .* Am_ch4;
        catch
            % Species not available, skip
        end

        % 3. Carbon Monoxide (CO)
        try
            Co_abs = Args.AbsorptionData.getInterpolated('CO', Lambda);
            Tau_total = Tau_total + Co_abs .* Abundance_co .* Am_co;
        catch
            % Species not available, skip
        end

        % 4. Nitrous Oxide (N2O)
        try
            N2o_abs = Args.AbsorptionData.getInterpolated('N2O', Lambda);
            Tau_total = Tau_total + N2o_abs .* Abundance_n2o .* Am_n2o;
        catch
            % Species not available, skip
        end

        % 5. Carbon Dioxide (CO2)
        try
            Co2_abs = Args.AbsorptionData.getInterpolated('CO2', Lambda);
            Tau_total = Tau_total + Co2_abs .* Abundance_co2 .* Am_co2;
        catch
            % Species not available, skip
        end

        % 6. Nitrogen (N2)
        try
            N2_abs = Args.AbsorptionData.getInterpolated('N2', Lambda);
            Tau_total = Tau_total + N2_abs .* Abundance_n2 .* Am_n2;
        catch
            % Species not available, skip
        end

        % 7. Oxygen-Oxygen collision complex (O4)
        try
            O4_abs = Args.AbsorptionData.getInterpolated('O4', Lambda) * 1e-46;
            Tau_total = Tau_total + O4_abs .* Abundance_o4 .* Am_o4;
        catch
            % Species not available, skip
        end
    
        % =========================================================================
        % TRACE GASES PROCESSING (always included)
        % =========================================================================

        % 1. Nitric Acid, HNO3
        try
            HNO3_Data = Args.AbsorptionData.getTraceGasCoefficients('HNO3', Lambda);
            if HNO3_Data.has_temp_correction && isfield(HNO3_Data, 'b0')
                % Apply temperature correction: Hno3_abs = 1e-20 * xs * exp(1e-3 * b0 * (234.2 - 298))
                Hno3_abs = 1e-20 * HNO3_Data.absorption .* exp(1e-3 * HNO3_Data.b0 * (234.2 - 298));
            else
                % No temperature correction available, use base absorption
                Hno3_abs = HNO3_Data.absorption;
            end
            Hno3_abundance = 1e-4 * 3.637 * (Pp0^0.12319);
            Tau_total = Tau_total + Hno3_abs .* Hno3_abundance .* Am_hno3;
        catch
            % Species not available, skip
        end
        
        % 2. Nitrogen Dioxide, NO2
        try
            NO2_Data = Args.AbsorptionData.getTraceGasCoefficients('NO2', Lambda);
            if NO2_Data.has_temp_correction && isfield(NO2_Data, 'b0')
                % Apply temperature correction: No2_abs = (sigma + b0 * (228.7 - 220))
                No2_abs = NO2_Data.absorption + NO2_Data.b0 * (228.7 - 220);
            else
                % No temperature correction available, use base absorption
                No2_abs = NO2_Data.absorption;
            end
            No2_abundance = 1e-4 * min(1.8599 + 0.18453 * Pp0, 41.771 * Pp0);
            Tau_total = Tau_total + No2_abs .* No2_abundance .* Am_no2;
        catch
            % Species not available, skip
        end
        
        % 3. Nitrogen Trioxide, NO3
        try
            NO3_Data = Args.AbsorptionData.getTraceGasCoefficients('NO3', Lambda);
            if NO3_Data.has_temp_correction && isfield(NO3_Data, 'b0')
                % Apply temperature correction: No3_abs = (xs + b0 * (225.3 - 230))
                No3_abs = NO3_Data.absorption + NO3_Data.b0 * (225.3 - 230);
            else
                % No temperature correction available, use base absorption
                No3_abs = NO3_Data.absorption;
            end
            No3_abundance = 5e-5;
            Tau_total = Tau_total + No3_abs .* No3_abundance .* Am_no3;
        catch
            % Species not available, skip
        end
        
        % 4. Nitric Oxide, NO
        try
            NO_Data = Args.AbsorptionData.getTraceGasCoefficients('NO', Lambda);
            No_abs = NO_Data.absorption;  % NO typically has no temperature correction
            No_abundance = 1e-4 * min(0.74307 + 2.4015 * Pp0, 57.079 * Pp0);
            Tau_total = Tau_total + No_abs .* No_abundance .* Am_no;
        catch
            % Species not available, skip
        end
        
        % 5. Sulfur Dioxide, SO2 (combination of SO2U and SO2I)
        So2_abs = zeros(NumWavelengths, 1);
        try
            SO2U_Data = Args.AbsorptionData.getTraceGasCoefficients('SO2U', Lambda);
            if SO2U_Data.has_temp_correction && isfield(SO2U_Data, 'b0')
                % Apply temperature correction: So2_abs = (sigma + b0 * (247 - 213))
                So2u_contrib = SO2U_Data.absorption + SO2U_Data.b0 * (247 - 213);
            else
                So2u_contrib = SO2U_Data.absorption;
            end
            So2_abs = So2_abs + So2u_contrib;
        catch
            % SO2U not available, skip
        end
        try
            SO2I_Data = Args.AbsorptionData.getTraceGasCoefficients('SO2I', Lambda);
            So2_abs = So2_abs + SO2I_Data.absorption;  % SO2I typically has no temperature correction
        catch
            % SO2I not available, skip
        end
        if any(So2_abs > 0)
            So2_abundance = 1e-4 * 0.11133 * (Pp0^0.812) * exp(0.81319 + 3.0557 * (Pp0^2) - 1.578 * (Pp0^3));
            Tau_total = Tau_total + So2_abs .* So2_abundance .* Am_so2;
        end
        
        % 6. Ammonia, NH3
        if Pp0 > 0
            try
                Nh3_abs = Args.AbsorptionData.getInterpolated('NH3', Lambda);
                Log_pp0 = log(Pp0);
                Nh3_abundance = exp(-8.6499 + 2.1947 * Log_pp0 - 2.5936 * (Log_pp0^2) - ...
                                   1.819 * (Log_pp0^3) - 0.65854 * (Log_pp0^4));
                Tau_total = Tau_total + Nh3_abs .* Nh3_abundance .* Am_nh3;
            catch
                % Species not available, skip
            end
        end
        
        % 7. Bromine Monoxide, BrO
        try
            BrO_Data = Args.AbsorptionData.getTraceGasCoefficients('BrO', Lambda);
            Bro_abs = BrO_Data.absorption;  % BrO typically has no temperature correction (Loschmidt already in data)
            Bro_abundance = 2.5e-6;
            Tau_total = Tau_total + Bro_abs .* Bro_abundance .* Am_bro;
        catch
            % Species not available, skip
        end
        
        % 8. Formaldehyde, CH2O
        try
            CH2O_Data = Args.AbsorptionData.getTraceGasCoefficients('CH2O', Lambda);
            if CH2O_Data.has_temp_correction && isfield(CH2O_Data, 'b0')
                % Apply temperature correction: Ch2o_abs = (xs + b0 * (264 - 293))
                Ch2o_abs = CH2O_Data.absorption + CH2O_Data.b0 * (264 - 293);
            else
                % No temperature correction available, use base absorption
                Ch2o_abs = CH2O_Data.absorption;
            end
            Ch2o_abundance = 3e-4;
            Tau_total = Tau_total + Ch2o_abs .* Ch2o_abundance .* Am_ch2o;
        catch
            % Species not available, skip
        end
        
        % 9. Nitrous Acid, HNO2
        try
            HNO2_Data = Args.AbsorptionData.getTraceGasCoefficients('HNO2', Lambda);
            Hno2_abs = HNO2_Data.absorption;  % HNO2 typically has no temperature correction (Loschmidt already in data)
            Hno2_abundance = 1e-4;
            Tau_total = Tau_total + Hno2_abs .* Hno2_abundance .* Am_hno2;
        catch
            % Species not available, skip
        end
        
        % 10. Chlorine Nitrate, ClNO3
        try
            ClNO_Data = Args.AbsorptionData.getTraceGasCoefficients('ClNO', Lambda);
            if ClNO_Data.has_temp_correction && isfield(ClNO_Data, 'b0')
                TCl = 230;  % K
                if isfield(ClNO_Data, 'b1')
                    % Full quadratic correction: Clno_abs = xs * (1 + b0*(TCl-296) + b1*(TCl-296)^2)
                    Clno_abs = ClNO_Data.absorption .* (1 + ClNO_Data.b0 * (TCl - 296) + ClNO_Data.b1 * ((TCl - 296)^2));
                else
                    % Linear correction only
                    Clno_abs = ClNO_Data.absorption .* (1 + ClNO_Data.b0 * (TCl - 296));
                end
            else
                % No temperature correction available, use base absorption
                Clno_abs = ClNO_Data.absorption;
            end
            Clno_abundance = 1.2e-4;
            Tau_total = Tau_total + Clno_abs .* Clno_abundance .* Am_clno;
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

