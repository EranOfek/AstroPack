function Result = waterTransmission(Lambda, ParamMatrix, Args)
    % Water vapor transmission of the Earth atmosphere
    % Input  : - Lambda - Wavelength array (Angstrom), column vector.
    %                    If empty, returns ArgNames structure for parameters.
    %          - ParamMatrix - Parameter matrix where each row is [ZenithAngle_deg, PWV_cm, Pressure_mbar].
    %                        ZenithAngle_deg: Solar zenith angle in degrees [0-90].
    %                        PWV_cm: Precipitable water vapor in cm [0.1-10].
    %                        Pressure_mbar: Atmospheric pressure in mbar [800-1100].
    %          * ...,key,val,...
    %            'AbsorptionData' - Pre-loaded AbsorptionData object for fast interpolation.
    %                              Default is [].
    %            'Return' - Pre-computed results for caching (external cache pattern).
    %                      Default is [].
    %            'UsePersistentCache' - Enable/disable persistent cache. Default is true.
    %            'Tolerance' - Parameter comparison tolerance. Default is 1e-12.
    %            'GetArgNames' - Return ArgNames structure instead of calculating.
    %                           Default is false.
    % Output : - Result - Transmission matrix (wavelengths × parameter_sets) [0-1]
    %                    OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Example: % Basic usage:
    %          Lambda = linspace(3000, 11000, 401)';
    %          ParamMatrix = [45, 1.0, 1013; 60, 3.0, 950; 30, 1.5, 1020];  % Multiple parameter sets
    %          Result = astro.transmission.waterTransmission(Lambda, ParamMatrix);
    %          AbsData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    %          Result = astro.transmission.waterTransmission(Lambda, ParamMatrix, 'AbsorptionData', AbsData);
    %
    %          % Get parameter information:
    %          ArgNames = astro.transmission.waterTransmission('GetArgNames', true);
    %
    %          % Usage with CompositeFun:
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('Water vapor', @astro.transmission.waterTransmission, [], 'Par', [45, 2.5, 1013]);

    arguments
        Lambda        = linspace(3000,11000,401)'
        ParamMatrix   = [30, 1.0, 965]          % [ZenithAngle_deg, PWV_cm, Pressure_mbar]
        Args.AbsorptionData = []
        Args.Return = []
        Args.UsePersistentCache logical = true  % Enable/disable persistent cache
        Args.Tolerance = 1e-12                  % Parameter comparison tolerance
        Args.GetArgNames logical = false        % Return ArgNames structure instead of calculating
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2, 3}, ...
                       'Description', {'ZenithAngle_deg', 'PWV_cm', 'Pressure_mbar'}, ...
                       'Min', {0, 0.1, 800}, ...
                       'Max', {90, 10, 1100});
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
        error('ParamMatrix must have 3 columns: [ZenithAngle_deg, PWV_cm, Pressure_mbar]');
    end

    % Create or validate AbsorptionData object
    if isempty(Args.AbsorptionData)
        Args.AbsorptionData = astro.transmission.loadAbsorptionInterpolantsSMARTS();
    end

    % Extract parameters
    ZenithAngles = ParamMatrix(:, 1);  % Column vector
    PWV_cm = ParamMatrix(:, 2);        % Column vector
    Pressure_mbar = ParamMatrix(:, 3); % Column vector
    NumParamSets = size(ParamMatrix, 1);
    NumWavelengths = length(Lambda);

    % Validate zenith angles
    if any(ZenithAngles > 90 | ZenithAngles < 0)
        error('Zenith angles out of range [0, 90] degrees');
    end

    % Validate PWV values
    if any(PWV_cm < 0)
        error('PWV values must be non-negative');
    end

    % Validate pressure values
    if any(Pressure_mbar <= 0)
        error('Pressure values must be positive');
    end

    % Get all H2O coefficients interpolated to Lambda wavelengths
    H2O_Data = Args.AbsorptionData.getH2OCoefficients(Lambda);

    % Initialize result matrix
    Result = zeros(NumWavelengths, NumParamSets);

    % Calculate transmission for each parameter set using full SMARTS model
    for i = 1:NumParamSets
        % Current parameters
        Z_ = ZenithAngles(i);
        Pw_ = PWV_cm(i);
        Pressure = Pressure_mbar(i);

        % Calculate airmass for water vapor
        Airmasses = astro.transmission.airmassSMARTS(Z_);
        Am_ = Airmasses.water;

        % SMARTS 2.9.5 Water Vapor Model Implementation
        % Following the exact algorithm from transmissionFast.atmospheric.waterTransmittance

        % === Bw Calculation (Water Vapor Correction) ===
        % Reference precipitable water values by band
        Pw0 = 4.11467 * ones(size(H2O_Data.band));
        Pw0(H2O_Data.band == 2) = 2.92232;
        Pw0(H2O_Data.band == 3) = 1.41642;
        Pw0(H2O_Data.band == 4) = 0.41612;
        Pw0(H2O_Data.band == 5) = 0.05663;

        Pww0 = Pw_ - Pw0;

        % Basic quadratic fit (vectorized)
        Bw = 1 + H2O_Data.bwa0 .* Pww0 + H2O_Data.bwa1 .* (Pww0.^2);

        % Apply different fitting functions based on Ifitw
        mask1 = (H2O_Data.ifitw == 1);
        if any(mask1)
            Bw(mask1) = Bw(mask1) ./ (1 + H2O_Data.bwa2(mask1) .* Pww0(mask1));
        end

        mask2 = (H2O_Data.ifitw == 2);
        if any(mask2)
            Bw(mask2) = Bw(mask2) ./ (1 + H2O_Data.bwa2(mask2) .* (Pww0(mask2).^2));
        end

        mask6 = (H2O_Data.ifitw == 6);
        if any(mask6)
            Bw(mask6) = H2O_Data.bwa0(mask6) + H2O_Data.bwa1(mask6) .* Pww0(mask6);
        end

        % Set Bw = 1 where absorption is negligible
        Bw(H2O_Data.absorption <= 0) = 1;

        % Clip to valid range
        Bw = max(0.05, min(7.0, Bw));

        % === Bm Calculation (Airmass Correction) ===
        Am1 = Am_ - 1;
        Am12 = Am1^2;

        Bm = ones(size(H2O_Data.ifitm));

        % Different fitting functions based on Ifitm
        mask0 = (H2O_Data.ifitm == 0);
        if any(mask0)
            Bm(mask0) = H2O_Data.bma1(mask0) .* (Am_.^H2O_Data.bma2(mask0));
        end

        mask1 = (H2O_Data.ifitm == 1);
        if any(mask1)
            Bmx = (1 + H2O_Data.bma0(mask1)*Am1 + H2O_Data.bma1(mask1)*Am12) ./ (1 + H2O_Data.bma2(mask1)*Am1);
            Bm(mask1) = Bmx;
        end

        mask2 = (H2O_Data.ifitm == 2);
        if any(mask2)
            Bmx = (1 + H2O_Data.bma0(mask2)*Am1 + H2O_Data.bma1(mask2)*Am12) ./ (1 + H2O_Data.bma2(mask2)*Am12);
            Bm(mask2) = Bmx;
        end

        mask3 = (H2O_Data.ifitm == 3);
        if any(mask3)
            Bmx = (1 + H2O_Data.bma0(mask3)*Am1 + H2O_Data.bma1(mask3)*Am12) ./ (1 + H2O_Data.bma2(mask3)*sqrt(Am1));
            Bm(mask3) = Bmx;
        end

        mask5 = (H2O_Data.ifitm == 5);
        if any(mask5)
            Bmx = (1 + H2O_Data.bma0(mask5)*(Am1.^0.25)) ./ (1 + H2O_Data.bma2(mask5)*(Am1.^0.1));
            Bm(mask5) = Bmx;
        end

        % Set Bm = 1 where absorption is negligible
        Bm(H2O_Data.absorption <= 0) = 1;

        % Clip to valid range
        Bm = max(0.05, min(7.0, Bm));

        % === Bmw Calculation (Combined Water-Airmass Correction) ===
        Bmw = Bm .* Bw;

        % Define conditions where simple multiplication applies
        Cond1 = abs(Bw - 1) < 1e-6;
        Cond2 = ((H2O_Data.ifitm ~= 0) | (H2O_Data.absorption <= 0)) & (abs(Bm - 1) < 1e-6);
        Cond3 = ((H2O_Data.ifitm == 0) | (H2O_Data.absorption <= 0)) & (Bm > 0.968) & (Bm < 1.0441);
        Cond4 = (H2O_Data.ifitmw == -1) | (H2O_Data.absorption <= 0);
        Combined_cond = Cond1 | Cond2 | Cond3 | Cond4;

        % For complex cases, use advanced fitting
        complex_mask = ~Combined_cond;
        if any(complex_mask)
            W0 = Pw0;  % Reuse pre-computed values

            Amw = Am_ * (Pw_ ./ W0);
            Amw1 = Amw - 1;
            Amw12 = Amw1.^2;

            Bmwx = ones(size(Bmw));

            % Different fitting functions based on Ifitmw
            mask0 = (H2O_Data.ifitmw == 0) & (H2O_Data.absorption > 0);
            if any(mask0)
                Bmwx(mask0) = H2O_Data.bmwa1(mask0) .* (Amw(mask0).^H2O_Data.bmwa2(mask0));
            end

            mask1 = (H2O_Data.ifitmw == 1) & (H2O_Data.absorption > 0);
            if any(mask1)
                Bmwx(mask1) = (1 + H2O_Data.bmwa0(mask1).*Amw1(mask1) + H2O_Data.bmwa1(mask1).*Amw12(mask1)) ./ ...
                              (1 + H2O_Data.bmwa2(mask1).*Amw1(mask1));
            end

            mask2 = (H2O_Data.ifitmw == 2) & (H2O_Data.absorption > 0);
            if any(mask2)
                Bmwx(mask2) = (1 + H2O_Data.bmwa0(mask2).*Amw1(mask2) + H2O_Data.bmwa1(mask2).*Amw12(mask2)) ./ ...
                              (1 + H2O_Data.bmwa2(mask2).*Amw12(mask2));
            end

            % Apply advanced fitting where conditions are not met
            Bmw(complex_mask) = Bmwx(complex_mask);
        end

        % Clip to valid range
        Bmw = max(0.05, min(7.0, Bmw));

        % === Bp Calculation (Pressure Correction) ===
        Pwm = Pw_ * Am_;
        Pp0 = Pressure / 1013.25;
        Pp01 = max(0.65, Pp0);
        Pp02 = Pp01^2;
        Qp = 1 - Pp0;
        Qp1 = min(0.35, Qp);
        Qp2 = Qp1^2;

        % Default pressure correction
        Bp = (1 + 0.1623 * Qp) * ones(size(H2O_Data.band));

        % Band-specific corrections
        mask2 = (H2O_Data.band == 2) & (H2O_Data.absorption > 0);
        if any(mask2)
            Bp(mask2) = 1 + 0.08721 * Qp1;
        end

        mask3 = (H2O_Data.band == 3) & (H2O_Data.absorption > 0);
        if any(mask3)
            A = 1 - H2O_Data.bpa1(mask3) .* Qp1 - H2O_Data.bpa2(mask3) .* Qp2;
            Bp(mask3) = A;
        end

        mask4 = (H2O_Data.band == 4) & (H2O_Data.absorption > 0);
        if any(mask4)
            A4 = 1 - H2O_Data.bpa1(mask4) .* Qp1 - H2O_Data.bpa2(mask4) .* Qp2;
            B4 = 1 - Pwm .* exp(-0.63486 + 6.9149*Pp01 - 13.853*Pp02);
            Bp(mask4) = A4 .* B4;
        end

        mask5 = (H2O_Data.band == 5) & (H2O_Data.absorption > 0);
        if any(mask5)
            A5 = 1 - H2O_Data.bpa1(mask5) .* Qp1 - H2O_Data.bpa2(mask5) .* Qp2;
            B5 = 1 - Pwm .* exp(8.9243 - 18.197*Pp01 + 2.4141*Pp02);
            Bp(mask5) = A5 .* B5;
        end

        % Set Bp = 1 where pressure effects are negligible
        Bp((abs(Qp) < 1e-5) | (H2O_Data.absorption <= 0)) = 1;

        % Clip to valid range
        Bp = max(0.3, min(1.7, Bp));

        % === Final Optical Depth and Transmission Calculation ===
        Pwm = (Pw_ * Am_).^0.9426;
        TauWater = Bmw .* Bp .* H2O_Data.absorption * Pwm;

        % Calculate transmission
        Result(:, i) = exp(-TauWater);
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