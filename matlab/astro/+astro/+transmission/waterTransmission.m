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
        ParamMatrix   = [30, 1.4, 965]          % [ZenithAngle_deg, PWV_cm, Pressure_mbar]
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

    % Calculate airmasses for all zenith angles at once (vectorized)
    Airmasses = astro.transmission.airmassSMARTS(ZenithAngles);

    % Group by unique (PWV, Pressure) pairs
    NonZA = [PWV_cm, Pressure_mbar];
    [UniqNonZA, ~, Ic] = unique(NonZA, 'rows');

    Result = zeros(NumWavelengths, NumParamSets);

    % Pre-compute band-dependent reference water values (same for all groups)
    Pw0 = 4.11467 * ones(size(H2O_Data.band));
    Pw0(H2O_Data.band == 2) = 2.92232;
    Pw0(H2O_Data.band == 3) = 1.41642;
    Pw0(H2O_Data.band == 4) = 0.41612;
    Pw0(H2O_Data.band == 5) = 0.05663;

    % Pre-compute Ifitm masks (same for all groups)
    fitm0 = (H2O_Data.ifitm == 0);
    fitm1 = (H2O_Data.ifitm == 1);
    fitm2 = (H2O_Data.ifitm == 2);
    fitm3 = (H2O_Data.ifitm == 3);
    fitm5 = (H2O_Data.ifitm == 5);
    abs_neg = (H2O_Data.absorption <= 0);

    for ig = 1:size(UniqNonZA, 1)
        Mask = (Ic == ig);
        Am_vec = Airmasses.water(Mask)';  % [1 x Ngroup]

        Pw_ = UniqNonZA(ig, 1);
        Pressure = UniqNonZA(ig, 2);

        % === Bw Calculation (Water Vapor Correction) — same for all sources in group ===
        Pww0 = Pw_ - Pw0;
        Bw = 1 + H2O_Data.bwa0 .* Pww0 + H2O_Data.bwa1 .* (Pww0.^2);

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
        Bw(abs_neg) = 1;
        Bw = max(0.05, min(7.0, Bw));

        % === Bp Calculation (Pressure Correction) — same for all sources in group ===
        Pp0 = Pressure / 1013.25;
        Pp01 = max(0.65, Pp0);
        Pp02 = Pp01^2;
        Qp = 1 - Pp0;
        Qp1 = min(0.35, Qp);
        Qp2 = Qp1^2;

        Bp = (1 + 0.1623 * Qp) * ones(size(H2O_Data.band));
        bmask2 = (H2O_Data.band == 2) & ~abs_neg;
        if any(bmask2)
            Bp(bmask2) = 1 + 0.08721 * Qp1;
        end
        bmask3 = (H2O_Data.band == 3) & ~abs_neg;
        if any(bmask3)
            Bp(bmask3) = 1 - H2O_Data.bpa1(bmask3) .* Qp1 - H2O_Data.bpa2(bmask3) .* Qp2;
        end
        bmask4 = (H2O_Data.band == 4) & ~abs_neg;
        bmask5 = (H2O_Data.band == 5) & ~abs_neg;
        Bp((abs(Qp) < 1e-5) | abs_neg) = 1;
        Bp = max(0.3, min(1.7, Bp));

        % === Per-source airmass-dependent terms: Bm, Bmw, Pwm ===
        % Loop over sources in this group for airmass-dependent Bm/Bmw
        Ngroup = sum(Mask);
        GroupResult = zeros(NumWavelengths, Ngroup);

        for js = 1:Ngroup
            Am_ = Am_vec(js);

            % Bm Calculation (Airmass Correction)
            Am1 = Am_ - 1;
            Am12 = Am1^2;
            Bm = ones(size(H2O_Data.ifitm));

            if any(fitm0)
                Bm(fitm0) = H2O_Data.bma1(fitm0) .* (Am_.^H2O_Data.bma2(fitm0));
            end
            if any(fitm1)
                Bm(fitm1) = (1 + H2O_Data.bma0(fitm1)*Am1 + H2O_Data.bma1(fitm1)*Am12) ./ (1 + H2O_Data.bma2(fitm1)*Am1);
            end
            if any(fitm2)
                Bm(fitm2) = (1 + H2O_Data.bma0(fitm2)*Am1 + H2O_Data.bma1(fitm2)*Am12) ./ (1 + H2O_Data.bma2(fitm2)*Am12);
            end
            if any(fitm3)
                Bm(fitm3) = (1 + H2O_Data.bma0(fitm3)*Am1 + H2O_Data.bma1(fitm3)*Am12) ./ (1 + H2O_Data.bma2(fitm3)*sqrt(Am1));
            end
            if any(fitm5)
                Bm(fitm5) = (1 + H2O_Data.bma0(fitm5)*(Am1.^0.25)) ./ (1 + H2O_Data.bma2(fitm5)*(Am1.^0.1));
            end
            Bm(abs_neg) = 1;
            Bm = max(0.05, min(7.0, Bm));

            % Bmw Calculation (Combined Water-Airmass Correction)
            Bmw = Bm .* Bw;
            Cond1 = abs(Bw - 1) < 1e-6;
            Cond2 = ((H2O_Data.ifitm ~= 0) | abs_neg) & (abs(Bm - 1) < 1e-6);
            Cond3 = ((H2O_Data.ifitm == 0) | abs_neg) & (Bm > 0.968) & (Bm < 1.0441);
            Cond4 = (H2O_Data.ifitmw == -1) | abs_neg;
            Combined_cond = Cond1 | Cond2 | Cond3 | Cond4;
            complex_mask = ~Combined_cond;

            if any(complex_mask)
                Amw = Am_ * (Pw_ ./ Pw0);
                Amw1 = Amw - 1;
                Amw12 = Amw1.^2;
                Bmwx = ones(size(Bmw));

                cmask0 = (H2O_Data.ifitmw == 0) & ~abs_neg;
                if any(cmask0)
                    Bmwx(cmask0) = H2O_Data.bmwa1(cmask0) .* (Amw(cmask0).^H2O_Data.bmwa2(cmask0));
                end
                cmask1 = (H2O_Data.ifitmw == 1) & ~abs_neg;
                if any(cmask1)
                    Bmwx(cmask1) = (1 + H2O_Data.bmwa0(cmask1).*Amw1(cmask1) + H2O_Data.bmwa1(cmask1).*Amw12(cmask1)) ./ ...
                                  (1 + H2O_Data.bmwa2(cmask1).*Amw1(cmask1));
                end
                cmask2 = (H2O_Data.ifitmw == 2) & ~abs_neg;
                if any(cmask2)
                    Bmwx(cmask2) = (1 + H2O_Data.bmwa0(cmask2).*Amw1(cmask2) + H2O_Data.bmwa1(cmask2).*Amw12(cmask2)) ./ ...
                                  (1 + H2O_Data.bmwa2(cmask2).*Amw12(cmask2));
                end
                Bmw(complex_mask) = Bmwx(complex_mask);
            end
            Bmw = max(0.05, min(7.0, Bmw));

            % Band 4 and 5 Bp corrections depend on Pwm = Pw_ * Am_
            Pwm_val = Pw_ * Am_;
            Bp_s = Bp;  % start from group-level Bp
            if any(bmask4)
                A4 = 1 - H2O_Data.bpa1(bmask4) .* Qp1 - H2O_Data.bpa2(bmask4) .* Qp2;
                B4 = 1 - Pwm_val .* exp(-0.63486 + 6.9149*Pp01 - 13.853*Pp02);
                Bp_s(bmask4) = A4 .* B4;
            end
            if any(bmask5)
                A5 = 1 - H2O_Data.bpa1(bmask5) .* Qp1 - H2O_Data.bpa2(bmask5) .* Qp2;
                B5 = 1 - Pwm_val .* exp(8.9243 - 18.197*Pp01 + 2.4141*Pp02);
                Bp_s(bmask5) = A5 .* B5;
            end
            Bp_s((abs(Qp) < 1e-5) | abs_neg) = 1;
            Bp_s = max(0.3, min(1.7, Bp_s));

            % Final Optical Depth and Transmission
            Pwm = (Pw_ * Am_).^0.9426;
            TauWater = Bmw .* Bp_s .* H2O_Data.absorption * Pwm;
            GroupResult(:, js) = exp(-TauWater);
        end

        Result(:, Mask) = GroupResult;
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