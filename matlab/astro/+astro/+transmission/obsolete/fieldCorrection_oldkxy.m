function Result = fieldCorrection_oldkxy(Coords, ParamMatrix, Args)
    % Field-dependent magnitude corrections using Chebyshev polynomials for LAST detector
    % This function calculates field-dependent magnitude corrections based on detector
    % position using Chebyshev polynomial expansion up to 4th order.
    % Input  : - Coords - Coordinate matrix [X_coord, Y_coord] as Nx2 matrix
    %                     X_coord: X pixel coordinates (column 1)
    %                     Y_coord: Y pixel coordinates (column 2)
    %                     Default is [863, 863] (detector center)
    %          - ParamMatrix - Parameter matrix [NumSets x 10]:
    %                          Columns 1-5: X coefficients [kx0, kx, kx2, kx3, kx4]
    %                          Columns 6-9: Y coefficients [ky, ky2, ky3, ky4] (ky0=0 hardcoded)
    %                          Column 10: kxy (Cross term kxy² * T1(x)*T1(y) - squared to match Python)
    %                          Default is zeros(1,10) (no field correction).
    %          * ...,key,val,...
    %            'Return' - External cached result. Default is [].
    %            'UsePersistentCache' - Use persistent cache. Default is true.
    %            'GetArgNames' - Return parameter names structure. Default is false.
    %            'Tolerance' - Tolerance for cache comparison. Default is 1e-10.
    % Output : - Result - Field corrections in magnitudes [NumCoords x NumParamSets]
    % Author : D. Kovaleva (Nov 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Define coordinates and parameters
    %          Coords = [100, 100; 863, 863; 1600, 1600];
    %          Params = [0.1, 0.02, 0.01, 0, 0, 0.02, 0.01, 0, 0, 0.005];
    %          Corrections = astro.transmission.fieldCorrection_oldkxy(Coords, Params);
    %          % Use with CompositeFun
    %          TransFun = tools.math.fun.CompositeFun();
    %          TransFun.addFun('FieldCorrection', @astro.transmission.fieldCorrection_oldkxy, [], ...
    %                          'Par', zeros(1,10), 'FitPar', true(1,10));

    arguments
        Coords = [863, 863]  % Default: center of 1726x1726 detector
        ParamMatrix = zeros(1, 10)  % Default: no correction
        Args.Return = []
        Args.UsePersistentCache logical = true
        Args.GetArgNames logical = false
        Args.Tolerance = 1e-10
    end

    % ====================================================================
    % CONSTANTS: LAST Detector coordinate bounds (pixels)
    % ====================================================================
    MIN_COORD = 0;
    MAX_COORD = 1726;
    KY0 = 0;  % Y constant offset (hardcoded to zero)

    % ====================================================================
    % RETURN ARGUMENT NAMES IF REQUESTED
    % ====================================================================
    if Args.GetArgNames
        Result = struct('Name', {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, ...
                       'Description', {'kx0', 'kx', 'kx2', 'kx3', 'kx4', ...
                                      'ky', 'ky2', 'ky3', 'ky4', 'kxy'}, ...
                       'Min', {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, ...
                       'Max', {1, 1, 1, 1, 1, 1, 1, 1, 1, 1});
        return;
    end

    % ====================================================================
    % CHECK EXTERNAL CACHE
    % ====================================================================
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % ====================================================================
    % CHECK PERSISTENT CACHE
    % ====================================================================
    persistent CachedResult CachedParams CachedCoords

    if Args.UsePersistentCache && ~isempty(CachedResult)
        if isequal(size(Coords), size(CachedCoords)) && ...
           isequal(size(ParamMatrix), size(CachedParams))
            if all(abs(Coords(:) - CachedCoords(:)) < Args.Tolerance) && ...
               all(abs(ParamMatrix(:) - CachedParams(:)) < Args.Tolerance)
                Result = CachedResult;
                return;
            end
        end
    end

    % ====================================================================
    % PARSE AND VALIDATE INPUTS
    % ====================================================================

    % Validate Coords
    if size(Coords, 2) ~= 2
        error('Coords must be Nx2 matrix [X_coord, Y_coord]');
    end

    X_coord = Coords(:, 1);  % Column vector
    Y_coord = Coords(:, 2);  % Column vector
    NumCoords = length(X_coord);

    % Validate ParamMatrix
    if size(ParamMatrix, 2) ~= 10
        error('ParamMatrix must have 10 columns: [kx0, kx, kx2, kx3, kx4, ky, ky2, ky3, ky4, kxy]');
    end
    NumParamSets = size(ParamMatrix, 1);

    % ====================================================================
    % NORMALIZE COORDINATES TO [-1, 1] FOR CHEBYSHEV POLYNOMIALS
    % ====================================================================

    % Rescale X and Y coordinates to [-1, 1] interval
    % Formula: X_norm = 2 * (X - MIN) / (MAX - MIN) - 1
    X_normalized = 2 * (X_coord - MIN_COORD) / (MAX_COORD - MIN_COORD) - 1;
    Y_normalized = 2 * (Y_coord - MIN_COORD) / (MAX_COORD - MIN_COORD) - 1;

    % ====================================================================
    % GENERATE CHEBYSHEV POLYNOMIAL FUNCTION (FIRST KIND, ORDERS 0-4)
    % ====================================================================
    % tools.math.fun.chebyshevFun returns a function that evaluates
    % Chebyshev polynomials T0, T1, T2, T3, T4 and returns them as columns
    ChebyFun = tools.math.fun.chebyshevFun(1, [0, 1, 2, 3, 4]);

    % Evaluate Chebyshev polynomials at normalized coordinates
    % T_x: [NumCoords x 5] matrix with columns [T0(x), T1(x), T2(x), T3(x), T4(x)]
    T_x = ChebyFun(X_normalized);
    T_y = ChebyFun(Y_normalized);

    % ====================================================================
    % CALCULATE FIELD CORRECTIONS FOR EACH PARAMETER SET
    % ====================================================================

    % Preallocate result matrix [NumCoords x NumParamSets]
    Result = zeros(NumCoords, NumParamSets);

    % Calculate for each parameter set
    for i = 1:NumParamSets
        % Extract coefficient vectors for this parameter set
        % kxcoeff: [kx0, kx, kx2, kx3, kx4] from columns 1-5
        kxcoeff = ParamMatrix(i, 1:5);

        % kycoeff: [ky0, ky, ky2, ky3, ky4] with ky0=0 hardcoded
        kycoeff = [KY0, ParamMatrix(i, 6:9)];

        % Cross term coefficient
        kxy = ParamMatrix(i, 10);

        % Calculate Chebyshev polynomial components using matrix multiplication
        % T_x is [NumCoords x 5], kxcoeff is [1 x 5], result is [NumCoords x 1]
        Cheb_x = T_x * kxcoeff';
        Cheb_y = T_y * kycoeff';

        % Calculate cross term: kxy² * T1(x) * T1(y)
        % T1(x) is column 2 of T_x (since columns are [T0, T1, T2, T3, T4])
        % NOTE: This matches Python implementation which squares kxy
        % Python code: Cheb_xy_x(xcoor_)*Cheb_xy_y(ycoor_) = kxy*x * kxy*y = kxy²*x*y
        Cross_term = (kxy * kxy) * T_x(:, 2) .* T_y(:, 2);

        % Total field correction (in magnitudes)
        Result(:, i) = Cheb_x + Cheb_y + Cross_term;
    end

    % ====================================================================
    % STORE IN PERSISTENT CACHE
    % ====================================================================

    if Args.UsePersistentCache
        CachedResult = Result;
        CachedParams = ParamMatrix;
        CachedCoords = Coords;
    end
end
