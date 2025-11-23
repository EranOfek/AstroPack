function CalibData = findCalibrators(Obj, Metadata, Args)
    % Find Gaia DR3 calibrators with XP spectra for photometric calibration
    % This is a placeholder function based on transmissionFast implementation.
    %
    % Input  : - Obj - AstroImage or AstroCatalog object with LAST sources
    %          - Metadata - Structure with observation metadata (airmass, temp, pressure)
    %          * ...,key,val,...
    %            'SearchRadius' - Gaia cone search radius [arcsec]. Default is 1.0.
    %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
    %            'MinSN' - Minimum S/N for calibrators. Default is 5.
    %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
    %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
    %            'FluxColName' - LAST flux column name. Default is 'FLUX_APER_3'.
    %            'Verbose' - Enable verbose output. Default is true.
    %
    % Output : - CalibData - Structure with calibrator data:
    %            .Spec - Cell array {N x 2} with Gaia XP spectra:
    %                    Column 1: Flux values (343 wavelength points)
    %                    Column 2: Flux error values
    %            .Coords - Structure array with coordinate information:
    %                .Gaia_RA, .Gaia_Dec, .LAST_RA, .LAST_Dec, .LAST_X, .LAST_Y
    %            .LASTData - Table with LAST catalog data for matched sources
    %            .Metadata - Observation metadata
    %            .GaiaCatalog - AstroCatalog with Gaia source data (for output)
    %
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Author: D. Kovaleva (Nov 2025)
    % Based on: transmissionFast.data.findCalibratorsForAstroImage
    %           transmissionFast.data.findCalibratorsWithCoords
    % Example:
    %   Metadata = struct('Airmass', 1.2, 'Temperature', 15, 'Pressure', 965);
    %   CalibData = imUtil.calib.findCalibrators(AI, Metadata, ...
    %                   'SearchRadius', 1.0, 'MagRange', [12 16]);

    arguments
        Obj  % AstroImage or AstroCatalog
        Metadata struct
        Args.SearchRadius = 1.0  % arcsec
        Args.MagRange = [12 16]
        Args.MinSN = 5
        Args.MaxSN = 1000
        Args.FilterBadFlags logical = true
        Args.FluxColName = 'FLUX_APER_3'
        Args.Verbose logical = true
    end

    % Constants for Gaia XP spectra columns
    FluxIni = 7;      % Start of flux values in GAIADR3spec
    FluxEnd = 349;    % End of flux values
    EFluxIni = 350;   % Start of flux errors
    EFluxEnd = 692;   % End of flux errors

    RAD = 180/pi;  % Conversion factor

    % ====================================================================
    % STEP 1: EXTRACT CATALOG FROM INPUT OBJECT
    % ====================================================================

    if isa(Obj, 'AstroImage')
        % For AstroImage, use first element if array
        if numel(Obj) > 1
            warning('findCalibrators:multipleImages', ...
                    'Multiple AstroImage elements provided. Using first element only.');
        end
        Cat = Obj(1).CatData;
  %  elseif isa(Obj, 'AstroCatalog')         %%%%% zatychka - remove later
    else  
  Cat = Obj;
  %  else
  %      error('Input must be AstroImage or AstroCatalog object');
    end

    % Check if Cat is valid
    if isempty(Cat)
        error('Input catalog is empty. No sources available for calibration.');
    end

    % Verify Cat is AstroCatalog
 %   if ~isa(Cat, 'AstroCatalog')                       %%%% dummy
 %       error('Extracted catalog is not an AstroCatalog object (class: %s)', class(Cat));
 %   end

    % Get the catalog table - AstroCatalog has .Table property (table format)
    % and .Catalog property (double matrix format)
    Tab = Cat.Table;

    % Verify we got a table
    if isempty(Tab)
        error('Catalog table is empty. No sources available for calibration.');
    end

    if ~istable(Tab)
        error('Cat.Table is not a table (class: %s). Expected table. Check AstroCatalog object structure.', ...
              class(Tab));
    end

    % ====================================================================
    % STEP 2: APPLY QUALITY FILTERS
    % ====================================================================

    if Args.Verbose
        fprintf('Starting calibrator search with %d sources...\n', height(Tab));
    end

    % Filter 1: Magnitude range
    magFilterMask = true(height(Tab), 1);
    if ismember('MAG_PSF', Tab.Properties.VariableNames)
        magFilterMask = (Tab.MAG_PSF >= Args.MagRange(1)) & (Tab.MAG_PSF <= Args.MagRange(2));
        if Args.Verbose
            fprintf('  Magnitude filter (%g-%g): %d sources passed\n', ...
                    Args.MagRange(1), Args.MagRange(2), sum(magFilterMask));
        end
    end

    Tab = Tab(magFilterMask, :);

    % Filter 2: Bad FLAGS (optional)
    if Args.FilterBadFlags && ismember('FLAGS', Tab.Properties.VariableNames)
        badFlagsMask = false(height(Tab), 1);
        for i = 1:height(Tab)
            flags = Tab.FLAGS(i);
            % Check for critical bad flags
            isSaturated = bitget(flags, 1);
            isNaN = bitget(flags, 7);
            isNegative = bitget(flags, 11);
            isCR = bitget(flags, 15);
            isNearEdge = bitget(flags, 24);

            % Mark as bad if it has multiple problematic flags
            if (isSaturated + isNaN + isNegative + isCR + isNearEdge) >= 2
                badFlagsMask(i) = true;
            end
        end
        Tab = Tab(~badFlagsMask, :);

        if Args.Verbose
            fprintf('  FLAGS filter: %d sources passed\n', height(Tab));
        end
    end

    % Filter 3: S/N range
    if ismember('SN', Tab.Properties.VariableNames)
        snMask = (Tab.SN >= Args.MinSN) & (Tab.SN <= Args.MaxSN);
        Tab = Tab(snMask, :);

        if Args.Verbose
            fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                    Args.MinSN, Args.MaxSN, height(Tab));
        end
    end

    % ====================================================================
    % STEP 3: CONE SEARCH FOR GAIA XP SPECTRA
    % ====================================================================

    Nsrc = height(Tab);

    % Initialize arrays
    SrcSpec = cell(Nsrc, 2);
    MagPSF = zeros(Nsrc, 1);

    % Coordinate arrays
    Gaia_RA = nan(Nsrc, 1);
    Gaia_Dec = nan(Nsrc, 1);
    LAST_RA = nan(Nsrc, 1);
    LAST_Dec = nan(Nsrc, 1);
    LAST_X = nan(Nsrc, 1);
    LAST_Y = nan(Nsrc, 1);
    LAST_idx = zeros(Nsrc, 1);

    if Args.Verbose
        fprintf('Searching for Gaia XP spectra (radius=%.1f arcsec)...\n', Args.SearchRadius);
    end

    matchCount = 0;
    for i = 1:Nsrc
        % Cone search in GAIADR3spec catalog
        [Sp, ~, ~, D] = catsHTM.cone_search('GAIADR3spec', ...
                                            Tab.RA(i)/RAD, Tab.Dec(i)/RAD, ...
                                            Args.SearchRadius);

        if D > 0
            matchCount = matchCount + 1;

            % Extract Gaia XP spectra
            SrcSpec{i, 1} = Sp(:, FluxIni:FluxEnd);    % Flux
            SrcSpec{i, 2} = Sp(:, EFluxIni:EFluxEnd);  % Flux error

            % Store magnitude
            MagPSF(i) = Tab.MAG_PSF(i);

            % Store coordinates (Gaia in radians, convert to degrees)
            Gaia_RA(i) = Sp(1, 1) * RAD;
            Gaia_Dec(i) = Sp(1, 2) * RAD;

            % Store LAST coordinates and positions
            LAST_RA(i) = Tab.RA(i);
            LAST_Dec(i) = Tab.Dec(i);
            LAST_X(i) = Tab.X(i);
            LAST_Y(i) = Tab.Y(i);
            LAST_idx(i) = i;
        end
    end

    if Args.Verbose
        fprintf('  Found %d matches with Gaia XP spectra\n', matchCount);
    end

    % ====================================================================
    % STEP 4: FILTER INVALID MATCHES
    % ====================================================================

    % Remove sources without spectra
    validMask = MagPSF > 0;

    % Remove LAST sources with multiple Gaia matches
    if sum(validMask) > 0
        last_idx_valid = LAST_idx(validMask);
        [unique_last, ~, idx_map] = unique(last_idx_valid);
        counts = accumarray(idx_map, 1);

        % Find duplicated LAST indices
        duplicated = unique_last(counts > 1);

        % Mark duplicates as invalid
        for i = 1:length(duplicated)
            validMask(LAST_idx == duplicated(i)) = false;
        end

        if Args.Verbose && ~isempty(duplicated)
            fprintf('  Removed %d sources with multiple Gaia matches\n', ...
                    length(duplicated) * 2);  % Each duplicated source has >=2 matches
        end
    end

    % ====================================================================
    % STEP 5: EXTRACT VALID CALIBRATORS
    % ====================================================================

    Spec = SrcSpec(validMask, :);
    Mag = MagPSF(validMask);
    LASTData = Tab(validMask, :);

    % Build coordinate structure
    numValid = sum(validMask);
    Coords = struct();
    for i = 1:numValid
        idx = find(validMask, i, 'first');
        idx = idx(end);

        Coords(i).Gaia_RA = Gaia_RA(idx);
        Coords(i).Gaia_Dec = Gaia_Dec(idx);
        Coords(i).LAST_RA = LAST_RA(idx);
        Coords(i).LAST_Dec = LAST_Dec(idx);
        Coords(i).LAST_X = LAST_X(idx);
        Coords(i).LAST_Y = LAST_Y(idx);
        Coords(i).LAST_idx = LAST_idx(idx);
    end

    if Args.Verbose
        fprintf('Final calibrator count: %d\n', numValid);
    end

    % ====================================================================
    % STEP 6: CREATE OUTPUT STRUCTURE
    % ====================================================================

    CalibData = struct();
    CalibData.Spec = Spec;
    CalibData.Coords = Coords;
    CalibData.LASTData = LASTData;
    CalibData.Metadata = Metadata;

    % Create AstroCatalog with Gaia data for output
    % (placeholder - could be expanded to include full Gaia catalog)
    CalibData.GaiaCatalog = AstroCatalog();

    if Args.Verbose
        fprintf('Calibrator search complete.\n\n');
    end
end
