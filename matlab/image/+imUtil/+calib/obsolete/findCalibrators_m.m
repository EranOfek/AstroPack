function CalibData = findCalibrators_m(Obj, Metadata, Args)
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
    Npoint = FluxEnd - FluxIni + 1;

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
  %  elseif isa(Obj, 'AstroCatalog')         %%%%% remove later
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
   % for i = 1:Nsrc
        % Cone search in GAIADR3spec catalog
   %     [Sp, ~, ~, D] = catsHTM.cone_search('GAIADR3spec', ...
    %                                        Tab.RA(i)/RAD, Tab.Dec(i)/RAD, ...
     %                                       Args.SearchRadius);

        [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', 'Coo', [Cat.Table.RA/RAD, Cat.Table.Dec/RAD], 'Radius', Args.SearchRadius, 'CooUnits', 'rad', 'RadiusUnits','arcsec');  
 
% --- Build index mapping: pick ONLY matched pairs ---
% ResInd is per-element; typically single element:
if numel(ResInd) ~= 1
    error('This routine currently expects a single-element match; got %d.', numel(ResInd));
end

% For each source in Obj2 (LAST), index of nearest match in Obj1 (Gaia):
gaiaIdx_all   = ResInd.Obj2_IndInObj1;     % length = n(LAST in Cat)
dist_rad_all  = ResInd.Obj2_Dist;          % radians (for 'sphere')
nmatch_all    = ResInd.Obj2_NmatchObj1;

% Keep only rows with a valid Gaia index:
idxLastMatched = find(~isnan(gaiaIdx_all));
gaiaIdx        = double(gaiaIdx_all(idxLastMatched));
dist_rad       = dist_rad_all(idxLastMatched);
nmatch         = nmatch_all(idxLastMatched);

if isempty(idxLastMatched)
    warning('No Gaia XP matches found within %.1f arcsec', Args.SearchRadius);
    CalibData = struct('Spec', {}, 'Coords', {}, 'LASTData', {}, ...
                       'Metadata', Metadata, 'GaiaCatalog', AstroCatalog());
    return;
end

% --- Extract the matched tables in the SAME pairing order ---
% LAST side (restrict to matched)
LastTab = Cat.Table(idxLastMatched, :);

% Gaia side (select matched rows from the full cone-search result)
GaiaTabAll = CatH.Table;              % full cone-search
GaiaTab    = GaiaTabAll(gaiaIdx, :);  % only matched
Nmatch     = height(GaiaTab);

if Args.Verbose
    fprintf('  Found %d LAST↔Gaia matched pairs (nearest within radius)\n', Nmatch);
end

% Optionally, attach distance and multiplicity:
Dist_arcsec = convert.angular('rad','arcsec', dist_rad);
LastTab.Dist_match_arcsec  = Dist_arcsec;
LastTab.Nmatch_within_rad  = nmatch;

% ====================================================================
% STEP 4: EXTRACT XP SPECTRA AND COORDINATES (matched only)
% ====================================================================

% Columns 7–349 = XP flux, 350–692 = XP flux errors in GAIADR3spec
FluxIni  = 7;   FluxEnd  = 349;
EFluxIni = 350; EFluxEnd = 692;
Npoint   = FluxEnd - FluxIni + 1;

GaiaArr = table2array(GaiaTab);
SrcSpecCol1 = mat2cell(GaiaArr(:,FluxIni:FluxEnd),  ones(Nmatch,1), Npoint);  % flux
SrcSpecCol2 = mat2cell(GaiaArr(:,EFluxIni:EFluxEnd), ones(Nmatch,1), Npoint); % flux err
SrcSpec     = [SrcSpecCol1 SrcSpecCol2];

% Coordinates
RAD      = 180/pi;
Gaia_RA  = GaiaArr(:,1) * RAD;   % RA in rad -> deg
Gaia_Dec = GaiaArr(:,2) * RAD;   % Dec in rad -> deg

LAST_RA  = LastTab.RA;
LAST_Dec = LastTab.Dec;
LAST_X   = LastTab.X;
LAST_Y   = LastTab.Y;

% Magnitude vector aligned to matched LAST rows (if present)
if ismember('MAG_PSF', LastTab.Properties.VariableNames)
    MagPSF = LastTab.MAG_PSF;
else
    MagPSF = NaN(height(LastTab),1);
end

% ====================================================================
% STEP 5: (optional) de-duplicate if multiple LAST map to same Gaia
% ====================================================================
% Keep first occurrence per Gaia match, if you want strict 1–1 calibrators:
% [~, keep] = unique(gaiaIdx, 'stable');
% GaiaTab  = GaiaTab(keep, :);
% LastTab  = LastTab(keep, :);
% SrcSpec  = SrcSpec(keep, :);
% Gaia_RA  = Gaia_RA(keep); Gaia_Dec = Gaia_Dec(keep);
% LAST_RA  = LAST_RA(keep); LAST_Dec = LAST_Dec(keep);
% LAST_X   = LAST_X(keep);  LAST_Y   = LAST_Y(keep);
% MagPSF   = MagPSF(keep);
% Nmatch   = numel(keep);

% ====================================================================
% STEP 6: CREATE OUTPUT STRUCTURE
% ====================================================================

% Build coordinate structure array (matched order)
CalibCoords = struct( ...
    'Gaia_RA',  num2cell(Gaia_RA), ...
    'Gaia_Dec', num2cell(Gaia_Dec), ...
    'LAST_RA',  num2cell(LAST_RA), ...
    'LAST_Dec', num2cell(LAST_Dec), ...
    'LAST_X',   num2cell(LAST_X), ...
    'LAST_Y',   num2cell(LAST_Y));

CalibData = struct();
CalibData.Spec        = SrcSpec;
CalibData.Coords      = CalibCoords;
CalibData.LASTData    = LastTab;
CalibData.Metadata    = Metadata;

% Return a Gaia AstroCatalog with ONLY the matched rows
% (construct from CatH by row selection if you prefer keeping AstroCatalog type)
CalibData.GaiaCatalog = CatH;              % keep original type
CalibData.GaiaCatalog.Table = GaiaTab;     % overwrite with matched subset

if Args.Verbose
    fprintf('Calibrator search complete: %d matched calibrators.\n\n', Nmatch);
end