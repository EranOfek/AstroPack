function AbsData = loadAbsorptionInterpolants(Args)
    % Load all pre-computed absorption interpolants from individual .mat files
    % Input  : * ...,key,val,...
    %            'DataPath' - Path to interpolant .mat files.
    %              Default is '/home/dana/matlab/data/spec/Atmosphere/Transmission/'.
    %            'Species' - Cell array of species to load.
    %              Default is all species.
    %            'Verbose' - Display loading progress. Default is false.
    % Output : - AbsData - Structure with getInterpolated method for compatibility.
    %            Usage: AbsData.getInterpolated('O3UV', Lambda)
    % Author : D. Kovaleva (Oct 2025)
    % Example: (1) Bulk upload of all interpolants for molecular absorption
    %              in atmosphere:
    %              AbsData = astro.transmission.loadAbsorptionInterpolants();
    %              O3_values = AbsData.getInterpolated('O3UV', linspace(300, 400, 101)');
    %          (2) Direct uploading of selected interpolants:
    %          load('~/matlab/data/spec/Atmosphere/Transmission/Abs_O3UV.mat', 'Abs_O3UV');
    %              Lambda = linspace(300, 400, 101)';
    %              Values = Abs_O3UV(Lambda);

    arguments
        Args.DataPath = "~/matlab/data/spec/Atmosphere/Transmission/";
        Args.Species  = {'O3UV', 'O3IR', 'H2O', 'CO2', 'CH4', 'N2O', 'CO', 'NO2', 'NO', 'NO3', 'SO2I', 'SO2U', 'NH3', 'HNO2', 'HNO3', 'CH2O', 'BrO', 'ClNO', 'O2', 'O4', 'N2'};
        Args.Verbose logical = false;
    end

    % Persistent cache to avoid reloading files
    persistent CachedAbsData CachedDataPath CachedSpecies

    % Check if we can return cached data
    if ~isempty(CachedAbsData)
        % Check if request matches cache (same DataPath and Species)
        if isequal(CachedDataPath, Args.DataPath) && isequal(CachedSpecies, Args.Species)
            if Args.Verbose
                fprintf('Returning cached absorption interpolants (%d species)\n', length(fieldnames(CachedAbsData.Interpolants)));
            end
            AbsData = CachedAbsData;
            return;
        end
    end

    % Initialize structure to hold interpolants
    Abs_Interpolants = struct();

    if Args.Verbose
        fprintf('Loading interpolant functions for %d species...\n', length(Args.Species));
    end

    SuccessCount = 0;
    for i = 1:length(Args.Species)
        Species = Args.Species{i};
        try
            % Construct filename for individual species file
            FileName = sprintf('Abs_%s.mat', Species);
            FilePath = fullfile(Args.DataPath, FileName);

            % Check if file exists
            if ~exist(FilePath, 'file')
                if Args.Verbose
                    fprintf('  Failed  %-8s: file not found\n', Species);
                end
                continue;
            end

            % Load the interpolant with the species-specific variable name
            VarName = sprintf('Abs_%s', Species);
            LoadedData = load(FilePath, VarName);

            if ~isfield(LoadedData, VarName)
                if Args.Verbose
                    fprintf('  Failed  %-8s: variable %s not found in file\n', Species, VarName);
                end
                continue;
            end

            % Store the interpolant
            Abs_Interpolants.(Species) = LoadedData.(VarName);

            SuccessCount = SuccessCount + 1;

            if Args.Verbose
                fprintf('  Loaded %-8s: interpolant loaded\n', Species);
            end

        catch ME
            if Args.Verbose
                fprintf('  Failed  %-8s: %s\n', Species, ME.message);
            end
        end
    end

    if Args.Verbose
        fprintf('Successfully loaded %d/%d species\n', SuccessCount, length(Args.Species));
    end

    % Create output structure with getInterpolated method for compatibility
    AbsData = struct();

    % Add getInterpolated as a function handle that accesses the interpolants
    AbsData.getInterpolated = @(Species, Lambda) interpolateSpecies(Abs_Interpolants, Species, Lambda);

    % Add method to get full H2O coefficient structure for complex calculations
    AbsData.getH2OCoefficients = @(Lambda) getH2OAllCoefficients(Abs_Interpolants, Lambda);

    % Also store the interpolants directly for direct access if needed
    AbsData.Interpolants = Abs_Interpolants;

    % Cache the result for future calls
    CachedAbsData = AbsData;
    CachedDataPath = Args.DataPath;
    CachedSpecies = Args.Species;
end

function Values = interpolateSpecies(Interpolants, Species, Lambda)
    % Helper function to interpolate species data
    if ~isfield(Interpolants, Species)
        AvailableSpecies = fieldnames(Interpolants);
        error('Species "%s" not available. Available species: %s', ...
              Species, strjoin(AvailableSpecies, ', '));  % DEBUGGING
    end

    % Handle H2O compound interpolant structure
    if strcmp(Species, 'H2O') && isstruct(Interpolants.(Species))
        % For H2O, return the basic absorption coefficient by default
        % Full coefficient access available via direct structure access
        Values = Interpolants.(Species).absorption(Lambda);
    else
        % For other species, use standard single interpolant
        Values = Interpolants.(Species)(Lambda);
    end
end

function H2O_Data = getH2OAllCoefficients(Interpolants, Lambda)
    % Get all H2O coefficients interpolated to Lambda wavelengths
    % Input  : - Interpolants - Structure containing H2O compound interpolants
    %          - Lambda - Wavelength array for interpolation
    % Output : - H2O_Data - Structure with all interpolated coefficients

    if ~isfield(Interpolants, 'H2O')
        error('H2O interpolants not available');
    end

    if ~isstruct(Interpolants.H2O)
        error('H2O interpolants not in compound format. Regenerate interpolants with updated createAbsorptionInterpolants.');
    end

    H2O_Interp = Interpolants.H2O;
    H2O_Data = struct();

    % Interpolate all coefficient arrays to Lambda
    H2O_Data.wavelength = Lambda(:);  % Ensure column vector
    H2O_Data.absorption = H2O_Interp.absorption(Lambda);
    H2O_Data.band = H2O_Interp.band(Lambda);

    % Water vapor fitting coefficients
    H2O_Data.ifitw = H2O_Interp.ifitw(Lambda);
    H2O_Data.bwa0 = H2O_Interp.bwa0(Lambda);
    H2O_Data.bwa1 = H2O_Interp.bwa1(Lambda);
    H2O_Data.bwa2 = H2O_Interp.bwa2(Lambda);

    % Airmass fitting coefficients
    H2O_Data.ifitm = H2O_Interp.ifitm(Lambda);
    H2O_Data.bma0 = H2O_Interp.bma0(Lambda);
    H2O_Data.bma1 = H2O_Interp.bma1(Lambda);
    H2O_Data.bma2 = H2O_Interp.bma2(Lambda);

    % Combined water-airmass fitting coefficients
    H2O_Data.ifitmw = H2O_Interp.ifitmw(Lambda);
    H2O_Data.bmwa0 = H2O_Interp.bmwa0(Lambda);
    H2O_Data.bmwa1 = H2O_Interp.bmwa1(Lambda);
    H2O_Data.bmwa2 = H2O_Interp.bmwa2(Lambda);

    % Pressure fitting coefficients
    H2O_Data.bpa1 = H2O_Interp.bpa1(Lambda);
    H2O_Data.bpa2 = H2O_Interp.bpa2(Lambda);

    % Ensure all arrays are column vectors for consistency
    Fields = fieldnames(H2O_Data);
    for i = 1:length(Fields)
        if isnumeric(H2O_Data.(Fields{i}))
            H2O_Data.(Fields{i}) = H2O_Data.(Fields{i})(:);
        end
    end
end