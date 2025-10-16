function AbsData = loadAbsorptionInterpolants(Args)
    % Load pre-computed absorption interpolants from individual .mat files
    % Input  : * ...,key,val,...
    %            'DataPath' - Path to interpolant .mat files.
    %              Default is '/home/dana/matlab/data/transmission_fitter/'.
    %            'Species' - Cell array of species to load.
    %              Default is all species.
    %            'Verbose' - Display loading progress. Default is false.
    % Output : - AbsData - Structure with getInterpolated method for compatibility.
    %            Usage: AbsData.getInterpolated('O3UV', Lambda)
    % Author : D. Kovaleva (Oct 2025)
    % Example: (1) AbsData = astro.transmission.loadAbsorptionInterpolants();
    %          O3_values = AbsData.getInterpolated('O3UV', linspace(300, 400, 101)');
    %          (2) load('/home/dana/matlab/data/transmission_fitter/Abs_O3UV.mat', 'Abs_O3UV');
    %          Lambda = linspace(300, 400, 101)';
    %          Values = Abs_O3UV(Lambda);

    arguments
        Args.DataPath = "~/matlab/data/spec/Atmosphere/Transmission/";
        Args.Species  = {'O3UV', 'O3IR', 'H2O', 'CO2', 'CH4', 'N2O', 'CO', 'NO2', 'NO', 'NO3', 'SO2I', 'SO2U', 'NH3', 'HNO2', 'HNO3', 'CH2O', 'BrO', 'ClNO', 'O2', 'O4', 'N2'};
        Args.Verbose logical = false;
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

    % Also store the interpolants directly for direct access if needed
    AbsData.Interpolants = Abs_Interpolants;
end

function Values = interpolateSpecies(Interpolants, Species, Lambda)
    % Helper function to interpolate species data
    if ~isfield(Interpolants, Species)
        AvailableSpecies = fieldnames(Interpolants);
        error('Species "%s" not available. Available species: %s', ...
              Species, strjoin(AvailableSpecies, ', '));  % DEBUGGING
    end

    % Call the interpolant
    Values = Interpolants.(Species)(Lambda);
end