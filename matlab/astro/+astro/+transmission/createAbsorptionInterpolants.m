function createAbsorptionInterpolants(Args)
    % Create pre-computed griddedInterpolant objects for absorption data
    % This function converts the existing .dat files into efficient .mat files
    % containing individual interpolant functions for each species
    %
    % Input  : * ...,key,val,...
    %            'DataPath' - Path to absorption data directory.
    %              Default is '/home/dana/matlab/data/transmission_fitter/'.
    %            'OutputPath' - Path to save interpolant .mat files.
    %              Default is same as DataPath.
    %            'Species' - Cell array of species to process.
    %              Default is all available species.
    %            'Verbose' - Display progress. Default is true.
    % Output : None (creates individual .mat files for each species).
    % Author : D. Kovaleva (Oct 2025)
    % Example: astro.transmission.createAbsorptionInterpolants('Verbose', true);

    arguments
        Args.DataPath string = '~/matlab/data/transmission_fitter/';
        Args.OutputPath string = '~/matlab/data/spec/Atmosphere/Transmission/';
        Args.Species cell = {}; % Empty means all available
        Args.Verbose logical = true;
    end

    % Set default output path
    if isempty(Args.OutputPath) || strlength(Args.OutputPath) == 0
        Args.OutputPath = Args.DataPath;
    end

    % Define all available species and their file mappings
    SpeciesFiles = containers.Map();
    SpeciesFiles('O3UV') = 'Abs_O3UV.dat';
    SpeciesFiles('O3IR') = 'Abs_O3IR.dat';
    SpeciesFiles('H2O') = 'Abs_H2O.dat';
    SpeciesFiles('CO2') = 'Abs_CO2.dat';
    SpeciesFiles('CH4') = 'Abs_CH4.dat';
    SpeciesFiles('N2O') = 'Abs_N2O.dat';
    SpeciesFiles('CO') = 'Abs_CO.dat';
    SpeciesFiles('NO2') = 'Abs_NO2.dat';
    SpeciesFiles('NO') = 'Abs_NO.dat';
    SpeciesFiles('NO3') = 'Abs_NO3.dat';
    SpeciesFiles('SO2I') = 'Abs_SO2I.dat';
    SpeciesFiles('SO2U') = 'Abs_SO2U.dat';
    SpeciesFiles('NH3') = 'Abs_NH3.dat';
    SpeciesFiles('HNO2') = 'Abs_HNO2.dat';
    SpeciesFiles('HNO3') = 'Abs_HNO3.dat';
    SpeciesFiles('CH2O') = 'Abs_CH2O.dat';
    SpeciesFiles('BrO') = 'Abs_BrO.dat';
    SpeciesFiles('ClNO') = 'Abs_ClNO.dat';
    SpeciesFiles('O2') = 'Abs_O2.dat';
    SpeciesFiles('O4') = 'Abs_O4.dat';
    SpeciesFiles('N2') = 'Abs_N2.dat';

    % Determine which species to process
    if isempty(Args.Species)
        SpeciesToProcess = keys(SpeciesFiles);
    else
        SpeciesToProcess = Args.Species;
    end

    if Args.Verbose
        fprintf('Creating interpolant objects for %d species...\n', length(SpeciesToProcess));
        fprintf('Data path: %s\n', Args.DataPath);
        fprintf('Output path: %s\n', Args.OutputPath);
    end

    % Process each species
    SuccessCount = 0;
    for i = 1:length(SpeciesToProcess)
        Species = SpeciesToProcess{i};

        try
            if Args.Verbose
                fprintf('Processing %s...', Species);
            end

            % Check if species is known
            if ~SpeciesFiles.isKey(Species)
                if Args.Verbose
                    fprintf(' SKIPPED (unknown species)\n');
                end
                continue;
            end

            FileName = SpeciesFiles(Species);
            FilePath = fullfile(Args.DataPath, FileName);

            % Check if file exists
            if ~exist(FilePath, 'file')
                if Args.Verbose
                    fprintf(' SKIPPED (file not found)\n');
                end
                continue;
            end

            % Load and parse data based on species type
            if strcmp(Species, 'H2O')
                % Special handling for H2O - get all coefficient data
                [Wavelength, H2O_AllData] = parseSpeciesData(FilePath, Species);

                % Create compound interpolant structure with all coefficients
                H2O_Interpolants = struct();

                % Basic absorption (linear interpolation)
                H2O_Interpolants.absorption = griddedInterpolant(Wavelength, H2O_AllData.absorption, 'linear', 'nearest');

                % Band information (nearest neighbor - integer values)
                H2O_Interpolants.band = griddedInterpolant(Wavelength, H2O_AllData.band, 'nearest', 'nearest');

                % Water vapor fitting coefficients
                H2O_Interpolants.ifitw = griddedInterpolant(Wavelength, H2O_AllData.ifitw, 'nearest', 'nearest');  % Integer
                H2O_Interpolants.bwa0 = griddedInterpolant(Wavelength, H2O_AllData.bwa0, 'linear', 'nearest');
                H2O_Interpolants.bwa1 = griddedInterpolant(Wavelength, H2O_AllData.bwa1, 'linear', 'nearest');
                H2O_Interpolants.bwa2 = griddedInterpolant(Wavelength, H2O_AllData.bwa2, 'linear', 'nearest');

                % Airmass fitting coefficients
                H2O_Interpolants.ifitm = griddedInterpolant(Wavelength, H2O_AllData.ifitm, 'nearest', 'nearest');  % Integer
                H2O_Interpolants.bma0 = griddedInterpolant(Wavelength, H2O_AllData.bma0, 'linear', 'nearest');
                H2O_Interpolants.bma1 = griddedInterpolant(Wavelength, H2O_AllData.bma1, 'linear', 'nearest');
                H2O_Interpolants.bma2 = griddedInterpolant(Wavelength, H2O_AllData.bma2, 'linear', 'nearest');

                % Combined water-airmass fitting coefficients
                H2O_Interpolants.ifitmw = griddedInterpolant(Wavelength, H2O_AllData.ifitmw, 'nearest', 'nearest'); % Integer
                H2O_Interpolants.bmwa0 = griddedInterpolant(Wavelength, H2O_AllData.bmwa0, 'linear', 'nearest');
                H2O_Interpolants.bmwa1 = griddedInterpolant(Wavelength, H2O_AllData.bmwa1, 'linear', 'nearest');
                H2O_Interpolants.bmwa2 = griddedInterpolant(Wavelength, H2O_AllData.bmwa2, 'linear', 'nearest');

                % Pressure fitting coefficients
                H2O_Interpolants.bpa1 = griddedInterpolant(Wavelength, H2O_AllData.bpa1, 'linear', 'nearest');
                H2O_Interpolants.bpa2 = griddedInterpolant(Wavelength, H2O_AllData.bpa2, 'linear', 'nearest');

                % Store the compound interpolant as the main interpolant
                Interpolant = H2O_Interpolants;
            else
                % Standard handling for all non-H2O species
                [Wavelength, RawData] = parseSpeciesData(FilePath, Species);

                % Apply species-specific corrections (fixed temperature corrections for trace gases)
                CorrectedAbsorption = applyAbsorptionCorrections(RawData, Species);

                % Create standard interpolant function with corrected data
                Interpolant = griddedInterpolant(Wavelength, CorrectedAbsorption, 'linear', 'nearest');
            end

            % Create output filename with species prefix
            OutputFile = fullfile(Args.OutputPath, sprintf('Abs_%s.mat', Species));

            % For H2O, create the compound interpolant variable
            if strcmp(Species, 'H2O')
                eval(sprintf('Abs_%s = H2O_Interpolants;', Species));
            else
                % For other species, use standard single interpolant
                eval(sprintf('Abs_%s = Interpolant;', Species));
            end

            % Save just the interpolant function with the correct variable name
            save(OutputFile, sprintf('Abs_%s', Species), '-v7.3');

            SuccessCount = SuccessCount + 1;

            if Args.Verbose
                fprintf(' OK [%.1f-%.1f nm, %d points]\n', ...
                        min(Wavelength), max(Wavelength), length(Wavelength));
            end

        catch ME
            if Args.Verbose
                fprintf(' ERROR: %s\n', ME.message);
            end
        end
    end

    if Args.Verbose
        fprintf('\nInterpolant creation completed!\n');
        fprintf('Successfully created %d/%d interpolants\n', SuccessCount, length(SpeciesToProcess));
        fprintf('Files saved to: %s\n', Args.OutputPath);
        fprintf('\nUsage example:\n');
        fprintf('  load(''%s/Abs_O3UV.mat'', ''Abs_O3UV'');\n', Args.OutputPath);
        fprintf('  Lambda = linspace(300, 400, 101)'';\n');
        fprintf('  Values = Abs_O3UV(Lambda);\n');
    end
end

function [Wavelength, H2O_Data] = parseSpeciesData(FilePath, Species)
    % Parse absorption data based on species-specific format
    % For H2O, returns structure with all coefficients; for others, returns just absorption

    switch Species
        case 'SO2I'
            % SO2I wavelengths are in Angstroms, need conversion to nm
            RawData = readmatrix(FilePath, 'FileType', 'text', 'Delimiter', '\t', 'NumHeaderLines', 1);
            Wavelength = RawData(:, 1) / 10;  % Convert Angstroms to nm (3955Å -> 395.5nm)
            H2O_Data = RawData(:, 2);          % Absorption coefficient

        case 'H2O'
            % Water vapor has complex multi-column format - preserve all coefficients
            RawMatrix = readmatrix(FilePath, 'FileType', 'text', 'Delimiter', '\t', 'NumHeaderLines', 1);
            KeepColumns = [1:3, 5:8, 10:13, 15:18, 20:21]; % Skip columns 4, 9, 14, 19, 22
            RawDataClean = RawMatrix(:, KeepColumns);

            % Extract all coefficient data
            Wavelength = RawDataClean(:, 1);        % Wavelength (nm)

            % Create structure to hold all H2O data
            H2O_Data = struct();
            H2O_Data.absorption = RawDataClean(:, 2);        % Main absorption coefficient
            H2O_Data.band = RawDataClean(:, 3);              % Band information

            % Water vapor fitting coefficients (Bw calculation)
            H2O_Data.ifitw = RawDataClean(:, 4);             % Water fitting type
            H2O_Data.bwa0 = RawDataClean(:, 5);              % Water coefficient 0
            H2O_Data.bwa1 = RawDataClean(:, 6);              % Water coefficient 1
            H2O_Data.bwa2 = RawDataClean(:, 7);              % Water coefficient 2

            % Airmass fitting coefficients (Bm calculation)
            H2O_Data.ifitm = RawDataClean(:, 8);             % Airmass fitting type
            H2O_Data.bma0 = RawDataClean(:, 9);              % Airmass coefficient 0
            H2O_Data.bma1 = RawDataClean(:, 10);             % Airmass coefficient 1
            H2O_Data.bma2 = RawDataClean(:, 11);             % Airmass coefficient 2

            % Combined water-airmass fitting coefficients (Bmw calculation)
            H2O_Data.ifitmw = RawDataClean(:, 12);           % Combined fitting type
            H2O_Data.bmwa0 = RawDataClean(:, 13);            % Combined coefficient 0
            H2O_Data.bmwa1 = RawDataClean(:, 14);            % Combined coefficient 1
            H2O_Data.bmwa2 = RawDataClean(:, 15);            % Combined coefficient 2

            % Pressure fitting coefficients (Bp calculation)
            H2O_Data.bpa1 = RawDataClean(:, 16);             % Pressure coefficient 1
            H2O_Data.bpa2 = RawDataClean(:, 17);             % Pressure coefficient 2

        case {'O3UV', 'O3IR'}
            % Ozone data (UV and IR bands)
            RawData = readtable(FilePath, 'Delimiter', '\t', 'ReadVariableNames', false);
            Wavelength = RawData.Var1;     % nm
            H2O_Data = RawData.Var2;       % absorption cross-section (renamed for consistency)

        otherwise
            % Generic molecular absorption data with multi-column support
            RawData = readmatrix(FilePath, 'FileType', 'text', 'Delimiter', '\t', 'NumHeaderLines', 1);
            Wavelength = RawData(:, 1);     % nm

            if size(RawData, 2) == 1
                error('File has only one column: %s', FilePath);
            elseif size(RawData, 2) == 2
                % Single absorption column
                H2O_Data = RawData(:, 2);  % For non-H2O species, return absorption directly
            else
                % Multi-column data - preserve temperature coefficients
                % Check which species need special handling
                TraceGasesWithTempCorrection = {'HNO3', 'NO2', 'NO3', 'SO2U', 'SO2I', 'CH2O', 'ClNO', 'HNO2', 'NH3', 'BrO'};

                if ismember(Species, TraceGasesWithTempCorrection)
                    % Return structure with all coefficients
                    H2O_Data = struct();

                    % Common pattern: column 2 is base absorption, column 3+ are temperature coefficients
                    if size(RawData, 2) >= 2
                        H2O_Data.absorption = RawData(:, 2);  % Base absorption coefficient
                    end

                    if size(RawData, 2) >= 3
                        H2O_Data.b0 = RawData(:, 3);  % Temperature coefficient b0
                    end

                    if size(RawData, 2) >= 4
                        H2O_Data.b1 = RawData(:, 4);  % Additional coefficient b1 (for ClNO)
                    end

                    % Store number of columns for reference
                    H2O_Data.num_columns = size(RawData, 2) - 1;  % Excluding wavelength column
                else
                    % For other species, just use first absorption column
                    H2O_Data = RawData(:, 2);
                end
            end
    end

    % Ensure column vectors and validate data
    Wavelength = Wavelength(:);

    if isstruct(H2O_Data)
        % For structures (H2O and multi-column trace gases), ensure all arrays are column vectors
        FieldNames = fieldnames(H2O_Data);
        for i = 1:length(FieldNames)
            if isnumeric(H2O_Data.(FieldNames{i}))
                H2O_Data.(FieldNames{i}) = H2O_Data.(FieldNames{i})(:);
            end
        end

        % Basic validation
        if isfield(H2O_Data, 'absorption')
            if length(Wavelength) ~= length(H2O_Data.absorption)
                error('Wavelength and absorption arrays have different lengths');
            end

            if any(isnan(Wavelength)) || any(isnan(H2O_Data.absorption))
                error('Data contains NaN values');
            end
        end

        % Sort by wavelength if not already sorted
        if ~issorted(Wavelength)
            [Wavelength, SortIdx] = sort(Wavelength);
            % Sort all numeric coefficient arrays
            for i = 1:length(FieldNames)
                if isnumeric(H2O_Data.(FieldNames{i})) && length(H2O_Data.(FieldNames{i})) == length(SortIdx)
                    H2O_Data.(FieldNames{i}) = H2O_Data.(FieldNames{i})(SortIdx);
                end
            end
        end
    else
        % For other species, H2O_Data contains just the absorption values
        H2O_Data = H2O_Data(:);

        % Basic validation
        if length(Wavelength) ~= length(H2O_Data)
            error('Wavelength and absorption arrays have different lengths');
        end

        if any(isnan(Wavelength)) || any(isnan(H2O_Data))
            error('Data contains NaN values');
        end

        % Sort by wavelength if not already sorted
        if ~issorted(Wavelength)
            [Wavelength, SortIdx] = sort(Wavelength);
            H2O_Data = H2O_Data(SortIdx);
        end
    end

    if any(Wavelength <= 0)
        error('Wavelength contains non-positive values');
    end

    % Add boundary zeros for extrapolation (all species except H2O)
    % This ensures griddedInterpolant with 'nearest' extrapolation returns 0 outside data range
    if ~strcmp(Species, 'H2O')
        if isstruct(H2O_Data)
            % For structures (trace gases with temperature coefficients)
            % Add zero values at boundaries for all numeric fields
            Wavelength = [Wavelength(1) - 0.1; Wavelength; Wavelength(end) + 0.1];

            FieldNames = fieldnames(H2O_Data);
            for i = 1:length(FieldNames)
                if isnumeric(H2O_Data.(FieldNames{i})) && length(H2O_Data.(FieldNames{i})) > 1
                    H2O_Data.(FieldNames{i}) = [0; H2O_Data.(FieldNames{i}); 0];
                end
            end
        else
            % For simple arrays (most gases)
            Wavelength = [Wavelength(1) - 0.1; Wavelength; Wavelength(end) + 0.1];
            H2O_Data = [0; H2O_Data; 0];
        end
    end
end

function CorrectedAbsorption = applyAbsorptionCorrections(RawData, Species)
    % Apply species-specific absorption coefficient corrections with fixed reference temperatures
    % Based on SMARTS 2.9.5 model corrections
    % Input  : - RawData - Either absorption vector or structure with {absorption, b0, b1}
    %          - Species - Species name
    % Output : - CorrectedAbsorption - Corrected absorption coefficients (column vector)

    switch Species
        case 'O4'
            % O4 needs NO correction here - temperature-dependent scaling applied in umgTransmission
            % The 1e-46 scaling is also applied in umgTransmission at runtime
            if isstruct(RawData)
                CorrectedAbsorption = RawData.absorption;
            else
                CorrectedAbsorption = RawData;
            end

        case 'NO2'
            % NO2 temperature correction: sigma + b0*(T-T0)
            if isstruct(RawData) && isfield(RawData, 'b0')
                Sigma = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 228.7;  % K
                T0 = 220.0;     % K reference
                CorrectedAbsorption = constant.Loschmidt * (Sigma + B0 * (T_ref - T0));
            else
                % Single column - apply Loschmidt scaling only
                if isstruct(RawData)
                    CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = constant.Loschmidt * RawData;
                end
            end

        case 'SO2U'
            % SO2U temperature correction: sigma + b0*(T-T0)
            if isstruct(RawData) && isfield(RawData, 'b0')
                Sigma = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 247.0;  % K
                T0 = 213.0;     % K reference
                CorrectedAbsorption = constant.Loschmidt * (Sigma + B0 * (T_ref - T0));
            else
                if isstruct(RawData)
                    CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = constant.Loschmidt * RawData;
                end
            end

        case 'HNO3'
            % HNO3 exponential temperature correction: 1e-20 * xs * exp(1e-3 * b0 * (T-T0))
            if isstruct(RawData) && isfield(RawData, 'b0')
                Xs = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 234.2;  % K
                T0 = 298.0;     % K reference
                CorrectedAbsorption = 1e-20 * constant.Loschmidt * Xs .* exp(1e-3 * B0 * (T_ref - T0));
            else
                if isstruct(RawData)
                    CorrectedAbsorption = 1e-20 * constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = 1e-20 * constant.Loschmidt * RawData;
                end
            end

        case 'NO3'
            % NO3 temperature correction: xs + b0*(T-T0)
            if isstruct(RawData) && isfield(RawData, 'b0')
                Xs = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 225.3;  % K
                T0 = 230.0;     % K reference
                CorrectedAbsorption = constant.Loschmidt * (Xs + B0 * (T_ref - T0));
            else
                if isstruct(RawData)
                    CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = constant.Loschmidt * RawData;
                end
            end

        case 'HNO2'
            % HNO2 Loschmidt scaling
            if isstruct(RawData)
                CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
            else
                CorrectedAbsorption = constant.Loschmidt * RawData;
            end

        case 'CH2O'
            % CH2O temperature correction: xs + b0*(T-T0)
            if isstruct(RawData) && isfield(RawData, 'b0')
                Xs = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 264.0;  % K
                T0 = 293.0;     % K reference
                CorrectedAbsorption = constant.Loschmidt * (Xs + B0 * (T_ref - T0));
            else
                if isstruct(RawData)
                    CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = constant.Loschmidt * RawData;
                end
            end

        case 'BrO'
            % BrO Loschmidt scaling
            if isstruct(RawData)
                CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
            else
                CorrectedAbsorption = constant.Loschmidt * RawData;
            end

        case 'ClNO'
            % ClNO quadratic temperature correction: xs*(1+b0*(T-T0)+b1*(T-T0)^2)
            if isstruct(RawData) && isfield(RawData, 'b0')
                Xs = RawData.absorption;
                B0 = RawData.b0;
                T_ref = 230.0;  % K
                T0 = 296.0;     % K reference
                DT = T_ref - T0;

                if isfield(RawData, 'b1')
                    B1 = RawData.b1;
                    CorrectedAbsorption = constant.Loschmidt * Xs .* (1 + B0 * DT + B1 * (DT^2));
                else
                    % Linear correction only
                    CorrectedAbsorption = constant.Loschmidt * Xs .* (1 + B0 * DT);
                end
            else
                if isstruct(RawData)
                    CorrectedAbsorption = constant.Loschmidt * RawData.absorption;
                else
                    CorrectedAbsorption = constant.Loschmidt * RawData;
                end
            end

        otherwise
            % No corrections needed for: O2, N2, CO, CO2, CH4, N2O, NH3, O3UV, O3IR
            % (Ozone Loschmidt scaling applied at runtime in ozoneTransmission.m)
            if isstruct(RawData)
                CorrectedAbsorption = RawData.absorption;
            else
                CorrectedAbsorption = RawData;
            end
    end

    % Ensure absorption is a column vector
    CorrectedAbsorption = CorrectedAbsorption(:);
end