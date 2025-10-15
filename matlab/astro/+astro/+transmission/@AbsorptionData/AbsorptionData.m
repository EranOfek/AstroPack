classdef AbsorptionData < handle
    % AbsorptionData - Smart absorption data manager with on-demand interpolation
    % This class provides high-performance access to atmospheric absorption data
    % with intelligent caching and on-demand interpolation for transmission calculations.
    % Usage:
    %   AbsData = AbsorptionData();
    %   O3_values = AbsData.getInterpolated('O3UV', Lambda);
    %   H2O_data = AbsData.getInterpolated('H2O', Lambda);

    properties (Access = private)
        RawData        % Structure containing pre-loaded absorption coefficients
        WavelengthGrid % Structure containing original wavelength grids
        InterpCache    % Cache for interpolated results
        DataPath       % Path to absorption data files
        AvailableSpecies % List of loaded species
    end

    properties (Access = public)
        Verbose = false  % Display loading and cache statistics
    end

    methods
        function obj = AbsorptionData(Args)
            % Constructor - loads all absorption data
            % Input: Args.DataPath - Path to absorption data directory
            %        Args.Species - Cell array of species to load (optional)
            %        Args.Verbose - Display progress information

            arguments
                Args.DataPath string = fullfile('+astro', 'Data', 'Absorption');
                Args.Species cell = {'O3UV', 'H2O', 'CO2', 'CH4', 'N2O', 'O2', 'O4', 'NO2', 'BrO', 'CH2O', 'HNO2'};
                Args.Verbose = false;
            end

            obj.DataPath = '~/matlab/data/spec/Atmosphere/Transmission/'; % fullfile('+astro', 'Data', 'Absorption');  % Fixed location
            obj.Verbose = Args.Verbose;
            obj.InterpCache = containers.Map();
            obj.RawData = struct();
            obj.WavelengthGrid = struct();
            obj.AvailableSpecies = {};

            % Load all specified absorption data
            obj.loadAllData(Args.Species);
        end

        function Values = getInterpolated(obj, Species, Lambda, Args)
            % Get interpolated absorption values for specified wavelengths
            %
            % Input:
            %   Species - Species name (string): 'O3UV', 'H2O', etc.
            %   Lambda - Target wavelength array (nm)
            %   Args.Method - Interpolation method (default: 'linear')
            %   Args.ExtrapolationValue - Value for out-of-range wavelengths (default: 0)
            %
            % Output:
            %   Values - Interpolated absorption values

            arguments
                obj
                Species string
                Lambda (:,1) double
                Args.Method string = 'linear';
                Args.ExtrapolationValue (1,1) double = 0;
            end

            % Validate species
            if ~ismember(Species, obj.AvailableSpecies)
                error('Species "%s" not available. Available species: %s', ...
                      Species, strjoin(obj.AvailableSpecies, ', '));
            end

            % Generate cache key based on wavelength grid and species
            WaveKey = obj.computeWavelengthKey(Lambda);
            CacheKey = sprintf('%s_%s_%s_%.0f', Species, WaveKey, Args.Method, Args.ExtrapolationValue);

            % Check cache first
            if obj.InterpCache.isKey(CacheKey)
                Values = obj.InterpCache(CacheKey);
                if obj.Verbose
                    fprintf('Cache hit for %s interpolation\n', Species);
                end
                return;
            end

            % Perform interpolation
            if obj.Verbose
                fprintf('Interpolating %s data for %d wavelength points...\n', ...
                        Species, length(Lambda));
            end

            OriginalWave = obj.WavelengthGrid.(Species);
            OriginalData = obj.RawData.(Species);

            % Handle different data structures
            if isstruct(OriginalData)
                % Complex data with multiple fields (like H2O)
                ComplexValues = obj.interpolateComplexData(OriginalData, OriginalWave, Lambda, Args);
                % Return only the absorption field for transmission calculations
                Values = ComplexValues.absorption;
            else
                % Simple array data
                Values = interp1(OriginalWave, OriginalData, Lambda, ...
                               Args.Method, Args.ExtrapolationValue);
            end

            % Cache the result
            obj.InterpCache(CacheKey) = Values;

            if obj.Verbose
                fprintf('Cached %s interpolation result\n', Species);
            end
        end

        function Species = getAvailableSpecies(obj)
            % Get list of available absorption species
            Species = obj.AvailableSpecies;
        end

        function clearCache(obj)
            % Clear interpolation cache
            obj.InterpCache = containers.Map();
            if obj.Verbose
                fprintf('Interpolation cache cleared\n');
            end
        end

        function Stats = getCacheStats(obj)
            % Get cache statistics
            Stats.NumEntries = obj.InterpCache.Count;
            Stats.Keys = keys(obj.InterpCache);

            if obj.Verbose
                fprintf('Cache contains %d interpolated datasets\n', Stats.NumEntries);
            end
        end

        function Info = getDataInfo(obj, Species)
            % Get information about loaded absorption data
            %
            % Input:
            %   Species - Species name (optional, returns info for all if empty)
            %
            % Output:
            %   Info - Structure with data information

            arguments
                obj
                Species string = "";
            end

            if Species == ""
                % Return info for all species
                Info = struct();
                for i = 1:length(obj.AvailableSpecies)
                    sp = obj.AvailableSpecies{i};
                    Info.(sp) = obj.getSingleSpeciesInfo(sp);
                end
            else
                % Return info for specific species
                if ~ismember(Species, obj.AvailableSpecies)
                    error('Species "%s" not available', Species);
                end
                Info = obj.getSingleSpeciesInfo(Species);
            end
        end
    end

    methods (Access = private)
        function loadAllData(obj, SpeciesList)
            % Load absorption data for all specified species

            if obj.Verbose
                fprintf('Loading absorption data for %d species...\n', length(SpeciesList));
            end

            for i = 1:length(SpeciesList)
                Species = SpeciesList{i};
                obj.loadSingleSpecies(Species);
                obj.AvailableSpecies{end+1} = Species;

                if obj.Verbose
                    WaveInfo = obj.WavelengthGrid.(Species);
                    fprintf('  Loaded %s: %.1f-%.1f nm (%d points)\n', ...
                            Species, min(WaveInfo), max(WaveInfo), length(WaveInfo));
                end
            end

            if obj.Verbose
                fprintf('Loaded %d species\n', length(obj.AvailableSpecies));
            end
        end

        function loadSingleSpecies(obj, Species)
            % Load absorption data for a single species

            % Define file mapping (from loadAbsorptionData.m)
            FileMapping = containers.Map();
            FileMapping('O3UV') = 'Abs_O3UV.dat';
            FileMapping('O3IR') = 'Abs_O3IR.dat';
            FileMapping('H2O') = 'Abs_H2O.dat';
            FileMapping('CO2') = 'Abs_CO2.dat';
            FileMapping('CH4') = 'Abs_CH4.dat';
            FileMapping('N2O') = 'Abs_N2O.dat';
            FileMapping('CO') = 'Abs_CO.dat';
            FileMapping('NO2') = 'Abs_NO2.dat';
            FileMapping('NO') = 'Abs_NO.dat';
            FileMapping('NO3') = 'Abs_NO3.dat';
            FileMapping('SO2I') = 'Abs_SO2I.dat';
            FileMapping('SO2U') = 'Abs_SO2U.dat';
            FileMapping('NH3') = 'Abs_NH3.dat';
            FileMapping('HNO2') = 'Abs_HNO2.dat';
            FileMapping('HNO3') = 'Abs_HNO3.dat';
            FileMapping('CH2O') = 'Abs_CH2O.dat';
            FileMapping('BrO') = 'Abs_BrO.dat';
            FileMapping('ClNO') = 'Abs_ClNO.dat';
            FileMapping('O2') = 'Abs_O2.dat';
            FileMapping('O4') = 'Abs_O4.dat';
            FileMapping('N2') = 'Abs_N2.dat';

            % Get the correct filename
            if ~FileMapping.isKey(Species)
                error('Unknown species: %s', Species);
            end

            FileName = FileMapping(Species);
            FilePath = fullfile(obj.DataPath, FileName);

            % Load the data - no error handling for missing files
            Data = readmatrix(FilePath);

            % First column is wavelength, rest is absorption data
            obj.WavelengthGrid.(Species) = Data(:, 1);

            if size(Data, 2) == 2
                % Simple absorption data
                obj.RawData.(Species) = Data(:, 2);
            else
                % Complex data with multiple columns
                obj.RawData.(Species) = obj.parseComplexData(Data, Species);
            end
        end

        function ComplexData = parseComplexData(obj, Data, Species)
            % Parse complex absorption data (like H2O with multiple columns)

            if strcmp(Species, 'H2O')
                % H2O has specific format: wavelength, absorption, band, coefficients
                ComplexData = struct();
                ComplexData.absorption = Data(:, 2);
                ComplexData.band = Data(:, 3);

                % Additional columns for fitting coefficients if present
                if size(Data, 2) > 3
                    ComplexData.coefficients = Data(:, 4:end);
                end
            else
                % Generic multi-column format
                ComplexData = struct();
                ComplexData.absorption = Data(:, 2);
                if size(Data, 2) > 2
                    ComplexData.additional = Data(:, 3:end);
                end
            end
        end

        function Values = interpolateComplexData(obj, OriginalData, OriginalWave, Lambda, Args)
            % Interpolate complex multi-field data structures

            Values = struct();

            % Interpolate each field
            Fields = fieldnames(OriginalData);
            for i = 1:length(Fields)
                Field = Fields{i};
                FieldData = OriginalData.(Field);

                if isvector(FieldData)
                    % Simple vector interpolation
                    Values.(Field) = interp1(OriginalWave, FieldData, Lambda, ...
                                           Args.Method, Args.ExtrapolationValue);
                else
                    % Matrix interpolation (interpolate each column)
                    Values.(Field) = zeros(length(Lambda), size(FieldData, 2));
                    for j = 1:size(FieldData, 2)
                        Values.(Field)(:, j) = interp1(OriginalWave, FieldData(:, j), Lambda, ...
                                                      Args.Method, Args.ExtrapolationValue);
                    end
                end
            end
        end

        function Key = computeWavelengthKey(obj, Lambda)
            % Compute unique key for wavelength array
            % Uses hash of wavelength characteristics for fast cache lookup

            if length(Lambda) < 50
                % For short arrays, use direct hash
                Key = sprintf('%.6f', sum(Lambda) + length(Lambda) + Lambda(1) + Lambda(end));
            else
                % For long arrays, use statistical hash
                Key = sprintf('%.6f_%.6f_%.6f_%d', ...
                             min(Lambda), max(Lambda), mean(Lambda), length(Lambda));
            end
        end

        function Info = getSingleSpeciesInfo(obj, Species)
            % Get information about a single species

            Info = struct();
            Info.species = Species;
            Info.wavelengthRange = [min(obj.WavelengthGrid.(Species)), ...
                                   max(obj.WavelengthGrid.(Species))];
            Info.numWavelengthPoints = length(obj.WavelengthGrid.(Species));
            Info.dataType = class(obj.RawData.(Species));

            if isstruct(obj.RawData.(Species))
                Info.fields = fieldnames(obj.RawData.(Species));
            else
                Info.dataSize = size(obj.RawData.(Species));
            end
        end
    end
end