function createInstrumentalPolynomials(Args)
    % Create pre-computed polynomial fit function handles for instrumental transmission data
    % This function converts CSV files into efficient .mat files containing
    % polynomial function handles (2nd order) for each instrumental component
    %
    % Input  : * ...,key,val,...
    %            'DataPath' - Path to instrumental data CSV files.
    %              Default is '~/matlab/data/telescope/LAST/'.
    %            'OutputPath' - Path to save polynomial .mat files.
    %              Default is '~/matlab/data/spec/Telescope/LAST/'.
    %            'Components' - Cell array of components to process.
    %              Default is {'Mirror', 'Corrector'}.
    %            'PolynomialOrder' - Order of polynomial fit. Default is 2.
    %            'Verbose' - Display progress. Default is true.
    % Output : None (creates individual .mat files for each component).
    % Author : D. Kovaleva (Oct 2025)
    % Example: astro.transmission.createInstrumentalPolynomials('Verbose', true);
    %          astro.transmission.createInstrumentalPolynomials('Components', {'Mirror'});

    arguments
        Args.DataPath string = '~/matlab/data/telescope/LAST/';
        Args.OutputPath string = '~/matlab/data/spec/Telescope/LAST/';
        Args.Components cell = {'Mirror', 'Corrector'};
        Args.PolynomialOrder double = 2;
        Args.Verbose logical = true;
    end

    % Expand paths
    DataPath = char(Args.DataPath);
    if startsWith(DataPath, '~')
        DataPath = fullfile(getenv('HOME'), DataPath(3:end));
    end

    OutputPath = char(Args.OutputPath);
    if startsWith(OutputPath, '~')
        OutputPath = fullfile(getenv('HOME'), OutputPath(3:end));
    end

    % Create output directory if it doesn't exist
    if ~exist(OutputPath, 'dir')
        mkdir(OutputPath);
        if Args.Verbose
            fprintf('Created output directory: %s\n', OutputPath);
        end
    end

    % Define component file mappings
    ComponentFiles = containers.Map();
    ComponentFiles('Mirror') = 'StarBrightXLT_Mirror_Reflectivity.csv';
    ComponentFiles('Corrector') = 'StarBrightXLT_Corrector_Transmission.csv';

    % Component data types
    ComponentDataType = containers.Map();
    ComponentDataType('Mirror') = 'reflectance';
    ComponentDataType('Corrector') = 'transmission';

    if Args.Verbose
        fprintf('Creating instrumental polynomial fits for %d components...\n', length(Args.Components));
        fprintf('Data path: %s\n', DataPath);
        fprintf('Output path: %s\n', OutputPath);
        fprintf('Polynomial order: %d\n', Args.PolynomialOrder);
    end

    % Process each component
    SuccessCount = 0;
    for i = 1:length(Args.Components)
        Component = Args.Components{i};

        try
            if Args.Verbose
                fprintf('Processing %s...', Component);
            end

            % Check if component is known
            if ~ComponentFiles.isKey(Component)
                if Args.Verbose
                    fprintf(' SKIPPED (unknown component)\n');
                end
                continue;
            end

            FileName = ComponentFiles(Component);
            FilePath = fullfile(DataPath, FileName);

            % Check if file exists
            if ~exist(FilePath, 'file')
                if Args.Verbose
                    fprintf(' SKIPPED (file not found: %s)\n', FilePath);
                end
                continue;
            end

            % Read CSV data
            RawData = readmatrix(FilePath);

            if size(RawData, 2) < 2
                error('File has insufficient columns: %s', FilePath);
            end

            Wavelength = RawData(:, 1);  % nm
            DataValues = RawData(:, 2) / 100;  % Convert percentage to fraction (0-1)

            % Sort by wavelength if not already sorted
            if ~issorted(Wavelength)
                [Wavelength, SortIdx] = sort(Wavelength);
                DataValues = DataValues(SortIdx);
            end

            % Ensure column vectors
            Wavelength = Wavelength(:);
            DataValues = DataValues(:);

            % Validate data
            if any(isnan(Wavelength)) || any(isnan(DataValues))
                error('Data contains NaN values');
            end

            if any(Wavelength <= 0)
                error('Wavelength contains non-positive values');
            end

            % Fit polynomial (2nd order by default)
            PolyCoeffs = polyfit(Wavelength, DataValues, Args.PolynomialOrder);

            % Create function handle that can be called like an interpolant
            % This allows identical usage: func(Lambda) instead of polyval(coeffs, Lambda)
            PolyFunc = @(x) polyval(PolyCoeffs, x);

            % Calculate fit quality metrics
            FittedValues = polyval(PolyCoeffs, Wavelength);
            Residuals = DataValues - FittedValues;
            RMSE = sqrt(mean(Residuals.^2));
            MaxError = max(abs(Residuals));

            % Create output filename
            DataType = ComponentDataType(Component);
            OutputFile = fullfile(OutputPath, sprintf('%s_%s.mat', Component, DataType));

            % Create variable with component name (store the function handle)
            VarName = sprintf('%s_%s', Component, DataType);
            eval(sprintf('%s = PolyFunc;', VarName));

            % Also save coefficients for reference
            CoeffsVarName = sprintf('%s_%s_coeffs', Component, DataType);
            eval(sprintf('%s = PolyCoeffs;', CoeffsVarName));

            % Save function handle and coefficients
            save(OutputFile, VarName, CoeffsVarName, '-v7.3');

            SuccessCount = SuccessCount + 1;

            if Args.Verbose
                fprintf(' OK [%.1f-%.1f nm, %d points, RMSE=%.6f, MaxErr=%.6f]\n', ...
                        min(Wavelength), max(Wavelength), length(Wavelength), RMSE, MaxError);
            end

        catch ME
            if Args.Verbose
                fprintf(' ERROR: %s\n', ME.message);
            end
        end
    end

    if Args.Verbose
        fprintf('\nInstrumental polynomial fit creation completed!\n');
        fprintf('Successfully created %d/%d polynomial fits\n', SuccessCount, length(Args.Components));
        fprintf('Files saved to: %s\n', OutputPath);
        fprintf('\nUsage example:\n');
        fprintf('  %% Load function handles directly:\n');
        fprintf('  load(''%s/Mirror_reflectance.mat'', ''Mirror_reflectance'');\n', OutputPath);
        fprintf('  load(''%s/Corrector_transmission.mat'', ''Corrector_transmission'');\n', OutputPath);
        fprintf('  Lambda = linspace(300, 1100, 401)'';\n');
        fprintf('  Mirror_refl = Mirror_reflectance(Lambda);\n');
        fprintf('  Corrector_trans = Corrector_transmission(Lambda);\n');
    end
end
