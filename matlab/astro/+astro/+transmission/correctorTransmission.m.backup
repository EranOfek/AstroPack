function Trans_corrector = correctorTransmission(Lam, Config)
    % Calculate corrector transmission from data file using
    % (a) polynomial fitting of instrumental data or (b) piecewise linear
    % interpolation of instrumental data
    % Input :  - Lam (double array): Wavelength array in nm
    %          - Config (struct): Configuration struct from inputConfig()
    %            Uses Config.Instrumental.Components.Corrector.Data_file
    %            Uses Config.Instrumental.Components.Corrector.Method
    % Output : - Trans_corrector (double array): Corrector transmission values (0-1)
    % Author : D. Kovaleva (Sep 2025)
    % Reference:  Garrappa et al. 2025, A&A 699, A50.
    % Example: Config = transmissionFast.inputConfig('default');
    %          Lam = transmissionFast.utils.makeWavelengthArray(Config);
    %          Trans = transmissionFast.instrumental.correctorTransmission(Lam, Config);
    %          % Use piecewise method
    %          Config.Instrumental.Components.Corrector.Method = 'piecewise_';
    %          Trans = transmissionFast.instrumental.correctorTransmission(Lam, Config);
    
    arguments
        Lam = []  % Will use cached wavelength array from Config if empty
        Config = transmissionFast.inputConfig()
    end
    
    % Use cached wavelength array if Lam not provided
    if isempty(Lam)
        if isfield(Config, 'WavelengthArray') && ~isempty(Config.WavelengthArray)
            Lam = Config.WavelengthArray;
        else
            % Fallback to calculation if cached array not available
            Lam = transmissionFast.utils.makeWavelengthArray(Config);
        end
    end
    
    % Persistent variables for caching computed corrector transmission
    persistent correctorCache lastMethod lastDataFile lastLam
    
    % Extract parameters from Config
    Method = Config.Instrumental.Components.Corrector.Method;
    DataFile = Config.Instrumental.Components.Corrector.Data_file;
    
    % Check if we can use persistent cached result
    if ~isempty(correctorCache) && isequal(lastLam, Lam) && strcmp(lastMethod, Method) && strcmp(lastDataFile, DataFile)
        Trans_corrector = correctorCache;
        return;
    end
    
    % Need to calculate - not in persistent cache or cache invalid
    % ALWAYS try to use cached data from inputConfig first (avoids file I/O)
    if isfield(Config, 'InstrumentalData') && isfield(Config.InstrumentalData, 'Corrector')
        correctorData = Config.InstrumentalData.Corrector;
        
        % Check if data loaded successfully
        if isfield(correctorData, 'error')
            error('transmission:correctorTransmission:cachedDataError', ...
                  'Cached corrector data has error: %s', correctorData.error);
        end
        
        if isempty(correctorData.wavelength) || isempty(correctorData.transmission)
            error('transmission:correctorTransmission:emptyCachedData', ...
                  'Cached corrector data is empty');
        end
        
        Corrector_wavelength = correctorData.wavelength;
        Corrector_transmission = correctorData.transmission;  % Already converted to fraction in cache
        
        % Adjust transmission function to wavelength array
        if strcmp(Method, 'polyfit_')
            Ci = polyfit(Corrector_wavelength, Corrector_transmission, 2);
            Trans_corrector = polyval(Ci, Lam);
        elseif strcmp(Method, 'piecewise_')
            Trans_corrector = interp1(Corrector_wavelength, Corrector_transmission, Lam, 'linear', 'extrap');
        else
            error('transmission:correctorTransmission:invalidMethod', ...
                  'Invalid method: %s. Use ''polyfit_'' or ''piecewise_''', Method);
        end
        
    else
        % ONLY use file I/O as absolute fallback if cached data not available
        Data_file = Config.Instrumental.Components.Corrector.Data_file;
        
        warning('transmission:correctorTransmission:usingFileIO', ...
                'Using file I/O fallback - cached data not available in inputConfig');
        
        if exist(Data_file, 'file')
            % Read corrector transmission data
            Data = readmatrix(Data_file);
            Corrector_wavelength = Data(:, 1);
            Corrector_transmission = Data(:, 2) / 100;  % Convert percentage to fraction
            
            % Adjust transmission function to wavelength array
            if strcmp(Method, 'polyfit_')
                Ci = polyfit(Corrector_wavelength, Corrector_transmission, 2);
                Trans_corrector = polyval(Ci, Lam);
            elseif strcmp(Method, 'piecewise_')
                Trans_corrector = interp1(Corrector_wavelength, Corrector_transmission, Lam, 'linear', 'extrap');
            else
                error('transmission:correctorTransmission:invalidMethod', ...
                      'Invalid method: %s. Use ''polyfit_'' or ''piecewise_''', Method);
            end    
        else
            error('transmission:correctorTransmission:fileNotFound', ...
                  'Corrector transmission data file not found: %s', Data_file);
        end
    end
    
    % Cache the result for future calls in persistent variables
    correctorCache = Trans_corrector;
    lastMethod = Method;
    lastDataFile = DataFile;
    lastLam = Lam;
    
