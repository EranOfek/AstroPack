function [Result] = run_SExtractor(FileName, Args)
    % Run SExtractor on a FITS image and return the catalog as a MATLAB table.
    % Input  : - FileName: path to the input image (FITS).
    %          * ...,key,val,...
    %            'SExPath' - Path to the SExtractor executable.
    %                   Default '/home/eran/bin/sex'.
    %            'ConfigFile' - SExtractor configuration file (.sex). If
    %                   empty, runs without `-c` (built-in defaults, then
    %                   overridden by the name/value pairs below).
    %                   Default ''.
    %            'Params' - Cell array of SExtractor output parameters.
    %                   A temporary .param file is generated from this list.
    %                   Ignored when 'ParamFile' is supplied.
    %                   Default is a common photometric set (see code).
    %            'ParamFile' - Path to an existing .param file.
    %                   Default ''.
    %            'CatalogName' - Output catalog filename. If empty, a temp
    %                   file is used. Default ''.
    %            'CatalogType' - 'ASCII_HEAD' (default) or 'ASCII'. The
    %                   catalog must be ASCII for read-back into a table.
    %            'SharePath' - Directory holding SExtractor data files
    %                   (default.conv, default.nnw, ...). On Debian/Ubuntu
    %                   this is /usr/share/source-extractor.
    %                   Default '/usr/share/source-extractor'.
    %            'FilterName' - Convolution kernel file (FILTER_NAME). If
    %                   not an absolute path, resolved against SharePath.
    %                   Default 'default.conv'.
    %            'StarNnwName' - Neural-network weights file (STARNNW_NAME),
    %                   used by CLASS_STAR. Resolved against SharePath if
    %                   not absolute. Default 'default.nnw'.
    %            'DetectThresh','AnalysisThresh','DetectMinArea',
    %            'SatLevel','Gain','PixelScale','SeeingFWHM','Filter',
    %            'BackSize','BackFilterSize','PhotApertures',
    %            'PhotAutoParams','WeightImage','WeightType','Verbose' -
    %                   Common SExtractor options passed as -KEY VALUE.
    %            'ExtraArgs' - Cell array {'KEY',VAL,...} of additional
    %                   -KEY VALUE pairs appended to the command line.
    %                   Default {}.
    %            'DeleteCatalog' - Delete catalog file after reading.
    %                   Default true.
    % Output : - Result: a MATLAB table with one row per detected source.
    %            Multi-valued SExtractor parameters (e.g. MAG_APER over
    %            several apertures) get suffixed _1, _2, ... .
    % Author : Claude + Eran Ofek (Jun 2026)
    % Example: T = imUtil.external.run_SExtractor('image.fits');
    %          T = imUtil.external.run_SExtractor('image.fits', ...
    %                  'DetectThresh',3, ...
    %                  'Params',{'X_IMAGE','Y_IMAGE','MAG_AUTO'});

    arguments
        FileName                     char
        Args.SExPath                 char    = '/home/eran/bin/sex'
        Args.ConfigFile              char    = ''
        Args.Params                  cell    = {'NUMBER','X_IMAGE','Y_IMAGE', ...
            'XWIN_IMAGE','YWIN_IMAGE','ALPHA_J2000','DELTA_J2000', ...
            'FLUX_AUTO','FLUXERR_AUTO','MAG_AUTO','MAGERR_AUTO', ...
            'FLUX_RADIUS','FWHM_IMAGE','A_IMAGE','B_IMAGE', ...
            'THETA_IMAGE','ELONGATION','ELLIPTICITY','FLAGS', ...
            'BACKGROUND','CLASS_STAR'}
        Args.ParamFile               char    = ''
        Args.CatalogName             char    = ''
        Args.CatalogType             char    = 'ASCII_HEAD'
        Args.SharePath               char    = '/usr/share/source-extractor'
        Args.FilterName              char    = 'default.conv'
        Args.StarNnwName             char    = 'default.nnw'
        Args.DetectThresh                    = 1.5
        Args.AnalysisThresh                  = 1.5
        Args.DetectMinArea                   = 5
        Args.SatLevel                        = 60000
        Args.Gain                            = 1.0
        Args.PixelScale                      = 1.0
        Args.SeeingFWHM                      = 1.2
        Args.Filter                  char    = 'Y'
        Args.BackSize                        = 64
        Args.BackFilterSize                  = 3
        Args.PhotApertures                   = 5
        Args.PhotAutoParams          char    = '2.5,3.5'
        Args.WeightImage             char    = ''
        Args.WeightType              char    = 'NONE'
        Args.Verbose                 char    = 'QUIET'
        Args.ExtraArgs               cell    = {}
        Args.DeleteCatalog           logical = true
    end

    if ~any(strcmpi(Args.CatalogType, {'ASCII','ASCII_HEAD'}))
        error('CatalogType must be ASCII or ASCII_HEAD for table read-back.');
    end

    % Parameter file
    if isempty(Args.ParamFile)
        ParamFile = [tempname '.param'];
        FID = fopen(ParamFile, 'w');
        fprintf(FID, '%s\n', Args.Params{:});
        fclose(FID);
        AutoParam = true;
    else
        ParamFile = Args.ParamFile;
        AutoParam = false;
    end

    % Catalog file
    if isempty(Args.CatalogName)
        CatalogName = [tempname '.cat'];
    else
        CatalogName = Args.CatalogName;
    end

    % Compose command
    Cmd = sprintf('%s ''%s''', Args.SExPath, FileName);
    if ~isempty(Args.ConfigFile)
        Cmd = sprintf('%s -c ''%s''', Cmd, Args.ConfigFile);
    end

    FilterPath  = resolveSharePath(Args.FilterName,  Args.SharePath);
    StarNnwPath = resolveSharePath(Args.StarNnwName, Args.SharePath);

    NV = { ...
        'CATALOG_NAME',    CatalogName, ...
        'CATALOG_TYPE',    upper(Args.CatalogType), ...
        'PARAMETERS_NAME', ParamFile, ...
        'FILTER_NAME',     FilterPath, ...
        'STARNNW_NAME',    StarNnwPath, ...
        'DETECT_THRESH',   Args.DetectThresh, ...
        'ANALYSIS_THRESH', Args.AnalysisThresh, ...
        'DETECT_MINAREA',  Args.DetectMinArea, ...
        'SATUR_LEVEL',     Args.SatLevel, ...
        'GAIN',            Args.Gain, ...
        'PIXEL_SCALE',     Args.PixelScale, ...
        'SEEING_FWHM',     Args.SeeingFWHM, ...
        'FILTER',          Args.Filter, ...
        'BACK_SIZE',       Args.BackSize, ...
        'BACK_FILTERSIZE', Args.BackFilterSize, ...
        'PHOT_APERTURES',  Args.PhotApertures, ...
        'PHOT_AUTOPARAMS', Args.PhotAutoParams, ...
        'VERBOSE_TYPE',    Args.Verbose };

    if ~isempty(Args.WeightImage)
        NV = [NV, {'WEIGHT_IMAGE', Args.WeightImage, ...
                   'WEIGHT_TYPE',  Args.WeightType}];
    end

    NV = [NV, Args.ExtraArgs];
    for I = 1:2:numel(NV)
        Val = NV{I+1};
        if isnumeric(Val) || islogical(Val)
            Val = num2str(Val);
        end
        Cmd = sprintf('%s -%s %s', Cmd, NV{I}, Val);
    end

    % Run
    [Status, Output] = system(Cmd);
    if Status ~= 0
        error('SExtractor failed (status %d):\n%s\nCommand: %s', ...
              Status, Output, Cmd);
    end

    % Read catalog into table
    Result = readSExCatalog(CatalogName, Args.CatalogType);

    % Cleanup
    if Args.DeleteCatalog && isfile(CatalogName)
        delete(CatalogName);
    end
    if AutoParam && isfile(ParamFile)
        delete(ParamFile);
    end
end


function P = resolveSharePath(Name, SharePath)
    % Resolve Name against SharePath unless it is already absolute or
    % already exists as given.
    if isempty(Name)
        P = Name;
        return
    end
    if startsWith(Name, '/') || isfile(Name)
        P = Name;
    else
        P = fullfile(SharePath, Name);
    end
end


function T = readSExCatalog(CatalogName, CatalogType)
    Data = readmatrix(CatalogName, 'FileType','text', 'CommentStyle','#');
    Ncol = size(Data, 2);
    if strcmpi(CatalogType, 'ASCII_HEAD')
        Names = parseAsciiHeadColumns(CatalogName, Ncol);
    else
        Names = compose('Var%d', 1:Ncol);
    end
    T = array2table(Data, 'VariableNames', cellstr(Names));
end


function Names = parseAsciiHeadColumns(CatalogName, Ncol)
    % Parse `# N NAME ...` header lines. Multi-element parameters span
    % consecutive columns; their entries get suffixed _1, _2, ... .
    FID = fopen(CatalogName, 'r');
    OC  = onCleanup(@() fclose(FID)); %#ok<NASGU>

    StartCol  = [];
    StartName = {};
    Line = fgetl(FID);
    while ischar(Line)
        if isempty(Line) || Line(1) ~= '#'
            break
        end
        Tok = strsplit(strtrim(Line(2:end)));
        if numel(Tok) >= 2
            N = str2double(Tok{1});
            if isfinite(N) && N > 0
                StartCol(end+1)  = N;      %#ok<AGROW>
                StartName{end+1} = Tok{2}; %#ok<AGROW>
            end
        end
        Line = fgetl(FID);
    end

    Names  = repmat({''}, 1, Ncol);
    Nparam = numel(StartCol);
    for I = 1:Nparam
        From = StartCol(I);
        if I < Nparam
            To = StartCol(I+1) - 1;
        else
            To = Ncol;
        end
        Span = To - From + 1;
        if Span == 1
            Names{From} = StartName{I};
        else
            for K = 1:Span
                Names{From + K - 1} = sprintf('%s_%d', StartName{I}, K);
            end
        end
    end
    Empty = cellfun(@isempty, Names);
    Names(Empty) = compose('Var%d', find(Empty));
end
