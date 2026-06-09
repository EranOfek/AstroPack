function [Result] = run_AstrometryNet(Input, Args)
    % Run astrometry.net (solve-field) and return the WCS solution as a
    % 2-column {Keyword,Value} cell array.
    %
    % Accepted Input:
    %   - char/string: path to a FITS image, or a FITS binary table with
    %     X,Y[,FLUX] columns (an xylist). The mode is auto-detected from
    %     the file structure.
    %   - Numeric Nx2 or Nx3 matrix [X Y] or [X Y Mag]. A temporary
    %     xylist FITS is created and passed to solve-field. The Mag
    %     column is stored as FLUX = -Mag so solve-field's default
    %     descending FLUX sort puts brightest first.
    %
    % Input  : - Input image / xylist FITS / [X,Y] or [X,Y,Mag] matrix.
    %          * ...,key,val,...
    %            'SolveField' - solve-field executable.
    %                   Default 'solve-field'.
    %            'ConfigFile' - --config path. If empty solve-field uses
    %                   its system default. Default ''.
    %            'Scale' - [low high] pixel scale. Default [].
    %            'ScaleUnits' - 'arcsecperpix' (default), 'arcminwidth',
    %                   'degwidth', 'focalmm'.
    %            'RA','Dec','Radius' - approximate centre + search
    %                   radius (degrees). Default [].
    %            'Width','Height' - image size in pixels. Required for
    %                   xylist file input; auto-computed for matrix
    %                   input. Default [].
    %            'Downsample' - --downsample N. Default [].
    %            'Cpulimit' - --cpulimit seconds. Default 60.
    %            'WorkDir' - working directory for solve-field outputs.
    %                   Default tempname.
    %            'KeepWorkDir' - keep WorkDir after run. Default false.
    %            'ExtraArgs' - cell array of additional command-line
    %                   tokens, e.g. {'--no-tweak','--depth','40'}.
    %                   Default {}.
    %            'OnlyWCS' - drop structural FITS keywords (SIMPLE,
    %                   BITPIX, NAXIS*, EXTEND, END). Default true.
    % Output : - Result: Nx2 cell {Keyword,Value} read from the .wcs
    %            file produced by solve-field. Empty Nx0 cell if
    %            solve-field did not converge.
    % Example: KV = imUtil.external.run_AstrometryNet('image.fits', ...
    %               'Scale',[0.9 1.1]);
    %          KV = imUtil.external.run_AstrometryNet([X,Y,Mag], ...
    %               'Width',2048,'Height',2048, ...
    %               'RA',180,'Dec',30,'Radius',1);

    arguments
        Input
        Args.SolveField  char    = 'solve-field'
        Args.ConfigFile  char    = ''
        Args.Scale               = []
        Args.ScaleUnits  char    = 'arcsecperpix'
        Args.RA                  = []
        Args.Dec                 = []
        Args.Radius              = []
        Args.Width               = []
        Args.Height              = []
        Args.Downsample          = []
        Args.Cpulimit            = 60
        Args.WorkDir     char    = ''
        Args.KeepWorkDir logical = false
        Args.ExtraArgs   cell    = {}
        Args.OnlyWCS     logical = true
    end

    % Working dir
    WorkDir = Args.WorkDir;
    if isempty(WorkDir)
        WorkDir = tempname;
    end
    if ~isfolder(WorkDir)
        mkdir(WorkDir);
    end
    Cleanup = onCleanup(@() cleanupWorkDir(WorkDir, Args.KeepWorkDir)); %#ok<NASGU>

    % Resolve input
    IsXYList = false;
    HasFlux  = false;
    if isnumeric(Input)
        if size(Input,2) < 2 || size(Input,2) > 3
            error('Matrix input must have 2 or 3 columns: [X Y] or [X Y Mag].');
        end
        X = Input(:,1);
        Y = Input(:,2);
        if size(Input,2) == 3
            Flux    = -Input(:,3);
            HasFlux = true;
        else
            Flux = [];
        end
        if isempty(Args.Width)
            Args.Width  = ceil(max(X,[],'omitnan')) + 1;
        end
        if isempty(Args.Height)
            Args.Height = ceil(max(Y,[],'omitnan')) + 1;
        end
        InputPath = fullfile(WorkDir, 'input.xyls');
        writeXYList(InputPath, X, Y, Flux);
        IsXYList = true;
    elseif ischar(Input) || isstring(Input)
        InputPath = char(Input);
        if ~isfile(InputPath)
            error('Input file not found: %s', InputPath);
        end
        [IsXYList, HasFlux] = looksLikeXYList(InputPath);
    else
        error('Input must be a FITS path or a numeric matrix.');
    end

    if IsXYList && (isempty(Args.Width) || isempty(Args.Height))
        error('Width and Height must be provided for xylist input.');
    end

    % Compose command
    [~, Base] = fileparts(InputPath);
    Cmd = sprintf('%s --no-plots --overwrite --dir ''%s''', ...
                  Args.SolveField, WorkDir);
    Cmd = [Cmd ' --new-fits none --solved none --match none ' ...
                '--rdls none --corr none --index-xyls none'];

    if IsXYList
        Cmd = sprintf('%s --xylist ''%s'' --width %d --height %d', ...
                      Cmd, InputPath, Args.Width, Args.Height);
        if HasFlux
            Cmd = [Cmd ' --sort-column FLUX'];
        end
    else
        Cmd = sprintf('%s ''%s''', Cmd, InputPath);
    end

    if ~isempty(Args.ConfigFile)
        Cmd = sprintf('%s --config ''%s''', Cmd, Args.ConfigFile);
    end
    if numel(Args.Scale) == 2
        Cmd = sprintf('%s --scale-low %g --scale-high %g --scale-units %s', ...
                      Cmd, Args.Scale(1), Args.Scale(2), Args.ScaleUnits);
    end
    if ~isempty(Args.RA) && ~isempty(Args.Dec)
        Cmd = sprintf('%s --ra %g --dec %g', Cmd, Args.RA, Args.Dec);
        if ~isempty(Args.Radius)
            Cmd = sprintf('%s --radius %g', Cmd, Args.Radius);
        end
    end
    if ~isempty(Args.Downsample)
        Cmd = sprintf('%s --downsample %d', Cmd, Args.Downsample);
    end
    if ~isempty(Args.Cpulimit)
        Cmd = sprintf('%s --cpulimit %d', Cmd, Args.Cpulimit);
    end
    for I = 1:numel(Args.ExtraArgs)
        Cmd = sprintf('%s %s', Cmd, Args.ExtraArgs{I});
    end

    % Run
    [Status, Output] = system(Cmd);
    if Status ~= 0
        error('solve-field failed (status %d):\n%s\nCommand: %s', ...
              Status, Output, Cmd);
    end

    % Read .wcs
    WcsPath = fullfile(WorkDir, [Base '.wcs']);
    if ~isfile(WcsPath)
        warning('solve-field did not produce %s -- no solution.', WcsPath);
        Result = cell(0,2);
        return
    end
    Result = readWcsKeywords(WcsPath, Args.OnlyWCS);
end


function writeXYList(Path, X, Y, Flux)
    import matlab.io.*
    if isfile(Path)
        delete(Path);
    end
    F = fits.createFile(Path);
    fits.createImg(F, 'byte_img', [0 0]);   % empty primary HDU
    if isempty(Flux)
        Names = {'X','Y'};
        Forms = {'1E','1E'};
    else
        Names = {'X','Y','FLUX'};
        Forms = {'1E','1E','1E'};
    end
    fits.createTbl(F, 'binary', numel(X), Names, Forms);
    fits.writeCol(F, 1, 1, single(X));
    fits.writeCol(F, 2, 1, single(Y));
    if ~isempty(Flux)
        fits.writeCol(F, 3, 1, single(Flux));
    end
    fits.closeFile(F);
end


function [IsXY, HasFlux] = looksLikeXYList(Path)
    IsXY    = false;
    HasFlux = false;
    try
        Info = fitsinfo(Path);
    catch
        return
    end
    if isfield(Info,'BinaryTable') && ~isempty(Info.BinaryTable)
        Cols = upper(string({Info.BinaryTable(1).Keywords{:,2}}));
        Names = upper(string(Info.BinaryTable(1).Header.FieldNames));
        % Some fitsinfo versions don't populate FieldNames; fall back
        % to scanning TTYPE keywords.
        if all(strlength(Names) == 0)
            KV  = Info.BinaryTable(1).Keywords;
            Sel = startsWith(upper(KV(:,1)), 'TTYPE');
            Names = upper(string(KV(Sel, 2)));
        end
        IsXY    = any(Names == "X") && any(Names == "Y");
        HasFlux = any(Names == "FLUX");
    end
end


function KV = readWcsKeywords(WcsPath, OnlyWCS)
    Info = fitsinfo(WcsPath);
    KV = Info.PrimaryData.Keywords;
    if isempty(KV)
        KV = cell(0,2);
        return
    end
    if OnlyWCS
        Drop = ismember(upper(KV(:,1)), ...
            {'SIMPLE','BITPIX','NAXIS','NAXIS1','NAXIS2','EXTEND','END'});
        KV(Drop, :) = [];
    end
    KV = KV(:, 1:2);
end


function cleanupWorkDir(D, Keep)
    if Keep || ~isfolder(D)
        return
    end
    try
        rmdir(D, 's');
    catch
    end
end
