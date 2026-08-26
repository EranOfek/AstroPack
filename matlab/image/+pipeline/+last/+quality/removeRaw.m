function [Result] = removeRaw(T, Args)
    % Remove LAST RAW images selected by quality cuts, with a dry run option.
    %   Takes the quality table produced by pipeline.last.quality.raw,
    %   applies the requested cuts, and deletes the images that fail them.
    %   By default nothing is deleted: the function only lists the files
    %   that would be removed ('DryRun' is true).
    %
    %   Every candidate is validated before it is deleted:
    %   it must be an existing regular file, it must reside in the
    %   sub directory named by 'RequireSubDir' (by default 'failed'), its
    %   name must look like a LAST RAW image, and the LAST.nn.nn.nn literal
    %   of its path must agree with the one recorded in the table. A
    %   candidate that fails any of these is reported and skipped, never
    %   deleted. Rows whose measurement failed (non empty Error) are never
    %   selected, since their metrics are unknown.
    %
    % Input  : - The quality table returned by pipeline.last.quality.raw,
    %            or the name of the tab separated text file it wrote.
    %          * ...,key,val,...
    %            % ---------- cuts ----------
    %            'MaxFWHM' - Remove images with FWHM above this value
    %                   [pix]. If empty, not applied. Default is [].
    %            'MinFWHM' - Remove images with FWHM below this value
    %                   [pix]. If empty, not applied. Default is [].
    %            'MaxBack' - Remove images with Back above this value
    %                   [ADU]. If empty, not applied. Default is [].
    %            'MinBack' - Remove images with Back below this value
    %                   [ADU]. If empty, not applied. Default is [].
    %            'MaxVar' - Remove images with Var above this value
    %                   [ADU^2]. If empty, not applied. Default is [].
    %            'MinVar' - Remove images with Var below this value
    %                   [ADU^2]. If empty, not applied. Default is [].
    %            'BackAnomaly' - Remove images whose BackAnomaly flag is
    %                   set. Default is false.
    %            'VarAnomaly' - Remove images whose VarAnomaly flag is set.
    %                   Default is false.
    %            'HistAnomaly' - Remove images whose HistAnomaly flag is
    %                   set. Default is false.
    %            'HighPixAnomaly' - Remove images whose HighPixAnomaly flag
    %                   is set. Default is false.
    %            'Expression' - An additional selection given as a function
    %                   handle of the table, e.g.
    %                   @(T) T.FWHM>8 & T.Sky>1000.
    %                   If empty, not applied. Default is [].
    %            'Combine' - How to combine the cuts above: 'or' removes an
    %                   image failing any of them, 'and' only an image
    %                   failing all of them. Default is 'or'.
    %            % ---------- safety ----------
    %            'DryRun' - If true, do not delete anything, only report
    %                   the files that would be deleted. Default is true.
    %            'RequireSubDir' - The images must reside in a directory
    %                   with this name, otherwise they are skipped. Set to
    %                   '' to disable this protection. Default is 'failed'.
    %            'FileNameTemplate' - Regular expression a file name must
    %                   match in order to be deleted.
    %                   Default is '^LAST\.\d\d\.\d\d\.\d\d_\d{8}\.\d{6}\.\d{3}_.*_sci_raw_.*\.fits(\.fz)?$'.
    %            'ManifestFile' - Name of an append only text file to which
    %                   every deleted file is recorded (time, path, size,
    %                   reason). Written only when 'DryRun' is false. If
    %                   empty, no manifest is written - not recommended.
    %                   Default is 'removeRaw_manifest.txt'.
    %            'Verbosity' - 0 - silent, 1 - summary, 2 - one line per
    %                   file. Default is 1.
    % Output : - A table of the selected images, with the columns of the
    %            input table restricted to the identification and the
    %            metrics that were cut on, plus:
    %            Reason - which cuts the image failed.
    %            Status - 'deleted', 'dry-run', 'skipped: <why>', or
    %                     'error: <message>'.
    %            Bytes  - size of the file.
    % Author : Sasha (2026 Aug)
    % Example: % list what a FWHM cut would remove
    %          R = pipeline.last.quality.removeRaw(T, 'MaxFWHM',8);
    %          % actually remove them
    %          R = pipeline.last.quality.removeRaw(T, 'MaxFWHM',8, 'DryRun',false);

    arguments
        T
        Args.MaxFWHM              = [];
        Args.MinFWHM              = [];
        Args.MaxBack              = [];
        Args.MinBack              = [];
        Args.MaxVar               = [];
        Args.MinVar               = [];
        Args.BackAnomaly logical  = false;
        Args.VarAnomaly logical   = false;
        Args.HistAnomaly logical  = false;
        Args.HighPixAnomaly logical = false;
        Args.Expression           = [];
        Args.Combine              = 'or';

        Args.DryRun logical       = true;
        Args.RequireSubDir        = 'failed';
        Args.FileNameTemplate     = '^LAST\.\d\d\.\d\d\.\d\d_\d{8}\.\d{6}\.\d{3}_.*_sci_raw_.*\.fits(\.fz)?$';
        Args.ManifestFile         = 'removeRaw_manifest.txt';
        Args.Verbosity            = 1;
    end

    if ischar(T) || isstring(T)
        T = readtable(T, 'Delimiter','tab', 'FileType','text', 'TextType','string');
    end
    if ~istable(T)
        error('pipeline:last:quality:removeRaw:badInput', 'First input must be a table or a file name');
    end
    Nrow = height(T);
    if Nrow==0
        Result = table;
        return
    end

    % ---- build the selection ----
    [Flag, Reason] = selectRows(T, Args);

    % Rows whose measurement failed carry unknown metrics - never select them.
    if any(strcmp('Error', T.Properties.VariableNames))
        Bad = strlength(string(T.Error))>0;
        if any(Bad & Flag) && Args.Verbosity>0
            fprintf('pipeline.last.quality.removeRaw: %d selected rows have a measurement error and are deselected\n', sum(Bad & Flag));
        end
        Flag = Flag & ~Bad;
    end

    Ind  = find(Flag);
    Nsel = numel(Ind);
    if Args.Verbosity>0
        fprintf('pipeline.last.quality.removeRaw: %d of %d images selected%s\n', ...
                Nsel, Nrow, dryRunSuffix(Args.DryRun));
    end
    if Nsel==0
        Result = table;
        return
    end

    % ---- validate and act ----
    Status = strings(Nsel,1);
    Bytes  = zeros(Nsel,1);
    for I=1:1:Nsel
        Irow     = Ind(I);
        FullName = fullfile(char(T.Path(Irow)), char(T.FileName(Irow)));

        Why = validateCandidate(FullName, T, Irow, Args);
        if ~isempty(Why)
            Status(I) = "skipped: " + Why;
        else
            D        = dir(FullName);
            Bytes(I) = D.bytes;
            if Args.DryRun
                Status(I) = "dry-run";
            else
                try
                    delete(FullName);
                    Status(I) = "deleted";
                    writeManifest(Args.ManifestFile, FullName, D.bytes, Reason(Irow));
                catch ME
                    Status(I) = "error: " + string(ME.message);
                    Bytes(I)  = 0;
                end
            end
        end

        if Args.Verbosity>1
            fprintf('   %-9s %s  [%s]\n', Status(I), FullName, Reason(Irow));
        end
    end

    % ---- assemble the report ----
    KeepCols = intersect({'ProjName','Node','Mount','Camera','Path','FileName', ...
                          'FWHM','Back','Var','VarRatio','Sky', ...
                          'BackAnomaly','VarAnomaly','HistAnomaly','HighPixAnomaly'}, ...
                         T.Properties.VariableNames, 'stable');
    Result        = T(Ind, KeepCols);
    Result.Reason = Reason(Ind);
    Result.Status = Status;
    Result.Bytes  = Bytes;

    if Args.Verbosity>0
        Ndel = sum(startsWith(Status,"deleted"));
        Ndry = sum(Status=="dry-run");
        Nskp = sum(startsWith(Status,"skipped"));
        Nerr = sum(startsWith(Status,"error"));
        fprintf('pipeline.last.quality.removeRaw: deleted %d, dry-run %d, skipped %d, errors %d, %.2f GB\n', ...
                Ndel, Ndry, Nskp, Nerr, sum(Bytes)./1e9);
        if Args.DryRun
            fprintf('pipeline.last.quality.removeRaw: nothing was deleted - rerun with ''DryRun'',false to delete\n');
        end
    end
end

% ------------------------------------------------------------------------
function [Flag, Reason] = selectRows(T, Args)
    % Apply the individual cuts and combine them.
    Nrow   = height(T);
    Reason = strings(Nrow,1);

    Cuts = {};
    Cuts = addCut(Cuts, ~isempty(Args.MaxFWHM), @() T.FWHM>Args.MaxFWHM,  sprintf('FWHM>%g', Args.MaxFWHM));
    Cuts = addCut(Cuts, ~isempty(Args.MinFWHM), @() T.FWHM<Args.MinFWHM,  sprintf('FWHM<%g', Args.MinFWHM));
    Cuts = addCut(Cuts, ~isempty(Args.MaxBack), @() T.Back>Args.MaxBack,  sprintf('Back>%g', Args.MaxBack));
    Cuts = addCut(Cuts, ~isempty(Args.MinBack), @() T.Back<Args.MinBack,  sprintf('Back<%g', Args.MinBack));
    Cuts = addCut(Cuts, ~isempty(Args.MaxVar),  @() T.Var>Args.MaxVar,    sprintf('Var>%g',  Args.MaxVar));
    Cuts = addCut(Cuts, ~isempty(Args.MinVar),  @() T.Var<Args.MinVar,    sprintf('Var<%g',  Args.MinVar));
    Cuts = addCut(Cuts, Args.BackAnomaly,       @() T.BackAnomaly==1,     'BackAnomaly');
    Cuts = addCut(Cuts, Args.VarAnomaly,        @() T.VarAnomaly==1,      'VarAnomaly');
    Cuts = addCut(Cuts, Args.HistAnomaly,       @() T.HistAnomaly==1,     'HistAnomaly');
    Cuts = addCut(Cuts, Args.HighPixAnomaly,    @() T.HighPixAnomaly==1,  'HighPixAnomaly');
    Cuts = addCut(Cuts, ~isempty(Args.Expression), @() Args.Expression(T), 'Expression');

    Ncut = numel(Cuts);
    if Ncut==0
        error('pipeline:last:quality:removeRaw:noCut', ...
              'No cut was specified - refusing to select every image');
    end

    switch lower(Args.Combine)
        case 'or'
            Flag = false(Nrow,1);
        case 'and'
            Flag = true(Nrow,1);
        otherwise
            error('pipeline:last:quality:removeRaw:badCombine', '''Combine'' must be ''or'' or ''and''');
    end

    for I=1:1:Ncut
        % A NaN metric compares false, so an unmeasured image is never
        % selected by a numeric cut.
        This = logical(Cuts{I}.Fun());
        This = This(:);
        switch lower(Args.Combine)
            case 'or'
                Flag = Flag | This;
            case 'and'
                Flag = Flag & This;
        end
        Reason(This) = Reason(This) + string(Cuts{I}.Name) + ",";
    end
    Reason = regexprep(Reason, ',$', '');
end

% ------------------------------------------------------------------------
function Cuts = addCut(Cuts, DoIt, Fun, Name)
    if DoIt
        Cuts{end+1} = struct('Fun',Fun, 'Name',Name);
    end
end

% ------------------------------------------------------------------------
function Why = validateCandidate(FullName, T, Irow, Args)
    % Return an empty string if the file may be deleted, otherwise the
    % reason why it may not.
    Why = '';

    [Folder, Name, Ext] = fileparts(FullName);
    FileName = [Name, Ext];

    if ~isfile(FullName)
        Why = 'file does not exist';
        return
    end

    if ~isempty(Args.RequireSubDir)
        Parts = split(string(Folder), filesep);
        if ~any(strcmp(Parts, Args.RequireSubDir))
            Why = sprintf('not under a ''%s'' directory', Args.RequireSubDir);
            return
        end
    end

    if ~isempty(Args.FileNameTemplate) && isempty(regexp(FileName, Args.FileNameTemplate, 'once'))
        Why = 'file name is not a LAST RAW image';
        return
    end

    % The path must belong to the camera recorded in the table, so that a
    % table row that was edited or reordered cannot point the deletion at a
    % different camera.
    if any(strcmp('ProjName', T.Properties.VariableNames))
        ProjName = char(T.ProjName(Irow));
        if ~isempty(ProjName) && ~contains(Folder, ProjName)
            Why = sprintf('path does not match table ProjName %s', ProjName);
            return
        end
    end
end

% ------------------------------------------------------------------------
function writeManifest(ManifestFile, FullName, Bytes, Reason)
    % Append one record of a deleted file. Append only, so that a manifest
    % of an earlier run is never overwritten.
    if isempty(ManifestFile)
        return
    end
    FID = fopen(ManifestFile, 'a');
    if FID<0
        warning('pipeline:last:quality:removeRaw:manifest', ...
                'Could not open the manifest file %s', ManifestFile);
        return
    end
    Now = char(datetime('now', 'Format','yyyy-MM-dd''T''HH:mm:ss'));
    fprintf(FID, '%s\t%s\t%d\t%s\n', Now, FullName, Bytes, Reason);
    fclose(FID);
end

% ------------------------------------------------------------------------
function S = dryRunSuffix(DryRun)
    if DryRun
        S = ' (dry run)';
    else
        S = ' (DELETING)';
    end
end
