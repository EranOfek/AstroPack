function Result = cleanVisitsVer(Args)
    % Remove superseded reduction versions of LAST visits from an archive tree.
    %   A visit directory is named <HHMMSS>v<N>, where N counts the reductions of
    %   that visit that already existed when it was written (@FileNames starts at
    %   0, @AstroFileName at 1) - it is NOT the pipeline version. Both pipelines
    %   may therefore write any N, so selecting by a fixed version number is only
    %   meaningful for an archive produced by a single pipeline.
    % Input  : * ...,key,val,...
    %            'BasePath' - Archive tree to clean, e.g. a camera directory.
    %                   Default is '/marvin/LAST.01.01.01'.
    %            'Template' - File name to search in order to identify visit
    %                   directories. Default is '.status'.
    %            'Method' - One of:
    %                   'keepLatest' - Of each group of visit directories sharing
    %                           the same <HHMMSS>, keep the highest version and
    %                           remove the rest. Safe for a mixed v0/v1 archive.
    %                   'removeNon0' - Remove every visit whose version is not 0.
    %                           Was the historical default; with the v1 pipeline
    %                           this removes the NEW reductions, not the old ones.
    %                   'removeNon1' - Remove every visit whose version is not 1.
    %                   Default is 'keepLatest'.
    %            'DryRun' - If true, only report what would be removed.
    %                   Default is true.
    %            'Verbosity' - 0 silent, 1 summary, 2 per directory.
    %                   Default is 1.
    % Output : - A string array of the visit directories that were removed, or
    %            that would be removed when 'DryRun' is true.
    % Author : Eran Ofek (2024 Jul), rewritten Sep 2026
    % Example: % see what would go, without touching anything:
    %          R = pipeline.last.archiveMaintenance.cleanVisitsVer('BasePath','/last01e/data1/archive/LAST.01.01.01');
    %          % and then actually remove them:
    %          R = pipeline.last.archiveMaintenance.cleanVisitsVer('BasePath','/last01e/data1/archive/LAST.01.01.01', 'DryRun',false);

    arguments
        Args.BasePath                 = '/marvin/LAST.01.01.01';
        Args.Template                 = '.status';   %'LAST.*_coadd_*.fits'
        Args.Method                   = 'keepLatest';
        Args.DryRun logical           = true;
        Args.Verbosity                = 1;
    end

    Result = strings(0,1);

    if ~isfolder(Args.BasePath)
        error('cleanVisitsVer:NoBasePath', 'BasePath not found: %s', Args.BasePath);
    end

    F = io.files.findFiles(Args.Template, 'BasePath',Args.BasePath, 'IgnoreHidden',false,...
                                          'MinSize',[], 'MaxSize',[]);
    if isempty(F)
        if Args.Verbosity>0
            fprintf('No %s found under %s\n', Args.Template, Args.BasePath);
        end
        return
    end
    Folders    = {F.folder};
    AllFolders = unique(string(Folders(:)));

    % Parse the version out of the directory NAME only - a bare contains() over the
    % full path also matches 'v1' in a camera or mount name.
    Nf       = numel(AllFolders);
    Parsed   = false(Nf,1);
    Parent   = strings(Nf,1);
    TimeStr  = strings(Nf,1);
    Ver      = nan(Nf,1);
    for If=1:1:Nf
        [Parent(If), Name] = fileparts(AllFolders(If));
        Tok = regexp(Name, '^(\d{6})v(\d+)$', 'tokens', 'once');
        if ~isempty(Tok)
            Parsed(If)  = true;
            TimeStr(If) = Tok{1};
            Ver(If)     = str2double(Tok{2});
        end
    end

    switch lower(Args.Method)
        case 'keeplatest'
            % group by parent directory + start time, keep the highest version
            Key    = Parent + "|" + TimeStr;
            Flag   = false(Nf,1);
            UKey   = unique(Key(Parsed));
            for Ik=1:1:numel(UKey)
                InGroup = Parsed & Key==UKey(Ik);
                MaxVer  = max(Ver(InGroup));
                Flag    = Flag | (InGroup & Ver<MaxVer);
            end
        case 'removenon0'
            Flag = Parsed & Ver~=0;
        case 'removenon1'
            Flag = Parsed & Ver~=1;
        otherwise
            error('cleanVisitsVer:UnknownMethod', 'Unknown Method: %s', Args.Method);
    end

    Result = AllFolders(Flag);
    Nrm    = numel(Result);

    if Args.Verbosity>0
        fprintf('%s: %d of %d visit directories selected for removal (%d unparsed names left alone)\n',...
                Args.Method, Nrm, sum(Parsed), sum(~Parsed));
        if Args.DryRun
            fprintf('DryRun is true - nothing is removed. Pass ''DryRun'',false to apply.\n');
        end
    end

    for Irm=1:1:Nrm
        if Args.Verbosity>1
            fprintf('  %s\n', Result(Irm));
        end
        if ~Args.DryRun
            [Ok, Msg] = rmdir(Result(Irm), 's');
            if ~Ok
                fprintf('Failed to remove %s: %s\n', Result(Irm), Msg);
            end
        end
    end

end
