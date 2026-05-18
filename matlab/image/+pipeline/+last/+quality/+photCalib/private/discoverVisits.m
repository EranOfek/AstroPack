function [VisitDirs, VisitNames] = discoverVisits(BaseDir, Args)
    % Discover visit subdirectories under a base directory.
    % Description: Walks BaseDir for subdirectories matching a glob pattern
    %              (optionally recursively), drops hidden entries, applies an
    %              optional exclusion list, and errors if nothing remains.
    %              Shared visit-discovery helper for the photCalib batch and
    %              plotting functions (private package helper).
    % Input  : - BaseDir - parent directory (char/string).
    %          * ...,key,val,...
    %            'VisitGlob'     - Glob matching visit subdirectory names.
    %                              Default '*v*'.
    %            'Recursive'     - If true, match visit dirs at any depth
    %                              under BaseDir; otherwise only direct
    %                              children. Default false.
    %            'ExcludeVisits' - Visit names or full paths to drop. Char,
    %                              string, or cell thereof. Default {}.
    %            'AllowEmpty'    - If true, return empty instead of erroring
    %                              when no visit matches. Default false.
    % Output : - VisitDirs  - 1xN cell of full visit-directory paths.
    %          - VisitNames - 1xN cell of visit-directory basenames.
    % Author : photCalib package refactor (2026-05)
    % Example: [D,N] = discoverVisits('/data/2025/07/08', 'Recursive', true);

    arguments
        BaseDir              {mustBeTextScalar}
        Args.VisitGlob       {mustBeTextScalar} = '*v*'
        Args.Recursive       logical            = false
        Args.ExcludeVisits                      = {}
        Args.AllowEmpty      logical            = false
    end

    BaseDir = char(BaseDir);

    if Args.Recursive
        Listing = dir(fullfile(BaseDir, '**', Args.VisitGlob));
    else
        Listing = dir(fullfile(BaseDir, Args.VisitGlob));
    end
    Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));

    VisitDirs  = cell(1, numel(Listing));
    VisitNames = cell(1, numel(Listing));
    for I = 1:numel(Listing)
        VisitDirs{I}  = fullfile(Listing(I).folder, Listing(I).name);
        VisitNames{I} = Listing(I).name;
    end

    % Exclusion by basename or by full path
    if ~isempty(Args.ExcludeVisits)
        ExNames = cellstr(string(Args.ExcludeVisits));
        Keep    = ~(ismember(VisitNames, ExNames) | ismember(VisitDirs, ExNames));
        VisitDirs  = VisitDirs(Keep);
        VisitNames = VisitNames(Keep);
    end

    if isempty(VisitDirs) && ~Args.AllowEmpty
        ScopeStr = BaseDir;
        if Args.Recursive; ScopeStr = [ScopeStr ' (recursive)']; end
        error('photCalib:discoverVisits:NoVisits', ...
            'No subdirectories matching "%s" found under %s', ...
            Args.VisitGlob, ScopeStr);
    end
end
