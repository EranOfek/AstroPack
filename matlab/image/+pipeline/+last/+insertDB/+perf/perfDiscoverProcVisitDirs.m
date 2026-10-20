function Dirs = perfDiscoverProcVisitDirs(RootDir, ProcDirTemplate, ProcDirList)
    % Discover proc visit directories (v0) under RootDir + ProcDirTemplate.
    % Supports wildcard templates (proc/*) and a single visit path (.../proc/000224v0).
    if nargin < 3
        ProcDirList = [];
    end
    if ~isempty(ProcDirList)
        Dirs = ProcDirList;
        return
    end
    FullPattern = fullfile(RootDir, ProcDirTemplate);
    if ~contains(ProcDirTemplate, '*') && isfolder(FullPattern)
        [ParentFolder, VisitName, ~] = fileparts(FullPattern);
        if contains(VisitName, 'v0') && ~contains(ParentFolder, 're')
            Dirs = struct('folder', ParentFolder, 'name', VisitName);
            return
        end
    end
    D = dir(FullPattern);
    Dirs = D([D.isdir]);
    Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'}));
    Dirs = Dirs(contains({Dirs.name}, 'v0'));
    Dirs = Dirs(~contains({Dirs.folder}, 're'));
end
