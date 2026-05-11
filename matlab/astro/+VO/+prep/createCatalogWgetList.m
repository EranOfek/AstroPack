function createCatalogWgetList(BaseDir, CatRelDir, WriteDir, Args)
% Create wget and checksum lists for a single catsHTM catalog
% Package: VO.prep
% Description: Generate a wget list file and an md5 checksum file for
%              all *.hdf5 and *.mat files in a catsHTM catalog directory.
%              Output filenames follow the convention used by
%              catsHTM.create_indiv_catalog_lists4wget:
%                list.euler.wget.<dir_with_underscores>
%                list.euler.checksum.<dir_with_underscores>
%
%              Useful for catalogs that are not registered in
%              catsHTM.catalogs (e.g., NewCats/), or to regenerate wget
%              lists that have been lost or corrupted.
%
% Input  : - BaseDir   : Base catsHTM directory (e.g., '/euclid/catsHTM').
%          - CatRelDir : Catalog directory relative to BaseDir,
%                        with leading and trailing slashes
%                        (e.g., '/GLADE/v1/').
%          - WriteDir  : Directory where wget+checksum files are written.
% Args   : 'URL'          - Base download URL to use in wget lines.
%                           Default 'https://wao-data.org/catsHTM'.
%          'WithChecksum' - Write md5 checksum file. Default true.
%          'Verbose'      - Print progress. Default true.
% Output : null
% Author : Dana Kovaleva + Claude (Apr 2026)
% Example:
%   VO.prep.createCatalogWgetList('/euclid/catsHTM', '/GLADE/v1/', '~/tmp/cats');
%   VO.prep.createCatalogWgetList('/euclid/catsHTM', '/IPHAS/', '~/tmp/cats');

    arguments
        BaseDir   string
        CatRelDir string
        WriteDir  string
        Args.URL          string  = "https://wao-data.org/catsHTM"
        Args.WithChecksum logical = true
        Args.Verbose      logical = true
    end

    BaseDir   = char(BaseDir);
    CatRelDir = char(CatRelDir);
    WriteDir  = char(WriteDir);
    URL       = char(Args.URL);

    % Ensure leading and trailing slash on CatRelDir for consistent URLs
    if ~startsWith(CatRelDir, '/'), CatRelDir = ['/' CatRelDir]; end
    if ~endsWith(CatRelDir, '/'),   CatRelDir = [CatRelDir '/']; end

    if ~isfolder(WriteDir)
        mkdir(WriteDir);
    end

    CatDir = [BaseDir CatRelDir];
    if ~isfolder(CatDir)
        error('VO:prep:createCatalogWgetList', ...
            'Catalog directory does not exist: %s', CatDir);
    end

    % Collect all data files
    F = [dir(fullfile(CatDir, '*.hdf5')); dir(fullfile(CatDir, '*.mat'))];
    Nfiles = numel(F);
    if Nfiles == 0
        error('VO:prep:createCatalogWgetList', ...
            'No *.hdf5 or *.mat files found in %s', CatDir);
    end

    % Output filenames mirror catsHTM.create_indiv_catalog_lists4wget
    Suffix = strrep(CatRelDir, '/', '_');
    ListW  = fullfile(WriteDir, sprintf('list.euler.wget.%s',     Suffix));
    ListC  = fullfile(WriteDir, sprintf('list.euler.checksum.%s', Suffix));

    % wget parameters: download into the same relative path under cwd
    Pars = sprintf('-U Mozilla/5.0 --no-check-certificate -P .%s', ...
        CatRelDir(1:end-1));

    % Write wget list
    FIDw = fopen(ListW, 'w');
    if FIDw < 0
        error('VO:prep:createCatalogWgetList', ...
            'Cannot open for writing: %s', ListW);
    end
    OnExit = onCleanup(@() fclose(FIDw));

    for If = 1:Nfiles
        fprintf(FIDw, 'wget %s %s%s/%s\n', ...
            Pars, URL, CatRelDir(1:end-1), F(If).name);
    end
    clear OnExit;

    if Args.Verbose
        fprintf('Wrote wget list (%d files): %s\n', Nfiles, ListW);
    end

    % Write checksum file (operates on the catalog dir, not WriteDir)
    if Args.WithChecksum
        PrevDir = pwd;
        cd(CatDir);
        try
            FIDc = fopen(ListC, 'w');
            if FIDc < 0
                cd(PrevDir);
                error('VO:prep:createCatalogWgetList', ...
                    'Cannot open for writing: %s', ListC);
            end
            for If = 1:Nfiles
                [Status, Str] = system(sprintf('md5sum %s', F(If).name));
                if Status == 0
                    fprintf(FIDc, '%s', Str);
                else
                    fprintf('  WARNING: md5sum failed for %s\n', F(If).name);
                end
            end
            fclose(FIDc);
        catch ME
            cd(PrevDir);
            rethrow(ME);
        end
        cd(PrevDir);

        if Args.Verbose
            fprintf('Wrote checksum list (%d files): %s\n', Nfiles, ListC);
        end
    end
end
