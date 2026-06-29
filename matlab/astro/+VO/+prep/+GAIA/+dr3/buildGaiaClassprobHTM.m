function Result = buildGaiaClassprobHTM(VotFile, Args)
    % Build a local catsHTM of Gaia DR3 classprob from a pre-downloaded VOTable.
    % Description: Turn a pre-downloaded Gaia DR3 table into a small local
    %              catsHTM ('GAIADR3classprob') holding only RA, Dec and
    %              classprob_dsc_combmod_star, keyed on J2016 RA/Dec. This is
    %              the classifier source used by VO.prep.GAIA.dr3.mergeGAIADR3spec
    %              (the other 7 Gaia columns come from the local GAIADR3
    %              catsHTM). No TAP at runtime: the VOTable is downloaded once
    %              externally with a query such as
    %                SELECT gs.source_id, gs.ra, gs.dec, ap.classprob_dsc_combmod_star
    %                FROM gaiadr3.gaia_source AS gs
    %                JOIN gaiadr3.xp_sampled_mean_spectrum AS xp ON gs.source_id=xp.source_id
    %                JOIN gaiadr3.astrophysical_parameters AS ap ON gs.source_id=ap.source_id
    %              so the row set matches the GAIADR3spec (XP-sampled) population.
    %
    %              source_id is ignored: Gaia 19-digit ids overflow double
    %              precision and the join to GAIADR3spec is by sky position.
    %
    %              A binary VOTable is converted to a 3-column CSV with
    %              STILTS (tpipe keepcols ra/dec/classprob, dropping the
    %              19-digit source_id); a .csv input is read directly.
    % Input  : - VotFile : path to the downloaded table (.vot/.xml VOTable or
    %                       .csv). Must contain ra, dec, classprob_dsc_combmod_star.
    %          * ...,key,val,...
    %            'CatName'   - Output catsHTM name. Default 'GAIADR3classprob'.
    %            'HTM_Level' - HTM level. Default 8 (matches GAIADR3spec).
    %            'OutDir'    - Directory to build the catalog in. Default ''
    %                          (= pwd). build_htm_catalog writes to pwd, so
    %                          this temporarily cd-s there during the build.
    %            'StiltsJar' - Path to stilts.jar. Default
    %                          VO.TopCat.getStiltsJarPath().
    %            'ClassCol'  - classprob column name in the file. Default
    %                          'classprob_dsc_combmod_star'.
    %            'Verbose'   - Print progress. Default true.
    % Output : - Result struct with fields:
    %            .Nsrc    - number of sources written.
    %            .CatName - output catalog name.
    %            .CsvFile - CSV used (temp file if converted from VOTable).
    % Author : Dana Kovaleva (Jun 2026)
    % Example:
    %   cd('/home/dana/tmp/xpcols');
    %   R = VO.prep.GAIA.dr3.buildGaiaClassprobHTM('/home/dana/tmp/xpcols/sp_prob90.vot');

    arguments
        VotFile             char
        Args.CatName        char    = 'GAIADR3classprob'
        Args.HTM_Level      double  = 8
        Args.OutDir         char    = ''
        Args.StiltsJar      char    = ''
        Args.ClassCol       char    = 'classprob_dsc_combmod_star'
        Args.Verbose        logical = true
    end

    RAD = 180./pi;

    if ~isfile(VotFile)
        error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:NoFile', ...
            'Input file not found: %s', VotFile);
    end

    % --- Obtain a CSV (convert VOTable via STILTS if needed) ------------
    [~, ~, Ext] = fileparts(VotFile);
    if strcmpi(Ext, '.csv')
        CsvFile = VotFile;
    else
        Jar = Args.StiltsJar;
        if isempty(Jar)
            Jar = VO.TopCat.getStiltsJarPath();
        end
        if isempty(Jar) || ~isfile(char(Jar))
            error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:NoStilts', ...
                'stilts.jar not found (%s); pass a .csv file or set StiltsJar.', char(Jar));
        end
        CsvFile = [tempname, '.csv'];
        % keepcols drops source_id at conversion: smaller CSV and a cleaner
        % readtable (no 19-digit Long column) at tens-of-millions of rows.
        Cmd = sprintf(['java -jar "%s" tpipe in="%s" ifmt=votable ', ...
            'cmd=''keepcols "ra dec %s"'' ofmt=csv out="%s"'], ...
            char(Jar), VotFile, Args.ClassCol, CsvFile);
        if Args.Verbose
            fprintf('buildGaiaClassprobHTM: converting VOTable -> CSV via STILTS (ra/dec/%s)...\n', ...
                Args.ClassCol);
        end
        [St, Out] = system(Cmd);
        if St ~= 0 || ~isfile(CsvFile)
            error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:StiltsFailed', ...
                'STILTS conversion failed:\n%s', Out);
        end
    end

    % --- Read RA/Dec/classprob (map columns from header, read numerics
    %     with readmatrix - far faster than readtable at tens of millions) -
    Fid = fopen(CsvFile, 'r');
    if Fid < 0
        error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:NoCsv', ...
            'Cannot open CSV: %s', CsvFile);
    end
    HdrLine = fgetl(Fid);
    fclose(Fid);
    VN = strtrim(strsplit(HdrLine, ','));
    RaI    = find(strcmpi(VN, 'ra'),   1);
    DecI   = find(strcmpi(VN, 'dec'),  1);
    ClassI = find(strcmpi(VN, Args.ClassCol), 1);
    if isempty(RaI) || isempty(DecI) || isempty(ClassI)
        error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:MissingCol', ...
            'File must contain ra, dec and %s (found: %s).', ...
            Args.ClassCol, strjoin(VN, ', '));
    end

    if Args.Verbose
        fprintf('buildGaiaClassprobHTM: reading %s ...\n', CsvFile);
    end
    Data  = readmatrix(CsvFile, 'Delimiter', ',', 'NumHeaderLines', 1);
    Ra    = Data(:, RaI);
    Dec   = Data(:, DecI);
    Class = Data(:, ClassI);

    % Drop rows without a usable position.
    Good = isfinite(Ra) & isfinite(Dec);
    Mat  = [Ra(Good)./RAD, Dec(Good)./RAD, Class(Good)];   % RA/Dec -> radians

    if isempty(Mat)
        error('VO:prep:GAIA:dr3:buildGaiaClassprobHTM:Empty', ...
            'No rows with finite RA/Dec in %s.', CsvFile);
    end

    ColCell  = {'RA', 'Dec', 'classprob_dsc_combmod_star'};
    ColUnits = {'rad', 'rad', ''};

    Result = struct('Nsrc', size(Mat,1), 'CatName', Args.CatName, 'CsvFile', CsvFile);

    % --- Build the catsHTM (build_htm_catalog writes to pwd) ------------
    % onCleanup restores pwd at function exit, even on error during build.
    if ~isempty(Args.OutDir)
        if ~isfolder(Args.OutDir)
            mkdir(Args.OutDir);
        end
        OldDir = cd(Args.OutDir);
        RestoreDir = onCleanup(@() cd(OldDir));
    end

    if Args.Verbose
        fprintf('buildGaiaClassprobHTM: building %s (%d sources, level %d) in %s\n', ...
            Args.CatName, Result.Nsrc, Args.HTM_Level, pwd);
    end

    VO.prep.build_htm_catalog(Mat, 'CatName', Args.CatName, ...
        'HTM_Level', Args.HTM_Level, 'ColCell', ColCell, 'ColUnits', ColUnits);
end
