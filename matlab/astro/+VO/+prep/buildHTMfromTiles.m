function Nsrc = buildHTMfromTiles(Args)
% Build an HTM catalog from RA/Dec tile files that include a margin
% Package: VO.prep
% Description: Builds a catsHTM catalog from HDF5 tile files, each
%              holding the sources of one RA/Dec tile plus a margin of
%              the sources around it (e.g., the output of
%              VO/+prep/euclidDR1ToTiles.py). Since every tile file is
%              self-contained, each HTM cell whose center is inside a
%              tile is built from that tile file alone, with no reading
%              of neighbor files. The margin must exceed the HTM cell
%              radius, sqrt(2)*90/2^(HTM_Level-1) deg (0.25 deg at level
%              10), in Dec, and the same divided by cos(Dec) in RA.
%
%              Tile file format: dataset /data [Ncol x N] (read as
%              N x Ncol), with RA and Dec in radians, and attributes
%              MinRA, MaxRA, MinDec, MaxDec [rad] giving the exact tile
%              bounds (without the margin). The tiles must cover the
%              sky region of the catalog without gaps or overlaps.
%
%              The HDF5 cell files, the ColCell file and the index file
%              <CatName>_htm.hdf5 are written in LocalDir.
% Input  : * ...,key,val,...
%            'TileDir'     - Directory of the tile files.
%            'FilePattern' - Default is 'tile_*.hdf5'.
%            'CatName'     - Output catalog base name.
%            'ColNames'    - Cell array of column names. Default is {}:
%                            read from TileDir/columns.json.
%            'ColUnits'    - Cell array of column units. Default is {}:
%                            read from TileDir/columns.json.
%            'ColRA'       - RA column. Default is 1.
%            'ColDec'      - Dec column. Default is 2.
%            'HTM_Level'   - HTM level. Default is 10.
%            'NcatInFile'  - HTM cells per HDF5 file. Default is 100.
%            'IndStep'     - Index step for HDF5.save_cat. Default is 30.
%            'LocalDir'    - Output directory. Default is pwd.
%            'Resume'      - Skip the tiles listed in the completion log
%                            <CatName>_completed.mat. Default is true.
%            'Verbose'     - Default is true.
% Output : - Matrix [IndHTM, Nsrc] of the number of sources per cell.
% Author : Dana Kovaleva (Nov 2026)
% Example: VO.prep.buildHTMfromTiles('TileDir','~/tmp/EuclidDR1deep/tiles', ...
%              'CatName','EuclidDR1deep', 'LocalDir','~/tmp/EuclidDR1deep/htm');

    arguments
        Args.TileDir      string = ""
        Args.FilePattern  string = "tile_*.hdf5"
        Args.CatName      string = ""
        Args.ColNames     cell = {}
        Args.ColUnits     cell = {}
        Args.ColRA        double = 1
        Args.ColDec       double = 2
        Args.HTM_Level    double = 10
        Args.NcatInFile   double = 100
        Args.IndStep      double = 30
        Args.LocalDir     string = string(pwd)
        Args.Resume       logical = true
        Args.Verbose      logical = true
    end

    TileDir  = char(Args.TileDir);
    LocalDir = char(Args.LocalDir);
    CatName  = char(Args.CatName);
    if isempty(CatName)
        error('VO:prep:buildHTMfromTiles', 'CatName is required');
    end

    if isempty(Args.ColNames)
        Meta = jsondecode(fileread(fullfile(TileDir, 'columns.json')));
        Args.ColNames = Meta.ColNames(:).';
        Args.ColUnits = Meta.ColUnits(:).';
    end

    D = dir(fullfile(TileDir, char(Args.FilePattern)));
    Files = fullfile({D.folder}, {D.name});
    Nfile = numel(Files);
    if Nfile == 0
        error('VO:prep:buildHTMfromTiles', 'No tile files in %s', TileDir);
    end

    % tile bounds [rad]
    Bounds = zeros(Nfile, 4);   % [MinRA MaxRA MinDec MaxDec]
    for Ifile = 1:Nfile
        Bounds(Ifile,:) = [h5readatt(Files{Ifile}, '/data', 'MinRA'), ...
                           h5readatt(Files{Ifile}, '/data', 'MaxRA'), ...
                           h5readatt(Files{Ifile}, '/data', 'MinDec'), ...
                           h5readatt(Files{Ifile}, '/data', 'MaxDec')];
    end

    if ~exist(LocalDir, 'dir')
        mkdir(LocalDir);
    end
    OrigDir = pwd;
    cd(LocalDir);
    Cleanup = onCleanup(@() cd(OrigDir));

    HDF5.save_cat_colcell(CatName, Args.ColNames, Args.ColUnits);

    TotalTic = tic;
    [HTM, LevelHTM] = celestial.htm.htm_build(Args.HTM_Level);
    Ptr = LevelHTM(Args.HTM_Level).ptr(:);
    Coo = cat(3, HTM(Ptr).coo);
    MeanRA  = squeeze(mean(Coo(:,1,:), 1));   % the cell center used by build_htm_catalog
    MeanDec = squeeze(mean(Coo(:,2,:), 1));
    clear Coo
    if Args.Verbose
        fprintf('HTM level %d built (%.0f s), %d tiles\n', Args.HTM_Level, toc(TotalTic), Nfile);
    end

    % resume from the completion log
    CompletionLog = sprintf('%s_completed.mat', CatName);
    Nsrc = [Ptr, zeros(numel(Ptr), 1)];
    CompletedFiles = {};
    if Args.Resume && isfile(CompletionLog)
        Tmp = load(CompletionLog, 'CompletedFiles', 'Nsrc');
        CompletedFiles = Tmp.CompletedFiles;
        Nsrc = Tmp.Nsrc;
        if Args.Verbose
            fprintf('Resume: %d tiles already done\n', numel(CompletedFiles));
        end
    end
    % the first tile after a resume may be partly written
    CheckExist = ~isempty(CompletedFiles);

    LevelSub = LevelHTM;
    for Ifile = 1:Nfile
        [~, Bn] = fileparts(Files{Ifile});
        if ~ismember(Bn, CompletedFiles)
            FileTic = tic;
            InTile = MeanRA >= Bounds(Ifile,1) & MeanRA < Bounds(Ifile,2) & ...
                     MeanDec >= Bounds(Ifile,3) & MeanDec < Bounds(Ifile,4);
            Ncell = sum(InTile);
            Nrows = 0;
            if Ncell > 0
                Cat = h5read(Files{Ifile}, '/data');
                Nrows = size(Cat, 1);
                LevelSub(Args.HTM_Level).ptr = Ptr(InTile).';
                TileNsrc = VO.prep.build_htm_catalog(Cat, ...
                    'CatName', CatName, ...
                    'HTM_Level', Args.HTM_Level, ...
                    'ColRA', Args.ColRA, ...
                    'ColDec', Args.ColDec, ...
                    'ColCell', Args.ColNames, ...
                    'ColUnits', Args.ColUnits, ...
                    'DecRange', Bounds(Ifile,3:4), ...
                    'RARange', Bounds(Ifile,1:2), ...
                    'HTM', HTM, ...
                    'LevelHTM', LevelSub, ...
                    'NfilesInHDF', Args.NcatInFile, ...
                    'IndStep', Args.IndStep, ...
                    'SaveInd', false, ...
                    'CheckExist', CheckExist);
                clear Cat
                Done = ~isnan(TileNsrc(:,2));
                [~, Pos] = ismember(TileNsrc(Done,1), Ptr);
                Nsrc(Pos, 2) = TileNsrc(Done, 2);
            end
            CheckExist = false;
            CompletedFiles{end+1} = Bn; %#ok<AGROW>
            save(CompletionLog, 'CompletedFiles', 'Nsrc');
            if Args.Verbose
                fprintf('[%d/%d] %s: %d rows with margin, %d cells, %d sources in the tile (%.1f s)\n', ...
                    Ifile, Nfile, Bn, Nrows, Ncell, sum(Nsrc(InTile, 2)), toc(FileTic));
            end
        end
    end

    IndFileName = sprintf('%s_htm.hdf5', CatName);
    if isfile(IndFileName)
        delete(IndFileName);
    end
    catsHTM.saveHTMIndexFast(Args.HTM_Level, IndFileName, [], {}, Nsrc);
    if Args.Verbose
        fprintf('Index %s written; total sources %d; %.1f min\n', IndFileName, sum(Nsrc(:,2)), toc(TotalTic)/60);
    end
end
