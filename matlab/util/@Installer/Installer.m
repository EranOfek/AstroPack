% Installer class for AstroPack aux data sets
% In addition to the GitHub distribution, some additional datasets are
% available, and these datasets can be installed using this utility.
%
% The datasets and their properties are listed in the config/Installer.yml
% file.
%
% Author : Eran Ofek (Sep 2021)
% Examples:
% I = Installer;                        % create installer object
% I.seeAvailableData                    % print a table of all available datasets and description and size
% I.install                             % install all data sets [very large!]
% I.install({'Time','MinorPlanets'});   % install specific datasets
% I.install({'GAIA_SpecTemplate'});     % install specific datasets
% I.install({'cats'});                  % install specific datasets (and open tar file)
% Installer.catsHTMList                 % catsHTM catalogs available for download
% I.installCatsHTM({'FIRST','GALEXAIS'});  % download catsHTM catalogs (MD5-checked, resumable)
%
% #functions (autogen)
% Installer - constructor for the Installre class (a utility class for AstroPack installation
% getDataDir - Get data directory name DataName of data directory as appear in Insataller/seeAvailableData
% getFilesInDataDir - Return all file names in directory associated with DataName
% catsHTMList - List the catsHTM catalogs that Installer/installCatsHTM can download
% install - Install AstroPack data directories from AstroPack repository
% installCatsHTM - Download catsHTM catalogs (HDF5/HTM format) from the catsHTM server
% installSingle - Install single DataName (utility function for install)
% prep_cats - Prepare interface functions for the catalogs in the data directory Package: VO.search Description: Prepare interface functions for the catalogs in the data directory
% readElementsFileJPL - Read JPL orbital elements file
% readIERS_EOP - Read all IERS Earth Orientation File 'finals2000A.data.csv' documentation: http://hpiers.obspm.fr/eoppc/bul/bulb/explanatory.html http://maia.usno.navy.mil http://www.iers.org/nn_10968/IERS/EN/DataProducts/EarthOrientationData/eop.html?__nnn=true x_pole/y_pole:  Celestial Ephemeris Pole (CEP) relative to the International Reference Pole (IRP) are defined as x and y
% seeAvailableData - print a table of available data sets Example: I = Installer; I.seeAvailableData
% #/functions (autogen)
%

classdef Installer < Component
    % Installer class
    
    % Properties
    properties (SetAccess = public)
        Items       % From configuration
    end
    
    %--------------------------------------------------------
    methods % constructor
	
        function Obj = Installer(varargin)
            % constructor for the Installer class (a utility class for
            % AstroPack installation

            Obj.setName('Installer');
            if tools.os.iswindows
                io.msgLog(LogLevel.Info, 'Download wget for Windows from https://eternallybored.org/misc/wget/');
                io.msgLog(LogLevel.Info, 'and put wget.exe in a folder on the system path search');
            end

            % Load all items from configuration
            Obj.Items = Obj.Config.Data.Installer.Items;            
            if isempty(Obj.Items)
                error('Installer yml file is not loaded into configuration');
            end            
                        
        end
    end
    
    %--------------------------------------------------------	
    methods % main functions           
        
        function install(Obj, DataName, Args)
            % Install AstroPack data directories from AstroPack repository
            % Input  : - An Installer object.
            %          - A DataName to install (e.g., 'GAIA_SpecTemplate'),
            %            or a cell array of data names. If empty, install
            %            all data names in ConfigStruct.
            %            Default is empty.
            %          * ...,key,val,...
            %            'DataName' - A DataName to install (e.g., 'GAIA_SpecTemplate'),
            %                   or a cell array of data names. If empty, install
            %                   all data names in ConfigStruct.
            %                   If NaN then install
            %                   {'Time','MinorPlanets','MinorPlanetsCT'}.
            %                   Default is empty.
            %            'Delete' - A logical indicating if to delete
            %                   data before installation.
            %                   Default is true.
            %            'Server' - Source server: 'aws' (default) downloads from the
            %                   web; 'euclid' copies uncompressed data from the
            %                   local NFS mount (/euclid/matlab_data or
            %                   /mnt/euclid/matlab_data), skipping decompression.
            %            'Npwget' - Number of parallel wget. Default is 10.
            %            'wgetPars' - A cell array of additional wget
            %                   arguments. Default is '--quiet --timeout=10 --output-file=/dev/null --user-agent=Mozilla --no-check-certificate'.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Installer;
            %          I.install            % install all
            %          I.install(5)         % install the 5th data name: PicklesStellarSpec
            %          I.install('Time');   % install the Time data name
            %          I.install({'Time','MinorPlanets'})
            %          I.install({'cats'}, 'Server','euclid')  % copy from NFS

            arguments
                Obj
                DataName                  = {};
                Args.Delete(1,1) logical  = true;
                Args.Server               = 'aws';
                Args.Npwget               = 10;
                Args.wgetPars             = '--quiet --timeout=10 --output-file=/dev/null --user-agent=Mozilla --no-check-certificate';
            end

            % Allow index to Items, single char value, or cell with list of items
            if isnumeric(DataName)
                if isnan(DataName)
                    DataName = {'Time','MinorPlanets','MinorPlanetsCT'};
                else
                    List = fieldnames(Obj.Items);
                    DataName = List(DataName);           
                end
            elseif ischar(DataName)
                
                DataName = {DataName};
            elseif isempty(DataName)
                DataName = fieldnames(Obj.Items);
            end
                       
            io.msgLog(LogLevel.Info, 'install started: items to install: %d', numel(DataName));
            for i=1:1:numel(DataName)
                Name = DataName{i};
                if isfield(Obj.Items, Name)
                    Item = Obj.Items.(Name);                                                                         
                    try
                        Obj.installSingle(Item, 'Delete',Args.Delete, 'Server',Args.Server, 'Npwget',Args.Npwget, 'wgetPars',Args.wgetPars);
                    catch
                        io.msgLog(LogLevel.Error, 'installSingle failed: %s', Name);
                    end

                    % special treatment
                    switch Item.DataName
                        case 'cats'
                            Dir = Obj.getDataDir('cats');
                            Obj.prep_cats('Dir', Dir);
                    end                        
                else
                    io.msgLog(LogLevel.Error, 'install: unknown DataName: %s', Name);
                end
            end
            
            io.msgLog(LogLevel.Info, 'install: done');
        end
                
        function installSingle(Obj, DataStruct, Args)
            % Install single DataName (utility function for install)
            % Input  : - A structure with the DataName, SubDir, URL,
            %            SearchFile fields for the data to install
            %          * ...,key,val,...
            %            'Delete'  - Delete destination contents before install. Default is true.
            %            'Server'  - 'aws' (default) downloads from the web;
            %                        'euclid' copies from local NFS mount.
            %            'Npwget'  - Number of parallel wget. Default is 10.
            %            'wgetPars' - wget arguments string.
            % Author : Eran Ofek (Sep 2021)

            arguments
                Obj
                DataStruct
                Args.Delete(1,1) logical  = true;
                Args.Server               = 'aws';
                Args.Npwget               = 10;
                Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
            end

            % Create destination folder if needed
            PWD = pwd;
            Dir = Obj.getDataDir(DataStruct);
            if ~isfolder(Dir)
                io.msgLog(LogLevel.Info, 'creating folder: %s', Dir);
                mkdir(Dir);
            end
            cd(Dir);

            if strcmpi(Args.Server, 'euclid')
                % ---- NFS copy path ----
                NFSBase = Installer.findNFSBase();
                if isempty(NFSBase)
                    io.msgLog(LogLevel.Error, 'installSingle: no accessible NFS base path found');
                    cd(PWD);
                    return;
                end
                NFSSrc = fullfile(NFSBase, DataStruct.SubDir);
                if ~isfolder(NFSSrc)
                    io.msgLog(LogLevel.Error, 'installSingle: NFS source not found: %s', NFSSrc);
                    cd(PWD);
                    return;
                end
                if Args.Delete
                    io.msgLog(LogLevel.Info, 'deleting content before NFS copy: %s/*', Dir);
                    delete(fullfile(Dir, '*'));
                end
                io.msgLog(LogLevel.Info, 'copying from NFS: %s -> %s', NFSSrc, Dir);
                [Status, Msg] = system(sprintf("cp -r %s/* %s/", NFSSrc, Dir));
                if Status ~= 0
                    io.msgLog(LogLevel.Error, 'NFS copy failed: %s', Msg);
                    return;
                end

            else
                % ---- Web download path (original) ----
                PartsURL = regexp(DataStruct.URL,'/','split');
                Nfile    = numel(DataStruct.URL);
                if Nfile==1
                    switch lower(PartsURL{1}{end})
                        case {'index.html', 'index.htm'}
                            [List, IsDir, FileName] = www.find_urls(DataStruct.URL{1}, 'match', DataStruct.SearchFile);
                            List     = List(~IsDir);
                            FileName = FileName(~IsDir);
                        otherwise
                            List     = DataStruct.URL;
                            FileName = PartsURL{1}{end};
                    end
                else
                    List     = DataStruct.URL;
                    FileName = '*';
                end

                if numel(List) > 0
                    if Args.Delete
                        io.msgLog(LogLevel.Info, 'deleting content before reload: %s/*', pwd);
                        delete('*');
                    end
                    io.msgLog(LogLevel.Info, 'pwget: %d item(s) - %s', numel(List), List{1});

                    try
                        www.pwget(List, Args.wgetPars, Args.Npwget);
                    catch
                        io.msgLog(LogLevel.Error, 'www.pwget exception: %s', List{:});
                    end

                    % Wait for completion
                    pause(5);
                    io.files.files_arrived([], 10);

                    % Extract files
                    F = dir('*.gz');
                    for I=1:1:numel(F)
                        io.msgLog(LogLevel.Info, 'gunzip: %s in %s', F(I).name, pwd);
                        gunzip(F(I).name);
                    end
                    F = dir('*.tar');
                    for I=1:1:numel(F)
                        io.msgLog(LogLevel.Info, 'untar: %s in %s', F(I).name, pwd);
                        untar(F(I).name);
                    end
                    F = dir('*.tar.Z');
                    for I=1:1:numel(F)
                        io.msgLog(LogLevel.Info, 'untar: %s in %s', F(I).name, pwd);
                        system('uncompress *.Z');
                        untar(F(I).name);
                    end
                    F = dir('*.zip');
                    for I=1:1:numel(F)
                        io.msgLog(LogLevel.Info, 'unzip: %s in %s', F(I).name, pwd);
                        unzip(F(I).name);
                        if strcmp(DataStruct.DataName,'Starlib23')
                            !mv */* .;
                            !rmdir specs_513_fits;
                        end
                    end

                    % Delete original archives
                    try
                        io.msgLog(LogLevel.Info, 'deleting %s/*.gz', pwd);
                        delete('*.gz');
                    end
                    try
                        io.msgLog(LogLevel.Info, 'deleting %s/*.tar', pwd);
                        delete('*.tar');
                    end
                    try
                        io.msgLog(LogLevel.Info, 'deleting %s/*.zip', pwd);
                        delete('*.zip');
                    end
                end
            end

            cd(PWD);
        end
        
        function Result = seeAvailableData(Obj)
            % print a table of available data sets
            % Example: I = Installer; I.seeAvailableData
            
            % The long way, until I find how to do it shorter
            DataName = {};
            SubDir = {};
            Size = {};
            Description = {};
            URL = {};
            
            Names = fieldnames(Obj.Items);
            Cel = {};
            for i=1:numel(Names)
                 Item = Obj.Items.(Names{i});
%                 DataName{end+1} = Item.DataName;
%                 SubDir{end+1} = Item.SubDir;
%                 Size{end+1} = Item.Size;
%                 Description{end+1} = Item.Description;
%                 URL{end+1} = Item.URL;
                
                Cel{i, 1} = {Item.DataName};
                Cel{i, 2} = Item.SubDir;
                Cel{i, 3} = Item.Size;
                Cel{i, 4} = Item.Description;
                Cel{i, 5} = Item.URL{:};                
            end

            Result = cell2table(Cel, 'VariableNames', {'DataName', 'SubDir', 'Size [MB]', 'Description', 'URL'} );

             
%              Result = cell2table(...
%              [ {DataName}', {SubDir}', {Size}', {Description}', {URL}' ], ...
%                  'VariableNames', {'DataName', 'SubDir', 'Size [MB]', 'Description', 'URL'} );
                
%             Result = cell2table([{Obj.DataName}', ...
%                    {Obj.SubDir}', ...
%                    {Obj.Size}', ...
%                    {Obj.Description}',...
%                    {Obj.URL}'], 'VariableNames',{'DataName', 'SubDir', 'Size [MB]', 'Description', 'URL'});
            
        end
            
        function Dir = getDataDir(Obj, Item)
            % Get data directory name
            % DataName of data directory as appear in Insataller/seeAvailableData
            % Input  : - Installre object.
            %          - DataName
            % Output : - Directory name. Return empty if not found.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Insatller; I.getDataDir('Time')
            if ischar(Item) && isfield(Obj.Items, Item)
                Item = Obj.Items.(Item);
            end
            
            if ~isempty(Item)
                if tools.os.iswindows
                    Path = Obj.Config.Data.Installer.Default.InstallationLocationWindows;
                else
                    Path = Obj.Config.Data.Installer.Default.InstallationLocation;                    
                end                               
                Dir = fullfile(Path, Item.SubDir);
            else
                Dir = '';
            end
        end
   
        function [Files, Dir] = getFilesInDataDir(Obj, Name)
            % Return all file names in directory associated with DataName
            % Input  : - Installer object.
            %          - DataName
            % Output : - dir-function like output of all the file names in
            %            the data directory. Directories are removed.
            %          - Data dir containing the files.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Insatller; I.getFilesInDataDir('SpecGalQSO')

            Dir = Obj.getDataDir(Name);
            if isfolder(Dir)
                PWD = pwd;
                cd(Dir);
                Files = dir('*');
                Files = Files(~[Files.isdir]);
                cd(PWD);
            else
                Files = [];
            end
        end

        function Result = installCatsHTM(Obj, CatName, Args)
            % Download catsHTM catalogs (HDF5/HTM format) from the catsHTM server
            %   Each catalog goes into its own sub-directory of OutDir, as on
            %   the server (e.g., <OutDir>/GAIA/DR3/), which is the layout
            %   startup.m expects: startup('AstroPack_CatsHTMPath',OutDir).
            %   The file list and MD5 sums are read from the server checksum
            %   list (list.euler.checksum._<Dir>_); a copy of it is saved in
            %   the catalog directory. Files already present with the right
            %   MD5 are skipped, so an interrupted download resumes when the
            %   same command is run again. Files that fail the MD5 check are
            %   downloaded again, up to Nretry times.
            % Input  : - An Installer object.
            %          - catsHTM catalog name (e.g., 'GAIADR3'; case
            %            insensitive), a cell or string array of names, or
            %            'all' for all active catalogs in catsHTM.catalogs
            %            (tens of TB). See Installer.catsHTMList for names.
            %          * ...,key,val,...
            %            'OutDir' - catsHTM root directory. If empty, use the
            %                   ASTROPACK_CATSHTM_PATH environment variable, and
            %                   if it is not set, Installer.yml
            %                   Default.CatsHTMLocation. Default is ''.
            %            'Server' - 'web' downloads from BaseURL with wget (or
            %                   curl if wget is missing); 'euclid' copies from
            %                   the NFS mount (/euclid/catsHTM or
            %                   /mnt/euclid/catsHTM). Default is 'web'.
            %            'BaseURL' - catsHTM server URL. If empty, use
            %                   Installer.yml Default.CatsHTMURL. Default is ''.
            %            'Npar' - Number of parallel downloads (and MD5
            %                   checks). Default is 10.
            %            'CheckMD5' - Verify the MD5 of existing and downloaded
            %                   files. If false, existing files are skipped by
            %                   name and downloads are only checked to be
            %                   non-empty. Default is true.
            %            'Nretry' - Number of retries for files that failed.
            %                   Default is 2.
            %            'CheckCertificate' - Verify the server SSL
            %                   certificate. Default is true.
            %            'DryRun' - Only report what would be downloaded.
            %                   Default is false.
            % Output : - A table with one row per catalog: Name, Dir, Nfiles,
            %            Nskipped (already present and valid), Ngot
            %            (downloaded now), Nfailed, and Message.
            % Author : Dana Kovaleva (Oct 2026)
            % Example: I = Installer;
            %          I.installCatsHTM('ROSATfsc', 'DryRun',true)
            %          I.installCatsHTM('FIRST', 'OutDir','~/catsHTM')
            %          I.installCatsHTM({'GAIADR3','GALEXAIS'})
            %          I.installCatsHTM('PS1', 'Server','euclid')   % copy from NFS

            arguments
                Obj
                CatName
                Args.OutDir char                   = '';
                Args.Server char                   = 'web';
                Args.BaseURL char                  = '';
                Args.Npar(1,1) double              = 10;
                Args.CheckMD5(1,1) logical         = true;
                Args.Nretry(1,1) double            = 2;
                Args.CheckCertificate(1,1) logical = true;
                Args.DryRun(1,1) logical           = false;
            end

            Args.Server = validatestring(Args.Server, {'web','euclid'});
            Default = Obj.Config.Data.Installer.Default;
            if isempty(Args.OutDir)
                Args.OutDir = getenv('ASTROPACK_CATSHTM_PATH');
            end
            if isempty(Args.OutDir)
                if ~isfield(Default, 'CatsHTMLocation')
                    error('Installer:installCatsHTM:Config', 'Installer.yml lacks Default.CatsHTMLocation - pass OutDir');
                end
                Args.OutDir = Default.CatsHTMLocation;
            end
            Args.OutDir = expandHome(Args.OutDir);
            if isempty(Args.BaseURL)
                if ~isfield(Default, 'CatsHTMURL')
                    error('Installer:installCatsHTM:Config', 'Installer.yml lacks Default.CatsHTMURL - pass BaseURL');
                end
                Args.BaseURL = Default.CatsHTMURL;
            end
            Args.BaseURL = regexprep(Args.BaseURL, '/+$', '');

            SrcRoot = '';
            if strcmp(Args.Server, 'euclid')
                Candidates = {'/euclid/catsHTM', '/mnt/euclid/catsHTM'};
                Found = find(cellfun(@isfolder, Candidates), 1);
                if isempty(Found)
                    error('Installer:installCatsHTM:NoNFS', 'No catsHTM NFS mount found (%s)', strjoin(Candidates, ', '));
                end
                SrcRoot = Candidates{Found};
            end

            % catalog names -> registry entries
            Cats    = catsHTM.catalogs;
            CatName = cellstr(CatName);
            if isscalar(CatName) && strcmpi(CatName{1}, 'all')
                Active = arrayfun(@(D) isequal(D.Status,true) && isequal(D.iscatsHTM,true), Cats);
                Cats   = Cats(Active);
            else
                [Found, Ind] = ismember(lower(CatName), lower({Cats.Name}));
                if ~all(Found)
                    error('Installer:installCatsHTM:UnknownCat', 'Unknown catsHTM catalog(s): %s (see Installer.catsHTMList)', ...
                          strjoin(CatName(~Found), ', '));
                end
                Cats = Cats(Ind);
            end

            Ncat   = numel(Cats);
            Result = table('Size',[Ncat 7], 'VariableTypes',{'string','string','double','double','double','double','string'}, ...
                           'VariableNames',{'Name','Dir','Nfiles','Nskipped','Ngot','Nfailed','Message'});
            io.msgLog(LogLevel.Info, 'installCatsHTM: %d catalog(s) -> %s', Ncat, Args.OutDir);
            for Icat=1:1:Ncat
                Result(Icat,:) = installOneCatsHTM(Cats(Icat), Args, SrcRoot);
            end
            if ~Args.DryRun
                io.msgLog(LogLevel.Info, 'installCatsHTM: done; to use: startup(''AstroPack_CatsHTMPath'',''%s'')', Args.OutDir);
            end
        end

    end

    %--------------------------------------------------------
    methods (Static)
        
        function T = readElementsFileJPL(FileName, Type)
            % Read JPL orbital elements file
            % Input  : - File name.
            %            The file name is assumed to be in the data/
            %            directory as indicated in the Installer.yml config
            %            file.
            %          - File type: 'comet' | 'number' | 'unnum'
            %            If empty, will automatically figure out the file
            %            yppe. Default is [].
            % Output : - Table with orbital elements.
            % Author : Eran Ofek (Sep 2021)
            % Example: T = Installer.readElementsFileJPL('ELEMENTS.COMET');
            %          T = Installer.readElementsFileJPL('ELEMENTS.NUMBR');
            %          T = Installer.readElementsFileJPL('ELEMENTS.UNNUM');
            
            arguments
                FileName char
                Type             = [];
            end
            
            % CD to data directory
            PWD = pwd;
            I = Installer;
            PathI = I.getDataDir('MinorPlanets');
            FullFileName = sprintf('%s%s%s', PathI, filesep, FileName);
            
%             Ind = find(strcmp({I.DataName}, 'MinorPlanets'));
%             Ind = find(strcmp({I.DataName}, 'MinorPlanets'));
%             cd(I(Ind).InstallationLocation);
%             cd(I(Ind).SubDir);
                        
            FID = fopen(FullFileName,'r');
            Line = fgetl(FID);
            fclose(FID);
            ColNames = regexp(Line, '\s*', 'split');
            ColNames = ColNames(~cellfun(@isempty, ColNames));
            
            if isempty(Type)
                SplitedFileName = split(FileName, '.');
                Type            = SplitedFileName{2};
            end
                
                
            switch lower(Type)
                case 'comet'
                    % comets file
                    Format = '%43s %f %f %f %f %f %f %f %s\n';
                    ColNames = ColNames(2:end);
                    ColNames{1} = 'Designation';
                case {'numbr','number'}
                    % numbered asteroids file
                    Format = '%f %17s %f %f %f %f %f %f %f %f %f %s\n';
                    ColNames{1} = 'Number';
                    ColNames{2} = 'Designation';
                case 'unnum'
                    % un-numbered asteroids file
                    Format = '%12s %f %f %f %f %f %f %f %f %f %s\n';
            end
            
            FID = fopen(FullFileName,'r');
            C = textscan(FID, Format, 'Delimiter','\t', 'Headerlines',2);
            fclose(FID);
            
            T = table(C{:});
            
            T.Properties.VariableNames = ColNames;
            if any(strcmp(T.Properties.VariableNames, 'Designation'))
                T.Designation = strtrim(T.Designation);
            end
            if any(strcmp(T.Properties.VariableNames, 'Name'))
                T.Name = strtrim(T.Name);
            end
            if any(strcmp(T.Properties.VariableNames, 'Ref'))
                T.Ref = strtrim(T.Ref);
            end
            
            cd(PWD);
            
        end
                
        function [T1, T2, T3] = readIERS_EOP(FileInd)
            % Read all IERS Earth Orientation File 'finals2000A.data.csv'
            %   documentation: http://hpiers.obspm.fr/eoppc/bul/bulb/explanatory.html
            %   http://maia.usno.navy.mil
            %   http://www.iers.org/nn_10968/IERS/EN/DataProducts/EarthOrientationData/eop.html?__nnn=true
            %   x_pole/y_pole:  Celestial Ephemeris Pole (CEP) relative to the International Reference Pole (IRP) are defined as x and y
            %   dPsi/dEps - offset relative to IAU 1980 Theory of Nutation
            %       Utilities to use these files are available in the
            %       celestial.time package.
            % Input  : - File indices to read [1 2 3]. If not provided read
            %            all files.
            % Output : - The 'finals2000A table including
            %            UT1-UTC, X-pole, Y-pole, dPsi, dEpsilon, etc.
            %            from 1992 till ~3 months predictions into the future.
            %          - The EOP_14_C04_IAU1980_one_file_1962-now.txt file.
            %            Similar to finals, but from 1962 till now.
            %          - The EOP_C01_IAU2000_1846-now.txt file.
            %            First 6 columns from 1846 till now, including
            %            UT1-TAI.
            % Author : Eran Ofek (Sep 2021)
            % Example: [T1, T2, T3] = Installer.readIERS_EOP
            
            arguments
                FileInd = [1, 2, 3];
            end
            
            Finals   = 'finals2000A.data.csv';
            EOP_1962 = 'EOP_14_C04_IAU1980_one_file_1962-now.txt';
            EOP_1846 = 'EOP_C01_IAU2000_1846-now.txt';
            
            %PWD = pwd;
            %I = Installer;
            %cd(I.ConfigStruct.InstallationLocation);
            %IndTime = strcmp(I.ConfigStruct.DataName,'Time');
            %cd(I.ConfigStruct.SubDir{IndTime});
            
            if any(FileInd==1)
                T1 = io.files.readtable1(Finals);
            else
                T1 = [];
            end
            
            
            if any(FileInd==2)
                T2 = io.files.readtable1(EOP_1962);
                ColNames = {'Year','Month','Day','MJD','x','y','UT1_UTC','LOD','dPsi','dEps',      'xErr','yErr',    'UT1_UTCErr','LOD_Err','dPsiErr','dEpsilonErr'};
                ColUnits = {'',   '',     '',   'day','arcsec','arcsec','s','s','arcsec','arcsec','arcsec','arcsec','s','s','arcsec','arcsec'};
                T2.Properties.VariableNames = ColNames;
                T2.Properties.VariableUnits = ColUnits;
            else
                T2 = [];
            end
            
            if any(FileInd==3)
                T3 = io.files.readtable1(EOP_1846,'ReadVariableNames',false);
                T3 = T3(:,1:6);
                ColNames = {'MJD','PM_X','PM_Y','UT1_TAI','DX','DY'};
                ColUnits = {'day','arcsec','arcsec','s','arcsec','arcsec'};
                T3.Properties.VariableNames = ColNames;
                T3.Properties.VariableUnits = ColUnits;
            else
                T3 = [];
            end
        end
     
        function prep_cats(Args)
            % Prepare interface functions for the catalogs in the data directory
            % Package: VO.search
            % Description: Prepare interface functions for the catalogs in the data directory
            % Input  : * Arbitrary number of pairs if ...,keyword,value,...
            %            The possible keywords are possible:
            %            'Dir' - Directory in which the data catalogs resides.
            %                    The directory tree should be stored like a package.
            %                    each directory start with "+".
            %                    Default is '~/matlab/data/+datacats/';
            %            'Exten' - Cell array of file extensions to map.
            %                    Default is {'mat'}.
            % Output : null
            % License: GNU general public license version 3
            %     By : Eran O. Ofek                    Feb 2017
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: Installer.prep_cats

            arguments
                Args.Dir = [];
                Args.Exten = {'mat', 'fits'};
            end

            Next = numel(Args.Exten);
            PWD = pwd;

            for Iext=1:1:Next
                cd(Args.Dir);
                List = io.files.rdir(sprintf('*.%s',Args.Exten{Iext}));

                for If=1:1:numel(List)
                    cd(List(If).folder);

                    %--- Generate the interface file ---
                    ProgName = regexprep(List(If).name,Args.Exten{Iext},'m');
                    FunName  = regexprep(ProgName,'.m','');
                    
                    % print status to screen
                    io.msgLog(LogLevel.Info, 'prep_cats: Generating function %s\n', FunName);

                    switch lower(Args.Exten{Iext})
                        case 'mat'
                            % create and write function
                            FID=fopen(ProgName,'w');
                            fprintf(FID,'function varargout=%s(varargin)\n',FunName);
                            fprintf(FID,'%% Interface function for catalog: %s\n',FunName);
                            fprintf(FID,'%% Auto generated by %s\n',mfilename);
                            fprintf(FID,'\n');
                            fprintf(FID,'Nout = nargout;\n');
                            fprintf(FID,'[varargout{1:Nout}]=VO.search.catalog_interface(''%s'',''%s'',varargin{:});\n',List(If).name,List(If).folder);
                            fclose(FID);
                            
                        case 'fits'
                            % create and write FITS read function
                            FID=fopen(ProgName,'w');
                            fprintf(FID,'function varargout=%s(varargin)\n',FunName);
                            fprintf(FID,'%% Interface function for FITS file: %s\n',FunName);
                            fprintf(FID,'%% Auto generated by %s\n',mfilename);
                            fprintf(FID,'\n');
                            fprintf(FID,'Nout = nargout;\n');
                            fprintf(FID,'[varargout{1:Nout}]=FITS.fitsread(''%s%s%s'',varargin{:});\n',List(If).folder,filesep,List(If).name);
                            fclose(FID);

                        otherwise
                            error('Unknown Extension option');
                    end

                end
            end
            cd(PWD);

        end
    
        function Result = dataDir()
            % get data dir (static method)
            %   See also Installer/getDataDir
            % Input  : null
            % Output : - data dir
            % Author : Eran Ofek (Aug 2025)
            % Example: Installer.dataDir()

            Result = fileparts(mfilename('fullpath'));
            Result = sprintf('%s%s..%s..%s..%s..%sdata',Result, filesep, filesep, filesep, filesep, filesep);

        end

        function NFSBase = findNFSBase()
            % Return the first accessible NFS base path, or [] if none found.
            % Checks /euclid/matlab_data and /mnt/euclid/matlab_data in order.
            % Example: NFSBase = Installer.findNFSBase()

            Candidates = {'/euclid/matlab_data', '/mnt/euclid/matlab_data'};
            NFSBase = [];
            for I = 1:numel(Candidates)
                if isfolder(Candidates{I})
                    NFSBase = Candidates{I};
                    return;
                end
            end
        end

        function T = catsHTMList(Args)
            % List the catsHTM catalogs that Installer/installCatsHTM can download
            % Input  : * ...,key,val,...
            %            'All' - Include inactive registry entries.
            %                   Default is false.
            % Output : - Table with Name, Dir, Desc, Active.
            % Author : Dana Kovaleva (Oct 2026)
            % Example: T = Installer.catsHTMList

            arguments
                Args.All(1,1) logical = false;
            end

            Cats   = catsHTM.catalogs;
            Active = arrayfun(@(D) isequal(D.Status,true) && isequal(D.iscatsHTM,true), Cats);
            if ~Args.All
                Cats   = Cats(Active);
                Active = Active(Active);
            end
            Desc   = cellfun(@char, {Cats.Desc}, 'UniformOutput',false);   % a missing Desc is []
            T = table(string({Cats.Name}).', string({Cats.Dir}).', string(Desc).', Active(:), ...
                      'VariableNames',{'Name','Dir','Desc','Active'});
        end

    end
  	
    methods (Static)  % compile mex files
        function compileAllMex
            % Compile all the *.c and *.cpp in the AstroPack/matlab toolbox (Static)
            % Author : Eran Ofek (Jan 2022)
            % Example: Installer.compileAllMex
            
            Path = mfilename('fullpath');
            Splitted = split(Path, filesep);
            Path = tools.string.unsplit(Splitted(1:end-3), filesep);
            
            PWD = pwd;
            cd(Path)
            
            List.C   = io.files.rdir('*.c'); 
            List.CPP = io.files.rdir('*.cpp');
            
            Fields  = fieldnames(List);
            Nfields = numel(Fields);
            for Ifield=1:1:Nfields
                Nc = numel(List.(Fields{Ifield}));
                for Ic=1:1:Nc
                    Folder = List.(Fields{Ifield})(Ic).folder;
                    Name   = List.(Fields{Ifield})(Ic).name;
                    cd(Folder);
                    %Name
                    mex('-O', Name);
                end
            end
           
            cd(PWD);
        end
    end
    
    methods(Static) % unitTest
        Result = unitTest(Obj)
            % Unit test
            
   end
    
end

%--------------------------------------------------------
% Local functions for Installer/installCatsHTM

function Row = installOneCatsHTM(Cat, Args, SrcRoot)
    % Install one catsHTM catalog; return a Result-table row (cell)
    Dir      = strip(Cat.Dir, '/');                    % e.g., 'GAIA/DR3'
    ListName = sprintf('list.euler.checksum._%s_', strrep(Dir, '/', '_'));
    DestDir  = fullfile(Args.OutDir, Dir);
    Row      = {string(Cat.Name), string(Dir), NaN, NaN, NaN, NaN, ""};

    [Md5, Files, Text, Msg] = readChecksumList(ListName, Args, SrcRoot);
    if isempty(Msg) && isempty(Files)
        Msg = sprintf('empty checksum list %s', ListName);
    end

    if ~isempty(Msg)
        io.msgLog(LogLevel.Error, 'installCatsHTM: %s: %s', Cat.Name, Msg);
        Row{7} = string(Msg);
    else
        Nfile  = numel(Files);
        Have   = filesValid(DestDir, Files, Md5, Args);
        Failed = find(~Have);
        io.msgLog(LogLevel.Info, 'installCatsHTM: %s: %d file(s), %d present, %d to get -> %s', ...
                  Cat.Name, Nfile, sum(Have), numel(Failed), DestDir);
        Row(3:4) = {Nfile, sum(Have)};
        if Args.DryRun
            Row{7} = string(sprintf('dry run: %d file(s) to get', numel(Failed)));
        else
            if ~isfolder(DestDir)
                mkdir(DestDir);
            end
            Ngot = 0;
            Iter = 0;
            while ~isempty(Failed) && Iter<=Args.Nretry
                fetchFiles(Dir, Files(Failed), DestDir, Args, SrcRoot);
                OK     = filesValid(DestDir, Files(Failed), Md5(Failed), Args);
                Ngot   = Ngot + sum(OK);
                Failed = Failed(~OK);
                Iter   = Iter + 1;
            end
            % keep the checksum list with the catalog, for later checks
            FID = fopen(fullfile(DestDir, ListName), 'w');
            fwrite(FID, Text, 'char');
            fclose(FID);

            Row(5:6) = {Ngot, numel(Failed)};
            if isempty(Failed)
                Row{7} = "ok";
            else
                Row{7} = string(sprintf('failed: %s', strjoin(Files(Failed(1:min(5,end))), ', ')));
                io.msgLog(LogLevel.Error, 'installCatsHTM: %s: %d file(s) failed after %d retries', ...
                          Cat.Name, numel(Failed), Args.Nretry);
            end
        end
    end
end

function [Md5, Files, Text, Msg] = readChecksumList(ListName, Args, SrcRoot)
    % Read a catsHTM checksum list ('<md5>  <file>' per line) from the
    % server or the NFS mount. Only plain file names are accepted.
    Md5   = {};
    Files = {};
    Text  = '';
    Msg   = '';
    try
        if strcmp(Args.Server, 'euclid')
            Text = fileread(fullfile(SrcRoot, ListName));
        else
            Opt  = weboptions('ContentType','text', 'Timeout',60);
            if ~Args.CheckCertificate
                Opt.CertificateFilename = '';
            end
            Text = webread(sprintf('%s/%s', Args.BaseURL, ListName), Opt);
        end
    catch ME
        Msg = sprintf('cannot read %s: %s', ListName, ME.message);
    end
    if isempty(Msg)
        Tok = regexp(Text, '^([0-9a-fA-F]{32})\s+\*?([A-Za-z0-9_.+\-]+)\s*$', 'tokens', 'lineanchors');
        if ~isempty(Tok)
            Tok   = vertcat(Tok{:});
            Md5   = lower(Tok(:,1));
            Files = Tok(:,2);
        end
    end
end

function OK = filesValid(DestDir, Files, Md5, Args)
    % true for files that exist and have the expected MD5 (or, with
    % CheckMD5=false, exist and are non-empty)
    Full  = fullfile(DestDir, Files(:));
    OK    = isfile(Full);
    Exist = find(OK);
    if ~isempty(Exist)
        if Args.CheckMD5
            Got       = fileMD5(Full(Exist), Args.Npar);
            OK(Exist) = strcmp(Got, Md5(Exist));
        else
            OK(Exist) = cellfun(@(F) getfield(dir(F), 'bytes'), Full(Exist)) > 0;
        end
    end
end

function Hash = fileMD5(Files, Npar)
    % MD5 (lower-case hex) of each file; md5sum in parallel when available,
    % otherwise Java
    Hash = repmat({''}, numel(Files), 1);
    if isunix && system('command -v md5sum >/dev/null 2>&1')==0
        ListFile = writeNulList(Files);
        [~, Out] = system(sprintf('xargs -0 -P %d -n 20 md5sum < %s', Npar, shellQuote(ListFile)));
        delete(ListFile);
        Tok = regexp(Out, '^([0-9a-f]{32}) [ *](.+?)$', 'tokens', 'lineanchors');
        if ~isempty(Tok)
            Tok = vertcat(Tok{:});
            [Found, Ind] = ismember(Files, Tok(:,2));
            Hash(Found)  = Tok(Ind(Found), 1);
        end
    else
        for I=1:1:numel(Files)
            Hash{I} = javaMD5(Files{I});
        end
    end
end

function Hash = javaMD5(File)
    % MD5 of one file with java.security.MessageDigest
    Digest = java.security.MessageDigest.getInstance('MD5');
    FID    = fopen(File, 'r');
    Chunk  = 2^24;
    Done   = false;
    while ~Done
        Bytes = fread(FID, Chunk, '*uint8');
        if ~isempty(Bytes)
            Digest.update(typecast(Bytes, 'int8'));
        end
        Done = numel(Bytes) < Chunk;
    end
    fclose(FID);
    Hash = lower(reshape(dec2hex(typecast(Digest.digest, 'uint8'), 2).', 1, []));
end

function fetchFiles(Dir, Files, DestDir, Args, SrcRoot)
    % Get Files of catalog Dir into DestDir (overwriting), in chunks so
    % that progress is reported
    Nfile = numel(Files);
    Chunk = 200;
    for I1=1:Chunk:Nfile
        Ind = I1:min(I1+Chunk-1, Nfile);
        Out = fullfile(DestDir, Files(Ind));
        if strcmp(Args.Server, 'euclid')
            Src = fullfile(SrcRoot, Dir, Files(Ind));
            for I=1:1:numel(Src)
                [St, Msg] = copyfile(Src{I}, Out{I}, 'f');
                if ~St
                    io.msgLog(LogLevel.Warning, 'installCatsHTM: copy failed %s: %s', Src{I}, Msg);
                end
            end
        else
            URL = strcat(Args.BaseURL, '/', Dir, '/', Files(Ind));
            webGet(URL, Out, Args);
        end
        io.msgLog(LogLevel.Info, 'installCatsHTM: %s: %d/%d', Dir, Ind(end), Nfile);
    end
end

function webGet(URL, Out, Args)
    % Download URL{i} -> Out{i}: wget/curl in parallel (xargs) on unix,
    % sequential websave otherwise. Failures are caught by the MD5 check.
    if isunix && system('command -v wget >/dev/null 2>&1')==0
        Get = 'wget -q --tries=3 -U Mozilla/5.0';
        if ~Args.CheckCertificate
            Get = [Get ' --no-check-certificate'];
        end
        Get = [Get ' -O "$2" "$1"'];
    elseif isunix && system('command -v curl >/dev/null 2>&1')==0
        Get = 'curl -fsSL --retry 2 -A Mozilla/5.0';
        if ~Args.CheckCertificate
            Get = [Get ' -k'];
        end
        Get = [Get ' -o "$2" "$1"'];
    else
        Get = '';
    end

    if ~isempty(Get)
        Pairs    = reshape([URL(:) Out(:)].', [], 1);
        ListFile = writeNulList(Pairs);
        [St, Msg] = system(sprintf('xargs -0 -n 2 -P %d sh -c %s sh < %s', ...
                           Args.Npar, shellQuote([Get ' || rm -f "$2"']), shellQuote(ListFile)));
        delete(ListFile);
        if St~=0
            io.msgLog(LogLevel.Warning, 'installCatsHTM: some downloads failed (will retry): %s', strtrim(Msg));
        end
    else
        Opt = weboptions('Timeout',300);
        if ~Args.CheckCertificate
            Opt.CertificateFilename = '';
        end
        for I=1:1:numel(URL)
            try
                websave(Out{I}, URL{I}, Opt);
            catch ME
                io.msgLog(LogLevel.Warning, 'installCatsHTM: %s: %s', URL{I}, ME.message);
            end
        end
    end
end

function ListFile = writeNulList(Items)
    % write a NUL-separated list (for xargs -0) to a temporary file
    ListFile = [tempname '.lst'];
    FID = fopen(ListFile, 'w');
    fwrite(FID, unicode2native([strjoin(Items(:).', char(0)) char(0)], 'UTF-8'));
    fclose(FID);
end

function Str = shellQuote(Str)
    % single-quote a string for sh
    Str = ['''' strrep(Str, '''', '''\''''') ''''];
end

function P = expandHome(P)
    % expand a leading '~' (system() and some file functions do not)
    if startsWith(P, '~')
        P = [char(java.lang.System.getProperty('user.home')) P(2:end)];
    end
end
