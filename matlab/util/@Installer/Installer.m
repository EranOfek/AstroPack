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
%
% #functions (autogen)
% Installer - constructor for the Installre class (a utility class for AstroPack installation
% getDataDir - Get data directory name DataName of data directory as appear in Insataller/seeAvailableData
% getFilesInDataDir - Return all file names in directory associated with DataName
% install - Install AstroPack data directories from AstroPack repository
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
            %                   Default is empty.
            %            'Delete' - A logical indicating if to delete
            %                   data before installation.
            %                   Default is true.
            %            'Npwget' - Number of parallel wget. Default is 10.
            %            'wgetPars' - A cell array of additional wget
            %                   arguments. Default is '--quiet --timeout=10 --output-file=/dev/null --user-agent=Mozilla --no-check-certificate'.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Installer;
            %          I.install            % install all
            %          I.install(5)         % install the 5th data name: PicklesStellarSpec
            %          I.install('Time');   % install the Time data name
            %          I.install({'Time','MinorPlanets'})           
            
            arguments
                Obj
                DataName                  = {};
                Args.Delete(1,1) logical  = true;
                Args.Npwget               = 10;
                Args.wgetPars             = '--quiet --timeout=10 --output-file=/dev/null --user-agent=Mozilla --no-check-certificate';
            end

            % Allow index to Items, single char value, or cell with list of items
            if isnumeric(DataName)
                List = fieldnames(Obj.Items);
                DataName = List(DataName);           
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
                        Obj.installSingle(Item, 'Delete',Args.Delete, 'Npwget',Args.Npwget, 'wgetPars',Args.wgetPars);
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
            % Author : Eran Ofek (Sep 2021)
            % @Todo: Why static???
        
            arguments
                Obj
                DataStruct
                Args.Delete(1,1) logical  = true;
                Args.Npwget               = 10;
                Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
            end
            
            % Create folder if not exist
            PWD = pwd;
            Dir = Obj.getDataDir(DataStruct);          
            if ~isfolder(Dir)
                io.msgLog(LogLevel.Info, 'creating folder: %s', Dir);
                mkdir(Dir);
            end            
            cd(Dir);
       
            %
            PartsURL = regexp(DataStruct.URL,'/','split');
            Nfile    = numel(DataStruct.URL);
            if Nfile==1
                % check if file is index.html
                switch lower(PartsURL{1}{end})
                    case {'index.html', 'index.htm'}
                        % get all files in dir

                        [List, IsDir, FileName] = www.find_urls(DataStruct.URL{1}, 'match', DataStruct.SearchFile);
                        List     = List(~IsDir);
                        FileName = FileName(~IsDir);
                    otherwise
                        % direct file loading
                        List     = DataStruct.URL;  % cell
                        FileName = PartsURL{1}{end};
                end
            else
                % retrieve individual files (index.html is not supported)
                List     = DataStruct.URL;
                FileName = '*';
            end
            
            if numel(List) > 0
                % delete content before reload
                if Args.Delete
                    io.msgLog(LogLevel.Info, 'deleting content before reload: %s/*', pwd);
                    delete('*');
                end
                if numel(List) > 0
                    io.msgLog(LogLevel.Info, 'pwget: %d item(s) - %s', numel(List), List{1});
                end
                
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
