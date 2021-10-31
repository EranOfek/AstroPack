% Installer class for AstroPack aux data sets
% In addition to the GitHub distribution, some additional datasets are
% available, and these datasets can be installed using this utility.
%
% The datasets and their properties are listed in the config/Installer.yml
% file.
%
% Author : Eran Ofek (Sep 2021)
% Examples:
% I = Installer;  % create installer object
% I.seeAvailableData      % print a table of all available datasets and description and size
% I.install               % install all data sets [very large!]
% I.install({'Time','MinorPlanets'});
% I.install({'GAIA_SpecTemplate'}); % install specific datasets
% I.install({'cats'}); % install specific datasets (and open tar file)

% #functions (autogen)
% Installer - constructor for the Installre class (a utility class for AstroPack installation
% getDataDir - Get data directory name DataName of data directory as appear in Insataller/seeAvailableData
% getFilesInDataDir - Return all file names in directory associated with DataName
% install - Install AstroPack data directories from AstroPack repository
% installSingle - Install single DataName (utility function for install)
% prep_cats - Prepare interface functions for the catalogs in the data directory Package: VO.search Description: Prepare interface functions for the catalogs in the data directory
% readElementsFileJPL - Read JPL orbital elements file
% readIERS_EOP - Read all IERS Earth Orientation File 'finals2000A.data.csv' documentation: http://hpiers.obspm.fr/eoppc/bul/bulb/explanatory.html http://maia.usno.navy.mil http://www.iers.org/nn_10968/IERS/EN/DataProducts/EarthOrientationData/eop.html?__nnn=true x_pole/y_pole:  Celestial Ephemeris Pole (CEP) relative to the International Reference Pole (IRP) are defined as x and y
% search - Search data name in Installer object
% seeAvailableData - print a table of available data sets Example: I = Installer; I.seeAvailableData
% #/functions (autogen)
%


classdef Installer < Component
    % Installer class
    
    % Properties
    properties (SetAccess = public)
        DataName                    %
        InstallationLocation        %
        SubDir                      %
        URL                         %
        Size                        %
        SearchFile                  %
        Description                 %
        ConfigFile                  %
    end
    
    %--------------------------------------------------------
    methods % constructor
        function Obj = Installer(DataName)
            % constructor for the Installre class (a utility class for
            % AstroPack installation
            
            arguments
                DataName  = [];     %
            end

            if isnumeric(DataName) && ~isempty(DataName)
                Obj.DataName = [];
            else
                if isempty(DataName)
                    % load all files
                    DataName = fieldnames(Obj.Config.Data.Installer);
                    if isempty(DataName)
                        error('Installer yml files are not loaded into configuration');
                    end
                end
            
                % Since Config.Data.Installer contains also the generated 
                % 'FileName' field, we should not iterate it
                % @Todo: Find better solution, what if we have more fields
                % like this???
                Ndn = numel(DataName)-1;
                ConfigStruct = Obj.Config.Data.Installer;
                for Idn=1:1:Ndn
                    
                    % Create new element in array
                    Obj(Idn) = Installer(1);
                    
                    % Set data from configuration
                    Obj(Idn).DataName             = ConfigStruct.(DataName{Idn}).DataName;
                    Obj(Idn).InstallationLocation = ConfigStruct.(DataName{Idn}).InstallationLocation;
                    Obj(Idn).SubDir               = ConfigStruct.(DataName{Idn}).SubDir;
                    Obj(Idn).URL                  = ConfigStruct.(DataName{Idn}).URL;
                    Obj(Idn).Size                 = ConfigStruct.(DataName{Idn}).Size;
                    Obj(Idn).SearchFile           = ConfigStruct.(DataName{Idn}).SearchFile;
                    Obj(Idn).Description          = ConfigStruct.(DataName{Idn}).Description;
                end
            end
            
        end
    end
    
    
    methods % setters/getters
    
       
    end
    
    methods (Static)
        function installSingle(DataStruct, Args)
            % Install single DataName (utility function for install)
            % Input  : - A structure with the DataName, SubDir, URL,
            %            SearchFile fields fot the data to install
            % Author : Eran Ofek (Sep 2021)
            % @Todo: Why static???
        
            arguments
                DataStruct
                Args.Delete(1,1) logical  = true;
                Args.Npwget               = 10;
                Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
            end
            

            IsUnix = isunix || ismac;
            IsWindows = ~IsUnix;
            
            if IsWindows
                
                % Chen - Disable delete until we make sure that everything
                % is fine
                Args.Delete = false;
                
                % assume windows - replace / with \
                DataStruct.InstallationLocation = strrep(DataStruct.InstallationLocation,'/',filesep);
                DataStruct.SubDir = strrep(DataStruct.SubDir,'/',filesep);
            else
                DataStruct.InstallationLocation = strrep(DataStruct.InstallationLocation,'\',filesep);
                DataStruct.SubDir               = strrep(DataStruct.SubDir,'\',filesep);
            end
    

            PWD = pwd;
            
            % Windows
            if IsWindows
                PathWin = 'C:\AstroPack';
                if ~isfolder(PathWin)
                    mkdir(PathWin)
                end
                cd(PathWin);
                
            % Linux
            else
                cd('~/');
            end
            
            %
            if IsWindows
                DataStruct.InstallationLocation = strrep(DataStruct.InstallationLocation, '~', PathWin);
            end
            
            %
            io.msgLog(LogLevel.Info, 'Installer: %s', DataStruct.InstallationLocation);
            if ~isfolder(DataStruct.InstallationLocation)
                mkdir(DataStruct.InstallationLocation);
            end
            
            % create dir for installation
            if IsWindows
                cd(sprintf('%s%s',PathWin,filesep));
            else
                cd(sprintf('~%s',filesep));
            end
            
            cd(DataStruct.InstallationLocation);

            Parts = regexp(DataStruct.SubDir, filesep, 'split');

            Nparts = numel(Parts);
            SubDir = '';
            for Iparts=1:1:Nparts
                SubDir = sprintf('%s%s%s',SubDir,filesep,Parts{Iparts});
                if ~isfolder(Parts{Iparts})
                    mkdir(Parts{Iparts});
                end
                cd(Parts{Iparts});
            end
        
            PartsURL = regexp(DataStruct.URL,'/','split');
            Nfile    = numel(DataStruct.URL);
            if Nfile==1
                % check if file is index.html
                switch lower(PartsURL{1}{end})
                    case {'index.html','index.htm'}
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
            
            if numel(List)>0
                % delete content before reload
                if Args.Delete
                    delete('*');
                end
                if numel(List) > 0
                    io.msgLog(LogLevel.Info, 'pwget: %d items - %s', numel(List), List{1});
                end
                www.pwget(List, Args.wgetPars, Args.Npwget);
                
                pause(5);
                io.files.files_arrived([], 10);
                F = dir('*.gz');
                for I=1:1:numel(F)
                    io.msgLog(LogLevel.Info, 'gunzip: %s', F(I).name);
                    gunzip(F(I).name);
                end
                
                % Does it work on Windows???
                F = dir('*.tar');
                for I=1:1:numel(F)
                    io.msgLog(LogLevel.Info, 'untar: %s', F(I).name);
                    untar(F(I).name);
                end
                
                % Delete source archives                
                try
                    io.msgLog(LogLevel.Info, 'deleting *.gz');
                    delete('*.gz');
                end
                try
                    io.msgLog(LogLevel.Info, 'deleting *.tar');
                    delete('*.tar');
                end
            end
            cd(PWD);
            
        end
        
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
            Ind = find(strcmp({I.DataName}, 'MinorPlanets'));
            cd(I(Ind).InstallationLocation);
            cd(I(Ind).SubDir);
                        
            FID = fopen(FileName,'r');
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
            
            FID = fopen(FileName,'r');
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
                FileInd = [1 2 3];
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
                T1 = readtable(Finals);
            else
                T1 = [];
            end
            
            
            if any(FileInd==2)
                T2 = readtable(EOP_1962);
                ColNames = {'Year','Month','Day','MJD','x','y','UT1_UTC','LOD','dPsi','dEps',      'xErr','yErr',    'UT1_UTCErr','LOD_Err','dPsiErr','dEpsilonErr'};
                ColUnits = {'',   '',     '',   'day','arcsec','arcsec','s','s','arcsec','arcsec','arcsec','arcsec','s','s','arcsec','arcsec'};
                T2.Properties.VariableNames = ColNames;
                T2.Properties.VariableUnits = ColUnits;
            else
                T2 = [];
            end
            
            if any(FileInd==3)
                T3 = readtable(EOP_1846,'ReadVariableNames',false);
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
                Args.Exten = {'mat','fits'};
            end

            if isempty(Args.Dir)
                I   = Installer;
                Args.Dir = getDataDir(I, '+cats');
            end

            Next  = numel(Args.Exten);

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
                    fprintf('Generating function %s\n',FunName);

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
   
	
    methods % main functions
        function Ind = search(Obj, DataName)
            % Search data name in Installer object
            % Input  : - An Installer object
            %          - data name to search (e.g., 'Time')
            %            ir a cell array of data names.
            % Output : - Index of found data name in Installer object
            % Author : Eran Ofek (Sep 2021)
            % Example: Ind = search(I, 'MinorPlanets')
            %          Ind = search(I, {'Time','MinorPlanets'})
            
            if ischar(DataName)
                DataName = {DataName};
            end
            
            Ind = find(ismember({Obj.DataName}, DataName));
        end
            
        
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
            %                   arguments. Default is '-q -o /dev/null -U Mozilla --no-check-certificate'.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Installer;
            %          I.install   % install all
            %          I(5).install % install the 5th data name: PicklesStellarSpec
            %          I.install('Time'); % install the Time data name
            %          I.install({'Time','MinorPlanets'})           
            
            arguments
                Obj
                DataName                  = {};
                Args.Delete(1,1) logical  = true;
                Args.Npwget               = 10;
                Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
            end

            if ischar(DataName)
                DataName = {DataName};
            end
            
            Nobj = numel(Obj);
            if isempty(DataName)
                Ind = (1:1:Nobj);
            else
                Ind = Obj.search(DataName);
            end
                
            for I=1:1:numel(Ind)
                Iobj = Ind(I);
                try
                    Installer.installSingle(Obj(Iobj), 'Delete',Args.Delete, 'Npwget',Args.Npwget, 'wgetPars',Args.wgetPars);
                catch
                    io.msgLog(LogLevel.Error, 'installSingle failed: %d', Iobj);
                end
            end
        end
        
        
        function Result = seeAvailableData(Obj)
            % print a table of available data sets
            % Example: I = Installer; I.seeAvailableData
            
            Result = cell2table([{Obj.DataName}', ...
                   {Obj.SubDir}', ...
                   {Obj.Size}', ...
                   {Obj.Description}',...
                   {Obj.URL}'], 'VariableNames',{'DataName', 'SubDir', 'Size [MB]', 'Description', 'URL'});
            
        end

        
        function Dir = getDataDir(Obj, Name)
            % Get data directory name
            % DataName of data directory as appear in Insataller/seeAvailableData
            % Input  : - Installre object.
            %          - DataName
            % Output : - Directory name. Return empty if not found.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Insatller; I.getDataDir('Time')

            Ind = search(Obj, Name);
            if isempty(Ind)
                Dir = [];
            else
                Dir  = sprintf('%s%s%s' ,Obj(Ind).InstallationLocation, filesep, Obj(Ind).SubDir);
            end
        end

        
        function [Files, Dir] = getFilesInDataDir(Obj, Name)
            % Return all file names in directory associated with DataName
            % Input  : - Installre object.
            %          - DataName
            % Output : - dir-function like output of all the file names in
            %            the data directory. Directories are removed.
            %          - Data dir containing the files.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Insatller; I.getFilesInDataDir('SpecGalQSO')

            Dir = getDataDir(Obj, Name);
            PWD = pwd;
            cd(Dir);
            Files = dir('*');
            Files = Files(~[Files.isdir]);

        end
    end
	
    
    methods(Static) % unitTest
        Result = unitTest(Obj)
            % Dictionary unit test
            
   end
    
end
