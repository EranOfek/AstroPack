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
% I.install({'GAIA_SpecTemplate'}); % install specific datasets
% I.install({'+cats'}); % install specific datasets (and open tar file)


classdef Installer < Base
    % Installer class
    
    % Properties
    properties (SetAccess = public)
        InstallationLocation      = [];     
        ConfigFile
        ConfigStruct
    end
    
    %-------------------------------------------------------- 
    methods % constructor
        function Obj = Installer(ConfigFileName)
            % constructor for the Installre class (a utility class for
            % AstroPack installation
            
            arguments 
                ConfigFileName  = 'Installer.yml';
            end
            
            % populate the ConfigStruct field
            Obj.ConfigFile = ConfigFileName;
            
            
        end
    end
    
    
    methods % setters/getters
        function Result = get.InstallationLocation(Obj)
            % getter for InstallationLocation
            if isempty(Obj.InstallationLocation)
                % get from ConfigStruct
                Result = Obj.ConfigStruct.InstallationLocation;
                Obj.InstallationLocation = Result;
            else
                Result = Obj.InstallationLocation;
            end
        end
        
        function Obj = set.ConfigFile(Obj, ConfigFileName)
            % setter for ConfigFile - read config into ConfigStruct prop
            Obj.ConfigFile   = ConfigFileName;
            
            Config = Configuration;
            FullFileName = sprintf('%s%s%s', Config.Path, filesep, ConfigFileName);
            
            Config.loadFile(FullFileName, 'Field',false);
            Obj.ConfigStruct = Config.Data;
            clear Config;
            
        end
        
        function Obj = set.ConfigStruct(Obj, Struct)
            % setter for ConfigStruct - check validity of properties
           
            Ndn = numel(Struct.DataName);
            Nsd = numel(Struct.SubDir);
            Nu  = numel(Struct.URL);
            Ns  = numel(Struct.Size);
            Nsf = numel(Struct.SearchFile);
            Nd  = numel(Struct.Description);
            
            if Ndn~=Nsd || Ndn~=Nu || Ndn~=Ns || Ndn~=Nsf || Ndn~=Nd
                error('DataName, SubDir, URL, Size, GetTar, SearchFiule, Description properties in inastaller config files must have the same number of elements');
            end
            
            Obj.ConfigStruct = Struct;
            
        end
    end
    
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
            Ind = find(strcmp(I.ConfigStruct.DataName, 'MinorPlanets'));
            cd(I.ConfigStruct.InstallationLocation);
            cd(I.ConfigStruct.SubDir{Ind});
                        
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
        
        function T = readIERS_EOP(FileName)
            % Read IERS Earth Orientation File 'finals2000A.data.csv'
            %   documentation: http://hpiers.obspm.fr/eoppc/bul/bulb/explanatory.html
            %   http://maia.usno.navy.mil
            %   http://www.iers.org/nn_10968/IERS/EN/DataProducts/EarthOrientationData/eop.html?__nnn=true
            %   x_pole/y_pole:  Celestial Ephemeris Pole (CEP) relative to the International Reference Pole (IRP) are defined as x and y
            %   dPsi/dEps - offset relative to IAU 1980 Theory of Nutation
            % Input  : - File name to read. Default is 'finals2000A.data.csv'.
            % Output : - A table containing the file.
            % Author : Eran Ofek (Sep 2021)
            % Example: T = Installer.readIERS_EOP
            
            arguments
                FileName = 'finals2000A.data.csv';
            end
            
            %PWD = pwd;
            %I = Installer;
            %cd(I.ConfigStruct.InstallationLocation);
            %IndTime = strcmp(I.ConfigStruct.DataName,'Time');
            %cd(I.ConfigStruct.SubDir{IndTime});
            
            T = readtable(FileName);
        end
    end
   
	
    methods % main functions
        function install(Obj, DataName, Args)
            % Install AstroPack data directories from AstroPack repository
            % Input  : - An iInstaller object.
            %          - A DataName to install (e.g., 'GAIA_SpecTemplate'),
            %            or a cell array of data names. If empty, install
            %            all data names in ConfigStruct.
            %            Default is empty.
            %          * ...,key,val,...
            %            'Delete' - A logical indicating if to delete
            %                   data before installation.
            %                   Default is true.
            %            'Npwget' - Number of parallel wget. Default is 10.
            %            'wgetPars' - A cell array of additional wget
            %                   arguments. Default is '-q -o /dev/null -U Mozilla --no-check-certificate'.
            % Author : Eran Ofek (Sep 2021)
            % Example: I = Installer; I.install
            
            arguments
                Obj
                DataName                  = [];
                Args.Delete(1,1) logical  = true;
                Args.Npwget               = 10;
                Args.wgetPars             = '-q -o /dev/null -U Mozilla --no-check-certificate';
            end

            if isempty(DataName)
                DataName = Obj.ConfigStruct.DataName;
            else
                if ischar(DataName)
                    DataName = {DataName};
                end
            end
            
            if ~isunix && ~ismac
                % assume windows - replace / with \
                Obj.InstellationLocation = strrep(Obj.InstallationLocation,'/',filesep);
                Obj.CoonfigStruct.SubDir = strrep(Obj.ConfigStructSubDir,'/',filesep);
            else
                Obj.InstallationLocation = strrep(Obj.InstallationLocation,'\',filesep);
                Obj.ConfigStruct.SubDir  = strrep(Obj.ConfigStruct.SubDir,'\',filesep);
            end
    

            PWD = pwd;
            cd('~/');
            mkdir(Obj.InstallationLocation);

            Ndir = numel(DataName);
            
            for Idir=1:1:Ndir    
                % Identify requested data set in config
                Isel = find(strcmp(DataName{Idir}, Obj.ConfigStruct.DataName));
                
                % create dir for instellation
                cd(sprintf('~%s',filesep));
                cd(Obj.InstallationLocation);

                Parts = regexp(Obj.ConfigStruct.SubDir{Isel}, filesep, 'split');

                Nparts = numel(Parts);
                SubDir = '';
                for Iparts=1:1:Nparts
                    SubDir = sprintf('%s%s%s',SubDir,filesep,Parts{Iparts});
                    mkdir(Parts{Iparts});
                    cd(Parts{Iparts});
                end        
        
                PartsURL = regexp(Obj.ConfigStruct.URL{Isel},'/','split');
                if iscell(PartsURL)
                    % URL is a cell array of files URL
                    List = Obj.ConfigStruct.URL{Isel};
                    FileName = '*';
                else
                    switch lower(PartsURL{end})
                        case {'index.html','index.htm'}
                            % get all files in dir

                            [List,IsDir,FileName] = www.find_urls(Obj.ConfigStruct.URL{Isel},'match', Obj.ConfigStruct.SearchFile{Isel});
                            List     = List(~IsDir);
                            FileName = FileName(~IsDir);
                        otherwise
                            % direct file loading
                            List     = Obj.ConfigStruct.URL(Isel);  % cell
                            FileName = PartsURL{end};
                    end
                end

                if numel(List)>0
                    % delete content before reload
                    if Args.Delete
                        delete('*');
                    end
                    www.pwget(List, Args.wgetPars, Args.Npwget);
                    
                    
                    pause(5);
                    io.files.files_arrived([], 10);
                    F = dir('*.gz');
                    for I=1:1:numel(F)
                        gunzip(F(I).name);
                    end
                    F = dir('*.tar');
                    for I=1:1:numel(F)
                        untar(F(I).name);
                    end
                    try
                        delete('*.gz');
                    end
                    try
                        delete('*.tar');
                    end
                end
            end
            cd(PWD);
        end
        
        function Result = seeAvailableData(Obj)
            % print a table of available data sets
            % Example: I = Installer; I.seeAvailableData
            
            Result = cell2table([Obj.ConfigStruct.DataName(:), ...
                   Obj.ConfigStruct.SubDir(:), ...
                   Obj.ConfigStruct.Size(:), ...
                   Obj.ConfigStruct.Description(:),...
                   Obj.ConfigStruct.URL(:)], 'VariableNames',{'DataName', 'SubDir', 'Size [MB]', 'Description', 'URL'});
            
        end
    end
	
    
    methods(Static) % unitTest
        Result = unitTest(Obj)
            % Dictionary unit test
            
   end	
    
end