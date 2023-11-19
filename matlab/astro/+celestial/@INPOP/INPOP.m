% INPOP - A container and calculator class for INPOP ephemeris data
% This class can retrieve, store and use the INPOP ephemeris data
% (https://www.imcce.fr/inpop).
% This class can be use to calculate the positions and velocities
% of the main Solar System objects.
%
% Examples: 
%       % Two methods to download the INPOP data files:
%       I=Installer;      I.install('INPOP');
%       % or
%       celestial.INPOP.download
%       % Generate MAT files for faster first access
%       IP.convertAscii2mat
%       IP=celestial.INPOP   % create an INPOP object
%       IP.populateTables('all');  % load all planets / 100 years
%       IP.Constant   % get all INPOP constants
%       JD=[2414106.00,2451545]';
%       Pos = IP.getPos('Ear',JD);   % Get Eath Barycentric position, Equatorial J2000
%
% BUGS: (100 km offset in X coo) https://github.com/EranOfek/AstroPack/issues/281 
%
% NOTES [Answers from IMCCE]
%
% We distribute the INPOP ephemeris files on the following web page :
%
% https://www.imcce.fr/recherche/equipes/asd/inpop/download19a
%
% The timescale of the data are expressed in TDB or TCB. Usually, most of
% our users use files expressed in the timescale TDB. The name of the
% downloaded file contains TDB or TCB to discriminate the timescale
% argument of the tchebyvchev polynomials.
%
% On this web page, we distribute the ephemerids in two major formats :
% - "binary" : the internal file format is described in
% https://www.imcce.fr/content/medias/recherche/equipes/asd/inpop/inpop_file_format_2_0.pdf
% These file may be directly used using the calceph library.
%
% - spice  : these file format is decribed in
% https://arxiv.org/pdf/1507.04291.pdf
% section 5.3.2 and 5.3.3
% These file format may be directly used using the calceph library or spice library.
%
% You may find the calceph at https://www.imcce.fr/inpop/calceph
% Some examples are provided with the library, in several languages (C, fortran, python, ...).
%
% A python tutorial is available at
% https://mybinder.org/v2/gh/gastineau/demo_calceph_mybinder/master?filepath=index.ipynb
%
% Concerning the polynomials, the polynomials are as Chebyshev polynomials
% of the first kind. Some details about the evaluation are available at
% https://gitlab.obspm.fr/imcce_calceph/calceph/-/blob/master/src/calcephchebyshev.c
%


classdef INPOP < Base
    % OrbitalEl class for storing and manipulating orbital elements

    % Properties
    properties
        ChebyFun cell       = {};  % Chebyshev anonymous functions indexd by thir order-1
                                   % i.e., element 3 contains cheby polys 0,1,2
        PosTables struct    = struct('TT',[], 'Sun',[] ,'Mer',[], 'Ven',[], 'Ear',[], 'EMB',[], 'Moo',[], 'Lib',[], 'Mar',[], 'Jup',[], 'Sat',[], 'Ura',[], 'Nep',[], 'Plu',[]);
        VelTables struct    = struct('TT',[], 'Sun',[] ,'Mer',[], 'Ven',[], 'Ear',[], 'EMB',[], 'Moo',[], 'Lib',[], 'Mar',[], 'Jup',[], 'Sat',[], 'Ura',[], 'Nep',[], 'Plu',[]);
        
        Constant          = celestial.INPOP.readConstants;
    end
    
    properties (Constant)
        LatestVersion     = 'inpop21a';
        Location          = '~/matlab/data/SolarSystem/INPOP/';
        RangeShort        = [2414105.00, 2488985.00];
        ColTstart         = 1;
        ColTend           = 2;
        
    end
    
    methods % constructor
        function Obj = INPOP(Args)
            % INPOP class constructor - Create an empty celestial.INPOP object
            % Input  : * ...,key,val,...
            %            'PopOrder' - Populate Chebyshev polynomials of the
            %                   following orders. Default is [6:1:15].
            % Output : - A celestial.INPOP object
            % Author : Eran Ofek (May 2022) 
            
            arguments
                Args.PopOrder = [1:1:15];
            end
            
            Norder = numel(Args.PopOrder);
            for Iorder=1:1:Norder
                Obj.ChebyFun{Args.PopOrder(Iorder)+1} = tools.math.fun.chebyshevFun(1, [0:1:Args.PopOrder(Iorder)]);
            end
            
        end
    end
    
    methods  % getters/setters
        
    end
    
    methods (Static) % download/create INPOP files
        function FileName = inpopFileName(Args)
            % Construct INPOP data file name
            % Input  : * ...,key,val,...
            %            'Object' - Object name. If 'tar' then return the
            %                   tar file name.
            %                   Otherwise, this is a planet name.
            %            'FileData' - ['pos'] | 'vel'.
            %            'Version' - Default is 'inpop21a'.
            %            'TimeScale' - ['TDB'] | 'TCB'.
            %            'FileType' - Default is 'asc'.
            %            'TimePeriod' - ['100'] | '1000'.
            % Author : Eran Ofek (Apr 2022)
            % Example: celestial.INPOP.inpopFileName
            %          celestial.INPOP.inpopFileName('Object','Plu')
            
            arguments
                Args.Object        = 'tar';
                Args.Version       = 'inpop21a';
                Args.TimeScale     = 'TDB';   % 'TDB' | 'TCB'
                Args.FileType      = 'asc';
                Args.TimePeriod    = '100';    % '100' | '1000'
                Args.FileData      = 'pos';
            end
            
            switch lower(Args.Object)
                case 'tar'
                    % gzip tar file
                    FileName = sprintf('%s_%s_m%s_p%s_%s.tar.gz', Args.Version, Args.TimeScale, Args.TimePeriod, Args.TimePeriod, Args.FileType);
                otherwise
                    % planet
                    % inpop21a_TDB_m100_p100_asc_pos_Plu.asc
                    switch Args.Object
                        case 'TT'
                            ObjectName = Args.Object;
                        otherwise
                            ObjectName = Args.Object(1:3);
                    end
                    FileName = sprintf('%s_%s_m%s_p%s_%s_%s_%s.%s', Args.Version, Args.TimeScale, Args.TimePeriod, Args.TimePeriod, Args.FileType, Args.FileData, ObjectName, Args.FileType);
            end
        end
        
        function download(Args)
            % Download INPOP planetary ephemerides files from INPOP website
            % Input  : * ...,key,val,...
            %            'Location' - Location in which to download the
            %                   files. Default is '~/matlab/data/SolarSystem/INPOP/'.
            %            'Untar' - Untar downloaded files. Default is true.
            %            'URL' - A url of a specific file to download.
            %                   If empty, then use other argumntes to select
            %                   file to download. Default is [].
            %            'Version' - Default is 'inpop21a'.
            %            'TimeScale' - ['TDB'] | 'TCB'.
            %            'FileType' - Default is 'asc'.
            %            'Object' - Default is 'planets'.
            %            'TimePeriod' - ['100'] | '1000'.
            % Author : Eran Ofek (Apr 2022)
            % Example: celestial.INPOP.download
            
            arguments
                Args.Location      = '~/matlab/data/SolarSystem/INPOP/';
                Args.Untar logical = true;
                Args.URL           = [];
                Args.Version       = 'inpop21a';
                Args.TimeScale     = 'TDB';   % 'TDB' | 'TCB'
                Args.FileType      = 'asc';
                Args.Object        = 'planets';
                Args.TimePeriod    = '100';    % '100' | '1000'
            end
            
            Base = 'https://ftp.imcce.fr/pub/ephem';
            
            if ~isempty(Args.URL)
                % download specific file requested by user
                URL = Args.URL;
            else
                FileName = celestial.INPOP.inpopFileName('Object','tar', 'Version',Args.Version, 'TimeScale',Args.TimeScale, 'FileType',Args.FileType, 'TimePeriod', Args.TimePeriod);
                
                URL = sprintf('%s/%s/%s/%s',Base, Args.Object, Args.Version, FileName);
                % examples
                % https://ftp.imcce.fr/pub/ephem/planets/inpop21a/inpop21a_TDB_m100_p100_asc.tar.gz
                % https://ftp.imcce.fr/pub/ephem/planets/inpop21a/inpop21a_TCB_m100_p100_asc.tar.gz
                % https://ftp.imcce.fr/pub/ephem/planets/inpop21a/inpop21a_TDB_m1000_p1000_asc.tar.gz
                % https://ftp.imcce.fr/pub/ephem/planets/inpop21a/inpop21a_TCB_m1000_p1000_asc.tar.gz
            end
            
            PWD = pwd;
            mkdir(Args.Location)
            cd(Args.Location)
            [~,~,FileName] = www.wget(URL, 'OutputFile',[]);
            if Args.Untar
                pause(20);
                untar(FileName);
            end
            cd(PWD);
            
        end
        
        function [Result,FileName] = loadINPOP(Args)
            % Load an INPOP ephmerides file (in ASCII or MAT formats)
            %   The loaded file contains columns with:
            %       JDstart JDend Chebyshev coef. for the object position
            %       between JDstart and JDend.
            % Input  : * ...,key,val,...
            %            'Object' - Object name. If 'tar' then return the
            %                   tar file name.
            %                   Otherwise, this is a planet name.
            %            'Location' - Locaion in which the INPOP data files
            %                   are located.
            %                   Default is '~/matlab/data/SolarSystem/INPOP/'
            %            'FileData' - ['pos'] | 'vel'.
            %            'Version' - Default is 'inpop21a'.
            %            'TimeScale' - ['TDB'] | 'TCB'.
            %            'FileType' - 'mat' | 'asc'.
            %                   Use celestial.INPOP.convertAscii2mat to
            %                   create the mat files.
            %                   Default is 'asc'.
            %            'TimePeriod' - ['100'] | '1000'.
            % Output : - A matrix containing the requested data file.
            %            Containing columns with:
            %            JDstart JDend Chebyshev coef. for the object position
            %            between JDstart and JDend.
            %          - File name containing the data.
            % Author : Eran Ofek (Apr 2022)
            % Example: R = celestial.INPOP.loadINPOP('Object','Sun');
            %          R = celestial.INPOP.loadINPOP('Object','Sun','FileType','mat');
            
            arguments
                Args.Object        = 'Sun';
                Args.Location      = celestial.INPOP.Location;
                Args.Version       = 'inpop21a';
                Args.TimeScale     = 'TDB';   % 'TDB' | 'TCB'
                Args.FileType      = 'asc';   % 'mat' | 'asc';
                Args.TimePeriod    = '100';   % '100' | '1000'
                Args.FileData      = 'pos';
            end
            
            FileName    = celestial.INPOP.inpopFileName('Object', Args.Object, 'Version',Args.Version, 'TimeScale',Args.TimeScale, 'FileType',Args.FileType, 'TimePeriod',Args.TimePeriod, 'FileData',Args.FileData);
            FullFileName = sprintf('%s%s%s',Args.Location, filesep, FileName);
            
            switch lower(Args.FileType)
                case 'asc'
                    Result = readmatrix(FullFileName, 'NumHeaderLines',2,'FileType','text');
                case 'mat'
                    Result = io.files.load2(FullFileName);
                otherwise
                    error('Unknown FileType option');
            end
            
        end
        
        function convertAscii2mat(Args)
            % Convert the INPOP ascii files to fast-readout mat format files
            % Input  : * ...,key,val,...
            %            See function code for details
            % Author : Eran Ofek (Apr 2022)
            % Example: celestial.INPOP.convertAscii2mat('TimePeriod',{'100'});
            
            arguments
                Args.Object        = {'Sun','Mer','Ven','Ear','EMB','Moo','Mar','Jup','Sat','Ura','Nep','Plu','Lib','TT'};
                Args.Location      = celestial.INPOP.Location;
                Args.Version       = 'inpop21a';
                Args.TimeScale     = 'TDB';   % 'TDB' | 'TCB'
                Args.TimePeriod    = {'100','1000'};    % '100' | '1000'
                Args.FileData      = {'pos','vel'};
                Args.MatVersion    = '-v7.3';
            end
            
            PWD = pwd;
            cd(Args.Location);
            for Ifd=1:1:numel(Args.FileData)
                for Itp=1:1:numel(Args.TimePeriod)
                    for Iobj=1:1:numel(Args.Object)
                        if ~(strcmp(Args.Object{Iobj}, 'TT') && strcmp(Args.FileData{Ifd},'vel'))
                            [Table,FileName] = celestial.INPOP.loadINPOP('Object',Args.Object{Iobj},...
                                                                'Location',Args.Location,...
                                                                'Version',Args.Version,...
                                                                'TimeScale',Args.TimeScale,...
                                                                'TimePeriod',Args.TimePeriod{Itp},...
                                                                'FileData',Args.FileData{Ifd});
                            NewFileName = strrep(FileName,'asc','mat');
                            save(NewFileName, 'Table', Args.MatVersion);
                        end
                    end
                end
            end
            cd(PWD);
        end
        
        function Result = readConstants(FileName)
            % Read INPOP constants table into a structure
            % Input  : - A File name. Default is
            %            'inpop21a_TDB_m100_p100_asc_header.asc'.
            %            The file must be located in the directory
            %            specified in celestial.INPOP.Location.
            % Output : - A structure with the constant names.
            % Author : Eran Ofek (Apr 2022)
            % Example: Result = celestial.INPOP.readConstants
            
            arguments
                FileName = 'inpop21a_TDB_m100_p100_asc_header.asc';
            end
            
            FullFileName = sprintf('%s%s',celestial.INPOP.Location, FileName);
            
            FID = fopen(FullFileName);
            if FID>0
                C = textscan(FID,'%s %f\n','Delimiter','=', 'Headerlines',1);
                fclose(FID);
                C{1}   = strtrim(C{1});
                N      = numel(C{1});
                for I=1:1:N
                    Result.(C{1}{I}) = C{2}(I);
                end
            else
                warning('Can not find %s - try to download the INPOP files first',FullFileName);
                Result = [];
            end
        end
        
    end
    
    methods (Static)  % transformations
        function CooEcl = eqJ2000_2ecliptic(CooJ2000)
            % Rotate [X;Y;Z] Equatorial J2000 coordinates to ecliptic [X;Y;Z]
            % Input  : - A 3xN matrix of positions [X;Y;Z] in equatorial J2000.
            % Output : - A matrix of [X;Y;Z] ecliptic positions.
            % Author : Eran Ofek (May 2022)
            % Example: CooEcl = celestial.INPOP.eqJ2000_2ecliptic(rand(3,6))
            
            arguments
                CooJ2000(3,:)
            end
            
            RotM   = celestial.coo.rotm_coo('e');
            CooEcl = RotM * CooJ2000;
        end
    end
    
    methods  % aux/util functions
        function Result = isPopulated(Obj, Object, FileData)
            % Check if table for an object is populated with Chebyshev polynomials
            % Input  : - A celestial.INPOP object.
            %          - An object. Default is 'Ear'.
            %          - FileData {'pos' | 'vel'}. Default is 'pos'.
            % Output : - A logical indicating if the specific table is
            %            populated.
            % Author : Eran Ofek (May 2022)
            % Example: IP.isPopulated
            
            arguments
                Obj
                Object   = 'Ear';
                FileData = 'pos';
            end
            
            switch FileData
                case 'pos'
                    Result = ~isempty(Obj.PosTables.(Object));
                case 'vel'
                    Result = ~isempty(Obj.VelTables.(Object));
                otherwise
                    error('Unknown FileData option - should be pos or vel');
            end
            
        end
        
        function Obj = populateTables(Obj, Object, Args)
            % Populate pos/vel INPOP tables in the INPOP object.
            %   In the INPOP object, the PosTables and VelTables contains
            %   the pos/vel chebyshev coef. for each Solar System object.
            %   This function read the tables from disk, in some time
            %   range, and populate the PosTables or VelTables properties.
            % Input  : - A single element celestial.INPOP object.
            %          - Object name, or a cell array of object names for
            %            which to populate the Postables/VelTables.
            %            Possible strings: 'Sun', 'Mer',
            %            'Ven', 'Ear', 'EMB', 'Moo', 'Mar', 'Jup', 'Sat', 'Ura',
            %            'Nep', 'Plu', 'Lib', and 'all'.
            %          * ...,key,val,...
            %            'TimeSpan' - Either '100', '1000' (years), or
            %                   [MinJD MaxJD] vector of JD range.
            %                   Default is '100'.
            %            'OriginType' - File type from which to read
            %                   tables. Default is 'ascii'.
            %            'TimeScale' - Default is 'TDB'.
            %            'Version' - Default is Obj.LatestVersion
            %            'FileData' - Either ['pos'], or 'vel'. for
            %                   positional and veocity data tables.
            %            'FileType' - 'asc' | ['mat'].
            %                   Use celestial.INPOP.convertAscii2mat to
            %                   create the mat files.
            %            'PopForce' - A logical indicating if to repopulate
            %                   the table even if not empty.
            %                   Default is false.
            % Output : - A celestial.INPOP object in which the PosTables or
            %            VelTables are populated.
            % Author : Eran Ofek (Apr 2022)
            % Example: I = celestial.INPOP;
            %          I.populateTables;  % load 'pos' '100' years tables for Sun and Earth
            %          I.populateTables('Mars','TimeSpan',[2451545 2451545+365]); % load data in some specific range for Mars
            %          I.populateTables('all');
            %          I.populateTables('all','FileData','vel');
            
            arguments
                Obj
                Object           = {'Sun','Ear'};
                Args.TimeSpan    = '100';  % '1000' or [MinJD MaxJD]
                Args.OriginType  = 'ascii';
                Args.TimeScale   = 'TDB';
                Args.Version     = Obj.LatestVersion;
                Args.FileData    = 'pos';
                Args.FileType    = 'mat';
                Args.PopForce logical = false;
            end
            
            if ischar(Object)
                switch lower(Object)
                    case 'all'
                        Object = {'Sun', 'Mer', 'Ven', 'Ear', 'EMB', 'Moo', 'Mar', 'Jup', 'Sat', 'Ura', 'Nep', 'Plu', 'Lib'};
                    otherwise
                        Object = {Object};
                end
            end
            
            if isnumeric(Args.TimeSpan)
                MinJD = min(Args.TimeSpan(:));
                MaxJD = max(Args.TimeSpan(:));
                if MinJD<Obj.RangeShort(1) || MaxJD>Obj.RangeShort(2)
                    TimePeriod = '1000';
                else
                    TimePeriod =  '100';
                end
            else
                TimePeriod = Args.TimeSpan;
                MinJD = -Inf;
                MaxJD = Inf;
            end
            
            Nobject = numel(Object);
            for Iobject=1:1:Nobject
                % read data
                switch lower(Args.OriginType)
                    case 'ascii'
                        if ~Obj.isPopulated(Object{Iobject}, Args.FileData) || Args.PopForce
                            Table = celestial.INPOP.loadINPOP('TimeScale',Args.TimeScale,...
                                                          'Object',Object{Iobject},...
                                                          'Version',Args.Version,...
                                                          'TimePeriod',TimePeriod,...
                                                          'FileData',Args.FileData,...
                                                          'FileType',Args.FileType);
                        else
                            Table = [];
                        end

                    otherwise
                        error('Unknown OriginType option');
                end

                if ~isempty(Table)
                    % select data in some time range
                    Flag  = Table(:,Obj.ColTstart)>=MinJD & Table(:,Obj.ColTend)<=MaxJD;
                    Table = Table(Flag,:);

                    % store data
                    switch lower(Args.FileData)
                        case 'pos'
                            Obj.PosTables.(Object{Iobject}) = Table;
                        case 'vel'
                            Obj.VelTables.(Object{Iobject}) = Table;
                        otherwise
                            error('Unknown FileData option');
                    end
                end
                
            end
            
        end
        
        function Obj = populateAll(Obj, Args)
            % Populate all INPOP tables
            %   In the INPOP object, the PosTables and VelTables contains
            %   the pos/vel chebyshev coef. for each Solar System object.
            %   This function read the tables from disk, in some time
            %   range, and populate the PosTables or VelTables properties.
            % Input  : - A single element celestial.INPOP object.
            %          * ...,key,val,...
            %            'TimeSpan' - Either '100', '1000' (years), or
            %                   [MinJD MaxJD] vector of JD range.
            %                   Default is '100'.
            %            'OriginType' - File type from which to read
            %                   tables. Default is 'ascii'.
            %            'TimeScale' - Default is 'TDB'.
            %            'Version' - Default is Obj.LatestVersion
            %            'FileType' - 'asc' | ['mat'].
            %                   Use celestial.INPOP.convertAscii2mat to
            %                   create the mat files.
            % Output : - A celestial.INPOP object in which the PosTables or
            %            VelTables are populated.
            % Author : Eran Ofek (Apr 2022)
            % Example: I = celestial.INPOP;
            %          I.populateAll;
            
             arguments
                Obj
                Args.TimeSpan    = '100';  % '1000' or [MinJD MaxJD]
                Args.OriginType  = 'ascii';
                Args.TimeScale   = 'TDB';
                Args.Version     = Obj.LatestVersion;
                Args.FileData    = 'pos';
                Args.FileType    = 'mat';
                Args.PopForce logical = false;
             end
             
             Obj.populateTables('all', 'FileData','pos', 'TimeSpan',Args.TimeSpan, 'OriginType',Args.OriginType, 'TimeScale',Args.TimeScale, 'Version',Args.Version, 'FileType',Args.FileType);
             Obj.populateTables('all', 'FileData','vel', 'TimeSpan',Args.TimeSpan, 'OriginType',Args.OriginType, 'TimeScale',Args.TimeScale, 'Version',Args.Version, 'FileType',Args.FileType);
             
        end
    end
    
    methods % ephemeris evaluation
        function Pos = getPos(Obj, Object, JD, Args)
            % Get INPOP planetary positions (or velocities or time) by evaluation of the chebyshev polynomials.
            %   This function can get the [X,Y,Z] Barycentric position [km]
            %   with respect to the ICRS equatorial J2000.0 coordinate
            %   system.
            %   The function gets the positions for a single object and
            %   multiple times.
            %   The function can be used also to evaluate the TT time
            %   table (see getTT for details).
            %   Instructions on how to read the INPOP original files:
            %   https://www.imcce.fr/content/medias/recherche/equipes/asd/inpop/inpop_file_format_2_0.pdf
            % Input  : - A single-element celestial.INPOP object.
            %          - A planet/object name:
            %            'Sun', 'Mer', 'Ven', 'Ear', 'EMB', 'Moo', 'Mar',
            %            'Jup', 'Sat', 'Ura', 'Nep', 'Plu'.
            %            Default is 'Ear'.
            %          - A vector of JD at which to evaulate the
            %            positions/velocities.
            %            Alternatively this can be a 4/6 columns matrix of
            %            [day month year [Frac | H M S]].
            %            Default is 2451545.
            %          * ...,key,val,...
            %            'TimeScale' - The JD time scale. Default is 'TDB'.
            %            'OutUnits'  - 'km','cm','au',... for velocity this
            %                   is always, the same per day.
            %                   Default is 'au'.
            %                   Note that the value of he AU is taken from
            %                   the Constant.AU property.
            %            'IsEclipticOut' - A logical indicating if output
            %                   is in ecliptic coordinates (if false then output
            %                   is in equatorial J2000). Default is false.
            %            'Algo' - Algorithm. Currently a single option
            %                   exist.
            %            'MaxOrder' - Max. order of cheby. polynomials to
            %                   use. If Inf use all. Default is Inf.
            %           There are additional undocumented arguments - see
            %           code for details.
            % Output : - A 3 by number of epochs matrix of [X; Y; Z]
            %            positions or velocities.
            %            Units are given by OutUnits, or OutUnits per day.
            %            If 'IsEclipticOut' is false, then the output
            %            reference frame is equatorial ICRS J2000.
            % Author : Eran Ofek (Apr 2022)
            % Example: I = celestial.INPOP;
            %          I.populateTables;
            %          Pos = I.getPos
            %
            %          JD=[2414106.00,2451545]';
            %          Pos = I.getPos('Ear',JD);
            %          [Coo]=celestial.SolarSys.calc_vsop87(JD,'Earth','e','E'); Coo.*constant.au./1e5
            %          
            %          % test accuracy relative to VSOP87
            %          JD=(2451545:0.01:(2451545+100)).';
            %          [Coo,CVel]=celestial.SolarSys.calc_vsop87(JD,'Earth','e','E');     
            %          Pos = I.getPos('Ear',JD);     
            %          Vel = I.getVel('Ear',JD);
            %          max(abs(Coo(2,:).*constant.au./1e5 - Pos(2,:)))

            arguments
                Obj(1,1)
                Object               = 'Ear';
                JD                   = 2451545;
                Args.IsPos logical   = true;
                Args.TimeScale       = 'TDB';
                Args.OutUnits        = 'au';
                Args.IsEclipticOut logical = false;
                Args.Algo            = 1;
                Args.Ncoo            = 3;  % internal argument used to evaluate pos/vel (=3) or time (=1)
                Args.MaxOrder        = Inf;
            end
            
            if Args.IsPos
                TableName = 'PosTables';
            else
                % velocity
                TableName = 'VelTables';
            end
            
            if Args.Ncoo==1
                % override to original ynits [s]
                Args.OutUnits = 'km';
            end
            
            ColDataStart = 3;
            
            if min(size(JD))>1
                % assume input is rows of dats [Day, Mount, Year, H M S]
                JD = celestial.time.julday(JD);
            else
                JD = JD(:);
            end
            Njd = numel(JD);
            
            [Nrow, Ncol] = size(Obj.(TableName).(Object));
            if mod(Nrow,3)~=0
                error('Number of entries in table is not divided by 3');
            end
            
            if isempty(Obj.(TableName).(Object))
                error('%s table for object %s is not loaded - use populateTables to load table', TableName, Object);
            end
            
            Tstart = Obj.(TableName).(Object)(1:Args.Ncoo:end, Obj.ColTstart);
            Tend   = Obj.(TableName).(Object)(1:Args.Ncoo:end, Obj.ColTend);
            Tmid   = (Tstart + Tend).*0.5;
            Tstep  = Tmid(2) - Tmid(1);
            Thstep = Tstep.*0.5;
            
            % find the index of time for X-axis only
            IndVec       = (1:Nrow./Args.Ncoo).';
            % The index of the JD is measured in the Tmid vector and not in
            % the XYZ coef. table...
            %IndJD        = interp1(Tmid, IndVec, JD, 'nearest','extrap');
            IndJD        = ceil((JD - Tstart(1))./Tstep);
            ChebyOrder   = Ncol - Obj.ColTend;
            
            
            Norder = Ncol - ColDataStart + 1;
            Norder = min(Norder, Args.MaxOrder);
            VecOrder = (ColDataStart:1:(ColDataStart+Norder-1));
            
            Pos = zeros(Args.Ncoo, Njd);
            for Icoo=1:1:Args.Ncoo
                % need to do for each coordinate
                
                ChebyCoef    = Obj.(TableName).(Object)(IndJD.*Args.Ncoo+Icoo-Args.Ncoo, VecOrder); %ColDataStart:end);
            
                % maybe need to divide by half time span
                %ChebyEval    = Obj.ChebyFun{ChebyOrder}((JD - Tmid(IndJD))./Thstep);
                ChebyEval    = Obj.ChebyFun{Norder}((JD - Tmid(IndJD))./Thstep);
                
                Pos(Icoo,:)  = sum([ChebyCoef.*ChebyEval(:,1:Norder)].',1);
            end
            
            switch lower(Args.OutUnits)
                case 'km'
                    % do nothing [km, or km/day]
                case 'cm'
                    % cm or cm/day
                    Pos = Pos.*1e5;
                case 'au'
                    % au or au/day
                    Pos = Pos .* (1./Obj.Constant.AU);
                otherwise
                    error('Unknown OutUnits option');
            end
            
            if Args.IsEclipticOut
                Pos = celestial.INPOP.eqJ2000_2ecliptic(Pos);
            end
        end
        
        function Vel = getVel(Obj, Object, JD, Args)
            % Get INPOP planetary velocities by evaluation of the chebyshev polynomials.
            %   This function can get the [X,Y,Z] Barycentric velocities [km]
            %   with respect to the ICRS equatorial J2000.0 coordinate
            %   system.
            %   The function gets the positions for a single object and
            %   multiple times.
            %   The function can be used also to evaluate the TT time
            %   table (see getTT for details).
            % Input  : - A single-element celestial.INPOP object.
            %          - A planet/object name:
            %            'Sun', 'Mer', 'Ven', 'Ear', 'EMB', 'Moo', 'Mar',
            %            'Jup', 'Sat', 'Ura', 'Nep', 'Plu'.
            %            Default is 'Ear'.
            %          - A vector of JD at which to evaulate the
            %            positions/velocities.
            %            Alternatively this can be a 4/6 columns matrix of
            %            [day month year [Frac | H M S]].
            %            Default is 2451545.
            %          * ...,key,val,...
            %            'TimeScale' - The JD time scale. Default is 'TDB'.
            %            'OutUnits'  - 'km','cm','au',... for velocity this
            %                   is always, the same per day.
            %                   Default is 'au'.
            %                   Note that the value of he AU is taken from
            %                   the Constant.AU property.
            %            'IsEclipticOut' - A logical indicating if output
            %                   is in ecliptic coordinates (if false then output
            %                   is in equatorial J2000). Default is false.
            %            'Algo' - Algorithm. Currently a single option
            %                   exist.
            %           There are additional undocumented arguments - see
            %           code for details.
            % Output : - A 3 by number of epochs matrix of [X; Y; Z]
            %            velocities.
            %            Units are given by OutUnits, or OutUnits per day.
            % Author : Eran Ofek (Apr 2022)
            % Example: I = celestial.INPOP;
            %          I.populateTables('Ear','FileData','vel')
            %          Vel = I.getVel
            
            arguments
                Obj(1,1)
                Object               = 'Ear';
                JD                   = 2451545;
                Args.TimeScale       = 'TDB';
                Args.OutUnits        = 'au';
                Args.IsEclipticOut logical = false;
                Args.Algo            = 1;
               
            end
            
            Vel = Obj.getPos(Object, JD, 'IsPos',false, 'OutUnits',Args.OutUnits, 'Algo', Args.Algo, 'TimeScale',Args.TimeScale, 'IsEclipticOut',Args.IsEclipticOut);

        end
        
        function TT = getTT(Obj, JD, Args)
            % Evaulate TT-TDB or TCG-TCB
            %   Dynamical Time replaced ephemeris time as the independent argument in dynamical theories and ephemerides. Its unit of duration is based on the orbital motions of the Earth, Moon, and planets.
            %   Terrestrial Time (TT), (or Terrestrial Dynamical Time, TDT), with unit of duration 86400 SI seconds on the geoid, is the independent argument of apparent geocentric ephemerides. TDT = TAI + 32.184 seconds.
            %   Barycentric Dynamical Time (TDB), is the independent argument of ephemerides and dynamical theories that are referred to the solar system barycenter. TDB varies from TT only by periodic variations.
            %   Geocentric Coordinate Time (TCG) is a coordinate time having its spatial origin at the center of mass of the Earth. TCG differs from TT as: TCG - TT = Lg x (JD -2443144.5) x 86400 seconds, with Lg = 6.969291e-10.
            %   Barycentric Coordinate Time (TCB)is a coordinate time having its spatial origin at the solar system barycenter. TCB differs from TDB in rate. The two are related by: TCB - TDB = iLb x (JD -2443144.5) x 86400 seconds, with Lb = 1.550505e-08.
            % Input  : - A single-element celestial.INPOP object.
            %          - A vector of JD at which to evaulate the
            %            positions/velocities.
            %            Alternatively this can be a 4/6 columns matrix of
            %            [day month year [Frac | H M S]].
            %            Default is 2451545.
            %          * ...,key,val,...
            %            'TimeScale' - If 'TDB' will return TT-TDB
            %                       If 'TCB' will returm TCG-TCB.
            %            'Algo' - Algorithm. Currently a single option
            %                   exist.
            % Output : - TT-TDB or TCG-TCB [s].
            % Author : Eran Ofek (Apr 2022)
            % Example: JD=[2414106.00,2451545]';
            %          TT = I.getTT(JD);
            %          
            
            arguments
                Obj(1,1)
                JD                   = 2451545;
                Args.TimeScale       = 'TDB';
                Args.Algo            = 1;
            end
            
            TT = Obj.getPos('TT',JD, 'IsPos',true, 'OutUnits','km', 'Ncoo',1, 'Algo', Args.Algo, 'TimeScale',Args.TimeScale);

        end
    
        function [Pos, Vel] = getAll(Obj, JD, Args)
            % Get the position and velocity for all INPOP objects
            %   {'Sun','Mer','Ven','Ear','Moo','Mar','Jup','Sat','Ura','Nep','Plu'}
            % Input  : - A populated celestial.INPOP object
            %          - A vector of JD
            %          * ...,key,val,...
            %            'TimeScale' - The JD time scale. Default is 'TDB'.
            %            'OutUnits'  - 'km','cm','au',... for velocity this
            %                   is always, the same per day.
            %                   Default is 'au'.
            %                   Note that the value of he AU is taken from
            %                   the Constant.AU property.
            %            'IsEclipticOut' - A logical indicating if output
            %                   is in ecliptic coordinates (if false then output
            %                   is in equatorial J2000). Default is false.
            %            'Exclude' - A cell array of INPOP objects to
            %                   exclude. For excluded objects the output
            %                   will be set to NaN.
            %                   Default is {}.
            %            'Permute' - Permute output.
            %                   If [], then dimensions of output are:
            %                   [3 cco, JD, Body]
            %                   Default is [1 3 2] (i.e., [3 coo, Body, Time]
            % Output : - An array of objects position (see Permute argument
            %            for dimensions).
            %            Reference coordinate system is equatorial J2000 if
            %            IsEclipticOut=false, and ecliptic if
            %            IsEclipticOut=true.
            %          - An array of objects velocity (see Permute argument
            %            for dimensions).
            %            Uints are given by OutUnits per day.
            % Author : Eran Ofek (Oct 2023)
            % Example: I=celestial.INPOP;
            %          I.populateTables('all');
            %          I.populateTables('all','FileData','vel');
            %          [Pos,Vel]=I.getAll(2451545+(0:1));

            arguments
                Obj
                JD
                Args.TimeScale             = 'TDB';
                Args.OutUnits              = 'au';
                Args.IsEclipticOut logical = false;
                Args.Exclude               = {};
                Args.Permute               = [1 3 2];
            end
            
            G = (constant.G./(constant.au).^3 .*86400.^2 .* constant.SunM);  % [G: au^3 SunM^-1 day^-2]
    
            %Msun   = 1.98847e33;  % [gram]
            Bodies = {'Sun','Mer','Ven','Ear','Moo','Mar','Jup','Sat','Ura','Nep','Plu'};
            %                Mercury      Venus       Earth       Moon           Mars         Jupiter      Sat         Ura         Nep         Plu        
            %Mass   = [Msun,  0.330103e27, 4.86731e27, 5.97217e27, 7.34767309e25, 0.641691e27, 1898.125e27, 568.317e27, 86.8099e27, 102.4092e27 0.01303e27]./Msun;  % [solar mass]
            %Msun  = 1;
            %GM  = G .* Mass;
            
            Nt = numel(JD);
            
            Nbody    = numel(Bodies);
            Pos     = nan(3,Nt,Nbody);
            Vel     = nan(3,Nt,Nbody);
            for Ibody=1:1:Nbody
                if ~any(strcmp(Args.Exclude,Bodies{Ibody}))
                    Pos(:,:,Ibody) = Obj.getPos(Bodies{Ibody}, JD, 'OutUnits',Args.OutUnits, 'TimeScale',Args.TimeScale, 'IsEclipticOut',Args.IsEclipticOut);
                    if nargout>1
                        Vel(:,:,Ibody) = Obj.getVel(Bodies{Ibody}, JD, 'OutUnits',Args.OutUnits, 'TimeScale',Args.TimeScale, 'IsEclipticOut',Args.IsEclipticOut);
                    end
                end
            
            end
            if ~isempty(Args.Permute)
                Pos = permute(Pos, Args.Permute);
                Vel = permute(Vel, Args.Permute);
            end
            
        end
        
        function [Force,DFDT]=forceAll(Obj, JD, TargetXYZ, Args)
            % Calculate the Sun+Planets+Moon G*M on a Solar System object.
            % Input  : - A populated INPOP object (both Pos and Vel should
            %            be populated).
            %          - A vector of JD.
            %          - A 3XN matrix with the targets (target per colum)
            %            XYZ coordinate.
            %          * ...,key,val,...
            %            'TimeScale' - The JD time scale. Default is 'TDB'.
            %            'OutUnits'  - 'km','cm','au',... for velocity this
            %                   is always, the same per day.
            %                   Default is 'au'.
            %                   Note that the value of he AU is taken from
            %                   the Constant.AU property.
            %            'IsEclipticOut' - A logical indicating if output
            %                   is in ecliptic coordinates (if false then output
            %                   is in equatorial J2000). Default is false.
            %            'Exclude' - A cell array of INPOP objects to
            %                   exclude. For excluded objects the output
            %                   will be set to NaN.
            %                   Default is {}.
            % Output : - The force that acts on the list of targets at the
            %            give times.
            %          - The force derivative (per day).
            % Author : Eran Ofek (Nov 2023)
            % Example: I=celestial.INPOP;
            %          I.populateTables('all');
            %          I.populateTables('all','FileData','vel');
            %          [Force]=I.forceAll(2451545,[2 2 2]');
           
            arguments
                Obj
                JD
                TargetXYZ
                Args.TimeScale             = 'TDB';
                Args.OutUnits              = 'au';  % or AU/day
                Args.IsEclipticOut logical = false;
                Args.Exclude               = {}; %{'Mer','Ven','Ear','Moo','Mar','Jup','Sat','Ura','Nep','Plu'};
                
            end
            Permute  = [1 3 2];
            SEC_DAY  = 86400;
            Msun   = 1.98847e33;  % [gram]
            G        = (constant.G./(constant.au).^3 .*SEC_DAY.^2 .* Msun);  % [G: au^3 SunM^-1 day^-2]
            
            Bodies = {'Sun','Mer','Ven','Ear','Moo','Mar','Jup','Sat','Ura','Nep','Plu'};
            %                Mercury      Venus       Earth       Moon           Mars         Jupiter      Sat         Ura         Nep         Plu        
            Mass   = [Msun,  0.330103e27, 4.86731e27, 5.97217e27, 7.34767309e25, 0.641691e27, 1898.125e27, 568.317e27, 86.8099e27, 102.4092e27 0.01303e27];  % [gr]
            Mass   = Mass./Msun;   % [Msun]
            GM     = G .* Mass;
            
            % get position for all planets
            
            if nargout>1
                [Pos, Vel] = getAll(Obj, JD, 'TimeScale',Args.TimeScale,...
                                         'OutUnits',Args.OutUnits,...
                                         'IsEclipticOut',Args.IsEclipticOut,...
                                         'Exclude',Args.Exclude,...
                                         'Permute',Permute);
            else
                [Pos]      = getAll(Obj, JD, 'TimeScale',Args.TimeScale,...
                                         'OutUnits',Args.OutUnits,...
                                         'IsEclipticOut',Args.IsEclipticOut,...
                                         'Exclude',Args.Exclude,...
                                         'Permute',Permute);
            end
            % Calculate the force on Target at position TargetXYZ
            
            Njd   = size(TargetXYZ,2); % to support 1 date for multiple targets
    
            % transform planets coordinates to target reference frame
            PosRelToTarget = Pos - reshape(TargetXYZ, [3, 1, Njd]);

            % Distances
            DistToTarget = sqrt(sum(PosRelToTarget.^2,1));

            % calc force and sum over planets
            Force = squeeze(sum(GM .* PosRelToTarget./(DistToTarget.^3), 2, 'omitnan'));
            
            if nargout>1
                % calc force time derivative
                error('Not yet available - need also TargetV');
            end
            
        end
        
        function Result = compare2JPL(Obj, Args)
            % Compare INPOP to JPL horizons ephemeris
            % Author : Eran Ofek (Nov 2023)
            % Example: R=I.compare2JPL
           
            arguments
                Obj
                Args.BodyINPOP   = 'Mar';
                Args.BodyJPL     = '499';
                Args.JD = celestial.time.julday + (0:10:100)';
            end
           
            
            Njd = numel(Args.JD);
            Diff = zeros(Njd,3);
            for Ijd=1:1:Njd
                XYZ = Obj.getPos(Args.BodyINPOP, Args.JD(Ijd), 'IsEclipticOut',true, 'TimeScale','TT','OutUnits','km');
                [T] = celestial.SolarSys.getJPL_ephem(Args.BodyJPL,'EPHEM_TYPE','VECTORS','TimeScale','TT','CENTER','500@0','StartTime',Args.JD(Ijd), 'StopTime',Args.JD(Ijd)+0.5, 'OUT_UNITS','KM-S');
                Diff(Ijd,:) = [XYZ - [T.X; T.Y; T.Z]].';
            end
            Result.JD   = Args.JD;
            Result.Diff = Diff;
            
        end
        
    end
    
    methods(Static) % Unit test

        Result = unitTest()
            % unitTest for OrbitalEl class
    end

end
