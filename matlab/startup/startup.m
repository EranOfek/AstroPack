% NOTE: Remember to set environment variable ASTROPACK_PATH to AstroPack root folder
%
% The default matlab startup script for AstroPack
%
% Open with: edit(fullfile(userpath,'startup.m'))
%
% startup.m folder:
%
% Linux:   
%   Eran    - /home/eran/Documents/MATLAB/startup.m (Check)
%   Yossi   - /home/yossi/Documents/MATLAB/startup.m (Check)
%   Chen VM - /home/chent/Documents/MATLAB/startup.m
%
% Windows: 
%   Chen Desktop - C:\Users\chen\Documents\MATLAB\startup.m
%   Chen Laptop  - C:\Users\chent\OneDrive\Documents\MATLAB\startup.m
%

function startup(Args)
    % Startup file for AstroPack
    % Input  : ...,key,val,...
    %          'setGraphics' - Default is true.
    %          'setRandomNumbers' - Default is true.
    %          'setFormat' - Default is true.
    %          'setBaseVariables' - Default is true.
    %          
    %          'AstroPack_BasePath' - Path in which the AstroPack is
    %                   installed. If empty, then use default
    %                   ('~matlab/AstroPack). Default is [].
    %          'AstroPack_DataPath' - Path in which the Data dir is
    %                   instaalled. If empty, then use default
    %                   ('~matlab/data). Default is [].
    %          'AstroPack_CatsHTMPath' - Path in which the catsHTM is
    %                   installed. Default is '/euler/catsHTM'.
    %          'AstroPack_ConfigPath' - Path in which the config is
    %                   instaalled. If empty, then use default
    %                   ('~matlab/AstroPack/config). Default is [].
    %
    %          'EnvVar_BasePath' - Optional environment variable that
    %                   contaisn the AstroPack path. If the env. var content is
    %                   empty, then will use the AstroPack_BasePath
    %                   argument. Default is 'ASTROPACK_PATH'.
    %          'EnvVar_DataPath' - Like 'EnvVar_BasePath', but for the data
    %                   dir. Default is 'ASTROPACK_DATA_PATH'.
    %          'EnvVar_CatsHTMPath' - Like 'EnvVar_BasePath', but for the data
    %                   dir. Default is 'ASTROPACK_CATSHTM_PATH'.
    %          'EnvVar_ConfigPath' - Like 'EnvVar_BasePath', but for the data
    %                   dir. Default is 'ASTROPACK_CONFIG_PATH'.
    % Author : Eran Ofek (Jan 2022)
    % Example: startup,
    %          startup('AstroPack_CatsHTMPath','/last01/data/catsHTM','AstroPack_BasePath',  '/home/last01/ocs/matlab/LAST/AstroPack', 'AstroPack_ConfigPath','/home/last01/ocs/matlab/LAST/AstroPack/config', 'AstroPack_DataPath','/home/last01/ocs/matlab/data');
    
    arguments
        Args.setGraphics logical         = true;
        Args.setRandomNumbers logical    = true;
        Args.setFormat logical           = true;
        Args.setBaseVariables logical    = true;
        
        Args.AstroPack_BasePath          = [];  % e.g., '/home/eran/matlab/AstroPack'. If empty set to ~HOME/matlab/AstroPack, unless env var. exist
        Args.AstroPack_DataPath          = [];  % e.g., '/home/eran/matlab/data'. If empty set to ~HOME/matlab/data, unless env var. exist
        Args.AstroPack_CatsHTMPath       = '/last09/data1/catsHTM';
        Args.AstroPack_ConfigPath        = [];
        Args.AstroPack_ULTRASAT          = [];
        
        Args.EnvVar_BasePath             = 'ASTROPACK_PATH';
        Args.EnvVar_DataPath             = 'ASTROPACK_DATA_PATH';
        Args.EnvVar_CatsHTMPath          = 'ASTROPACK_CATSHTM_PATH';
        Args.EnvVar_ConfigPath           = 'ASTROPACK_CONFIG_PATH';
    end
    
    PWD = pwd;
    
    if Args.setGraphics
        setGraphics();
    end
    if Args.setRandomNumbers
        setRandomNumbers();
    end
    if Args.setFormat
        setFormat();
    end
    if Args.setBaseVariables
        setBaseVariables();
    end
    
    
    % get HomeDir
    if (ismac || isunix)
        % Linux / Mac
        HomeDir = getenv('HOME');
    else
        HomeDir = getenv('HOMEPATH');
    end
    
    %
    if isempty(Args.AstroPack_BasePath)
        % set to default value
        Args.AstroPack_BasePath = sprintf('%s%s%s%s%s',HomeDir,filesep,'matlab',filesep,'AstroPack');
    end
    if isempty(Args.AstroPack_DataPath)
        % set to default value
        Args.AstroPack_DataPath = sprintf('%s%s%s%s%s',HomeDir,filesep,'matlab',filesep,'data');
    end
    if isempty(Args.AstroPack_ULTRASAT)
        % set to default value
        Args.AstroPack_ULTRASAT = sprintf('%s%s%s%s%s',HomeDir,filesep,'matlab',filesep,'data',filesep,'ULTRASAT');
    end
    if isempty(Args.AstroPack_CatsHTMPath)
        % set to default value
        Args.AstroPack_CatsHTMPath = '/euler/catsHTM';
    end
    if isempty(Args.AstroPack_ConfigPath)
        % set to default value
        Args.AstroPack_ConfigPath = sprintf('%s%s%s',HomeDir,filesep,'config');
    end
    
    % get base path
    BasePath    = getEnvOrUseDefult(Args.EnvVar_BasePath, Args.AstroPack_BasePath);
    DataPath    = getEnvOrUseDefult(Args.EnvVar_DataPath, Args.AstroPack_DataPath);
    CatsHTMPath = getEnvOrUseDefult(Args.EnvVar_CatsHTMPath, Args.AstroPack_CatsHTMPath);
    ConfigPath  = getEnvOrUseDefult(Args.EnvVar_ConfigPath, Args.AstroPack_ConfigPath);
    
    % get list of directories to add to path
    DirList     = struct;
    DirList = defaultDirs_AstroPack_BasePath(DirList, BasePath);
    DirList = defaultDirs_AstroPack_DataPath(DirList, DataPath);
    DirList = defaultDirs_AstroPack_CatsHTMPath(DirList, CatsHTMPath);
    DirList = defaultDirs_AstroPack_ConfigPath(DirList, ConfigPath);
    
    
    %----------------------------------------------------------------------
    % Folders installed by Installer.install(), note that Installer.prep_cats()
    % is automatically called when installing 'cats' (same as the older
    % VO.prep.prep.data.dir() function)
    InstallerDataDir = DataPath;
    DirList.InstallerData = {{InstallerDataDir},...
                             {InstallerDataDir, 'spec'},...
                             {InstallerDataDir, 'spec/SpecGalQSO/'},...
                             {InstallerDataDir, 'spec/PicklesStellarSpec/'}};
    %----------------------------------------------------------------------
    % add all DirList to path
    FN  = fieldnames(DirList);
    Nfn = numel(FN);
    warning off;
    PathCount = 0;
    for Ifn=1:1:Nfn
        Tmp = DirList.(FN{Ifn});
        Ndir = numel(Tmp);
        for Idir=1:1:Ndir
            FullPath = sprintf('%s%s%s%s%s%s%s',fullfile(Tmp{Idir}{:}));
            % fprintf('AstroPack startup addpath: %s\n', FullPath);
            if exist(FullPath,'dir')
                addpath(FullPath);
                PathCount = PathCount + 1;
            end
        end
    end
    fprintf('AstroPack startup addpath count: %d\n', PathCount);
    fprintf('AstroPack startup done: %s\n', mfilename('fullpath'));
    
    
    cd(PWD);
end


function setGraphics
    % define graphics defaults
    % set the plot AxesFontSize and AxesFontName default
    set(0,'DefaultAxesFontSize',14);
    set(0,'DefaultAxesFontName','times');
end

function setRandomNumbers
    % random numbers
    % randomizing the seed of the matlab random number generator
    %rand('state',sum(100*clock));
    rng('shuffle');
end

function setFormat
    % display numbers format
    format short g
end

function setBaseVariables
    % define in session constants
    % Assign variables to the matlab workspace
    assignin('base','RAD',180./pi);    % Radian
end

function DirList=defaultDirs_AstroPack_BasePath(DirList,BasePath)
    % location relative to ~AstroPack/
    DirList.AstroPack_BasePath = {...   
             {BasePath,'matlab','astro'},...
             {BasePath,'matlab','base'},...
             {BasePath,'matlab','external'},...
             {BasePath,'matlab','external','mie_scattering'},...
             {BasePath,'matlab','external','kdtree','toolbox'},...
             {BasePath,'matlab','external','mcount'},...
             {BasePath,'matlab','external','str2doubles','str2doubles'},...
             {BasePath,'matlab','external','VChooseK'},...
             {BasePath,'matlab',fullfile('external','Inpaint_nans')},...
             {BasePath,'matlab','external','mcmcstat-master'},...
             {BasePath,'matlab','external','mcmcstat-master','src'},...
             {BasePath,'matlab','mex'},...
             {BasePath,'matlab',fullfile('mex','writematrix')},...
             {BasePath,'matlab','image'},...
             {BasePath,'matlab','pipeline'},...
             {BasePath,'matlab','soc'},...
             {BasePath,'matlab','util'},...
             {BasePath,'matlab','help'},...
             {BasePath,'matlab','obsolete'}            
             };
end

function DirList=defaultDirs_AstroPack_DataPath(DirList, BasePath)
    % location relative to ~AstroPack/
    DirList.AstroPack_DataPath = {...
             {BasePath,'SolarSystem','Time'},...
             {BasePath,'SolarSystem','VSOPE87'},...
             {BasePath,'SolarSystem','MinorPlanets'},...
             {BasePath,'SolarSystem','MinorPlanetsCT'},...
             {BasePath,'SolarSystem','INPOP'},...

             {BasePath},...
             {BasePath,'spec','GAIA_SpecTemplate'},...
             {BasePath,'spec','Sun'},...
             {BasePath,'spec','SpecReduction'},...
             {BasePath,'TestImages'},...
             };
end

function DirList=defaultDirs_AstroPack_ConfigPath(DirList, BasePath)
    % location relative to ~AstroPack/
    DirList.AstroPack_ConfigPath = {...
             {BasePath,'config'},...
             };
end

function DirList=defaultDirs_AstroPack_CatsHTMPath(DirList, CatsHTMDir)
    % location relative to ~AstroPack/
    DirList.CatsHTM = {{CatsHTMDir,'2MASS'},...
                           {CatsHTMDir,'2MASSxsc'},...
                           {CatsHTMDir,'AAVSO_VSX'},...
                           {CatsHTMDir,'AKARI'},...
                           {CatsHTMDir,'APASS'},...
                           {CatsHTMDir,'Cosmos'},...
                           {CatsHTMDir,'CRTS_per_var'},...
                           {CatsHTMDir,'DECaLS','DR5'},...
                           {CatsHTMDir,'FIRST'},...
                           {CatsHTMDir,'GAIA','DR1'},...
                           {CatsHTMDir,'GAIA','DR2'},...
                           {CatsHTMDir,'GAIA','DR2_19'},...
                           {CatsHTMDir,'GAIA','DRE3'},...
                           {CatsHTMDir,'GAIA','DR3'},...
                           {CatsHTMDir,'GAIA','DR3spec'},...
                           {CatsHTMDir,'GAIA','DR3extraGal'},...
                           {CatsHTMDir,'GALEX','DR6Plus7'},...
                           {CatsHTMDir,'GLADE','v1'},...
                           {CatsHTMDir,'GLADE','plus'},...
                           {CatsHTMDir,'GLIMPSE'},...
                           {CatsHTMDir,'HST','HSCv2'},...
                           {CatsHTMDir,'IPHAS','DR2'},...
                           {CatsHTMDir,'LAMOST','DR4'},...
                           {CatsHTMDir,'MergedCat','V2'},...
                           {CatsHTMDir,'NED','20180502'},...
                           {CatsHTMDir,'NOAO'},...
                           {CatsHTMDir,'NVSS'},...
                           {CatsHTMDir,'PGC'},...
                           {CatsHTMDir,'PS1'},...
                           {CatsHTMDir,'PTFpc'},...
                           {CatsHTMDir,'QSO','Flesch2021'},...
                           {CatsHTMDir,'ROSATfsc'},...
                           {CatsHTMDir,'SDSS','DR10'},...
                           {CatsHTMDir,'SDSS','DR14offset'},...
                           {CatsHTMDir,'Simbad_PM200'},...
                           {CatsHTMDir,'SkyMapper'},...
                           {CatsHTMDir,'SpecSDSS','DR14'},...
                           {CatsHTMDir,'SpecSDSS','DR17'},...
                           {CatsHTMDir,'Spitzer','IRACgc'},...
                           {CatsHTMDir,'Spitzer','SAGE'},...
                           {CatsHTMDir,'SWIREz'},...
                           {CatsHTMDir,'UCAC4'},...
                           {CatsHTMDir,'UCACGAIADR2accel'},...
                           {CatsHTMDir,'UKIDSS','DR10'},...
                           {CatsHTMDir,'unWISE'},...
                           {CatsHTMDir,'URAT1'},...
                           {CatsHTMDir,'VISTA'},...
                           {CatsHTMDir,'VISTA','Viking','DR2'},...
                           {CatsHTMDir,'VLASS','ep1'},...
                           {CatsHTMDir,'VST','ATLAS','DR3'},...
                           {CatsHTMDir,'VST','KiDS','DR3'},...
                           {CatsHTMDir,'WD','WDEDR3'},...
                           {CatsHTMDir,'WISE'},...
                           {CatsHTMDir,'XMM'},...
                           {CatsHTMDir,'ZTF','LCDR1'},...
                           {CatsHTMDir,'ZTF','SrcLCDR1'},...
                           {CatsHTMDir,'ZTF','ztfDR1var'}};
end


    
%--- utility functions ---    

function Str = cellElements2str(Cell, PreStr)
    %
    
    if ~iscell(Cell)
        Str = Cell;
    else
        N = numel(Cell);
        Str = Cell{1};
        for I=2:1:N
            Str = sprintf('%s%s%s',Str,filesep,Cell{I});
        end
    end
    if nargin>1
        Str = sprintf('%s%s%s',PreStr,filesep,Str);
    end
end


function EnvVar = getEnvOrUseDefult(EnVarName, DefaultVal)
    %
    EnvVar = getenv(EnVarName);
    if isempty(EnvVar)
        EnvVar = DefaultVal;
    end
    
    if isempty(EnvVar)
        error('Can not find definition to %s - Either edit startup, or set up the environment variable with this name');
    end
    
end

