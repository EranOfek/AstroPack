% NOTE: Remember to set environment variable ASTROPACK_PATH to AstroPack root folder
%
% The default matlab startup script for AstroPack
%
% Open with: edit(fullfile(userpath,'startup.m'))
%
% startup.m folder:
%
% Linux:   
%   Eran    - /home/eran/???
%   Yossi   - /home/???
%   Chen VM - /home/chent/Documents/MATLAB/startup.m
%
% Windows: 
%   Chen Desktop - C:\Users\chen\Documents\MATLAB\startup.m
%   Chen Laptop  - C:\Users\chent\OneDrive\Documents\MATLAB\startup.m
%

fprintf('AstroPack startup.m started: %s\n', mfilename('fullpath'));
fprintf('Master startup.m file is located in AstroPack/matlab/startup\n');

% Print compiler flags
if isdeployed
    fprintf('startup: isdeployed = true\n');
end    
if ismcc
    fprintf('startup: ismcc = true\n');
end        
    
    
% Do the actual work
doStartup();

fprintf('AstroPack startup.m done: %s\n', mfilename('fullpath'));
fprintf('Remember to set environment variable ASTROPACK_PATH to AstroPack root folder, for example /home/eran/matlab/AstroPack\n');
fprintf('Remember to set environment variable ASTROPACK_DATA_PATH to AstroPack root folder, for example ~/matlab/data\n');
fprintf('Remember to set environment variable ASTROPACK_CONFIG_PATH to configuration files folder, if not set, repo config/ is used\n');

%--------------------------------------------------------------------------
function doStartup()

    fprintf('AstroPack doStartup() started: %s\n', mfilename('fullpath'));

    % display numbers format
    format short g

    % random numbers
    % randomizing the seed of the matlab random number generator
    %rand('state',sum(100*clock));
    rng('shuffle');


    % define graphics defaults
    % set the plot AxesFontSize and AxesFontName default
    set(0,'DefaultAxesFontSize',14);
    set(0,'DefaultAxesFontName','times');


    % define in session constants
    % Assign variables to the matlab workspace
    assignin('base','RAD',180./pi);    % Radian


    % BaseDir
    MatlabDir    = 'matlab';          %%% <--- EDIT IF NEEDED
    AstroPackDir = 'AstroPack';       %%% <--- EDIT IF NEEDED
    CatsHTMDir   = '/euler/catsHTM';  %%% <--- EDIT IF NEEDED - use '', if no catsHTM - Add Windows support!

    if (ismac || isunix)
        % Linux / Mac
        HomeDir = getenv('HOME');
        AstroPackPath = getenv('ASTROPACK_PATH');
        if isempty(AstroPackPath)    
            AstroPackPath = fullfile(HomeDir, MatlabDir, AstroPackDir);
        end
        
        AstroPackDataPath = getenv('ASTROPACK_DATA_PATH');        
        if isempty(AstroPackPath)    
            AstroPackDataPath = fullfile(HomeDir, MatlabDir, 'data');
        end        
    else
        % Windows
        HomeDir = getenv('HOMEPATH');
        AstroPackPath = getenv('ASTROPACK_PATH');    
        if isempty(AstroPackPath)
            AstroPackPath = fullfile(HomeDir, MatlabDir, AstroPackDir);
        end
        
        AstroPackDataPath = getenv('ASTROPACK_DATA_PATH');    
        if isempty(AstroPackDataPath)
            AstroPackDataPath = 'C:/AstroPack/matlab/data';  % @Todo - Fix to use C:\AstroPack from getenv
        end            
    end

    if (isempty(HomeDir))
        error('Can not find home directory environment variable - edit the startup.m file accordingly');
    end

    if (isempty(AstroPackPath))
        error('Can not find AstroPack directory, set ASTROPACK_PATH - edit the startup.m file accordingly');
    end
    
    if (isempty(AstroPackDataPath))
        fprintf('Warning: Can not find AstroPack Data directory, set ASTROPACK_DATA_PATH (default is ~/matlab/data on Linux, C:\\AstroPack\\matlab\\data on Windows');
    end    


    % Configuation path, default is in repo config/ 
    AstroPackConfigPath = getenv('ASTROPACK_CONFIG_PATH');
    if isempty(AstroPackConfigPath)
        AstroPackConfigPath = fullfile(AstroPackPath, 'config');
    end
    fprintf('AstroPackConfigPath: %s\n', AstroPackConfigPath);
    fprintf('AstroPackDataPath: %s\n', AstroPackDataPath);
    %----------------------------------------------------------------------
    % addpath for AstroPack
    % This is a cell array of cell arrays
    % Each cell contains one path to add.
    % The inside cell gives the directory path - e.g., {'matlab','astro'} means .../matlab/astro/
    % Check with @Eran if we can remove the 'data' folders because they are
    % part of Installer
    DirList.AstroPack = {...
             {AstroPackPath,'data','test_images','fits_samples'},...
			 {AstroPackPath,'..','data','SolarSystem','Time'},...
			 {AstroPackPath,'..','data','SolarSystem','VSOPE87'},...
 			 {AstroPackPath,'..','data','SolarSystem','MinorPlanets'},...
			 {AstroPackPath,'..','data'},...
			 {AstroPackPath,'..','data','spec','GAIA_SpecTemplate'},...
             {AstroPackPath,'matlab','astro'},...
             {AstroPackPath,'matlab','base'},...
             {AstroPackPath,'matlab','external'},...
             {AstroPackPath,'matlab',fullfile('external','Inpaint_nans')},...
             {AstroPackPath,'matlab','mex'},...             
             {AstroPackPath,'matlab',fullfile('mex','writematrix')},...             
             {AstroPackPath,'matlab','image'},...
             {AstroPackPath,'matlab','pipeline'},...
             {AstroPackPath,'matlab','util'},...
             {AstroPackPath,'matlab','soc'},...
             {AstroPackPath,'matlab','help'},...
             {AstroPackPath,'matlab','obsolete'}
             };

    %----------------------------------------------------------------------
    if ~isempty(CatsHTMDir)
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
                           {CatsHTMDir,'GALEX','DR6Plus7'},...
                           {CatsHTMDir,'GLADE'},...
                           {CatsHTMDir,'GLIMPSE'},...
                           {CatsHTMDir,'HST','HSCv2'},...
                           {CatsHTMDir,'IPHAS','DR2'},...
                           {CatsHTMDir,'LAMOST','DR4'},...
                           {CatsHTMDir,'NED','20180502'},...
                           {CatsHTMDir,'NOAO'},...
                           {CatsHTMDir,'NVSS'},...
                           {CatsHTMDir,'PGC'},...
                           {CatsHTMDir,'PS1'},...
                           {CatsHTMDir,'PTFpc'},...
                           {CatsHTMDir,'ROSATfsc'},...
                           {CatsHTMDir,'SDSS','DR10'},...
                           {CatsHTMDir,'SDSS','DR14offset'},...
                           {CatsHTMDir,'Simbad_PM200'},...
                           {CatsHTMDir,'SkyMapper'},...
                           {CatsHTMDir,'SpecSDSS','DR14'},...
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
                           {CatsHTMDir,'VST','ATLAS','DR3'},...
                           {CatsHTMDir,'VST','KiDS','DR3'},...
                           {CatsHTMDir,'WISE'},...
                           {CatsHTMDir,'XMM'},...
                           {CatsHTMDir,'ZTF','LCDR1'},...
                           {CatsHTMDir,'ZTF','SrcLCDR1'},...
                           {CatsHTMDir,'ZTF','ztfDR1var'}};                     
    end       
    %----------------------------------------------------------------------
    % Folders installed by Installer.install(), note that Installer.prep_cats()
    % is automatically called when installing 'cats' (same as the older
    % VO.prep.prep.data.dir() function)
    InstallerDataDir = AstroPackDataPath;
    DirList.InstallerData = {{InstallerDataDir},...
                             {InstallerDataDir, 'spec'},...
                             {InstallerDataDir, 'spec/SpecGalQSO/'},...
                             {InstallerDataDir, 'spec/PicklesStellarSpec/'}};       
    %----------------------------------------------------------------------
    % add all DirList to path                 
    FN  = fieldnames(DirList);
    Nfn = numel(FN);    
    PathCount = 0;   
    if ~(ismcc || isdeployed)
        warning off;
    end    
    if ~isdeployed        
        for Ifn=1:1:Nfn
            Tmp = DirList.(FN{Ifn});
            Ndir = numel(Tmp);
            for Idir=1:1:Ndir
                FullPath = sprintf('%s%s%s%s%s%s%s',fullfile(Tmp{Idir}{:}));
                addpath(FullPath);
                PathCount = PathCount + 1;
            end
        end    
    end
    
    fprintf('AstroPack doStartup() addpath count: %d\n', PathCount);    
    if ~(ismcc || isdeployed)
        warning on;
    end

    % fprintf('AstroPack doStartup() cd: %s\n', AstroPackPath);
    % cd(AstroPackPath);

    fprintf('AstroPack doStartup() done: %s\n', mfilename('fullpath'));

end
