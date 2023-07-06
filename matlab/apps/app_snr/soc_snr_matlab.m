%==========================================================================
% Author: Chen Tishler, Dec. 2022
% Notes:
%    - mcc generates EXE file which is a self-extract ZIP, with all required source files.
%    - It is not possible to call addpath() in deployed application!
%    - snakeyaml-1.9.jar should be in the EXE folder.
%    - ASTROPACK_CONFIG_PATH environment must be set
%
% See: https://www.mathworks.com/help/compiler/create-and-install-a-standalone-application-from-matlab-code.html
% See: https://www.mathworks.com/help/compiler/mcc.html
% See: https://www.mathworks.com/help/compiler/isdeployed.html
%
% NOTE: To run in from within MATLAB, you have to change folder
%       to the folder of this file (i.e. app_snr/)
%==========================================================================

function soc_snr_matlab()
    % SNR App

    % Set logfile name
	fprintf('soc_snr_matlab started\n');
    LogFile.getSingleton('FileName', 'soc_snr_matlab');
            
    if isdeployed
        fprintf('soc_snr_matlab: isdeployed = TRUE\n');
        locate_externapp = which(fullfile('soc_snr_matlab.exe'));
        fprintf('which: %s\n', locate_externapp);
    end    
    
    if ismcc
        fprintf('soc_snr_matlab: ismcc = TRUE\n');
    end           

    % Test that we can find file 'Gal_Sc.txt'
    if isdeployed
        FName = 'Gal_Sc.txt';
        fprintf('\n\n\nChecking: %s\n', FName);
        fprintf('Before loadMap:\n');
        fprintf('which:\n');
        which(FName)
        MatFile = fileMapFind(FName, 'Assert', false);
        fprintf('FileMap: %s\n', MatFile);

        FMap = FileMap.getSingleton();
        if isunix
            FMap.StorageFileName = '/tmp/FileMap';
        else
            FMap.StorageFileName = 'c:/soc/snr/snr_matlab/FileMap';
        end
            
        FMap.loadMap();    
        fprintf('\nCalling FileMap.addAll ...\n');
        FMap.addAll(true);    

        fprintf('After loadMap\n');
        fprintf('which:\n');
        which(FName)    
        MatFile = fileMapFind(FName, 'Assert', false);
        fprintf('FileMap: %s\n', MatFile);
        fprintf('\n\n\n');
    end

    % Create component to trigger Config loading
    fprintf('Creating Comp\n');
    Comp = Component();
    fprintf('Config.Path: %s\n', Comp.Config.Path);
    fprintf('Comp.Config.Data.MsgLogger.FileName: %s\n', Comp.Config.Data.MsgLogger.FileName);

    % Call UltrasatPerf() to force the mcc compiler linking this class and
    % its related sources. Otherwise we will get this error:
    %     09:50:58.133 [DBG] doProcessSnr: creating UltrasatPerf2GUI
    %     09:50:58.176 [DBG] UltrasatPerf2GUI: UltrasatPerf2GUI:load: c:\soc\snr\snr_matlab\P90_UP_test_60_ZP_Var_Cern_21.mat
    %     Warning: Variable 'UP' originally saved as a UltrasatPerf cannot be instantiated as an object and will be read in as a uint32.    
    fprintf('Calling: Perf = UltrasatPerf(Init, false);\n');    
    Perf = UltrasatPerf('Init', false);
    fprintf('UltrasatPerf done\n');
    
    %try
        %if ~isdeployed
            fprintf('Calling FileProcessor.unitTest\n');
            FileProcessor.unitTest();
            fprintf('Returned from FileProcessor.unitTest\n');
        %end
    %catch
        %fprintf('soc_snr_matlab exception\n');
    %end
    
    fprintf('soc_snr_matlab done\n');
end
