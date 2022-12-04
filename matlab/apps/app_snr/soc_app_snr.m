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
%==========================================================================

function soc_app_snr()
    % SNR App

	fprintf('soc_app_snr started\n');
    
    if isdeployed
        fprintf('soc_app_snr: isdeployed = TRUE\n');
        locate_externapp = which(fullfile('soc_app_snr.exe'));
        fprintf('which: %s\n', locate_externapp);
    end    
    
    if ismcc
        fprintf('soc_app_snr: ismcc = TRUE\n');
    end        
    
    %path();
    
    if ~(ismcc || isdeployed)
        %addpath('D:\Ultrasat\AstroPack.git\');
        %addpath('D:\Ultrasat\AstroPack.git\matlab');
        %addpath('D:\Ultrasat\AstroPack.git\matlab\external');
        %addpath('D:\Ultrasat\AstroPack.git\config');
    end
    

    %if ~isdeployed
        fprintf('Creating Comp\n');
        Comp = Component();
        fprintf('Config.Path: %s\n', Comp.Config.Path);
        fprintf('Comp.Config.Data.MsgLogger.FileName: %s\n', Comp.Config.Data.MsgLogger.FileName);
    %end

    %try
        %if ~isdeployed
            fprintf('Calling FileProcessor.unitTest\n');
            FileProcessor.unitTest();
            fprintf('Returned from FileProcessor.unitTest\n');
        %end
    %catch
        %fprintf('soc_app_snr exception\n');
    %end
    
    fprintf('soc_app_snr done\n');
end