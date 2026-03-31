function [Result] = unitTest(Args)
    % A unit test for the LAST pipeline
    %     Optional detailed description
    % Input  : -
    %          * ...,key,val,... 
    %
    % Output : - 
    % Author : A.M. Krassilchtchikov (2026 Mar) 
    % Example: 
    % 
    arguments
        Args.LocalPath         = '~/LASTunitTest';
        Args.RAWImageDir       = '/mnt/marvin/LAST.01.01.01/2025/07/07/raw/'
        Args.CalibDir          = '/mnt/marvin/LAST.01.01.01/calib/'
        Args.RefPath           = '/mnt/euclid/last/data/references/v4/'
        Args.StartImage        = 'LAST.01.01.01_20250708.012814.769_clear_1718.c_015_001_001_sci_raw_Image_1.fits.fz' % currently not used 
        Args.StartJD           = 0;   % [24 4 2023]
        Args.EndJD             = Inf; % [25 4 2023]
        Args.RegenCalib        = false; % we do not know yet how to write the new calib to a local dir and use it from there
    end
    % arrange a local folder to store results 
    Args.LocalPath = tools.os.relPath2absPath(Args.LocalPath);    
    if ~isfolder(Args.LocalPath)
        mkdir(Args.LocalPath);
    end
    cd(Args.LocalPath);
    % create the daemon and configure paths 
    D=pipeline.last.pipes.PipelineDemon;
    D.setPath(Args.LocalPath,...
           'NewPath',Args.RAWImageDir,...
           'CalibPath',Args.CalibDir,...
           'FailedPath',strcat(Args.LocalPath,'/','failed/')...
           );
    D.RefPath = Args.RefPath;
    D.LogPath = strcat(Args.LocalPath,'/','log/');
    % run the pipeline
    D.main('StopWhenDone',true,'Insert2DB',false, 'SaveEpochProduct',{'Image','Mask','Cat','PSF'},'StopButton',false,...
        'StartJD', Args.StartJD, 'EndJD', Args.EndJD, ...
        'RegenCalib', Args.RegenCalib, ...
        'pipelineIArgs', {'UseParfor',true,'prePrepArgs',{'AstroImageReadArgs',{'UseMex', true}} } );
    % 
end
