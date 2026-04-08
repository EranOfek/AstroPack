function [Result] = unitTest(Args)
    % A unit test for the LAST pipeline
    %     Optional detailed description
    % Input  : -
    %          * ...,key,val,... 
    %
    % Output : - 
    % Author : A.M. Krassilchtchikov (2026 Mar) 
    % Example: pipeline.last.pipes.unitTest('StartTime',[8 7 2025 01 28 0]);
    % 
    arguments
        Args.LocalPath         = '~/LASTunitTest';
        Args.RAWImageDir       = '/mnt/marvin/LAST.01.01.01/2025/07/07/raw/'
        Args.CalibDir          = [] % '/mnt/marvin/LAST.01.01.01/calib/'
        Args.RefPath           = '/mnt/euclid/last/data/references/v4/'
        Args.StartImage        = [] % 'LAST.01.01.01_20250708.012814.769_clear_1718.c_015_001_001_sci_raw_Image_1.fits.fz' % currently not used 
        Args.StartTime         = [] % [2025 8 7 01 28 0] or 2025.3456
        Args.TimeInterval      = 450  % [s] 
        Args.MinInGroup        = 10 
        Args.RegenCalib        = false; % we do not know yet how to write the new calib to a local dir and use it from there
        Args.PipelineVersion   = 'dev'; % 'dev' or 'prod'
    end
    
    % arrange a local folder to store results 
    Args.LocalPath = tools.os.relPath2absPath(Args.LocalPath);    
    if ~isfolder(Args.LocalPath)
        mkdir(Args.LocalPath);
    end
    % determine calib directory
    if isempty(Args.CalibDir)
        Args.CalibDir = regexprep(Args.RAWImageDir, '^(.*LAST\.\d{2}\.\d{2}\.\d{2}).*$', '$1/calib/');
    end
    % if an explicit start image is given, override Args.StartTime:
    if isempty(Args.StartImage)
        if isempty(Args.StartTime)
            StartJD = 0;
            EndJD   = Inf;
        else
            StartJD = celestial.time.date2jd(Args.StartTime);
            EndJD   = StartJD + Args.TimeInterval/3600/24;
        end
    else
        Tokens    = regexp(Args.StartImage, 'LAST\.\d+\.\d+\.\d+_(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})(\d{2})', 'tokens');
        TimeParts = str2double(Tokens{1});
        StartJD   = celestial.time.date2jd([TimeParts(1) TimeParts(2) TimeParts(3) TimeParts(4) TimeParts(5) TimeParts(6)-10]); 
        EndJD     = StartJD + Args.TimeInterval/3600/24;    
    end
        
    % create the daemon and configure paths 
    switch lower(Args.PipelineVersion)
        
        case 'dev'            
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
                'StartJD', StartJD, 'EndJD', EndJD, ...
                'RegenCalib', Args.RegenCalib, ...
                'MinInGroup',Args.MinInGroup, ...
                'UpdateStatusFile', false, ...
                'pipelineIArgs', {'UseParfor',true,'prePrepArgs',{'AstroImageReadArgs',{'UseMex', true}} },...
                'MoveNew2Raw',false,...
                'DebugMode',true);
            
        case 'prod'
            
            D = pipeline.DemonLAST;
            
            D.ManualPath = 1;                        
            D.setPath(regexprep(Args.RAWImageDir, '^(.*LAST\.\d{2}\.\d{2}\.\d{2}).*$', '$1/'),...
                'NewPath',Args.RAWImageDir,...
                'CalibPath',Args.CalibDir); 
            D.FailedPath = strcat(Args.LocalPath,'/','failed/');
            D.LogPath = strcat(Args.LocalPath,'/','log/');
            D.SciPath = strcat(Args.LocalPath,'/','sci/');
            D.RefPath = Args.RefPath;
                        
            D.main('StopWhenDone',true,'Insert2DB',false, 'SaveEpochProduct',{'Image','Mask','Cat','PSF'},'StopButton',false,...
                'StartJD', StartJD, 'EndJD', EndJD, ...  
                'RegenCalib', Args.RegenCalib, ...
                'MinInGroup',Args.MinInGroup, ...
                'InsertTransients2DB', false, ...
                'UpdateStatusFile', false, ...
                'PauseDay',1,'PauseNight',1, ...
                'TempRawSci','*_sci_raw_*fits*',...
                'CompressedRAW',true);
                   
        otherwise 
            error('Unknown pipeline version');
    end
    % 
    Result = 'Passed';
end
