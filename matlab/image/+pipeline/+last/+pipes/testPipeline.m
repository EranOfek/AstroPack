function [Result] = testPipeline(Args)
    % A unit test for the LAST pipeline
    %     Optional detailed description
    % Input  : -
    %          * ...,key,val,...
    %            'LocalPath'        - the local directory to save the pipeline products and logs (def. '~/LASTunitTest')
    %            'RAWImageDir'      - a (remote) directory with RAW images; if empty, the function does nothing (def. empty)
    %            'CalibDir'         - path to calibration images; if empty, derived from 'RAWImageDir' (def. empty)
    %            'RefPath'          - path to reference images, used by PipelineII (def. '/mnt/euclid/last/data/references/v4/')
    %            'StartImage'       - explicit RAW image filename marking the start of the processed range; overrides 'StartTime' when set (def. a representative LAST filename)
    %            'StartTime'        - start time of the processed interval, as a date vector or JD; ignored if 'StartImage' is set (def. empty)
    %            'TimeInterval'     - length of the processed time interval, in seconds (def. 420)
    %            'MinInGroup'       - minimal number of RAW images in 'RAWImageDir' required to start processing (def. 10)
    %            'RegenCalib'       - regenerate the calibration images locally (def. false)
    %            'PipelineVersion'  - one of the following options (def. 'v1'):
    %                   'v1' - new pipeline.
    %                   'v0' - current (production) pipeline.
    %            'UseParfor'        - use MATLAB parallelization; disabling it may help accurate debugging (def. true)
    %            'DebugMode'        - run extra tests (e.g., DB injection) not required for a production run (def. false)
    %            'RemoveAfterWrite' - remove pipeline product files after they are written to disk; useful for massive tests (def. false)
    % Output : - filled visit directory Args.LocalPath/YYYY/MM/DD/proc/HHMMSSvXX (XX starting with 0)
    % Author : A.M. Krassilchtchikov (2026 Mar) 
    % Example: RAWImageDir = '/mnt/marvin/LAST.01.01.01/2025/07/07/raw/';
    %          pipeline.last.pipes.testPipeline('RAWImageDir',RAWImageDir,'StartTime',[8 7 2025 01 28 0],'RemoveAfterWrite',true);
    %          pipeline.last.pipes.testPipeline('RAWImageDir',RAWImageDir,'PipelineVersion','prod', 'StartImage','LAST.01.01.01_20250708.003700.313_clear_1718.c_001_001_001_sci_raw_Image_1.fits.fz');
    arguments
        Args.LocalPath         = '~/LASTunitTest';
        Args.RAWImageDir       = [] 
        Args.CalibDir          = [] 
        Args.RefPath           = '/mnt/euclid/last/data/references/v4/'
        Args.StartImage        = 'LAST.01.01.01_20250708.000029.080_clear_1718.c_001_001_001_sci_raw_Image_1.fits.fz' 
        Args.StartTime         = []    % [2025 8 7 01 28 0] or 2025.3456
        Args.TimeInterval      = 420   % [s] 
        Args.MinInGroup        = 10    % the minimal number of RAW images in 'RAWImageDir'required to start processing  
        Args.RegenCalib        = false % we do not know yet how to write the new calib to a local dir and use it from there
        Args.PipelineVersion   = 'v1'  % 'v0' is the production version, 'v1' is the development 
        Args.UseParfor         = true  % use the matlab parallelization; in some cases may prevent accurate debugging  
        Args.DebugMode         = false % do some tests (e.g., DB injection) not required for a production phase  
        Args.RemoveAfterWrite  = false % an option to remove pipeline product files after they are written to disk, useful for massive tests
    end
    
    % if running without explicit arguments, do nothing
    if isempty(Args.RAWImageDir)
        fprintf("LAST pipeline unit test: run me with at least 'RAWImageDir' defined\n");
       return 
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
        
        case 'v1'            
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
                'pipelineIArgs', {'UseParfor',Args.UseParfor,'prePrepArgs',{'AstroImageReadArgs',{'UseMex', true}} },...
                'MoveNew2Raw',false,...
                'DebugMode',Args.DebugMode,...
                'RemoveAfterWrite',Args.RemoveAfterWrite,...
                'DbHost','10.150.28.18');
            
        case 'v0'
            
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
                'CompressedRAW',true,...
                'LocalBase',Args.LocalPath,...
                'MoveRaw2OutputDir',false,...
                'Backup',false, 'DebugMode', true,...
                'DbHost','10.150.28.18');
                   
        otherwise 
            error('Unknown pipeline version');
    end
    % 
    Result = 'Passed';
end
