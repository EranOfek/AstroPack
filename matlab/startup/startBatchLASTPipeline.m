function startBatchLASTPipeline(Args)
    %startBatchLASTPipeline Starts the LAST pipeline in batch mode
    %   Prepares the batch environment needed by the LAST pipeline
    %
    %   Argument: [numeric] The DataDir Id
    %
    
    arguments
        Args.Id     % [numeric] Currently either 1 or 2, but maybe more
        Args.PipelineVersion = 'v0'  % 'v0' - current (production) pipeline.DemonLAST; 'v1' - development pipeline.last.pipes.PipelineDemon
    end

    if ~isfield(Args, 'Id') || ~isnumeric(Args.Id)
        error("Must supply 'Id' argument ([numeric] The DataDir Id)");
    end

    % Get a daemon object
    switch lower(Args.PipelineVersion)
        case 'v0'
            D = pipeline.DemonLAST;
        case 'v1'
            D = pipeline.last.pipes.PipelineDemon;
        otherwise
            error('Unknown PipelineVersion option');
    end

    % Setup the logging environment
    D.Logger.Console = false;
    D.Logger.Syslog.ProgName = sprintf("last-pipeline%d", Args.Id);

    % Tell the daemon which data directory to monitor
    D.DataDir = Args.Id;

    % Notify systemd (if SYSTEMD env. var. exists) that the service is
    % running and what is its main process id
    tools.systemd.mex.notify_ready;

    % Run the actual daemon - pause (rather than crash) and warn loudly if
    % the data disk gets critically full, resuming once space frees up
    D.main('StopButton', false, 'StopDiskFull', 99, 'PauseDiskFull', 300);
end
