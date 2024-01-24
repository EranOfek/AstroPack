function startBatchLASTPipeline(Args)
    %startBatchLASTPipeline Starts the LAST pipeline in batch mode
    %   Prepares the batch environment needed by the LAST pipeline
    %
    %   Argument: [numeric] The DataDir Id
    %
    
    arguments
        Args.Id     % [numeric] Currently either 1 or 2, but maybe more
    end
    
    if ~isfield(Args, 'Id') || ~isnumeric(Args.Id)
        error("Must supply 'Id' argument ([numeric] The DataDir Id)");
    end
    
    % Get a daemon object
    D = pipeline.DemonLAST;
    
    % Setup the logging environment
    D.Logger.Console = false;
    D.Logger.Syslog.ProgName = sprintf("last-pipeline%d", Args.Id);
    
    % Tell the daemon which data directory to monitor
    D.DataDir = Args.Id;
    
    % Notify systemd (if SYSTEMD env. var. exists) that the service is
    % running and what is its main process id
    tools.systemd.mex.notify_ready;
    
    % Run the actual daemon
    D.main('StopButton', false, 'StopFullDisk', 95);
end
