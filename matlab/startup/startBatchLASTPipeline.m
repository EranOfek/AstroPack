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

    % Record which pipeline version and which AstroPack revision are running
    D.Logger.msgLog(LogLevel.Info, 'startBatchLASTPipeline: Id=%d, PipelineVersion=%s, AstroPack=%s', ...
                    Args.Id, lower(Args.PipelineVersion), astroPackRevision);

    % Tell the daemon which data directory to monitor
    D.DataDir = Args.Id;

    % Notify systemd (if SYSTEMD env. var. exists) that the service is
    % running and what is its main process id
    tools.systemd.mex.notify_ready;

    % Run the actual daemon - pause (rather than crash) and warn loudly if
    % the data disk gets critically full, resuming once space frees up
    D.main('StopButton', false, 'StopDiskFull', 99, 'PauseDiskFull', 300);
end


function Rev = astroPackRevision()
    % Git revision of the AstroPack tree this function was loaded from
    % Output : - Revision string ('unknown' if it could not be obtained).

    Rev = 'unknown';
    try
        RepoDir = fileparts(fileparts(which(mfilename)));
        [Status, Out] = system(sprintf('git -C %s describe --always --dirty --abbrev=8', RepoDir));
        if Status==0
            % take the last line only - 'system' output may carry unrelated
            % loader messages on some nodes (LAST_issues #217)
            Lines = strsplit(strtrim(Out), newline);
            Rev   = strtrim(Lines{end});
        end
    catch
        % keep 'unknown'
    end
end
