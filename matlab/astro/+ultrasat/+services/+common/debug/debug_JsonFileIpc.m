function debug_JsonFileIpc()
    % debug_JsonFileIpc
    %   Creates temporary folders, instantiates JsonFileIpc,
    %   writes sample JSON files and runs short processing demo.
    %   Works on Windows, Linux, macOS.

    clc;
    fprintf('=== JsonFileIpc debug / demonstration ===\n\n');

    % 1. Prepare temporary folders
    baseTemp = tempdir();
    workFolder = fullfile(baseTemp, 'JsonIpcDebug');
    inputFolder = fullfile(workFolder, 'input');
    processedFolder = fullfile(workFolder, 'processed');

    if ~isfolder(inputFolder)
        mkdir(inputFolder);
    end
    if ~isfolder(processedFolder)
        mkdir(processedFolder);
    end

    fprintf('Using folders:\n');
    fprintf('  Input:     %s\n', inputFolder);
    fprintf('  Processed: %s\n\n', processedFolder);

    % 2. Create JsonFileIpc instance
    ipc = ultrasat.services.common.JsonFileIpc( ...
        'InputPath', inputFolder, ...
        'InputMask', '*.json', ...
        'ProcessedPath', processedFolder, ...
        'KeepProcessedFilesDays', 7, ...
        'WatchdogInterval', 10, ...
        'WatchdogFileName', fullfile(workFolder, 'watchdog.txt') ...
    );

    fprintf('JsonFileIpc created.\n');
    fprintf('Input path:  %s\n', ipc.InputPath);
    fprintf('Processed:   %s\n\n', ipc.ProcessedPath);

    % 3. Write sample JSON files
    writeSampleJson(inputFolder, 'job_001.json', ...
        struct('task', 'calibrate', 'sensor', 42, 'timestamp', '2026-02-10T14:30:00Z'));

    writeSampleJson(inputFolder, 'job_002.json', ...
        struct('task', 'process_image', 'filename', 'img_5678.fits', ...
               'params', struct('threshold', 3.14, 'maxiter', 100)));

    writeSampleJson(inputFolder, 'job_003.json', ...
        struct('command', 'status', 'id', 'health-check-17'));

    fprintf('Created 3 sample JSON files.\n\n');

    % 4. Simple demo callback
    simpleCallback = @(data) struct( ...
        'received', datestr(now, 'yyyy-mm-dd HH:MM:SS'), ...
        'status', 'ok', ...
        'original_task', data.task, ...
        'answer', 42 );

    ipc.Callback = simpleCallback;

    fprintf('Using simple demo callback.\n\n');

    % 5. Run short processing loop
    fprintf('Starting 15-second processing loop...\n');
    ipc.processLoop('DelaySec', 0.4, 'MaxProcessTime', 15);

    fprintf('\nDemo finished.\n');

    % 6. Summary
    processedFiles = dir(fullfile(processedFolder, '*.json'));
    if ~isempty(processedFiles)
        fprintf('Processed files (%d):\n', numel(processedFiles));
        for i = 1:numel(processedFiles)
            fprintf('  %s\n', processedFiles(i).name);
        end
    else
        fprintf('No files moved to processed folder.\n');
    end

    fprintf('\nDebug complete.\n');
    fprintf('Temp folder: %s\n', workFolder);
end

function writeSampleJson(folder, filename, dataStruct)
    fullpath = fullfile(folder, filename);
    jsonText = jsonencode(dataStruct, 'PrettyPrint', true);
    
    fid = fopen(fullpath, 'wt', 'n', 'UTF-8');
    if fid < 0
        error('Cannot create file: %s', fullpath);
    end
    fprintf(fid, '%s\n', jsonText);
    fclose(fid);
    
    fprintf('  Wrote: %s\n', filename);
end
