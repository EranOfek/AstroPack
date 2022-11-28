function Result = unitTest()
    % FileProcessor.unitTest
    
    io.msgLog(LogLevel.Test, 'FileProcessor test started');

    InputPath = 'c:/soc/snr/input';

    % Create objcet
    fp = FileProcessor('InputPath', InputPath, 'InputMask', '*.inp');
    fp.ProcessFileFunc = @FileProcessorCallback;

    % Input loop will call FileProcessorCallback (below) for each input
    % file found in the folder
    fp.process('DelaySec', 0.1);
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed');                          
    Result = true;
end




function FileProcessorCallback(FileName)
    io.msgLog(LogLevel.Info, 'FileProcessorCallback started: %s', FileName);
    
    disp(FileName);
    
    TmpFileName = strcat(FileName, '.out.tmp');
    OutFileName = strcat(FileName, '.out');
    
    % Write output to TmpFileName
    fid = fopen(TmpFileName,'wt');
    fprintf(fid, 'This is my reply.');
    fclose(fid);

    % Rename final file to output extension
    io.msgLog(LogLevel.Info, 'Rename output %s -> %s', TmpFileName, OutFileName);
    movefile(TmpFileName, OutFileName);
end




