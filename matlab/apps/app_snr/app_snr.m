function Result = snrapp()
    % FileProcessor.unitTest
    
    %io.msgLog(LogLevel.Test, 'FileProcessor test started');

    % Create instances
    while true
        io.msgLog(LogLevel.Info, 'SnrApp FileProcessor started');
        try
            fp = FileProcessor;
            fp.InputPath = '';
            fp.InputFileMask = '.inp';
            fp.ProcessFileFunc = @FileProcessorCallback;

            % Input loop will call FileProcessorCallback for each input
            % file found in the folder
            fp.InputLoop(10);

        catch
            io.msgLog(LogLevel.Info, 'SnrApp exception');
        end
    end
    
    %io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed');                          
    Result = true;
end



function FileProcessorCallback(FileName)
    io.msgLog(LogLevel.Info, 'FileProcessorCallback started: %s', FileName);
    
    disp(FileName);
    
    TmpFileName = FileName + '.tmp';    
    OutFileName = FileName + '.out';
    
    % Write output to TmpFileName
    
    % Rename final file to output extension
   io.msgLog(LogLevel.Info, 'Rename output %s -> %s', TmpFileName, OutFileName);
    movefile(TmpFileName, OutFileName);
end


