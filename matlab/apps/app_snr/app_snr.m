function Result = snrapp()
    % SNR App    
    
    %io.msgLog(LogLevel.Test, 'FileProcessor test started');

    InputPath = 'c:/soc/snr/input';
    
    % Application loop - process input files
    while true
        io.msgLog(LogLevel.Info, 'SnrApp FileProcessor started');
        
        % This try/catch block is to make sure that the application
        % will continue to work in case of exception
        try
            % Create objcet
            fp = FileProcessor('InputPath', InputPath, 'InputMask', '.inp');
            fp.ProcessFileFunc = @FileProcessorCallback;

            % Input loop will call FileProcessorCallback (below) for each input
            % file found in the folder
            fp.process('DelaySec', 0.1);

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


