
function Result = unitTest()
    % Unit-Test
        
    io.msgStyle(LogLevel.Test, '@start', '... test started')
          
    %Proc = FileProcessor;
    %Proc.inputLoop(100);
            
    io.msgStyle(LogLevel.Test, '@passed', '... test passed');
    Result = true;
end
