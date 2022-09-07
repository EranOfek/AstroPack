function Result = unitTest()
    % TimeSim.unitTest   
    io.msgLog(LogLevel.Test, 'TimeSim test started');

    % Create instances
    T = TimeSim.getSingleton();    
    while true
        disp(T.getDateTime());
        pause(1);
    end
  
    io.msgStyle(LogLevel.Test, '@passed', 'TimeSim test passed');                          
    Result = true;
end
