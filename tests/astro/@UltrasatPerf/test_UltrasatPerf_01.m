function Result = unitTest()
    % UltrasatPerf unitTest
    
    io.msgLog(LogLevel.Test, 'UltrasatPerf test started');

    % Use assert to validate functionality
    % assert( <condition should be true if ok> );

    %UP = UltrasatPerf();
    
    %
    %Sources = string({UP.Specs.ObjName});
    %assert(numel(Sources) > 0);
    
    % Todo
    % Test calcSNR()
    % = calcSNR();
    
    % @Todo = Test any other function / property ...
    
    io.msgLog(LogLevel.Test, 'UltrasatPerf test passed');
    Result = true;
end
