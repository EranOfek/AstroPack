function Result = unitTest()
    % LogFile.unitTest
    
    % Note that we cannot use here any of the msgLog() functions,
    % as we validate that LogFile works without any dependencies
    fprintf('LogFile test started\n');

    % Get singleton object
    Lf = LogFile.getSingleton();

    % Write some lines to file
    Lf.write('LogFile test started');
    for i=1:1:3
        Lf.write('Line: %d', i);
    end           
    Lf.write('LogFile test passed');

    % Done
    fprintf('LogFile test passed\n');
    Result = true;
end

