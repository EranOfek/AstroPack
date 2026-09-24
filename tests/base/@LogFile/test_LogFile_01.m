function Result = unitTest()
    % LogFile.unitTest
    
    % Note that we cannot use here any of the msgLog() functions,
    % as we validate that LogFile works without any dependencies
    fprintf('LogFile test started\n');

    % Get singleton object
    if ~isunix
        Path = 'C:\Temp';
    else
        Path = '/tmp';
    end
    
    % Set default path for log files
    LogFile.defaultPath(Path);
    
    Lf = LogFile.getSingleton('FileName', 'UnitTestLogFile', 'UseTimestamp', true);
    
    % Write some lines to file
    Lf.write('LogFile test started');
    Lf.write('Log line one');
    Lf.write('Log line two');

    % Test non-singleton log file with timestamp
    MyLf = LogFile(fullfile(Path, 'MyLogFile'), 'UseTimestamp', true);
    MyLf.write('My LogFile test started with TIMESTAMP');
    MyLf.write('My log line one');
    MyLf.write('My log line two');
    
    % Test non-singleton log file without timestamp
    MyLf = LogFile(fullfile(Path, 'MyLogFile'), 'UseTimestamp', false);
    MyLf.write('My LogFile test started WITHOUT timestamp');
    MyLf.write('My log line one');
    MyLf.write('My log line two');    
    
    MyLf = LogFile(fullfile(Path, 'MyLogFile'), 'DayFolder', true);
    MyLf.write('My LogFile test started with TIMESTAMP');
    MyLf.write('My log line one');
    MyLf.write('My log line two');
    
    % Done
    fprintf('LogFile test passed\n');
    Result = true;
end

