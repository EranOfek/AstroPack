
function Result = unitTest()
    % FileMap.unitTest
    
    io.msgLog(LogLevel.Test, 'FileMap test started');

    a = FileMap();
    a.add('c:/temp');
    a.scan();
    
    %f = a.FindFile('ZDbc.dproj');
    f = a.findFile('ZDbc.dproj');
    disp(f{1});
    
    f = a.findFile1('ZDbc1.dproj');
    disp(f);    
    
    f = a.findFile1('c:/temp/123.txtj');
    disp(f);        
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileMap test passed');                          
    Result = true;
end
