
function Result = unitTest()
    % FileMap.unitTest
    
    io.msgLog(LogLevel.Test, 'FileMap test started');

    %
    a = FileMap();
    testLoad = false;
    if testLoad
        a.loadMap();
        f = a.findFile('ZDbc.dproj');
        disp(f);
    else    
        if isunix
            a.add('~/dev');
            a.scan();
            a.saveMap();            
        else
            %a.add('c:/temp');
            %a.add('c:/Ultrasat');
            a.add('c:/');
            a.scan();
            a.saveMap();
        end
    end
    
    %f = a.FindFile('ZDbc.dproj');
    f = a.findFile('ZDbc.dproj');
    disp(f);  %{1});
    
    f = a.findFile1('ZDbc1.dproj');
    disp(f);    
    
    f = a.findFile1('c:/temp/123.txtj');
    disp(f);        
    
    %
    b = FileMap();
    b.loadMap();
    f = b.findFile('ZDbc.dproj');
    disp(f);
    
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileMap test passed');                          
    Result = true;
end
