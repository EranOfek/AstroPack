
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
            a.addFolder('~/dev');
            a.scanFolders();
            a.saveMap();            
        else
            %a.addFolder('c:/temp');
            %a.addFolder('c:/Ultrasat');
            a.addFolder('c:/temp');
            a.scanFolders();
            a.saveMap();
        end
    end
    
    %f = a.FindFile('ZDbc.dproj');
    f = a.findFile('ZDbc.dproj');
    disp(f);  %{1});
    
    f = a.findFile('ZDbc1.dproj', 'Single', true);
    disp(f);    
    
    f = a.findFile('c:/temp/123.txtj', 'Single', true);
    disp(f);        
    
    %
    b = FileMap();
    b.loadMap();
    f = b.findFile('ZDbc.dproj');
    disp(f);
    
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileMap test passed');                          
    Result = true;
end
