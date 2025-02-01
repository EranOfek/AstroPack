function Result = unitTest()
    % unitTest for AstroHeader
    % Example: Result = AstroHeader.unitTest
    %
    % To do:
    % -Add output checks for:
    %     -everything

    io.msgStyle(LogLevel.Test, '@start', 'AstroHeader test started')

    % Change to data directory
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);   
    
    % Test constructor
    H = AstroHeader('*.fits', 1);
    H = AstroHeader('WFPC2ASSNu5780205bx.fits');
    H = AstroHeader;
    
    HH = AstroHeader.createBasicHeader(1,{'WINDDIR',11;'M_STAT','ok';'NEW',1});
    if HH.numKeys~=69
        error('Header keyword count is incorrect');
    end
    
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME');
    if Val~=300
        error('getVal incorrect read');
    end
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','IsInputAlt',true);
    if Val~=300 || ~strcmp(Key, 'EXPTIME')
        error('getVal incorrect read');
    end
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME'); % return NaN
    if ~isnan(Val)
        error('getVal incorrect read');
    end
    [Val, Key, Comment, Nfound] = getVal(H, {'BB','EXPTIME','AA'});
    if Val~=300
        error('getVal incorrect read');
    end
    [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME','UseDict',false)
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','UseDict',false)
    if ~isnan(Val)
        error('getVal incorrect read');
    end
    
    [Result,C] = getStructKey(H, {'EXPTIME','A'});
    if Result.EXPTIME~=300 || ~isnan(Result.A)
        error('getStructKey incorrect read');
    end
    
    [Result,IK] = getCellKey([H,H], {'EXPTIME','bb'},'UseDict',false);
    if ~all(cell2mat(Result)==[300 NaN; 300 NaN])
        error('getCellKey incorrect read');
    end
    
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    deleteKey(H,{'EXPTIME','A','COMMENT'})
    deleteKey(H,{'EXPTIME','A','SKYSUB\d'}) % use regexp
    [Result,C] = getStructKey(H, {'EXPTIME','A'});
    if ~isnan(Result.EXPTIME)
        error('deleteKey failed');
    end
    
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    H.insertKey('stam')
    H.insertKey({'A','','';'B','',''},'end-1')
    [Result,C] = getStructKey(H, {'stam'});
    if ~isempty(Result.stam)
        error('insertKey failed');
    end

    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    H.replaceVal({'EXPTIME'},{100});       
    if H.Key.EXPTIME ~= 100
        error('replaceVal failed');
    end
    
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    R = isKeyVal([H, H],'EXPTIME',300);
    if ~all(R)
        error('isKeyVal failed');
    end
    R = isKeyVal([H;H], 'KSPOTS','on')
    if any(R)
        error('isKeyVal failed');
    end
    R = isKeyVal([H;H], 'KSPOTS','off','ValCaseSens',true);
    if any(R)
        error('isKeyVal failed');
    end
    
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    R = isKeyExist([H, H],'EXPTIME');
    if ~all(R)
        error('isKeyExist failed');
    end
    R = isKeyExist([H, H],'AEXPTIME');
    if any(R)
        error('isKeyExist failed');
    end
    R = isKeyExist([H; H],'AEXPTIME','IsInputAlt',true);
    if ~all(R)
        error('isKeyExist failed');
    end
    R = isKeyExist([H, H],'aaa');
    if any(R)
        error('isKeyExist failed');
    end
    
    Ans = isImType(H, 'bias');
    Ans = isImType(H, 'bias','CaseSens',false,'IsInputAlt',false);
    if Ans
        error('isImType failed');
    end
            
    if numel(unique(julday([H;H])))>1
        error('julday failed');
    end
    
    Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'});
    if ~all(Groups.ptr(:)' == [1 2])
        error('groupByKeyVal failed');
    end
    
     H = AstroHeader('PTF_Cropped.fits');
    [Lon, Lat, Height] = getObsCoo(H);
    
    if abs(Lat-33.357)>1e-3
        error('getObsCoo failed');
    end
    
    

    
    
    
    
    
    
    cd(PWD);      

    io.msgStyle(LogLevel.Test, '@passed', 'AstroHeader test passed')
    Result = true;

end
