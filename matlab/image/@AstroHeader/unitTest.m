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

    % construct an empty AstroHeader
    io.msgLog(LogLevel.Test, 'testing AstroHeader constructor 1/2')
    size_of_empty=[2,2];
    A = AstroHeader(size_of_empty);
    assert(numel(A)==4)
    assert(all(size(A)==size_of_empty));
    
    % read headers to AstroHeader, on construction
    io.msgLog(LogLevel.Test, 'testing AstroHeader constructor 2/2')
    H = AstroHeader('PTF*.fits', 1);
    assert(~isempty(H(1).Key.NAXIS));
    assert(~isempty(H(2).Key.DATE));

    % getVal()
    io.msgLog(LogLevel.Test, 'testing AstroHeader getVal')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME');
    assert(Val == 300)
    assert(prod(Key == 'EXPTIME'))
    assert(prod(Comment == ' exposure duration (seconds)--calculated'))
    assert(Nfound == 1)
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','IsInputAlt',true);
    assert(Val == 300)
    assert(prod(Key == 'EXPTIME'))
    assert(prod(Comment == ' exposure duration (seconds)--calculated'))
    assert(Nfound == 1)
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME'); % return NaN
    assert(isnan(Val))
    assert(prod(Key == 'AEXPTIME'))
    assert(isempty(Comment))
    assert(Nfound == 0)
    [Val, Key, Comment, Nfound] = getVal(H, {'BB','EXPTIME','AA'});
    assert(Val == 300)
    assert(prod(Key == 'EXPTIME'))
    assert(prod(Comment == ' exposure duration (seconds)--calculated'))
    assert(Nfound == 1)
    [Val, Key, Comment, Nfound] = getVal(H, 'EXPTIME','UseDict',false);
    assert(Val == 300)
    assert(prod(Key == 'EXPTIME'))
    assert(prod(Comment == ' exposure duration (seconds)--calculated'))
    assert(Nfound == 1)
    [Val, Key, Comment, Nfound] = getVal(H, 'AEXPTIME','UseDict',false);
    assert(isnan(Val))
    assert(prod(Key == 'AEXPTIME'))
    assert(isempty(Comment))
    assert(Nfound == 0)

    % getStructKey()
    io.msgLog(LogLevel.Test, 'testing AstroHeader getStructKey')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    [Result,C] = getStructKey(H, {'EXPTIME'});
    assert(Result.EXPTIME == 300)
    assert(prod(C.EXPTIME == ' exposure duration (seconds)--calculated'))
    [Result,C] = getStructKey(H, {'AEXPTIME','A'});
    assert(Result.AEXPTIME == 300)
    assert(prod(C.AEXPTIME == ' exposure duration (seconds)--calculated'))
    assert(isnan(Result.A))
    assert(isempty(C.A))
    [Result,C] = getStructKey(H, {'AEXPTIME','A'},'UseDict',false);
    assert(isnan(Result.AEXPTIME))
    assert(isempty(C.AEXPTIME))
    assert(isnan(Result.A))
    assert(isempty(C.A))

    % getCellKey
    io.msgLog(LogLevel.Test, 'testing AstroHeader getCellKey')
    H = AstroHeader('PTF*.fits',1);
    [Result,IK] = getCellKey(H, {'EXPTIME','bb'});
    [ResultA,IKA] = getCellKey(H, {'AEXPTIME','bb'});
    [s1, s2] = size(Result)
    for i = s1 : 1 : s1-1
        assert(Result{i,1} == ResultA{i,1})
        assert(isnan(Result{i,2}))
        assert(isnan(ResultA{i,2}))
    end
    assert(isnan(Result{s1, 1}))
    assert(isnan(ResultA{s1, 1}))
    [Result,IK] = getCellKey(H, {'AEXPTIME','bb'},'UseDict',false);
    for i = s1 : 1 : s1-1
        if i == 2
            continue
        end
        assert(isnan(Result{i,1}))
        assert(isnan(Result{i,2}))
    end

    % insertDefaultComments
    io.msgLog(LogLevel.Test, 'testing AstroHeader insertDefaultComments')
    H=AstroHeader();
    insertDefaultComments(H); % <---- still empty?

    % deleteKey
    io.msgLog(LogLevel.Test, 'testing AstroHeader deleteKey')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    deleteKey(H,{'EXPTIME','A','COMMENT'});
    deleteKey(H,{'EXPTIME','A','SKYSUB\d'}); % use regexp
    if ~isnan(getVal(H,'COMMENT'))
        error('Key value should be NaN');
    end

    % insertKey
    io.msgLog(LogLevel.Test, 'testing AstroHeader insertKey')
    H = AstroHeader();
    H.insertKey('Key1');
    H.insertKey({'KeyA','ValueA','CommentA';'KeyB','ValueB','CommentB'},'end-1');
    assert(strcmp(H.Data{1,1},'KeyA'));
    assert(strcmp(H.Data{2,1},'KeyB'));

    % replaceVal
    io.msgLog(LogLevel.Test, 'testing AstroHeader replaceVal')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    H.replaceVal({'COMMENT'},{'replaced'});
    assert(prod(H.getVal('COMMENT') == 'replaced'))

    % isKeyVal
    io.msgLog(LogLevel.Test, 'testing AstroHeader isKeyVal')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    assert(prod(isKeyVal([H, H],'EXPTIME',300)));
    assert(prod(isKeyVal([H;H], 'KSPOTS','off')));
    assert(~prod(isKeyVal([H;H], 'KSPOTS','off','ValCaseSens',true)));

    % isKeyExist
    io.msgLog(LogLevel.Test, 'testing AstroHeader isKeyExist')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    assert(prod(isKeyExist([H, H],'EXPTIME')));
    assert(~prod(isKeyExist([H, H],'AEXPTIME')));
    assert(prod(isKeyExist([H; H],'AEXPTIME','IsInputAlt',true)));
    assert(prod(~isKeyExist([H, H],'aaa')));

    % julday
    io.msgLog(LogLevel.Test, 'testing AstroHeader julday')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    [JD,ET] = julday(H);
    assert(abs(2.4512e06 - JD)/JD < 0.001) % because floats
    assert(ET == 300)
    [JD,ET] = julday([H;H]);
    assert(abs(2.4512e06 - JD(1))/JD(1) < 0.001) % because floats
    assert(ET(1) == 300)
    assert(abs(2.4512e06 - JD(2))/JD(2) < 0.001) % because floats
    assert(ET(2) == 300)

    % groupByKeyVal
    io.msgLog(LogLevel.Test, 'testing AstroHeader groupByKeyVal')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'});
    assert(prod(Groups(1).Content{1} == 'EXT'))
    assert(Groups(1).Content{2} == 69)
    assert(Groups(1).Content{3} == 300)

    H = AstroHeader('*.fits',1);
    Groups = groupByKeyVal([H,H],{'IMTYPE','FILTER1','EXPTIME'});

    % show
    io.msgLog(LogLevel.Test, 'testing AstroHeader show')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    H.show();

    % createBasicHeader
    io.msgLog(LogLevel.Test, 'testing AstroHeader createBasicHeader')
    H = AstroHeader.createBasicHeader;
    H = AstroHeader.createBasicHeader(1,{'WINDDIR',11;'M_STAT','ok';'NEW',1});
    assert(H.getVal('WINDDIR') == 11)
    assert(prod(H.getVal('M_STAT') == 'ok'))
    assert(H.getVal('NEW') == 1)
    H = AstroHeader.createBasicHeader(1,{'WINDDIR',11,'aa';'M_STAT','ok','jj'});
    assert(H.getVal('WINDDIR') == 11)
    assert(prod(H.getVal('M_STAT') == 'ok'))
    H = AstroHeader.createBasicHeader([1 2],'WINDDIR',11,'M_STAT','ok','NEW',1); 
    assert(H(2).getVal('WINDDIR') == 11)
    assert(prod(H(2).getVal('M_STAT') == 'ok'))
    assert(H(2).getVal('NEW') == 1)

    % funUnary
    io.msgLog(LogLevel.Test, 'testing AstroHeader funUnary')
    H=AstroHeader('WFPC2ASSNu5780205bx.fits');
    funUnary(H,@sin);
    assert(H.isKeyExist('HISTORY'))
    funUnary(H,@atan2);
    
    
    
    
    % isImType
    io.msgLog(LogLevel.Test, 'testing AstroHeader isImType')
    H=AstroHeader('*.fits');
    Ans = isImType(H, 'SCI');
    assert(Ans(1))
    s = size(Ans)
    for i = 2 : 1 : s
        assert(~Ans(i))
    end
    Ans = isImType(H, 'sci','CaseSens',false,'IsInputAlt',false);
    assert(Ans(1))
    for i = 2 : 1 : s
        assert(~Ans(i))
    end

    % read
    io.msgLog(LogLevel.Test, 'testing AstroHeader read')
    H=AstroHeader;
    H.read('FileName','WFPC2ASSNu5780205bx.fits','HDU',[1]);
    H=AstroHeader;
    H.read('FileName', {'WFPC2ASSNu5780205bx.fits', 'WFPC2ASSNu5780205bx.fits'});

    cd(PWD);      

    io.msgStyle(LogLevel.Test, '@passed', 'AstroHeader test passed')
    Result = true;

end
