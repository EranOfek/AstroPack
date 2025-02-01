function Result = unitTest()
    % unitTest for the Stack class
    % Example: Result = imProc.stack.unitTest
    
    io.msgLog(LogLevel.Test, 'imProc.stack test started');

    % applyUnaryFun - complete when fixed
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.applyUnaryFun(AI,1);
    R  = imProc.stack.applyUnaryFun(AI,[1 2]);
    R  = imProc.stack.applyUnaryFun(AI,{1 2}); % the same
    R  = imProc.stack.applyUnaryFun(AI,AI); 
    R  = imProc.stack.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
    R  = imProc.stack.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});

    % subtractOffset - complete when fixed
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.subtractOffset(AI,1);
    R  = imProc.stack.subtractOffset(AI,[1 2]);
    assert(all(R(1).Image==0,'all') )
    R  = imProc.stack.subtractOffset(AI,{1 2}); % the same
    R  = imProc.stack.subtractOffset(AI,AI); 
    assert(all(R(1).Image==0,'all') )
    R  = imProc.stack.subtractOffset(AI,@mean,'OpArgs',{'all'});
    assert(all(R(1).Image==0,'all') ,'problem with subtractOffset')

    % divideFactor
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.divideFactor(AI,1);
    R  = imProc.stack.divideFactor(AI,[1 2]);
    assert(all(R(1).Image==1,'all') && all(R(2).Image==1.5,'all'))
    R  = imProc.stack.divideFactor(AI,{1 2}); % the same
    R  = imProc.stack.divideFactor(AI,AI); 
    R  = imProc.stack.divideFactor(AI,@mean,'OpArgs',{'all'});
    assert(all(R(1).Image==1,'all') && all(R(2).Image==1,'all'),'problem with divideFactor')
    
    % funCube
    % the output arguments when SaveInProp is strange
    AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
    [Cube1, Cube2] = imProc.stack.funCube(AI,'SaveInProp',[]);
    [CAI] = imProc.stack.funCube(AI,'SaveInProp',{'ImageData','VarData'});
    assert(all(CAI.Image==Cube1,'all') && all(CAI.Var==Cube2,'all'),'problem with funCube')

    % coadd
    % why Result.Var is filled with CoaddVarEmpirical when there are
    % weights? 
    % default Args.OffsetArgs could be [1 2](for dim argument of many
    % functions like mean)
    AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    [Result, CoaddN] = imProc.stack.coadd(AI);
    AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)},'Var',{ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'StackMethod','wmean');
    assert(all(Result.Image<2,'all'),'problem with coadd');
    [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'Offset',@mean,'OffsetArgs',{[2 3]});
    assert(all(Result.Image==0,'all'),'problem with coadd');
    AI = AstroImage({ones(5), 2.*ones(6), 3.*ones(10)});
%     fails! empty back/mask/var images can't be combined with CCDSEC
%     [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'CCDSEC',[1 5 1 5]); 
    
    
    % functionalResponse
    AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
    Result = imProc.stack.functionalResponse(AI);
    Result = imProc.stack.functionalResponse(AI, 'Intensity',[1 2 10 11 13]);

    io.msgStyle(LogLevel.Test, '@passed', 'imProc.stack test passed');  
    Result = true;


end