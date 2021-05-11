function Result = unitTest()
    % unitTest for the Stack class
    % Example: Result = imProc.stack.unitTest

    % applyUnaryFun
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.applyUnaryFun(AI,1);
    R  = imProc.stack.applyUnaryFun(AI,[1 2]);
    R  = imProc.stack.applyUnaryFun(AI,{1 2}); % the same
    R  = imProc.stack.applyUnaryFun(AI,AI); 
    R  = imProc.stack.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
    R  = imProc.stack.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});

    % subtractOffset
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.subtractOffset(AI,1);
    R  = imProc.stack.subtractOffset(AI,[1 2]);
    R  = imProc.stack.subtractOffset(AI,{1 2}); % the same
    R  = imProc.stack.subtractOffset(AI,AI); 
    R  = imProc.stack.subtractOffset(AI,@mean,'OpArgs',{'all'});

    % divideFactor
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.divideFactor(AI,1);
    R  = imProc.stack.divideFactor(AI,[1 2]);
    R  = imProc.stack.divideFactor(AI,{1 2}); % the same
    R  = imProc.stack.divideFactor(AI,AI); 
    R  = imProc.stack.divideFactor(AI,@mean,'OpArgs',{'all'});

    % funCube
    AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
    [Cube1, Cube2] = imProc.stack.funCube(AI);
    [CAI] = imProc.stack.funCube(AI,'SaveInProp',{'ImageData','VarData'});

    % coadd
    AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    [Result, CoaddN] = imProc.stack.coadd(AI);

    % functionalResponse
    AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
    Result = imProc.stack.functionalResponse(AI);
    Result = imProc.stack.functionalResponse(AI, 'Intensity',[1 2 10 11 13]);


    Result = true;


end