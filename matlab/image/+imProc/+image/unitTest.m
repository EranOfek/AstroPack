function Result = unitTest()
    % unitTest for the +imProc.image package
    
    AI = AstroImage({rand(1024,1024)});
    Result = imProc.image.background(AI);
    Result = imProc.image.background(AI, 'BackFun', @median,...
                                         'BackFunPar',{[1 2]},...
                                         'VarFun',@imUtil.background.rvar,...
                                         'VarFunPar',{},...
                                         'SubSizeXY',[128 128],...
                                         'Overlap',16);
    AI = AstroImage({ones(1024,1024)});
    Result1 = imProc.image.background(AI);
    if ~all(Result1.BackData.Data==1,'all')
        error('Background was not calculated correctly');
    end
    if ~all(Result1.VarData.Data==0,'all')
        error('Variance was not calculated correctly');
    end
    
    Result2 = imProc.image.background(AI,'SubBack',true);
    if ~all( abs(Result2.ImageData.Data-0)<100.*eps )
        error('Background was not subtracted correctly');
    end
    
end
