function Result = unitTest()
    % unitTest for the +imProc.background package
    % Example: imProc.background.unitTest
    
    % background
    AI = AstroImage({rand(1024,1024)});
    Result = imProc.background.background(AI);
    Result = imProc.background.background(AI, 'BackFun', @median,...
                                         'BackFunPar',{[1 2]},...
                                         'VarFun',@imUtil.background.rvar,...
                                         'VarFunPar',{},...
                                         'SubSizeXY',[128 128],...
                                         'Overlap',16);
    AI = AstroImage({ones(1024,1024)});
    Result1 = imProc.background.background(AI);
    if ~all(Result1.BackData.Data==1,'all')
        error('Background was not calculated correctly');
    end
    if ~all(Result1.VarData.Data==0,'all')
        error('Variance was not calculated correctly');
    end
    
    Result2 = imProc.background.background(AI,'SubBack',true);
    if ~all( abs(Result2.ImageData.Data-0)<100.*eps )
        error('Background was not subtracted correctly');
    end
    
    [MatX, MatY] = meshgrid( (1:1:1000), (1:1:1000) );
    Z = 2+MatX +MatY + MatX.*MatY;
    AI = AstroImage({Z});
    [Result, Surface] = imProc.background.fitSurface(AI);
    if max(abs(Surface.Image - Z),[],'all')>1e-6
        error('Problem with fitSurface');
    end

    Result = true;
end