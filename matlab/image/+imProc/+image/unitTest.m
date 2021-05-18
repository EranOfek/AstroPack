function Result = unitTest()
    % unitTest for the +imProc.image package
    
    % background
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
    
    % images2cube
    AI = AstroImage({rand(1000,1000), rand(1000,1000), rand(1000,1000)});
    [CubeImage, CubeBack] = imProc.image.images2cube(AI);
    [CubeImage] = imProc.image.images2cube(AI,'CCDSEC',[1 2 2 5]);

    % cutouts
    AI=AstroImage({rand(1000,1000)});
    XY = rand(10000,2).*900 + 50;
    Cube = imProc.image.cutouts(AI, XY);
    Cube = imProc.image.cutouts(AI, XY,'Shift',true);
    Cube = imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true);

    % image2subimages
    AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
    Result = imProc.image.image2subimages(AI,[256 256]);

    
end
