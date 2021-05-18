function Result = unitTest()
    % unitTest for the +imProc.image package
   
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
