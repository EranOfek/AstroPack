function Result = unitTest()
    % unitTest for the +imProc.image package
   io.msgLog(LogLevel.Test, 'imProc.image test started');
   
    % images2cube
    AI = AstroImage({rand(1000,1000), rand(1000,1000), rand(1000,1000)});
    [CubeImage, CubeBack] = imProc.image.images2cube(AI);
    [CubeImage] = imProc.image.images2cube(AI,'CCDSEC',[1 2 2 5]);

    % cutouts
%     AI=AstroImage({rand(1000,1000)});
%     XY = rand(10000,2).*900 + 50;
%     Cube = imProc.image.cutouts(AI, XY);
%     Cube = imProc.image.cutouts(AI, XY,'Shift',true);
%     Cube = imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true);
    
    AI=AstroImage({[ones(50,50),ones(50,50)*2;3*ones(50,50),ones(50,50)*4]});
    XY = [20,20;3,20];
    
    % Ask Eran, imaginary part
    Cube = imProc.image.cutouts(AI, XY);
    Cube = real(imProc.image.cutouts(AI, XY,'Shift',true));
    Cube = real(imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true,'PadVal',1));
    
    XY = [50,50];
    Cube = imProc.image.cutouts(AI, XY);
    Cube = real(imProc.image.cutouts(AI, XY,'Shift',true,'IsCircFilt',true,'PadVal',1));

    % image2subimages
    AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
    Result = imProc.image.image2subimages(AI,[256 256]);
    %
    Im=[[ones(100,100), ones(100,100).*3];[ones(100,100).*2, ones(100,100).*4]];
    AI=AstroImage({Im});
    Result = imProc.image.image2subimages(AI,[100 100]);
    if ~(median(Result(1).Image(:))==1 && median(Result(2).Image(:))==2 && median(Result(3).Image(:))==3 && median(Result(4).Image(:))==4)
        error('Problem with image2subimages');
    end
    
    % interpOverNan
    AI = AstroImage({ones(100,100)});
    AI.Image(50,50:51)=NaN;
    AI.Image(70,70) = NaN;
    imProc.image.interpOverNan(AI,'Method','rowcol');
    if ~all(AI.Image==1,'all')
        error('Problem with interpOverNan');
    end
    AI = AstroImage({ones(100,100)},'Back',{2*ones(100,100)});
    imProc.image.interpOverNan(AI,'DataProp',{'Image','Back'});
    if ~all(~isnan(AI.Image),'all') || ~all(~isnan(AI.Back),'all')
        error('Problem with interpOverNan');
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.image test passed');
    Result = true;   
end
