function Result = unitTest()
    % unitTest for AstroImage
    % Example: AstroImage.unitTest 

    %io.msgStyle(LogLevel.Test, '@start', 'AstroImage test started')


    % @Chen 20/03/2024
    LastFileName = 'C:\Ultrasat\LAST\LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.fits';
    AI = AstroImage(LastFileName);

    filename = 'temp_fits_last0.fits';
    iterations = 10;
    tic;
    for i = 1:iterations
        if isfile(filename)
            delete(filename)
        end
        AI.write1(filename);
    end
    elapsedTime = toc;

    % Calculate and display the average writing time
    averageTime = elapsedTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);

    filename = 'temp_fits_last1.fits';
    iterations = 10;
    tic;
    for i = 1:iterations
        io.fits.fastWriteFITS(filename, AI.Image, AI.Header(9:end, :));
    end
    elapsedTime = toc;

    % Calculate and display the average writing time
    averageTime = elapsedTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);

    return;

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    Astro = AstroImage;

    % cast
    %io.msgLog(LogLevel.Test, 'testing AstroImage cast');
    AI = AstroImage({rand(10,10)},'Back',{rand(10,10)});
    Res = cast(AI,'single');
    if ~isa(Res.Back,'single')
        error('Type casting failed');
    end

    % Copy and check that the reference (ai1) points to the original
    % object, and the copy (ai2) has a newly generated Uuid
    AI.needUuid();
    ai1 = AI;
    assert(strcmp(ai1.Uuid, AI.Uuid));
    ai2 = AI.copy();
    assert(~strcmp(ai2.Uuid, AI.Uuid));

    % Modify data and validate
    ai1.ImageData.Data(1,1) = ai1.ImageData.Data(1,1) + 1;
    ai2.ImageData.Data(1,1) = ai2.ImageData.Data(1,1) + 2;
    assert(isequal(ai1.ImageData.Data, AI.ImageData.Data));
    assert(~isequal(ai2.ImageData.Data, AI.ImageData.Data));    
    
    
    % funBinaryProp
    % funBinary for a single property / no error propagation
    AI=AstroImage({ones(3,3)});
    Res = funBinaryProp(AI,3,@plus);
    if ~all(Res.Image==4)
        error('funBinaryProp failed');
    end
    Res = funBinaryProp(AI,[3 2 1],@plus);
    if ~all(Res.Image==[4 3 2])
        error('funBinaryProp failed');
    end
    Res = funBinaryProp(AI,{2 1},@plus);
    if numel(Res)~=2
        error('funBinaryProp failed');
    end
    if ~all(Res(1).Image==3,'all') || ~all(Res(2).Image==2,'all')
        error('funBinaryProp failed');
    end
    Res = funBinaryProp(AI,AI,@minus);
    if ~all(Res.Image==0)
        error('funBinaryProp failed');
    end
    Res = funBinaryProp(AI,AI,@minus,'DataProp','VarData');
    if ~all(Res.Image==1)
        error('funBinaryProp failed');
    end

    %io.msgLog(LogLevel.Test, 'testing AstroImage julday');
   % kra: the next line does not work (fixed by EO)
    AI = AstroImage('*.fits');
   [JD,ET] = julday(AI(1));

    % image2subimages
    %io.msgLog(LogLevel.Test, 'testing AstroImage image2subimages');
    AI = AstroImage({rand(1024, 1024)},'Back',{rand(1024, 1024)});
    Result = imProc.image.image2subimages(AI,[256 256]);
    s=Result(1).MaskData.bitStat;
    s.BitSummary;

    % check crop - complete
    AI = AstroImage({rand(100,100)},'Back',{rand(100,100)});
    Sec_ccdsec = [11 20 11 30];
    Res = crop(AI,Sec_ccdsec,'CreateNewObj',true);
    Sec_center = [Sec_ccdsec(1)+Sec_ccdsec(2),Sec_ccdsec(3)+Sec_ccdsec(4),...
                  Sec_ccdsec(2)-Sec_ccdsec(1),Sec_ccdsec(4)-Sec_ccdsec(3)]/2;
    Res2 = crop(AI,Sec_center,'Type','center','CreateNewObj',true);
    % kra: why after this line Res also changes (fixed by EO)
    if ~all(Res.Image==Res2.Image,'all')
        error('Problem with crop or plus operator with crop');
    end

    % overload operators
    %io.msgLog(LogLevel.Test, 'testing AstroImage operators')
    AI = AstroImage({ones(10,10), 2.*ones(20,20)});
    % perform: AI(1)+AI(1) and AI(2)+AI(2)
    R = AI + AI;
    % perform: AI(1)+2 to all elements
    R = AI + 2;
    if ~all(R(1).Image==3,'all')
        error('Problem with plus operator');
    end
    % add 2 to all elements in AI
    AI + 2;
    if ~all(AI(1).Image==3,'all')
        error('Problem with plus operator');
    end

    AI = AstroImage({ones(10,10), 2.*ones(10,10)});
    R = AI + AI;
    R = AI + AI(1);
    if ~all(R(2).Image==3)
        error('Problem with plus operator');
    end

    R = AI - AI;
    R = AI - 3;
    if ~all(R(2).Image==-1)
        error('Problem with minus operator');
    end

    R = AI.*AI;
    R = AI.*3;
    if ~all(R(2).Image==6)
        error('Problem with times operator');
    end

    R = AI./AI;
    R = AI./3;
    if ~all(R(2).Image==2./3)
        error('Problem with rdivide operator');
    end
            
    % overload operators with CCDSEC
    %io.msgLog(LogLevel.Test, 'testing AstroImage operators with CCDSEC')
    AI = AstroImage({ones(1024, 1024)});
    Sec = [1 256 1 256];
    Res = funBinaryProp(AI,AI,@plus,'CCDSEC1',Sec,'CCDSEC2',Sec,'CCDSEC',Sec);
    if ~all(crop(Res,Sec).Image==2)
        error('Problem with plus CCDSEC');
    end

    % conv
    %io.msgLog(LogLevel.Test, 'testing AstroImage conv');
    AI = AstroImage({rand(100,100), rand(200,200)});
    AI.conv(imUtil.kernel2.annulus); % create new object default is true, on purpose?
    Mat = zeros(30,30); Mat(15,15)=1;
    AI = AstroImage({Mat});
    Res = conv(AI, @imUtil.kernel2.gauss);

    % filter (cross-correlation)
    %io.msgLog(LogLevel.Test, 'testing AstroImage filter')
    AI = AstroImage({rand(100,100), rand(200,200)});
    AI.filter(imUtil.kernel2.annulus);
    Mat = zeros(30,30); Mat(15,15)=1;
    AI = AstroImage({Mat});
    Res2 = filter(AI, @imUtil.kernel2.gauss);
    
    % kra:
    %io.msgLog(LogLevel.Test, 'convolution vs. filter relative error')
    Diff = Res-Res2;
    Er = norm(Diff.Image)/norm(Res.Image);
    if(Er >= 1e-15)
        error('filter and conv do not match on a point-source')
    end

    % object2array
    % ???

    % imageIO2AstroImage
    %io.msgLog(LogLevel.Test, 'testing imageIO2AstroImage')
    I = ImageIO('WFPC2ASSNu5780205bx.fits','ReadHeader',1);
    AI = AstroImage.imageIO2AstroImage(I, 'ImageData', [], true);
    if (~prod(reshape((I.Data == AI.Image).',1,[])))
        error('problem with IO2AstroImage')
    end
    AI = AstroImage.imageIO2AstroImage(I, 'BackData', 2, true);

    % readImages2AstroImage
    %io.msgLog(LogLevel.Test, 'testing readImages2AstroImage')
    AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
    AI=AstroImage.readImages2AstroImage([]);
    AI=AstroImage.readImages2AstroImage([1 2]);
    AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)});
    AI=AstroImage.readImages2AstroImage({rand(10,10), rand(5,5)},'DataProp','VarData');
    AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','ImageData');
    AI=AstroImage.readImages2AstroImage({rand(10,10), rand(20,20)},'DataProp','BackData','Obj',AI);
    AI=AstroImage.readImages2AstroImage({rand(5,5), rand(10,10)},'DataProp','VarData','Obj',AI,'Scale',2);

    % isemptyImage
    %io.msgLog(LogLevel.Test, 'testing AstroImage isemptyImage')
    AI=AstroImage.readImages2AstroImage([]);
    [a,b]=AI.isemptyImage({'Image','Back'});
    assert(a*b);
    AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
    [a,b]=AI.isemptyImage({'Image','Back'});
    assert(~a);
    assert(b);
    AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'BackData');
    [a,b]=AI.isemptyImage({'Image','Back'});
    assert(a);
    assert(~b);

    % sizeImage
    %io.msgLog(LogLevel.Test, 'testing AstroImage sizeImage')
    AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'ImageData');
    [Ny, Nx] = AI.sizeImage;
    assert(Ny == 100);
    assert(Nx == 100);
    AI=AstroImage.readImages2AstroImage('WFPC2ASSNu5780205bx.fits', 'DataProp', 'BackData');
    [Ny, Nx] = AI.sizeImage('Back');
    assert(Ny == 100);
    assert(Nx == 100);

    % funCat
    %io.msgLog(LogLevel.Test, 'testing AstroImage funCat')
    AI = AstroImage({rand(10,10), rand(10,10)});
    AI(1).CatData.Catalog=rand(10,2);
    AI(2).CatData.Catalog=rand(10,2);
    funCat(AI,@sortrows,1);

    % funHeader
    %io.msgLog(LogLevel.Test, 'testing AstroImage funHeader')
    AI = AstroImage({rand(10,10), rand(10,10)});
    funHeader(AI,@insertKey,{'GAIN',2,''});
    assert(AI(1).getStructKey('GAIN').GAIN == 2);
    assert(AI(2).getStructKey('GAIN').GAIN == 2);
    

    % funHeaderScalar
    %io.msgLog(LogLevel.Test, 'testing AstroImage funHeaderScalar')
    AI = AstroImage({rand(10,10), rand(10,10)});
    funHeader(AI,@insertKey,{'DATEOBS','2003-07-24T18:28:58','';'EXPTIME',60,''}); %kra: insert a value
    %AI(1).getStructKey('DATE').DATEOBS %kra
    Res = funHeaderScalar(AI,@julday); %kra: generated NaN, fixed by EO via adding DATEOBS and EXPTIME
    assert( (Res(1) == 2452845.2704629627) && (Res(2) == 2452845.2704629627) );
    
    % getStructKey
    Im = ones(500,500);
    AI = AstroImage({poissrnd(Im.*1e3), poissrnd(Im*3e3), poissrnd(Im.*1e4), poissrnd(Im.*1e4), poissrnd(Im.*2e4)});
    AI(1).HeaderData.insertKey({'EXPTIME',1}); AI(2).HeaderData.insertKey({'EXPTIME',3}); AI(3).HeaderData.insertKey({'EXPTIME',10});
    AI(4).HeaderData.insertKey({'EXPTIME',10}); AI(5).HeaderData.insertKey({'EXPTIME',20});
    [Result] = getStructKey(AI, {'EXPTIME'});
    assert(isequal(vertcat(Result.EXPTIME), [1; 3; 10; 10; 20]));

    % setKeyVal
    AI = AstroImage({rand(10,10)});
    AI.setKeyVal('TYPE','science');
    assert(prod(AI.getStructKey('TYPE').TYPE == 'science'));

    % maskSet
    %io.msgLog(LogLevel.Test, 'testing AstroImage maskSet')
    AI = AstroImage({rand(3,3)},'Mask',{uint32(zeros(3,3))});
    AI.MaskData.Dict=BitDictionary('BitMask.Image.Default');
    Flag = false(3,3); Flag(1,2)=true;
    Res = AI.maskSet(Flag,'Saturated');
    assert( (Res.Mask(1,1) == 0) && (Res.Mask(1,2) == 1) ); %kra
    Res = AI.maskSet(Flag,'Streak');
    %assert( (Res.Mask(1,1) == 0) && (Res.Mask(1,2) == 1048576) ); %kra

    % isImType
    %io.msgLog(LogLevel.Test, 'testing AstroImage isImType')
    AI = AstroImage({rand(3,3)},'Mask',{uint32(zeros(3,3))});
    assert(~isImType(AI, 'bias'));
    AI.setKeyVal('IMTYPE','bias');
    assert(isImType(AI,'bias'))

    % funUnary
    %io.msgLog(LogLevel.Test, 'testing AstroImage funUnary')
    AI = AstroImage({10.*ones(10,10)},'Back',{ones(5,5)},'BackScale',2,'var',{ones(5,5)},'VarScale',2);
    B=AI.funUnary(@sin,'CreateNewObj',true);
    B=AI.funUnary(@mean,'OpArgs',{'all'}); 
    AI.PropagateErr=true; B=AI.funUnary(@mean,'OpArgs',{'all'});
    % B=AI.funUnary(@median,'OpArgs',{'all'}); % must result in error
    B=AI.funUnary(@median,'OpArgs',{'all'},'PropagateErr',false,'OperateOnVar',true);

    % funUnaryScalar
    %io.msgLog(LogLevel.Test, 'testing AstroImage funUnaryScalar')
    AI = AstroImage({randn(100,100), randn(100,100)},'Back',{randn(100,100), randn(100,100)});
    [A,B] = funUnaryScalar(AI, @mean, 'OpArgs',{'all'});
    [A,B] = funUnaryScalar(AI, @std, 'OpArgs',{[],'all'});

    % funBinaryImVar
    %io.msgLog(LogLevel.Test, 'testing AstroImage funBinaryImVar')
    AI = AstroImage({ones(3,3)},'Var',{ones(3,3)});
    Res1 = funBinaryImVar(AI,AI,@plus);
    Res2 = funBinaryImVar(AI,AI,@minus);
    Res3 = funBinaryImVar(AI,3,@times);
    
    % load and test real images
    AI1 = AstroImage('PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits');
    AI2 = AstroImage('FOCx38i0101t_c0f.fits');
    
    [Ny1, Nx1] = AI1.sizeImage;
    [Ny2, Nx2] = AI2.sizeImage;    
    Nymin = min([Ny1,Ny2]);
    Nxmin = min([Nx1,Nx2]);
    Sec_ccdsec = [1 Nxmin 1 Nymin];
    Res = crop(AI1./max(AI1.Image,[],'all'),Sec_ccdsec)+crop(AI2./max(AI2.Image,[],'all'),Sec_ccdsec);
    
    % cropLonLat
%     RA = 0;
%     Dec = 0;
%     [Result, Info] = cropLonLat(AI1, RA, Dec);

    % kra: additions
    %io.msgLog(LogLevel.Test, 'testing astroImage2AstroCatalog')
    AI = AstroImage({randn(5,5)});
    AI.CatData = AstroCatalog({'asu.fit'},'HDU',2);
    cat = astroImage2AstroCatalog(AI,'CreateNewObj',true);
    catsize = size(AI.CatData.Catalog); 
    for ifl=1:1:catsize(2)
        for objn=1:1:catsize(1)
            if ~isnan(AI.CatData.Catalog(objn,ifl))
                assert( AI.CatData.Catalog(objn,ifl) == cat.Catalog(objn,ifl) );
            end
        end
    end 
    
    % Test the behavior of arithmatic operations:
    AI1=AstroImage({ones(100,100)}, 'Mask',{uint32(ones(100,100))},'Exp',{ones(100,100)},'Back',{ones(100,100)});
    AI2=AstroImage({ones(100,100)}, 'Mask',{uint32(2.*ones(100,100))},'Exp',{ones(100,100)},'Back',{ones(100,100)});
    AI1.DataType=AstroDataType.Data;
    
    BI = AI1 + AI2;
    if ~all(BI.Image(1,1)==2)
        error('AstroImage arithmatic problem')
    end
    if ~all(BI.Exp(1,1)==2)
        error('AstroImage arithmatic problem')
    end
    % plus operator is not suppose to modify AI 
    if ~all(AI1.Image(1,1)==1)
        error('AstroImage arithmatic problem')
    end
    if ~all(AI1.Exp(1,1)==1)
        error('AstroImage arithmatic problem')
    end
    
    
    cd(PWD);

    %io.msgStyle(LogLevel.Test, '@passed', 'AstroImage test passed')
    Result = true;
end
