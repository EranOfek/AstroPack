function Result = unitTest
    % unitTest for ImageComponent
    io.msgStyle(LogLevel.Test, '@start', 'ImageComponent test started')

    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);

    io.msgLog(LogLevel.Test, 'testing ImageComponent needMapKey');
    IC = ImageComponent; 
    IC.needMapKey();
    assert(~isempty(IC.MapKey));

    % @Chen: Moved here from Component.m
    % convert2class
    io.msgLog(LogLevel.Test, 'testing ImageComponent convert2class')
    IC=ImageComponent; 
    IC.Image = 1;
    AI=convert2class(IC,{'Image'},{'Image'},@AstroImage);
    AI=convert2class(IC,{'Data'},{'ImageData.Data'},@AstroImage,'UseEval',true);

    % data2array
    io.msgLog(LogLevel.Test, 'testing ImageComponent data2array');
    IC= ImageComponent({1, 2});
    [A] = data2array(IC,'Image');
    [A,B] = data2array(IC,{'Image','Data'});

    Size = [2 2];
    IC = ImageComponent(Size);
    assert(all(size(IC) == Size), 'ImageComponent size is not consistent');

    Npix  = 10;
    IC = ImageComponent({rand(Npix,Npix), rand(2,2)});
    Scale = 5;
    IC = ImageComponent({rand(Npix,Npix), rand(2,2)},'Scale',Scale);
    assert(all(size(IC(1).Data)==[Npix Npix]), 'Image data is not consistent');            
    assert(all(size(IC(1).Image)==[Npix Npix].*Scale), 'Image rescaling is not consistent');

    % Read all FITS files
    io.msgLog(LogLevel.Test, 'testing ImageComponent fits reader');
    IC = ImageComponent('*.fits');

    % size and empty   
    io.msgLog(LogLevel.Test, 'testing ImageComponent sizeImage');
    IC = ImageComponent({rand(0,1), rand(10,12)});
    [Ny, Nx] = IC.sizeImage;
    io.msgLog(LogLevel.Test, 'testing ImageComponent isemptyImage');
    Res = IC.isemptyImage();
    assert(all(Res == [1, 0]));

    % funUnary
    io.msgLog(LogLevel.Test, 'testing ImageComponent funUnary');
    IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
    R = IC.funUnary(@sin);

    % funUnaryScalar
    io.msgLog(LogLevel.Test, 'testing ImageComponent funUnaryScalar');
    IC = ImageComponent({ones(10,10), zeros(5,4)}, 'Scale',5);
    R = IC.funUnary(@sin);


    IC.funUnary(@median,'OpArgs',{'all','omitnan'});
    IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
    IC.funUnary(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]); % CCDSEC=[1 2  1 3]
    IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
    IC.funUnary(@tanh,'CCDSEC',[1 2 1 3]);
    IC.funUnary(@tanh,'CCDSEC',[1 2 1 3],'OutOnlyCCDSEC',true);

    % funBinary
    io.msgLog(LogLevel.Test, 'testing ImageComponent funBinary');
    IC = ImageComponent({tools.rand.utrandi(10, 10)});
    IC2 = ImageComponent({tools.rand.utrandi(10,10)});

    % IC += IC2
    IC.funBinary(IC2, @plus, 'CreateNewObj', false);

    % R = IC + IC
    R = IC.funBinary(IC, @plus, 'CreateNewObj', true);
    assert(all(R.Image-2.*IC(1).Image == 0, 'all'), 'funBinary');

    % R = IC + copy(IC)
    IC3 = IC.copy();
    R = IC.funBinary(IC3, @plus, 'CreateNewObj',true);


    R   = funBinary([IC,IC],1,@plus,'CreateNewObj',true);
    IC1 = ImageComponent({rand(2,2)});
    IC2 = ImageComponent({rand(2,2)});
    R   = funBinary([IC1,IC2],{1 2},@plus,'CreateNewObj',true);
    IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5);
    R   = funBinary(IC,3,@times,'CreateNewObj',true);
    IC  = ImageComponent({rand(10,10), rand(2,2)},'Scale',5);
    R   = funBinary(IC,3,@times,'CreateNewObj',false); 
    IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5);
    IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true);
    IC  = ImageComponent({rand(10,10), rand(10,10)},'Scale',5);

    % operation between different regions in the 1st and 2nd image
    IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 3 5]);
    IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',true);
    assert(all(size(IB(1).Image)==[3 2]),  'Problem with output size in funBinary');

    IB  = funBinary(IC,IC(1),@plus,'CreateNewObj',true, 'CCDSEC1',[2 3 2 4],'CCDSEC2',[2 3 2 4],'OutOnlyCCDSEC',false);
    assert(all(size(IB(1).Image)==size(IC(1).Image)), 'Problem with output size in funBinary');


    % operate against a scalar for each image
    R   = funBinary([IC1, IC2],num2cell([1 3]),@plus,'CreateNewObj',true);
    if ~all(abs(R(1).Image - IC1.Image - 1)<1e-8,'all') || ~all(abs(R(2).Image - IC2.Image - 3)<1e-8,'all')
        error('Problem with arithmetics');
    end
    R   = funBinary([IC1],[1 3],@plus,'CreateNewObj',true);

    % In both these case funBinary regards [1 3] as a single image
    % and the opeation is done column wise
    R   = funBinary([IC1],[1 3],@plus,'CreateNewObj',true);  
    R1   = funBinary([IC1],{[1 3]},@plus,'CreateNewObj',true);
    if ~all(abs(R.Image-R1.Image)<1e-8,'all')
        error('Should return the same result');
    end

    % funUnaryScalar
    io.msgLog(LogLevel.Test, 'testing ImageComponent funUnaryScalar');
    IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
    R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'});
    IC = ImageComponent({rand(10,10), rand(5,4)},'Scale',5);
    R = IC.funUnaryScalar(@median,'OpArgs',{'all','omitnan'},'CCDSEC',[1 2 1 3]);

    % images2cube
    io.msgLog(LogLevel.Test, 'testing ImageComponent images2cube');
    IC=ImageComponent({ones(5,5),2.*ones(5,5),3.*ones(5,5)});
    Cube = images2cube(IC);
    images2cube(IC,'CCDSEC',[1 2 2 4]);
    IC=ImageComponent({ones(5,5),2.*ones(7,7),3.*ones(8,8)},'Scale',5);
    Cube = images2cube(IC,'CCDSEC',[1 2 2 4]);

    % shift and stack using the CCDSEC atgument            
    images2cube(IC,'CCDSEC',[1 2 2 4; 1 2 2 4; 2 3 3 5]);            

    % funStack - @Todo @Eran - FAILS
    io.msgLog(LogLevel.Test, 'testing ImageComponent funStack');
    IC = ImageComponent({2.*randn(100,100), 2.*randn(100,100), 2.*randn(100,100)});
    Var = ImageComponent({4.*ones(100,100), 4.*ones(100,100), 4.*ones(100,100)});
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = funStack(IC);
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'StackMethod','wmean','VarImage',Var);

    % coadd
    IC = ImageComponent({2.*randn(100,100), 2.*randn(100,100), 2.*randn(100,100)});
    Var = ImageComponent({4.*ones(100,100), 4.*ones(100,100), 4.*ones(100,100)});
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC);
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'StackMethod','wmean','VarImage',Var);
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'SubMethod',[10 10 10]);
    if abs(4-mean(CoaddVarEmpirical.Image,'all'))>0.1
        error('Mean should be close to 4');
    end
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'SubMethod',[10 10 10],'NormMethod',[0.5 0.5 0.5]); % mean(CoaddVarEmpirical.Image,'all') should be close to 1
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN] = coadd(IC,'SubMethod',[10 10 10],'NormMethod',[0.5 0.5 0.5],'NormEnd',5); % mean(CoaddVarEmpirical.Image,'all')
    % mean(Coadd.Image,'all') should be near -25
    %mean(CoaddVarEmpirical.Image,'all') should be near 25
    [Coadd, CoaddVarEmpirical, CoaddVar, CoaddN]=coadd(IC,'SubMethod',-100,'NormEnd',@median,'NormEndArgs',{'all'},'NormEndOperator',@rdivide);
    % mean(Coadd.Image,'all') should be near 1


    % replace
    io.msgLog(LogLevel.Test, 'testing ImageComponent replace');
    IC=ImageComponent({rand(5,5),2.*rand(5,5),3.*ones(5,5)});
    [Obj,ObjReplaced] = replace(IC, [0.2 1], 0.1);
    if any(Obj(1).Image>0.2)
        error('replace didnot work properly');
    end
    IC(2).Image(1,1) = NaN;
    [Obj,ObjReplaced] = replace(IC, [NaN], 0.1);
    if Obj(2).Image(1,1)~=0.1
        error('replace didnot work properly with NaNs');
    end
    % several ranges
    IC=ImageComponent({rand(5,5),2.*rand(5,5),3.*ones(5,5)});
    [Obj,ObjReplaced] = replace(IC, [0.2 1;0 0.2], [0.5 0.1]);

    % imresize - output is a matrix
    io.msgLog(LogLevel.Test, 'testing ImageComponent imresize');
    Result = imresize(IC, 2);

    % imrotate
    io.msgLog(LogLevel.Test, 'testing ImageComponent imrotate');
    IC=ImageComponent({rand(10,10)});
    IC.imrotate(10);

    % crop
    io.msgLog(LogLevel.Test, 'testing ImageComponent crop');
    IC=ImageComponent({rand(5,5),2.*rand(5,5),3.*ones(5,5)});
    % crop multiple images with a single crop
    Res = crop(IC,[1 2 1 3]);
    % crop single image with multiple crops
    Res = crop(IC(1),[1 2 1 3; 1 2 1 2]);
    % crop multiple image with multiple crops
    Res = crop(IC(1:2),[1 2 1 3; 1 2 1 2]);

    % image2subimages
    io.msgLog(LogLevel.Test, 'testing ImageComponent image2subimages');
    IC=ImageComponent; IC.Image=rand(1000,1000);
    [Result,EdgesCCDSEC,ListCenters,NoOverlapCCDSEC] = image2subimages(IC,[256 256]);

    % cutouts around selected positions in a single image
    io.msgLog(LogLevel.Test, 'testing ImageComponent cutouts');
    IC = ImageComponent({rand(1000,1000)});
    XY = rand(10000,2).*900 + 50;
    Cube = cutouts(IC, XY);
    if ~all(size(Cube)==[17 17 1e4])
        error('cutouts output size is incorrect');
    end
    Cube = cutouts(IC, XY,'Shift',true);
    Cube = cutouts(IC, XY,'Shift',true,'IsCircFilt',true);

    % funCutouts
    IC=ImageComponent({uint16(ones(100,100))});
    Result = funCutouts(IC, [1 1; 2 2; 10 10; 30 30], @tools.array.bitor_array)
    IC=ImageComponent({uint32(ones(100,100))});
    Result = funCutouts(IC, [1 1; 2 2; 10 10; 30 30], @tools.array.bitor_array,'CutAlgo','wmat')


    % imageComponent2AstroImage
    io.msgLog(LogLevel.Test, 'testing ImageComponent imageComponent2AstroImage');
    IC = ImageComponent({ones(3,3), zeros(3,3)});
    AI = IC.imageComponent2AstroImage;

    % cast
    io.msgLog(LogLevel.Test, 'testing ImageComponent cast');
    IC = ImageComponent({rand(10,10)});
    cast(IC,'single');

    % background
    io.msgLog(LogLevel.Test, 'testing ImageComponent background');
    IC = ImageComponent({100+randn(1000,1000), 50+randn(100,100)});
    [B,V] = background(IC);

    % subtractBack
    io.msgLog(LogLevel.Test, 'testing ImageComponent subtractBack');
    IC = ImageComponent({100+randn(1000,1000), 50+randn(100,100)});
    [B,V] = background(IC);
    subtractBack(IC, B);

    cd(PWD);

    io.msgStyle(LogLevel.Test, '@passed', 'ImageComponent test passed')
    Result = true;            

end        
