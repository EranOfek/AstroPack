function perfTest
    % performence tests for imUtil.interp
    
    %%
    AI = AstroImage('LAST.00.01.01_20220303.224914.224_clear__001_001_001_sci_raw_Image_1.fits');
    Image  = double(AI.Image( 2000+(1:1700), 1000+(1:1700)));
    Image  = 1000 + randn(1700,1700).*10;
    SizeIm = size(Image);
    
    Nsim = 1000;
    Irand = randi(SizeIm(1),Nsim,1);
    Jrand = randi(SizeIm(2),Nsim,1);
    Ind   = tools.array.sub2ind_fast(SizeIm, Irand, Jrand);
    
    TrueVal = 1000; % Image(Ind);
    
    Result = zeros(0,3);
    TestInd = 0;
    
    % test inpaint
    for Method=1:1:5
        ImageNaN = Image;
        ImageNaN(Ind) = NaN;

        tic;
        IntImage = inpaint_nans(ImageNaN, Method);
        T = toc;
        
        Diff = IntImage(Ind) - TrueVal;
        Val  = Image(Ind);
        plot(Val, Diff./Val,'.')
        hold on;
        TestInd = TestInd + 1;
        Result = [Result; [TestInd, T, tools.math.stat.rstd(Diff./Val)]];
    end
    
    
    % test biharmonic interpolation
    % slow
    if 1==0
    ImageNaN = Image;
    ImageNaN(Ind) = NaN;
    [MatX, MatY] = meshgrid(1:1:SizeIm(2), 1:1:SizeIm(1));
    Inn = ~isnan(ImageNaN(:));
    tic;
    vq = griddata(MatX(Inn),MatY(Inn),ImageNaN(Inn),Jrand,Irand, 'v4');
    T = toc;
    Diff = vq - TrueVal;
    Val  = Image(Ind);
    plot(Val, Diff./Val,'.')
    TestInd = TestInd + 1;
    Result = [Result; [TestInd, T, tools.math.stat.rstd(Diff./Val)]];
    end
    
    if 1==0
    % test cubic interpolation
    ImageNaN = Image;
    ImageNaN(Ind) = NaN;
    [MatX, MatY] = meshgrid(1:1:SizeIm(2), 1:1:SizeIm(1));
    Inn = ~isnan(ImageNaN(:));
    tic;
    vq = griddata(MatX(Inn),MatY(Inn),ImageNaN(Inn),Jrand,Irand, 'cubic');
    T = toc;
    Diff = vq - TrueVal;
    Val  = Image(Ind);
    plot(Val, Diff./Val,'.')
    TestInd = TestInd + 1;
    Result = [Result; [TestInd, T, tools.math.stat.rstd(Diff./Val)]];
    end
    
    % interpImageConv
    ImageNaN = Image;
    ImageNaN(Ind) = NaN;
    Kernel = imUtil.kernel2.gauss;
    tic;
    IntImage = imUtil.interp.interpImageConv(ImageNaN); %, Kernel);
    T = toc;
    Diff = IntImage(Ind) - TrueVal;
    Val  = Image(Ind);
    plot(Val, Diff./Val,'.')
    TestInd = TestInd + 1;
    Result = [Result; [TestInd, T, tools.math.stat.rstd(Diff./Val)]];
    
    % interpImageConvPix
    ImageNaN = Image;
    ImageNaN(Ind) = NaN;
    Kernel = imUtil.kernel2.gauss;
    tic;
    IntImage = imUtil.interp.interpImageConvPix(ImageNaN, Jrand, Irand); %Kernel, [Irand, Jrand]);
    T = toc;
    Diff = IntImage(Ind) - TrueVal;
    Val  = Image(Ind);
    plot(Val, Diff./Val,'.')
    TestInd = TestInd + 1;
    Result = [Result; [TestInd, T, tools.math.stat.rstd(Diff./Val)]];
        
    Result
end

