function [Result] = perfTest(X, Y, Args)
    % Perf-Test for imUtil.trans package
    % Example: imUtil.trans.perfTest

    %% imUtil.trans.mex.shift_lanczos3 compared to imUtil.trans.shift_fft
    Nkernel = 1000;
    Cube = single(imUtil.kernel2.gauss(1.5.*ones(Nkernel,1),[25 25]));
    DX   = rand(Nkernel,1).*4-2;
    DY   = rand(Nkernel,1).*4-2;

    Nsim = 300;
    tic;
    for Isim=1:1:Nsim
        OutFFT = imUtil.trans.shift_fft(Cube, DX, DY);
    end
    T1 = toc;
    tic;
    for Isim=1:1:Nsim
        OutL3  = imUtil.trans.mex.shift_lanczos3(Cube, DX, DY);
        %OutL3  = shift_cube_sinc_sep_simd(Cube, DX, DY);
        %OutL3  = shift_sinc_sep_simd_fastin(Cube, DX, DY);
    end
    T2 = toc;

    fprintf('imUtil.trans.mex.shift_lanczos3 is x %f faster comapred to imUtil.trans.shuft_fft\n',T1./T2);
   
    %% imUtil.trans.mex.shift_lanczos3 compared to interp2
    Image = single(rand(1716,1716));
    Dx = 2.9;
    Dy = 3.1;
    
    Nsim = 100;

    tic; 
    for I=1:1:Nsim
        OutL3  = imUtil.trans.mex.shift_lanczos3(Image,Dx, Dy);
    end
    T1=toc;

    tic;
    for I=1:1:Nsim
        OutL3  = imUtil.trans.shift_fft(Image,Dx, Dy); 
    end
    T2 = toc;

    tic;
    for I=1:1:Nsim
        X = (1:1:1716);
        Y = X + 3.1;
        X = X + 2.9;
        OutL3 = interp2(Image, X, Y, 'linear');
    end
    T3 = toc;

    fprintf('On 1716^2 image: imUtil.trans.mex.shift_lanczos3 is x %f faster than interp2\n',T3./T1);
    fprintf('On 1716^2 image: imUtil.trans.shift_fft is x %f faster than interp2\n',T3./T2);

    %%

    Result = true;
end
