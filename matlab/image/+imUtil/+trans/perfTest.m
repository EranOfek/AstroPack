function [Result] = perfTest(X, Y, Args)
    % Perf-Test for imUtil.trans package
    % Example: imUtil.trans.perfTest

    %%
    Nkernel = 1000;
    Cube = single(imUtil.kernel2.gauss(1.5.*ones(Nkernel,1),[25 25]));
    DX   = rand(Nkernel,1).*4-2;
    DY   = rand(Nkernel,1).*4-2;

    Nsim = 100;
    tic;
    for Isim=1:1:Nsim
        OutFFT = imUtil.trans.shift_fft(Cube, DX, DY);
    end
    T1 = toc;
    tic;
    for Isim=1:1:Nsim
        OutL3  = imUtil.trans.mex.shift_lanczos3(Cube, DX, DY);
    end
    T2 = toc;

    fprintf('imUtil.trans.mex.shift_lanczos3 is x %f faster comapred to imUtil.trans.shuft_fft\n',T1./T2);

    %%

    Result = true;
end
