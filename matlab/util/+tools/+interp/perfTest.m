function [Result] = perfTest
    % Perf test for tools.interp
    % Example: tools.interp.perfTest

    %%
    [Xin, Yin] = meshgrid((1:1716),(1:1716));
    [XoutM, YoutM] = meshgrid((1:1716)+5, (1:1716)+11);
    Xout = (1:1716)+5;
    Yout = (1:1716).'+11;
    Z    = Xin.^1.1 + Yin.^1.3;

    %% interp2
    tic; for i=1:100, A=interp2(Xin,Yin, Z, XoutM, YoutM, 'linear'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_bilinear_mex(Xin, Yin, Z, XoutM, YoutM); end, T1=toc;
    fprintf('interp2_bilinear_mex is x %f times faster than interp2 (matrix grid output)\n',T0./T1)
    
    tic; for i=1:100, A=interp2(Xin,Yin, Z, Xout, Yout, 'linear'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_bilinear_mex(Xin, Yin, Z, Xout, Yout); end, T1=toc;
    fprintf('interp2_bilinear_mex is x %f times faster than interp2 (vector grid output)\n',T0./T1)
    
    tic; for i=1:100, A=interp2(Xin,Yin, Z, Xout, Yout, 'cubic'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_cubic_mex(Xin, Yin, Z, Xout, Yout); end, T1=toc;
    fprintf('interp2_cubic_mex is x %f times faster than interp2 (vector grid output)\n',T0./T1)
    
    tic; for i=1:100, A=interp2(Xin,Yin, Z, Xout, Yout, 'nearest'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_nearest_mex(Xin, Yin, Z, Xout, Yout); end, T1=toc;
    fprintf('interp2_nearest_mex is x %f times faster than interp2 (vector grid output)\n',T0./T1)
    
    tic; for i=1:100, A=interp2(Xin,Yin, Z, Xout, Yout, 'cubic'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_lanczos2_mex(Xin, Yin, Z, Xout, Yout); end, T1=toc;
    fprintf('interp2_lanczos2_mex is x %f times faster than interp2(cubic) (vector grid output)\n',T0./T1)
    
    tic; for i=1:100, A=interp2(Xin,Yin, Z, Xout, Yout, 'cubic'); end, T0=toc;
    tic; for i=1:100, A1=tools.interp.mex.interp2_lanczos3_mex(Xin, Yin, Z, Xout, Yout); end, T1=toc;
    fprintf('interp2_lanczos3_mex is x %f times faster than interp2(cubic) (vector grid output)\n',T0./T1)
    
    



    %%

end
