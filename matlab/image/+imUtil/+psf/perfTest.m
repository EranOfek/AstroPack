function [Result] = perfTest(X, Y, Args)
    % perfTest for package: imUtil.psf

    %%

    K = imUtil.kernel2.gauss;
    CenterX = (size(K,2)+1)./2;
    CenterY = (size(K,1)+1)./2;
   
    Nsim = 1000;
    tic; for i=1:Nsim, R=imUtil.psf.radialProfile(K, [CenterY CenterX]);end, T1=toc;
    tic;for i=1:Nsim, [Rm,Mm,Sm]=imUtil.psf.mex.radialProfile_mex(K, CenterX, CenterY);end,T2=toc;
    fprintf('imUtil.psf.mex.radialProfile_mex is x %f faster than imUtil.psf.radialProfile (small stamp)\n',T1./T2);

    Nsim = 10;
    tic; for i=1:Nsim, R=imUtil.psf.radialProfile(K, [CenterY CenterX], 'radius',150);end, T1=toc;
    tic;for i=1:Nsim, [Rm,Mm,Sm]=imUtil.psf.mex.radialProfile_mex(K, CenterX, CenterY, 150);end,T2=toc;
    fprintf('imUtil.psf.mex.radialProfile_mex is x %f faster than imUtil.psf.radialProfile (large stamp)\n',T1./T2);


    %%

    Result = true;
end
