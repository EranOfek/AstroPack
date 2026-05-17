function Result = unitTest()
    % unitTest for imUtil.psf package   


    %% imUtil.psf.mex.fitGauss2D

    N = 1000;
    A = rand(N,1).*1.5+1;
    B = rand(N,1).*0.5+1;
    Rho = rand(N,1);

    G = imUtil.kernel2.gauss([A,B,Rho]);
    tic;
    [a,b,c,d,e,f]=imUtil.psf.mex.fitGauss2D(G, 1e-2);
    toc
    tic;
    for I=1:1:N
        [R(I),BF] = imUtil.psf.fitFunPSF(G(:,:,I), 'Funs',{@imUtil.kernel2.gauss}, 'Par0',{[2 2 0],[1]}, 'Norm0',[1 1]);
    end
    toc
    Par=reshape([R.Par],4,1000)';
    % allow for up to 3% results with errors exceeding 0.1
    if max(sum(abs(   [b,c,d,e] - [ones(N,1), A,B,Rho])>0.1) )./N >0.03
        error('Problem with imUtil.psf.mex.fitGauss2D');
    end
    if max( sum(abs(   [Par] - [ones(N,1), A,B,Rho])>0.1)     )./N > 0.03
        error('Problem with imUtil.psf.fitFunPSF');
    end

    %% imUtil.psf.stamp2full

    K=imUtil.kernel2.gauss(2.*ones(100,1));
    F=imUtil.psf.stamp2full(K,[31 32],'CenterPosition','center');
    M=imUtil.image.moment2(F(:,:,2),16,16);
    if abs(M.X-16.5)>1e-4 || abs(M.Y-16)>1e-4
        error('Problem with imUtil.psf.stamp2full');
    end

    %% 
    K=imUtil.kernel2.gauss(2.*ones(100,1));
    F=imUtil.psf.stamp2full(K,[31 31],'CenterPosition','center');
    Fs = imUtil.psf.full2stamp(F, [15 15], 'FullPosition','center');
    %old: Fs1 = imUtil.psf.full2stamp(K(:,:,1), 'StampHalfSize',[7 7],'IsCorner',false);

    M = imUtil.image.moment2(Fs(:,:,1),8,7.6);
    if abs(M.X-8)>1e-4 || abs(M.X-8)>1e-4
        error('Problem with imUtil.psf.full2stamp');
    end
    if max(abs(Fs-K),[],'all')>1e-3
        error('Problem with imUtil.psf.full2stamp');
    end
    % no on even image
    K=imUtil.kernel2.gauss(2.*ones(100,1));
    F=imUtil.psf.stamp2full(K,[31 32],'CenterPosition','center');
    Fs = imUtil.psf.full2stamp(F, [15 15], 'FullPosition','center');

    M = imUtil.image.moment2(Fs(:,:,1),8,7.6);
    if abs(M.X-8)>3e-4 || abs(M.X-8)>3e-4
        abs(M.X-8)
        abs(M.Y-8)
        error('Problem with imUtil.psf.full2stamp');
    end
    if max(abs(Fs-K),[],'all')>1e-3
        error('Problem with imUtil.psf.full2stamp');
    end

    %% imUtil.psf.radialProfile / imUtil.psf.mex.radialProfile_mex

    K = imUtil.kernel2.gauss;
    CenterX = (size(K,2)+1)./2;
    CenterY = (size(K,1)+1)./2;
    VecX = (1:size(K,2)) - CenterX;
    VecY = (1:size(K,1)) - CenterY;
    [MatX, MatY] = meshgrid(VecX, VecY);
    MatR = sqrt(MatX.^2 + MatY.^2);
     
    R=imUtil.psf.radialProfile(K, [CenterY CenterX]);
    [Rm,Mm,Sm]=imUtil.psf.mex.radialProfile_mex(K, CenterX, CenterY);

    % manual:
    
    II = find(MatR>=3 & MatR<4);
    if abs(mean(K(II))-Mm(4))>(10.*eps)
        error('Problem with imUtil.psf.mex.radialProfile_mex');
    end

    if any(abs(Mm(1:15)./R.MeanV - 1)>(10.*eps))
        Mm(1:15)./R.MeanV - 1
        error('Problem with imUtil.psf.mex.radialProfile_mex');
    end


    %%

	Result = true;
end
