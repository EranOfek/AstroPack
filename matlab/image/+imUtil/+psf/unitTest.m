function Result = unitTest()
    % unitTest for imUtil.psf package   

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
     
    tic; for i=1:1000, R=imUtil.psf.radialProfile(K, [CenterY CenterX]);end, T1=toc;
    tic;for i=1:1000, [Rm,Mm,Sm]=imUtil.psf.mex.radialProfile_mex(K, CenterX, CenterY);end,T2=toc;
    fprintf('imUtil.psf.mex.radialProfile_mex is x %f faster than imUtil.psf.radialProfile\n',T1./T2);

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
