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

    %%

	Result = true;
end
