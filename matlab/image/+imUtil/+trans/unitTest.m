function Result = unitTest()
    % Unit-Test for imUtil.trans package
    % Example: imUtil.trans.unitTest

    %% imUtil.trans.mex.imrotate_sinc

    G   = imUtil.kernel2.gauss([1.5 3 0.5]);
    NG  = imUtil.trans.mex.imrotate_sinc(G,6);
    NNG = imUtil.trans.mex.imrotate_sinc(NG,-6);
    if (max(abs(G-NNG),[],'all')./max(G,[],'all'))>0.01
        error('Problem with imUtil.trans.mex.imrotate_sinc');
    end

    %% imUtil.trans.mex.polyRadialDistortion
    CoefX = rand(5,1);
    X     = rand(1e2, 1e2);
    Y     = rand(1e2, 1e2);
    R     = rand(1e2, 1e2);
    X_Xpower = (0:1:4).';
    X_Ypower = (0:1:4).';
    X_Rpower = (0:1:4).';
    
    Xd0 = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^X_Rpower(:)),1); Xd0=reshape(Xd0,size(X));
    Xd1 = imUtil.trans.mex.polyRadialDistortion(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);
    if max(abs(Xd1-Xd0),[],'all')>1e-14
        error('Problem with imUtil.trans.mex.polyRadialDistortion');
    end

    Xd0 = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^0),1); Xd0=reshape(Xd0,size(X));
    Xd1 = imUtil.trans.mex.polyRadialDistortion(X, Y, R, CoefX, X_Xpower, X_Ypower, 0);
    if max(abs(Xd1-Xd0),[],'all')>1e-14
        error('Problem with imUtil.trans.mex.polyRadialDistortion');
    end
   
    R = 1;
    Xd0 = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^X_Rpower(:)),1); Xd0=reshape(Xd0,size(X));
    Xd1 = imUtil.trans.mex.polyRadialDistortion(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);                               
    if max(abs(Xd1-Xd0),[],'all')>1e-14
        error('Problem with imUtil.trans.mex.polyRadialDistortion');
    end
    
    X_Xpower = rand(5,1);
    R        = rand(1, 1e4);
    Xd0 = sum(CoefX(:) .* ((X(:).').^X_Xpower(:) ) .* ((Y(:).').^X_Ypower(:))  .* ((R(:).').^X_Rpower(:)),1); Xd0=reshape(Xd0,size(X));
    Xd1 = imUtil.trans.mex.polyRadialDistortion(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);                               
    if max(abs(Xd1-Xd0),[],'all')>1e-14
        error('Problem with imUtil.trans.mex.polyRadialDistortion');
    end

				

    %% imUtil.shift.shift_fft / shift_interp / mex.shift_lanczos3
    Nkernel = 2;
    Cube = single(imUtil.kernel2.gauss(1.5.*ones(Nkernel,1)));
    DX   = 2.*ones(Nkernel,1); %rand(Nkernel,1).*4-2;
    DY   = 2.*ones(Nkernel,1); %rand(Nkernel,1).*4-2;

    %OutInt = imUtil.trans.shift_interp(Cube, DX, DY);
    %OutIntR = imUtil.trans.shift_interp(OutInt,-DX,-DY);

    OutFFT = imUtil.trans.shift_fft(Cube, DX, DY);
    OutFFTR = imUtil.trans.shift_fft(OutFFT,-DX,-DY);

    %OutL3  = shift_cube_sinc_sep_simd(Cube, DX, DY);
    %OutL3R = shift_cube_sinc_sep_simd(OutL3, -DX, -DY);
    %OutL3  = shift_sinc_sep_simd_fastin(Cube, DX, DY);
    %OutL3R = shift_sinc_sep_simd_fastin(OutL3, -DX, -DY);


    OutL3  = imUtil.trans.mex.shift_lanczos3(Cube, DX, DY);
    OutL3R = imUtil.trans.mex.shift_lanczos3(OutL3, -DX, -DY);

    
    RelDiffL3  = (Cube(3:13,3:13,1)-OutL3R(3:13,3:13,1))./Cube(3:13,3:13,1);
    RelDiffFFT = (Cube(3:13,3:13,1)-OutFFTR(3:13,3:13,1))./Cube(3:13,3:13,1);

    %max(RelDiffL3,[],'all')
    if max(RelDiffL3,[],'all')>1e-12
        error('Problem with imUtil.trans.mex.shift_lanczos3');
    end
    if max(RelDiffFFT,[],'all')>1e-4
        error('Problem with imUtil.trans.shift_fft');
    end



    %%
    
	Result = true;
end
