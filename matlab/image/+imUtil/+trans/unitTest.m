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

    % Direction check: positive Rotation must match MATLAB's imrotate
    % convention (counter-clockwise in the array/display sense).
    Ntest = 41;
    Ctest = (Ntest+1)/2;
    Pt = zeros(Ntest,Ntest);
    Pt(Ctest, Ctest+10) = 1;  % bright pixel to the right of center
    PtRotSinc = imUtil.trans.mex.imrotate_sinc(Pt, 90);
    PtRotML   = imrotate(Pt, 90, 'nearest', 'crop');
    if ~isequal(find(PtRotSinc==max(PtRotSinc(:))), find(PtRotML==max(PtRotML(:))))
        error('imUtil.trans.mex.imrotate_sinc rotation direction does not match imrotate convention');
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



    %% imUtil.trans.shift_lanczos - shift fidelity (issue #1298)
    % Until Sep 2026 the whole-pixel part of the shift was applied only for
    % abs(shift)>1, so any shift in (-1,0) came out +1 pixel off and a shift of
    % exactly +-1 did nothing at all. These checks cover both.

    Gs     = imUtil.kernel2.gauss([2 2 0],[31 31]);
    Gs     = Gs./sum(Gs,'all');
    [Mr,Mc] = ndgrid(1:31,1:31);
    Centre = @(Q) [sum(Q.*Mr,'all')./sum(Q,'all'), sum(Q.*Mc,'all')./sum(Q,'all')];
    Cen0   = Centre(Gs);
    % NB: shift_lanczos takes [ShiftX, ShiftY] with X along the columns
    TestShift = [0.3 0; -0.3 0; 0 0.3; 0 -0.3; 0.5 0; -0.5 0; 0.8 0; -0.8 0; ...
                 1 0; -1 0; 0 1; 0 -1; 1.3 0; -1.3 0; 0.35 -0.35; 2.7 -3.2];
    for Icirc = [false true]
        for Ish = 1:size(TestShift,1)
            Cen = Centre(imUtil.trans.shift_lanczos(Gs, TestShift(Ish,:), 3, Icirc, 0)) - Cen0;
            % the Lanczos kernel compresses the centroid by a few percent of the
            % requested shift, hence the tolerance
            if abs(Cen(2)-TestShift(Ish,1))>0.06 || abs(Cen(1)-TestShift(Ish,2))>0.06
                error('Problem with imUtil.trans.shift_lanczos: shift [%g %g] gave [%g %g]', ...
                      TestShift(Ish,1), TestShift(Ish,2), Cen(2), Cen(1));
            end
        end
    end

    % a whole-pixel shift must be exact and must conserve the flux
    Delta = zeros(21,21); Delta(11,11) = 1;
    for Ish = [3 -3 7 -7]
        Moved = imUtil.trans.shift_lanczos(Delta, [Ish 0], 3, false, 0);
        if abs(Moved(11,11+Ish)-1)>1e-10 || abs(sum(Moved,'all')-1)>1e-10
            error('Problem with imUtil.trans.shift_lanczos: whole pixel shift is not exact');
        end
    end

    % a sub-pixel shift must conserve the flux, and shifting back must restore the image
    CubeL = single(imUtil.kernel2.gauss(2.5.*ones(10,1),[31 31]));
    ShiftL = 3.*(rand(10,2)-0.5);
    if max(abs(sum(imUtil.trans.shift_lanczos(CubeL, ShiftL, 3, false, 0),[1 2])./sum(CubeL,[1 2]) - 1),[],'all')>1e-5
        error('Problem with imUtil.trans.shift_lanczos: flux is not conserved');
    end
    for Icirc = [false true]
        BackL = imUtil.trans.shift_lanczos(imUtil.trans.shift_lanczos(CubeL, ShiftL, 3, Icirc, 0), ...
                                           -ShiftL, 3, Icirc, 0);
        if max(abs(double(BackL(8:24,8:24,:))-double(CubeL(8:24,8:24,:))),[],'all')./double(max(CubeL,[],'all'))>0.01
            error('Problem with imUtil.trans.shift_lanczos: shift/unshift does not restore the image');
        end
    end

    % the m-code and the mex must agree (both are lanczos3, non-circular)
    if max(abs(double(imUtil.trans.mex.shift_lanczos3(CubeL, ShiftL(:,1), ShiftL(:,2))) - ...
               double(imUtil.trans.shift_lanczos(CubeL, ShiftL, 3, false, 0))),[],'all') ...
               ./double(max(CubeL,[],'all')) > 0.01
        error('imUtil.trans.shift_lanczos and imUtil.trans.mex.shift_lanczos3 disagree');
    end

    % shifting the image completely out of the frame must leave only PadVal
    if ~all(abs(imUtil.trans.shift_lanczos(single(ones(11)), [30 0], 3, false, 7) - 7)<1e-4, 'all')
        error('Problem with imUtil.trans.shift_lanczos: PadVal is not applied');
    end

    %%
    
	Result = true;
end
