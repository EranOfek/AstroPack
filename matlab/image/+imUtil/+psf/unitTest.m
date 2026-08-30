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


    %% imUtil.psf.combinePSF

    P1 = imUtil.kernel2.gauss([2 2 0],[15 15]);   P1 = P1./sum(P1,'all');
    P2 = imUtil.kernel2.gauss([3 3 0],[15 15]);   P2 = P2./sum(P2,'all');
    Tol = 100.*eps;

    % equal weights give the plain mean
    C = imUtil.psf.combinePSF({P1,P2});
    if max(abs(C-(P1+P2)./2),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - equal weights');
    end

    % the weights act through their ratio only, and the result is normalized
    C1 = imUtil.psf.combinePSF({P1,P2}, 'Weights',[1 3]);
    C2 = imUtil.psf.combinePSF({P1,P2}, 'Weights',[10 30]);
    if max(abs(C1-(0.25.*P1+0.75.*P2)),[],'all') > Tol || ~isequal(C1,C2) || abs(sum(C1,'all')-1) > Tol
        error('Problem with imUtil.psf.combinePSF - weighted mean');
    end

    % the input stamps are normalized before the combination
    C3 = imUtil.psf.combinePSF({5.*P1, P2}, 'Weights',[1 3]);
    if max(abs(C3-C1),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - input normalization');
    end

    % the numeric [Ny,Nx,Npsf] form matches the cell array form
    C4 = imUtil.psf.combinePSF(cat(3,P1,P2), 'Weights',[1 3]);
    if max(abs(C4-C1),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - numeric input form');
    end

    % a single stamp is returned as it is
    if max(abs(imUtil.psf.combinePSF({P1})-P1),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - single stamp');
    end

    % the variance of a weighted mean of independent estimates: sum(W_i^2*Var_i),
    % and it follows the normalization applied to its own stamp
    V1 = 0.1.*P1;  V2 = 0.2.*P2;
    [~, CV]  = imUtil.psf.combinePSF({P1,P2}, 'Weights',[1 3], 'Var',{V1,V2});
    [~, CV2] = imUtil.psf.combinePSF({5.*P1,P2}, 'Weights',[1 3], 'Var',{25.*V1,V2});
    if max(abs(CV-(0.25.^2.*V1+0.75.^2.*V2)),[],'all') > Tol || max(abs(CV2-CV),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - variance propagation');
    end

    % extra stamp dimensions (e.g. a 'Purpose' cube) survive, each slice
    % being combined on its own
    D1 = imUtil.kernel2.gauss([2.5 2.5 0],[15 15]);  D1 = D1./sum(D1,'all');
    D2 = imUtil.kernel2.gauss([3.5 3.5 0],[15 15]);  D2 = D2./sum(D2,'all');
    CQ = imUtil.psf.combinePSF({cat(3,P1,D1), cat(3,P2,D2)}, 'Weights',[1 3]);
    if ~isequal(size(CQ),[15 15 2]) || max(abs(CQ(:,:,1)-C1),[],'all') > Tol || ...
            max(abs(CQ(:,:,2)-(0.25.*D1+0.75.*D2)),[],'all') > Tol
        error('Problem with imUtil.psf.combinePSF - multi-D stamps');
    end

    % without normalization the scale of the input stamps is kept
    if abs(sum(imUtil.psf.combinePSF({2.*P1, 2.*P2}, 'Norm',false),'all')-2) > Tol
        error('Problem with imUtil.psf.combinePSF - Norm=false');
    end

    % invalid input must be rejected
    BadInput = {};
    try, imUtil.psf.combinePSF({P1,P2}, 'Weights',[1 2 3]); BadInput{end+1}='weight count'; end %#ok<TRYNC>
    try, imUtil.psf.combinePSF({P1,P2}, 'Weights',[1 -1]);  BadInput{end+1}='negative weight'; end %#ok<TRYNC>
    try, imUtil.psf.combinePSF({P1,P2(1:13,1:13)});         BadInput{end+1}='size mismatch'; end %#ok<TRYNC>
    try, imUtil.psf.combinePSF({P1,P2}, 'Var',{V1});        BadInput{end+1}='variance count'; end %#ok<TRYNC>
    try, imUtil.psf.combinePSF(cat(4,P1,P2));               BadInput{end+1}='4-D numeric input'; end %#ok<TRYNC>
    if ~isempty(BadInput)
        error('Problem with imUtil.psf.combinePSF - not rejected: %s', strjoin(BadInput,', '));
    end

    %%

	Result = true;
end
