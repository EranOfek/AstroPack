function Result=perfTest()
    % perfTest for imUtil.stack

    %% imUtil.stack.wcoaddRobust (and mex)

    Nim = 20;
    ImSize = 171;
    Im  = ones(ImSize,ImSize,Nim);
    F_k = rand(1,1,Nim).*0.0001+1;
    ZP0 = 3;
    ZP  = ZP0 - 2.5.*log10(F_k);
    
    Bm  = 100;
    Im  = Im.*Bm.*F_k; 
    B   = Bm.*squeeze(F_k);
    V   = Bm.*squeeze(F_k);
    
    Im = poissrnd(Im);
    
    %---
    ZP = 25;
    ZP0 = 25;
    RemoveMinMax = false;
    Niter = 0;
    SigmaClip = [2 2];
    StdMethod = 1;
    
    tic;
    [C, Cvar] = imUtil.stack.wcoaddRobust(Im, B, 'UseMex',false,'Var',V, 'F',F_k, 'ZP',ZP,'ZP0',ZP0,'RemoveMinMax',RemoveMinMax,'Niter',Niter,'SigmaClip',SigmaClip, 'StdMethod',StdMethod);
    T1=toc;
    
    Fs = squeeze(F_k);
    tic;
    [C1, Cvar1] = imUtil.stack.mex.wcoaddRobust_mex(Im, B, V, Fs, ZP, ZP0, RemoveMinMax, Niter, SigmaClip, StdMethod);
    T2=toc;
    fprintf('wcoaddRobust_mex is x %f faster\n', T1./T2)
    
    %fprintf('Max diff : %f, %f\n',max(abs(C-C1),[],'all'), max(abs(Cvar-Cvar1),[],'all'));
    
    
    
    %median(Cvar,'all','omitmissing')
    %var(C,[],'all','omitmissing')
    
    %---
    ZP = 25;
    ZP0 = 25;
    RemoveMinMax = true;
    Niter = 1;
    SigmaClip = [2 2];
    StdMethod = 1;
    
    tic;
    [C, Cvar] = imUtil.stack.wcoaddRobust(Im, B, 'UseMex',false,'Var',V, 'F',F_k, 'ZP',ZP,'ZP0',ZP0,'RemoveMinMax',RemoveMinMax,'Niter',Niter,'SigmaClip',SigmaClip, 'StdMethod',StdMethod);
    T1=toc;
    
    Fs = squeeze(F_k);
    tic;
    [C1, Cvar1] = imUtil.stack.mex.wcoaddRobust_mex(Im, B, V, Fs, ZP, ZP0, RemoveMinMax, Niter, SigmaClip, StdMethod);
    T2=toc;
    fprintf('wcoaddRobust_mex is x %f faster\n', T1./T2)
    
    %fprintf('Max diff : %f, %f\n',max(abs(C-C1),[],'all'), max(abs(Cvar-Cvar1),[],'all'));
    
    
    %---
    ZP = 25;
    ZP0 = 25;
    RemoveMinMax = true;
    Niter = 1;
    SigmaClip = [2 2];
    StdMethod = 2;
    
    tic;
    [C, Cvar] = imUtil.stack.wcoaddRobust(Im, B, 'UseMex',false,'Var',V, 'F',F_k, 'ZP',ZP,'ZP0',ZP0,'RemoveMinMax',RemoveMinMax,'Niter',Niter,'SigmaClip',SigmaClip, 'StdMethod',StdMethod);
    T1=toc;
    
    Fs = squeeze(F_k);
    tic;
    [C1, Cvar1] = imUtil.stack.mex.wcoaddRobust_mex(Im, B, V, Fs, ZP, ZP0, RemoveMinMax, Niter, SigmaClip, StdMethod);
    T2=toc;
    fprintf('wcoaddRobust_mex is x %f faster\n', T1./T2)
    
    %fprintf('Max diff : %f, %f\n',max(abs(C-C1),[],'all'), max(abs(Cvar-Cvar1),[],'all'));
    
    %---
    ZP = 25;
    ZP0 = 25;
    RemoveMinMax = true;
    Niter = 1;
    SigmaClip = [2 2];
    StdMethod = 3;
    
    tic;
    [C, Cvar] = imUtil.stack.wcoaddRobust(Im, B, 'UseMex',false,'Var',V, 'F',F_k, 'ZP',ZP,'ZP0',ZP0,'RemoveMinMax',RemoveMinMax,'Niter',Niter,'SigmaClip',SigmaClip, 'StdMethod',StdMethod);
    T1=toc;
    
    Fs = squeeze(F_k);
    tic;
    [C1, Cvar1] = imUtil.stack.mex.wcoaddRobust_mex(Im, B, V, Fs, ZP, ZP0, RemoveMinMax, Niter, SigmaClip, StdMethod);
    T2=toc;
    fprintf('wcoaddRobust_mex is x %f faster\n', T1./T2)
    
    %fprintf('Max diff : %f, %f\n',max(abs(C-C1),[],'all'), max(abs(Cvar-Cvar1),[],'all'));
    


    Result = true;
end