% Package Unit-Test: tools.interp
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    Size = 1726;
    V=single(rand(Size,Size));                                                             
    [MatX,MatY]=meshgrid((1:Size),(1:Size));
    MatX1=single(MatX+0.1); MatY1=single(MatY+5.2);

    % interp2d_bilinear
    tic;
    for I=1:1:100
        Vq=tools.interp.interp2d_bilinear(V,MatX1,MatY1);
    end
    T=toc;
    fprintf('tools.interp.interp2d_bilinear (mex) : %f\n',T);

    tic;
    for I=1:1:100
        Vq1=interp2(V,MatX1,MatY1,'linear');
    end
    T=toc;                        
    fprintf('interp2 / linear : %f\n',T);


    if max(abs(Vq-Vq1),[],'all')>1e-6
        error('Problem with: tools.interp.interp2d_bilinear (mex)');
    end


    % interp2d_mex_uint32_nearest
    V = uint32(rand(Size, Size).*1e4);
    [MatX,MatY]=meshgrid((1:Size),(1:Size));
    MatX1=single(MatX+0.1);
    MatY1=single(MatY+5.2);
    tic;
    for I=1:1:100
        Vq = interp2(V, MatX1, MatY1, 'nearest');
    end
    T=toc;
    fprintf('interp2 / nearest : %f\n',T);

    tic;
    Vx = (1:1726);
    for I=1:1:100
        Vq1 = tools.interp.mex.interp2d_mex_uint32_nearest(Vx,Vx,V, MatX1, MatY1);
    end
    T=toc;
    fprintf('interp2d_mex_uint32_nearest : %f\n',T);

    if max(abs(Vq - Vq1),[],'all')>0
        error('Proble with: tools.interp.mex.interp2d_mex_uint32_nearest');
    end


    %% tools.interp.interp_diff

    Deg=4;
    X=(5:0.4:500).';
    Fun = @(x) 0.1.*x.^1.7 + 0.2.*x.^-0.9;
    Y= Fun(X);
    Xp = rand(1e4,1).*300+10;

    Yi = tools.interp.interp_diff(X,Y,Xp, Deg);

    Yexact = Fun(Xp);

    %max(abs(Yi-Yexact))
    if max(abs(Yi-Yexact))>1e-7
        max(abs(Yi-Yexact))
        error('Problem with tools.interp.interp_diff')
    end

    % speed compared to old/obsolete version:
    %tic;
    %for i=1:1e3
    %    %Yi = tools.interp.obsolete.interp_diff(X,Y,Xp, Deg);
    %    Yi = tools.interp.interp_diff(X,Y,Xp, Deg);
    %end
    %toc

    %%

    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

