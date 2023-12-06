function Result = unitTest()
    % unitTest for the +imProc.background package
    io.msgLog(LogLevel.Test, 'imProc.background test started');
    
    % background
    % fast_median is not supported
    AI = AstroImage({rand(1024,1024)});
    Result = imProc.background.background(AI,'UseFastMedian',false,'Overlap',0);
    Result = imProc.background.background(AI, 'BackFun', @median,...
                                         'BackFunPar',{[1 2]},...
                                         'VarFun',@imUtil.background.rvar,...
                                         'VarFunPar',{},...
                                         'SubSizeXY',[128 128],...
                                         'Overlap',16,'UseFastMedian',false);
    AI = AstroImage({ones(1024,1024)});
    Result1 = imProc.background.background(AI,'UseFastMedian',false);
    if ~all(Result1.BackData.Data==1,'all')
        error('Background was not calculated correctly');
    end
    if ~all(Result1.VarData.Data==0,'all')
        error('Variance was not calculated correctly');
    end
    
    Result2 = imProc.background.background(AI,'SubBack',true,'UseFastMedian',false);
    if ~all( abs(Result2.ImageData.Data-0)<100.*eps )
        error('Background was not subtracted correctly');
    end
    
    [MatX, MatY] = meshgrid( (1:1:1000), (1:1:1000) );
    Z = 1+MatX +MatY + MatX.*MatY;
    AI = AstroImage({Z});
    [Result, Surface] = imProc.background.fitSurface(AI);
    if max(abs(Surface.Resid),[],'all')>1e-6
        error('Problem with fitSurface');
    end
    
    Z = MatX + 3*MatY + 0*sin(MatX);
    AI = AstroImage({Z});
    [Result2, Surface] = imProc.background.fitSurface(AI,'Fun',{@(x,y)ones(size(x)),@(x,y)x+3*y,@(x,y)sin(x)},'Niter',1);
%     [Result2, Surface] = imProc.background.fitSurface(AI,'Fun',{@(x,y)ones(size(x)),@(x,y)x+3*y},'Niter',1);
    if max(abs(Surface.Resid),[],'all')>1e-6
        error('Problem with fitSurface');
    end
    
    VecX = (11:10:990).';
    VecY = VecX;
    [MatX, MatY] = meshgrid( VecX, VecY);
    Z = 2+MatX +MatY + MatX.*MatY;
    AI = AstroImage({Z});
    [Result, Surface] = imProc.background.fitSurface(AI); %, 'SizeIJ',[1000 1000], 'VecX',VecX, 'VecY',VecY);
    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
%     cd(DataSampleDir);

    AI = AstroImage([DataSampleDir,'/PTF_201411204943_i_p_scie_t115144_u023050379_f02_p100037_c02.fits']);
    [SmBackEst, BackEst] = imProc.background.filterSources(AI);
    
    
    cd(PWD);
    io.msgStyle(LogLevel.Test, '@passed', 'imProc.background test passed');
    Result = true; 
end