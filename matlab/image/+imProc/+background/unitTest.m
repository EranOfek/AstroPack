function Result = unitTest()
    % unitTest for the imProc.background package
    %io.msgLog(LogLevel.Test, 'imProc.background test started');
    
    % background
    % fast_median is not supported
    AI = AstroImage({poissrnd(100,1024,1024)});
    Result = imProc.background.background(AI,'UseFastMedian',false,'Overlap',0);
    Result = imProc.background.background(AI, 'BackFun', @median,...
                                         'BackFunPar',{[1 2]},...
                                         'VarFun',@imUtil.background.rvar,...
                                         'VarFunPar',{},...
                                         'SubSizeXY',[128 128],...
                                         'Overlap',16,'UseFastMedian',false);
    AI = AstroImage({poissrnd(100,1024,1024)});
    Result1 = imProc.background.background(AI,'UseFastMedian',false);
    if ~all(abs(Result1.Back-100)<2,'all')
        error('Background was not calculated correctly');
    end
    if mean(Result1.VarData.Data-100,'all')>3
        error('Variance was not calculated correctly');
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
    
    % imProc.background.backVar failure path + imProc.background.isFailedBack (issue #1226)
    % An element whose background estimation fails must get NaN Back/Var,
    % be reported in FailedList, and carry the nine background keywords
    % with a blank (NaN) value - not the previous element's values and
    % not [] (which the mex header writers serialize as 0).
    BackVarArgs = {'Method',@imUtil.background.modeVar_LogHist, 'Block',[512 512], ...
                   'PoissVar',true, 'Ncoadd',1, 'RN2',13, ...
                   'MethodArgs',{{'MinVal',10, 'MaxVal',7000},{}}};
    Keys = {'MEANBCK','MEDBCK','STDBCK','MEANVAR','MEDVAR','MINBCK','MAXBCK','BCKMTHD','VARMTHD'};
    AI = AstroImage([1 2]);
    Good = 200 + sqrt(200).*randn(600,'single');
    AI(1).ImageData.Image = Good;
    AI(2).ImageData.Image = Good - max(Good(:)) - 10;   % all pixels <= 0 -> LogHist throws
    [AI, FailedList] = imProc.background.backVar(AI, BackVarArgs{:});
    if ~isequal(FailedList, 2)
        error('Problem with imProc.background.backVar: FailedList');
    end
    if ~all(cellfun(@(K) AI(2).HeaderData.isKeyExist(K), Keys)) || ...
       ~all(cellfun(@(K) isnan(AI(2).HeaderData.getVal(K)), Keys))
        error('Problem with imProc.background.backVar: failed element keywords must be present and NaN');
    end
    if ~(isfinite(AI(1).HeaderData.getVal('MEDBCK')) && AI(1).HeaderData.getVal('MEDBCK')>0) || ...
       AI(1).HeaderData.getVal('BCKMTHD')~=2
        error('Problem with imProc.background.backVar: healthy element keywords');
    end
    % the blank value must survive [Struct.KEY] concatenation as NaN (see #1194)
    St = AI.getStructKey({'MEDBCK'});
    CatVals = [St.MEDBCK];
    if ~(isfloat(CatVals) && numel(CatVals)==2 && isnan(CatVals(2)))
        error('Problem with imProc.background.backVar: [Struct.MEDBCK] concatenation');
    end
    % [healthy, failed, never estimated] -> [0 1 0]
    AI3  = AstroImage({single(rand(20))});
    Flag = imProc.background.isFailedBack([AI(1), AI(2), AI3]);
    if ~isequal(Flag(:).', [false true false])
        error('Problem with imProc.background.isFailedBack');
    end
    
    cd(PWD);
    %io.msgStyle(LogLevel.Test, '@passed', 'imProc.background test passed');
    Result = true; 
end