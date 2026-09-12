function Result = unitTest()
    % unitTest for the Stack class
    % Example: Result = imProc.stack.unitTest
    

    %% applyUnaryFun - complete when fixed
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.applyUnaryFun(AI,1);
    R  = imProc.stack.applyUnaryFun(AI,[1 2]);
    R  = imProc.stack.applyUnaryFun(AI,{1 2}); % the same
    R  = imProc.stack.applyUnaryFun(AI,AI); 
    R  = imProc.stack.applyUnaryFun(AI,@mean,@minus,'OpArgs',{'all'});
    R  = imProc.stack.applyUnaryFun(AI,@mean,@rdivide,'OpArgs',{'all'});

    %% subtractOffset - complete when fixed
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.subtractOffset(AI,1);
    R  = imProc.stack.subtractOffset(AI,[1 2]);
    assert(all(R(1).Image==0,'all') )
    R  = imProc.stack.subtractOffset(AI,{1 2}); % the same
    R  = imProc.stack.subtractOffset(AI,AI); 
    assert(all(R(1).Image==0,'all') )
    R  = imProc.stack.subtractOffset(AI,@mean,'OpArgs',{'all'});
    assert(all(R(1).Image==0,'all') ,'problem with subtractOffset')

    %% divideFactor
    AI = AstroImage({ones(3,3), 3.*ones(4,4)});
    R  = imProc.stack.divideFactor(AI,1);
    R  = imProc.stack.divideFactor(AI,[1 2]);
    assert(all(R(1).Image==1,'all') && all(R(2).Image==1.5,'all'))
    R  = imProc.stack.divideFactor(AI,{1 2}); % the same
    R  = imProc.stack.divideFactor(AI,AI); 
    R  = imProc.stack.divideFactor(AI,@mean,'OpArgs',{'all'});
    assert(all(R(1).Image==1,'all') && all(R(2).Image==1,'all'),'problem with divideFactor')
    
    %% funCube
    % the output arguments when SaveInProp is strange
    %AI = AstroImage({rand(10,10), rand(10,10), rand(10,10)});
    %[Cube1, Cube2] = imProc.stack.funCube(AI);
    %[CAI] = imProc.stack.funCube(AI,'SaveInProp',{'ImageData','VarData'});
    %assert(all(CAI.Image==Cube1,'all') && all(CAI.Var==Cube2,'all'),'problem with funCube')

    %% coadd
    % why Result.Var is filled with CoaddVarEmpirical when there are
    % weights? 
    % default Args.OffsetArgs could be [1 2](for dim argument of many
    % functions like mean)
    AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    [Result, CoaddN] = imProc.stack.coadd(AI);
    AI = AstroImage({ones(5,5), 2.*ones(5,5), 3.*ones(5,5)},'Var',{ones(5,5), 2.*ones(5,5), 3.*ones(5,5)});
    [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'StackMethod','wmean');
    assert(all(Result.Image<2,'all'),'problem with coadd');
    [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'Offset',@mean,'OffsetArgs',{[2 3]});
    assert(all(Result.Image==0,'all'),'problem with coadd');
    AI = AstroImage({ones(5), 2.*ones(6), 3.*ones(10)});
%     fails! empty back/mask/var images can't be combined with CCDSEC
%     [Result, CoaddN,Cube] = imProc.stack.coadd(AI,'CCDSEC',[1 5 1 5]); 
    
    
    % functionalResponse
    AI = AstroImage({ones(3,3), 2.*ones(3,3), 10.*ones(3,3), 11.*ones(3,3), 13.*ones(3,3)});
    Result = imProc.stack.functionalResponse(AI);
    Result = imProc.stack.functionalResponse(AI, 'Intensity',[1 2 10 11 13]);

    %% stitchCrops - PSF propagation (issue #1233)
    % the crops are produced by the real cutter, so that they carry consistent
    % CCDSEC / ORIGSEC / ORIGUSEC keywords; the catalogs and the PSFs are
    % synthetic, as only the PSF propagation is checked here
    rng(1);
    AIfull = AstroImage({rand(200,200)});
    SI     = imProc.image.image2subimages(AIfull, [], 'Nxy',[2 2], 'OverlapXY',[10 10]);
    Ncr    = numel(SI);
    Npsf   = 15;
    CubePSF = zeros(Npsf, Npsf, Ncr, 'single');
    for Icr=1:1:Ncr
        % a minimal catalog: stitchCrops needs all the XPEAK/X1/X synonyms and RA/Dec
        Nsrc = 20;
        Xs   = 5 + rand(Nsrc,1).*(size(SI(Icr).Image,2)-10);
        Ys   = 5 + rand(Nsrc,1).*(size(SI(Icr).Image,1)-10);
        SI(Icr).CatData = AstroCatalog({[Xs Ys Xs Ys Xs Ys Xs./100 Ys./100]},...
                                       'ColNames',{'XPEAK','YPEAK','X1','Y1','X','Y','RA','Dec'});
        % a distinct PSF per crop
        Pcr = imUtil.kernel2.gauss([1.5+0.3.*Icr, 1.5+0.3.*Icr, 0], [Npsf Npsf]);
        Pcr = single(Pcr./sum(Pcr,'all'));
        SI(Icr).PSFData.DataPSF = Pcr;
        SI(Icr).PSFData.DataVar = 0.01.*Pcr;
        SI(Icr).PSFData.Nstars  = 10.*Icr;
        CubePSF(:,:,Icr)        = Pcr;
    end
    TolS = 10.*eps('single');   % the PSF stamps are single precision

    % the default leaves the PSF of the stitched image empty
    Rst = imProc.stack.stitchCrops(SI);
    assert(Rst.PSFData.isemptyPSF, 'stitchCrops: the default PSFMethod must leave the PSF empty')

    % 'central' copies the PSF of one crop, and copies it rather than aliasing it
    Rst  = imProc.stack.stitchCrops(SI, 'PSFMethod','central');
    Icen = find(arrayfun(@(I) isequal(Rst.PSFData.DataPSF, SI(I).PSFData.DataPSF), 1:1:Ncr), 1);
    assert(~isempty(Icen), 'stitchCrops: the central PSF must equal the PSF of one of the crops')
    KeepNstars         = SI(Icen).PSFData.Nstars;
    Rst.PSFData.Nstars = -1;
    assert(isequal(SI(Icen).PSFData.Nstars, KeepNstars),...
           'stitchCrops: the central PSF must be a copy of the crop PSF, not an alias of it')

    % 'wmean' returns a normalized convex combination of the crop PSFs
    Rst    = imProc.stack.stitchCrops(SI, 'PSFMethod','wmean');
    MeanP  = Rst.PSFData.DataPSF;
    MeanV  = Rst.PSFData.DataVar;
    assert(abs(sum(MeanP,'all')-1) < TolS, 'stitchCrops: the weighted mean PSF must be normalized')
    assert(all(MeanP >= min(CubePSF,[],3)-TolS, 'all') && all(MeanP <= max(CubePSF,[],3)+TolS, 'all'),...
           'stitchCrops: the weighted mean PSF must lie between the crop PSFs')
    assert(~any(arrayfun(@(I) isequal(MeanP, SI(I).PSFData.DataPSF), 1:1:Ncr)),...
           'stitchCrops: the weighted mean PSF must differ from each single crop PSF')
    assert(isequal(Rst.PSFData.Nstars, sum(10.*(1:1:Ncr))),...
           'stitchCrops: Nstars must be summed over the contributing crops')
    assert(isequal(size(MeanV), [Npsf Npsf]), 'stitchCrops: the PSF variance must be propagated')

    % a mean of identical PSFs reproduces them
    SIu = SI.copy;
    for Icr=1:1:Ncr
        SIu(Icr).PSFData.DataPSF = SI(1).PSFData.DataPSF;
    end
    Rst = imProc.stack.stitchCrops(SIu, 'PSFMethod','wmean');
    assert(max(abs(Rst.PSFData.DataPSF - SI(1).PSFData.DataPSF), [], 'all') < TolS,...
           'stitchCrops: a mean of identical PSFs must reproduce them')

    % crops without a usable PSF are ignored
    SIe = SI.copy;
    SIe(1).PSFData.DataPSF = [];
    SIe(2).PSFData.DataPSF = zeros(Npsf, Npsf, 'single');
    Rst = imProc.stack.stitchCrops(SIe, 'PSFMethod','wmean');
    assert(all(isfinite(Rst.PSFData.DataPSF),'all') && abs(sum(Rst.PSFData.DataPSF,'all')-1) < TolS,...
           'stitchCrops: crops with an empty or zero PSF must be ignored')
    for Icr=1:1:Ncr
        SIe(Icr).PSFData.DataPSF = [];
    end
    Rst = imProc.stack.stitchCrops(SIe, 'PSFMethod','wmean');
    assert(Rst.PSFData.isemptyPSF,...
           'stitchCrops: with no usable crop PSF the stitched PSF must stay empty')

    % PSFs on different grids cannot be averaged: fall back to the central crop
    % (the warning below is raised on purpose)
    SIm = SI.copy;
    SIm(1).PSFData.DataPSF = SIm(1).PSFData.DataPSF(2:end-1, 2:end-1);
    lastwarn('');
    Rst = imProc.stack.stitchCrops(SIm, 'PSFMethod','wmean');
    [~, WarnId] = lastwarn;
    assert(strcmp(WarnId, 'imProc:stack:stitchCrops:PSFGridMismatch'),...
           'stitchCrops: a PSF grid mismatch must be reported')
    assert(any(arrayfun(@(I) isequal(Rst.PSFData.DataPSF, SIm(I).PSFData.DataPSF), 1:1:Ncr)),...
           'stitchCrops: a PSF grid mismatch must fall back to a single crop PSF')

    % a multi-dimensional ('Purpose') PSF cube survives the combination, while
    % its 2D variance is combined against the leading slice of the cube
    SIc = SI.copy;
    for Icr=1:1:Ncr
        SIc(Icr).PSFData.DataPSF    = cat(3, SI(Icr).PSFData.DataPSF, circshift(SI(Icr).PSFData.DataPSF,1,1));
        SIc(Icr).PSFData.DimName{1} = 'Purpose';
        SIc(Icr).PSFData.DimVals{1} = [1 2];
    end
    Rst = imProc.stack.stitchCrops(SIc, 'PSFMethod','wmean');
    assert(isequal(size(Rst.PSFData.DataPSF), [Npsf Npsf 2]) && strcmp(Rst.PSFData.DimName{1},'Purpose'),...
           'stitchCrops: a multi-D PSF cube must survive the combination')
    assert(max(abs(Rst.PSFData.DataPSF(:,:,1) - MeanP), [], 'all') < TolS,...
           'stitchCrops: the leading slice of a PSF cube must combine as a plain 2D stamp does')
    assert(isequaln(Rst.PSFData.DataVar, MeanV),...
           'stitchCrops: the variance of a PSF cube must combine against its leading slice')

    % an unknown method is rejected
    try
        imProc.stack.stitchCrops(SI, 'PSFMethod','nonsense');
        error('stitchCrops: an unknown PSFMethod must raise an error')
    catch ME
        assert(contains(ME.message,'Unknown PSFMethod'), 'stitchCrops: unexpected error: %s', ME.message)
    end

    Result = true;


end