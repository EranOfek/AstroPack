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

    %% stitchCrops - crops with empty catalogs (issue #1279)
    % a CROP column tells which crop each row of the stitched catalog came from
    SIk = SI.copy;
    for Icr=1:1:Ncr
        Cat = SI(Icr).CatData.Catalog;
        SIk(Icr).CatData = AstroCatalog({[Cat, Icr.*ones(size(Cat,1),1)]},...
                                        'ColNames',[SI(Icr).CatData.ColNames, {'CROP'}]);
    end
    Rall = imProc.stack.stitchCrops(SIk);
    Crop = Rall.CatData.getCol('CROP');
    assert(isequal(unique(Crop).', 1:1:Ncr), 'stitchCrops: every crop must contribute to the reference stitch')

    % an empty catalog: no columns at all (a missing Cat product), or columns
    % but no rows, possibly fewer columns than the other crops have
    Empty0x0 = AstroCatalog;
    EmptyNar = AstroCatalog;
    EmptyNar.Catalog  = zeros(0,2);
    EmptyNar.ColNames = {'XPEAK','YPEAK'};
    for Iempty=[1 3]
        for EmptyCat={Empty0x0, EmptyNar}
            SIe = SIk.copy;
            SIe(Iempty).CatData = EmptyCat{1}.copy;
            Rst = imProc.stack.stitchCrops(SIe);
            assert(isequal(Rst.CatData.Catalog, Rall.CatData.Catalog(Crop~=Iempty,:)) && ...
                   isequal(Rst.CatData.ColNames, Rall.CatData.ColNames),...
                   'stitchCrops: an empty catalog in crop %d must only remove the rows of that crop', Iempty)
            assert(isequaln(Rst.Image, Rall.Image) && isequal(Rst.MaskData.Data, Rall.MaskData.Data),...
                   'stitchCrops: an empty catalog must not change the stitched image')
        end
    end

    % no crop has a source: the image is stitched, the catalog is empty but keeps
    % the columns, and the WCS is reported as failed without querying anything
    SIe = SIk.copy;
    for Icr=1:1:Ncr
        SIe(Icr).CatData.Catalog = SIe(Icr).CatData.Catalog([],:);
    end
    SIe(1).CatData = AstroCatalog;
    Rst = imProc.stack.stitchCrops(SIe, 'UpdateWCS',true, 'UpdateZP',true);
    assert(isempty(Rst.CatData.Catalog) && isequal(Rst.CatData.ColNames, SIk(2).CatData.ColNames),...
           'stitchCrops: with no sources the stitched catalog must be empty, with the columns of the crops')
    assert(~Rst.WCS.Success, 'stitchCrops: with no sources the WCS must be reported as failed')
    assert(isequaln(Rst.Image, Rall.Image), 'stitchCrops: with no sources the image must still be stitched')

    % a catalog with rows but without the pixel columns is an error
    SIe = SIk.copy;
    SIe(2).CatData = AstroCatalog({rand(3,2)}, 'ColNames',{'RA','Dec'});
    try
        imProc.stack.stitchCrops(SIe);
        error('stitchCrops: a catalog without pixel columns must raise an error')
    catch ME
        assert(strcmp(ME.identifier,'imProc:stack:stitchCrops:NoPixelColumns'),...
               'stitchCrops: unexpected error: %s', ME.message)
    end

    Result = true;


end