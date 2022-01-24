function [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd] = multiRaw2proc(FilesList, Args)
    % 
    % Example: L=io.files.filelist('LAST*science.fits');
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(289:308),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(249:268),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(329:348),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(349:368),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(369:388),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(389:408),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(409:428),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(429:448),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(449:468),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(469:488),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(209:228),'CalibImages',CI);    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(489:508),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(509:528),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(129:148),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(189:208),'CalibImages',CI);
    
    
    % fails
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(169:188),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(58:77),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(529:548),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(549:568),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(619:623),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(624:627),'CalibImages',CI);
    
    
    
    % with bad images:
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(309:328),'CalibImages',CI);
    
    
    arguments
        FilesList                                               % Cell array, regexp, or AstroIamge
        Args.CalibImages CalibImages          = [];
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.SubImageSizeXY                   = [1600 1600];  % empty - full image
        Args.OverlapXY                        = [64 64];
        
        Args.IdentifyBadImagesCCDSEC          = [3001 4000 3001 4000];
        
        Args.AstroImageReadArgs cell          = {};
        Args.ImageSizeXY                      = []; % if empty, get size from first image header
        
        Args.SameField logical                = true;
        Args.CatName                          = 'GAIAEDR3';
        
        Args.singleRaw2procArgs cell          = {};
        Args.DeletePropAfterSrcFinding        = {'Back','Var'};
        Args.UpdateCounter logical            = true;
        
        Args.coaddArgs cell                   = {'StackArgs',{'MeanFun',@mean, 'StdFun',@tools.math.stat.nanstd, 'Nsigma',[3 3], 'MaxIter',2}};
        
        % Background and source finding
        Args.backgroundArgs cell              = {};
        Args.BackSubSizeXY                    = [128 128];
        Args.ZP                               = 25;
        % source finding
        Args.Threshold                        = 5;
        Args.ColCell cell                     = {'XPEAK','YPEAK',...
                                                 'X1', 'Y1',...
                                                 'X2','Y2','XY',...
                                                 'SN','BACK_IM','VAR_IM',...  
                                                 'BACK_ANNULUS', 'STD_ANNULUS', ...
                                                 'FLUX_APER', 'FLUXERR_APER',...
                                                 'MAG_APER', 'MAGERR_APER',...
                                                 'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
        Args.findMeasureSourcesArgs cell      = {};
        
        Args.photometricZPArgs cell           = {};
        
        % Astrometry
        Args.Scale                            = 1.25;
        Args.Tran                             = Tran2D('poly3');
        Args.astrometryRefineArgs cell        = {};
        
        % Match against external catalog: 'MergedCat
        Args.CoaddMatchMergedCat logical      = true;
        Args.MergedMatchMergedCat logical     = true;
        
        Args.mergeCatalogsArgs cell           = {};
        
        Args.ReturnRegisteredAllSI logical    = true;  % use true if you want the return AllSI to be registered versions. If you don't care use true (should be faster/less mem)
        
        Args.StackMethod                      = 'sigmaclip';
        Args.Asteroids_PM_MatchRadius         = 3;
        Args.DeleteBackBeforeCoadd logical    = true;
        Args.DeleteVarBeforeCoadd logical     = true;
        
        
        Args.SubDir = '';  % no sub dir
    end
    
    
    
    
    if isa(FilesList, 'AstroImage')
        % FileList is an AstroImage
        AI = FilesList;
%         [SizeY, SizeX] = AI(1).sizeImage;
    else
        % FileList is a cell array of images
% tic;
%         OutN = FITS.get_keys(FilesList{1}, {'NAXIS1','NAXIS2'});
%         SizeX = real(str2doubleq(OutN{1}));
%         SizeY = real(str2doubleq(OutN{2}));
%     
%         [CCDSEC,UnCCDSEC,Center,Nxy,NewNoOverlap] = imUtil.cut.subimage_grid([SizeX, SizeY], 'SubSizeXY',[1600 1600],...
%                                                                                                'OverlapXY',[64 64]);
% 
%         Nccdsec = size(CCDSEC,1);
%         for Iim=1:1:numel(FilesList)
%             for Iccdsec=1:1:Nccdsec
%                 if Iccdsec==1
%                     ReadHeader = true;
%                 else
%                     ReadHeader = false;
%                 end
%                 AI(Iim,Iccdsec) = AstroImage(FilesList{Iim}, Args.AstroImageReadArgs{:}, 'CCDSEC',CCDSEC(Iccdsec,:),'ReadHeader',ReadHeader);
%                 if Iccdsec>1
%                     AI(Iim,Iccdsec).HeaderData = AI(Iim,1).HeaderData;
%                 end
%             end
%         end
% toc

                                                                                         
%         
%         
%         [CCDSEC,UnCCDSEC,Center,Nxy,NewNoOverlap] = imUtil.cut.subimage_grid([SizeX, SizeY], 'SubSizeXY',Args.SubImageSizeXY,...
%                                                                                                'OverlapXY',Args.OverlapXY);
%                                                                                          
%         
    
        %[SubImage,CCDSEC,Center,NooverlapCCDSEC,NewNoOverlap,Nxy] = partition_subimage(
        
%tic;
        AI = AstroImage(FilesList, Args.AstroImageReadArgs{:});
%toc

    end
        
    
    
    % search for bad images
    [Result,~] = imProc.stat.identifyBadImages(AI, 'CCDSEC',Args.IdentifyBadImagesCCDSEC);
    AI = AI(~[Result.BadImageFlag]);

    Nim = numel(AI);
    
    %Nsub = 24;
    %AllSI = AstroImage([Nim, Nsub]);
    AstrometricCat = [];
    for Iim=1:1:Nim
        %Iim
        
        if Iim==1 || ~Args.SameField || isempty(AstrometricCat)
            % need to generate AstrometricCat for field
            %tic;
            % ResultSingle(Iim) is not needed
            % AllSI(Iim,:),
            [SI, BadImageFlag, AstrometricCat] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                                      'CatName',Args.CatName,...
                                                                                      'DeletePropAfterSrcFinding',Args.DeletePropAfterSrcFinding,...
                                                                                      'RefineSearchRadius',10,...
                                                                                      'RemoveBadImages',false,...
                                                                                      'findMeasureSourcesArgs',Args.findMeasureSourcesArgs,...
                                                                                      'ColCell',Args.ColCell,...
                                                                                      'Threshold',Args.Threshold,...
                                                                                      Args.singleRaw2procArgs{:});
            %toc
            
        else
            %tic;
            [SI, BadImageFlag, ~] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                         'CatName',AstrometricCat,...
                                                                         'WCS',AllSI(Iim-1,:),...
                                                                         'DeletePropAfterSrcFinding',Args.DeletePropAfterSrcFinding,...
                                                                         'RefineSearchRadius',10,...
                                                                         'RemoveBadImages',false,...
                                                                         'findMeasureSourcesArgs',Args.findMeasureSourcesArgs,...
                                                                         'ColCell',Args.ColCell,...
                                                                         'Threshold',Args.Threshold,...
                                                                         Args.singleRaw2procArgs{:});
            %toc
            
        end
       
        if Iim==1
            % alocate AstroImage for all sub images
            Nsub  = numel(SI);
            AllSI = AstroImage([Nim, Nsub]);
        end
                
        if ~BadImageFlag
            AllSI(Iim,:) = SI;

            % clean data that will not be used later on
            % AllSI(Iim,:) = AllSI(Iim,:).deleteProp(Args.DeletePropAfterSrcFinding);

            % add keywords to Header
            if Args.UpdateCounter
                for Isub=1:1:Nsub
                    AllSI(Iim,Isub).HeaderData.replaceVal({'COUNTER'}, Iim);
                end
            end
        end
        
    end
    %clear AI;
    %clear SI;

    % delete Back and Var before coaddition
    if Args.DeleteBackBeforeCoadd
        AllSI.deleteProp('Back');
    end
    if Args.DeleteVarBeforeCoadd
        AllSI.deleteProp('Var');
    end
    
    Args.ReturnRegisteredAllSI = false;
    [MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd] = pipeline.generic.procMergeCoadd(AllSI,...
                                                                                             'mergeCatalogsArgs',Args.mergeCatalogsArgs,...
                                                                                             'MergedMatchMergedCat',Args.MergedMatchMergedCat,...
                                                                                             'CoaddMatchMergedCat',Args.CoaddMatchMergedCat,...
                                                                                             'coaddArgs',Args.coaddArgs,...
                                                                                             'backgroundArgs',Args.backgroundArgs,...
                                                                                             'BackSubSizeXY',Args.BackSubSizeXY,...
                                                                                             'findMeasureSourcesArgs',Args.findMeasureSourcesArgs,...
                                                                                             'ZP',Args.ZP,...
                                                                                             'ColCell',Args.ColCell,...
                                                                                             'Threshold',Args.Threshold,...
                                                                                             'astrometryRefineArgs',Args.astrometryRefineArgs,...
                                                                                             'Scale',Args.Scale,...
                                                                                             'Tran',Args.Tran,...
                                                                                             'CatName',Args.CatName,...
                                                                                             'photometricZPArgs',Args.photometricZPArgs,...
                                                                                             'ReturnRegisteredAllSI',Args.ReturnRegisteredAllSI,...
                                                                                             'StackMethod',Args.StackMethod,...
                                                                                             'Asteroids_PM_MatchRadius',Args.Asteroids_PM_MatchRadius,...
                                                                                             'DeleteBackBeforeCoadd',false,...
                                                                                             'DeleteVarBeforeCoadd',false);
                                                                                         
    
    
   
    


    
    % save products
    Args.SaveProcIm     = true;
    Args.SaveProcMask   = true;
    Args.SaveProcCat    = true;
    Args.SaveMatchCat   = true;
    Args.SaveMatchMat   = true;
    Args.SaveCoaddIm    = true;
    Args.SaveCoaddMask  = true;
    Args.SaveCoaddCat   = true;
    
    Args.SaveProcIm     = false;
    Args.SaveProcMask   = false;
    Args.SaveProcCat    = false;
    Args.SaveMatchCat   = false;
    Args.SaveMatchMat   = false;
    Args.SaveCoaddIm    = false;
    Args.SaveCoaddMask  = false;
    Args.SaveCoaddCat   = false;
    
    
    if isnumeric(Args.SubDir)
        Args.SubDir = str2double(Args.SubDir);
    end
    
    Args.SubDir = '9';
    BasePath='/euler/archive'; %'/last04/data1/archive';
    
    %%% MUST save AllSI before procMergeCoadd !!!
    
    %tic;
    % Save individual proc images
    IP   = ImagePath;
    Future = saveProduct(IP, AllSI, 'Save',Args.SaveProcIm,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Image',  'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Image', 'SubDir',Args.SubDir, 'BasePath',BasePath});


    % Save individual mask images
    IP   = ImagePath;
    Future = saveProduct(IP, AllSI, 'Save',Args.SaveProcMask,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Mask', 'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Mask', 'SubDir',Args.SubDir, 'BasePath',BasePath});
    
                       
    % Save individual catalog of images
    IP   = ImagePath;
    Future = saveProduct(IP, AllSI, 'Save',Args.SaveProcCat,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Cat', 'IsSimpleFITS',false, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Cat', 'SubDir',Args.SubDir, 'BasePath',BasePath});


    % Save MergedCat
    IP   = ImagePath;
    IP.readFromHeader(Coadd(1));
    MergedJD = 0.5.*(min(MatchedS(1).JD) + max(MatchedS(1).JD));
    IP.Time  = MergedJD;
    Future = saveProduct(IP, MergedCat, 'Save',Args.SaveMatchCat,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'FileType','fits', 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',false,...
                           'SetProp',{'Product','Cat', 'Level','merged', 'Counter',0, 'SubDir',Args.SubDir, 'BasePath',BasePath});

    % Save MatchedS  
    IP   = ImagePath;
    IP.readFromHeader(Coadd(1));
    MergedJD = 0.5.*(min(MatchedS(1).JD) + max(MatchedS(1).JD));
    IP.Time  = MergedJD;
    Future = saveProduct(IP, MergedCat, 'Save',Args.SaveMatchMat,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'FileType','hdf5'},...
                           'PropFromHeader',false,...
                           'SetProp',{'Product','MergedMat', 'Level','merged', 'Counter',0, 'SubDir',Args.SubDir, 'BasePath',BasePath});
                   
              
                       
    % Save Coadd images
    IP   = ImagePath;
    IP.Counter = 0;
    IP.PathLevel = 'proc';
    Future = saveProduct(IP, Coadd, 'Save',Args.SaveCoaddIm,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Image',  'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Image', 'SubDir',Args.SubDir, 'BasePath',BasePath});
                  
    % Save Coadd Mask images
    IP   = ImagePath;
    IP.Counter = 0;
    IP.PathLevel = 'proc';
    Future = saveProduct(IP, Coadd, 'Save',Args.SaveCoaddIm,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Mask',  'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Mask', 'SubDir',Args.SubDir, 'BasePath',BasePath});
                              
    % Save Coadd Cat   
    IP   = ImagePath;
    IP.Counter = 0;
    IP.PathLevel = 'proc';
    Future = saveProduct(IP, Coadd, 'Save',Args.SaveCoaddIm,...
                           'ParEval',false,...
                           'SaveFun',@write1,...
                           'SaveFunArgs',{'Cat',  'IsSimpleFITS',true, 'FileType','fits', 'WriteHeader',true, 'Append',false, 'OverWrite',true, 'WriteTime',false},...
                           'PropFromHeader',true,...
                           'SetProp',{'Product','Cat', 'SubDir',Args.SubDir, 'BasePath',BasePath});
                                                 
    
    %toc
    
    
    % for testing:
%     clear Coadd
%     clear ResultCoadd
%     clear RegisteredImages
%     clear MergedCat
%     clear MatchedS
%     clear ResultSubIm
%     clear AllSI
    
end

