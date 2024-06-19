function [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd, RawHeader, OnlyMP] = multiRaw2procCoadd(FilesList, Args)
    % Basic processing of a multiple raw image, of the same field, into a processed images and coadd image
    %   Including:
    %       Reading the image
    %       Generate a mask image and mask saturated pixels
    %       Subtract bias/dark image
    %       Divide by flat image
    %       Remove Fringe image
    %       Break image to sub images
    %       Estimate background
    %       Basic source findinging
    %       Astrometry
    %       Update astrometry in catalog
    %       Photometric ZP
    %       Update photometric ZP in catalog
    %       Merge the catalogs
    %       Search for variability
    %       Search for proper motion
    %       Align images
    %       Coadd images
    %       Catalog of coadd image
    %       Add PSF-fit photometry to coadd catalog
    %       Match coadd catalog against external catalog
    % Input  : - List of images, of the smae field, to process.
    %            This can be a cell array of images, or a character array
    %            with regular expressions, or an AstroImage array.
    %          * ...,key,val,...
    %            'SubDir' - SubDir is an ImagePath class argument specifying
    %                   the sub directory in which to store the data products.
    %                   In case of a large number of data products it is
    %                   recomended that each sequence of images will have a
    %                   different SubDir. Default is ''.
    %            'BasePath' - Base path in which to store the data.
    %                   Default is '/last02w/data1/archive'.
    %
    
    %
    %            ---- Basic calibration arguments ---
    %            'CalibImages' - A CalibImages object containing the dark
    %                   and flat images. If empty, will attempt using the
    %                   'Dark','Flat','Fringe' arguments. DEfault is [].
    %            'Dark' - An AstroImage containing the dark image. This
    %                   argument is suprceeded by 'CalibImages'.
    %                   Default is [].
    %            'Flat' - An AstroImage containing the flat image. This
    %                   argument is suprceeded by 'CalibImages'.
    %                   Default is [].
    %            'Fringe' - An AstroImage containing the fringe image. This
    %                   argument is suprceeded by 'CalibImages'.
    %                   Default is [].
    %
    %            ---- Image partitioning arguments ---
    %            'CCDSEC' - A one line CCDSEC [Xmin Xmax Ymin Ymax] of section of
    %                   images on which to run the code on all the images.
    %                   Only these sections will be read into memory and
    %                   processed. If empty, read the tntire image.
    %                   Default is [].
    %            'SubImageSizeXY' - Approximate [X, Y] size of sub images.
    %                   The output images will have approximately this size.
    %                   Default is [1600 1600].
    %            'OverlapXY' - Approximate overlpa in [X,Y] between sub
    %                   images. Default is [64 64].
    %            --- Identify bad images arguments ---
    %            'IdentifyBadImagesCCDSEC' - Bad images udentification is
    %                   done in a small section of the full image defined by this
    %                   CCDSEC [Xmin Xmax Ymin Ymax].
    %                   Default is [3001 4000 3001 4000].
    %
    %            --- Reading images arguments ---            
    %            'AstroImageReadArgs' - A cell array of additional
    %                   arguments to pass to the AstroImage constructor when
    %                   reading images. Default is {}.
    %
    %            --- Photometry/Astrometry related ---
    %            'backgroundArgs' - A cell array of additional arguments to
    %                   pass to the imProc.background.background function.
    %                   Default is {}.
    %            'SameField' - A logical indicating if analyzing the same
    %                   field. Default is true.
    %            'CatName' - Astrometric and photometric catalog name.
    %                   Default is 'GAIAEDR3'.
    %            'CooOffset' - Approximate [RA Dec] offsets in deg of the image
    %                   center compared to the header RA/Dec.
    %                   Default is [0 0].
    %
    %            'singleRaw2procArgs' - A cell array of arguments to pass
    %                   to pipeline.generic.singleRaw2proc. Default is {}.
    %            'DeletePropAfterSrcFinding' - A cell array of AstroImage
    %                   property names to delete after the source finding is
    %                   completed. Default is {'Back','Var'}.
    %            'UpdateCounter' - A logical indicating if to add a keyword
    %                   name 'COUNTER' to the image header. This keyword
    %                   contains the image number in the sequence.
    %                   Default is true.
    %
    %            'coaddArgs' - A cell array of additional aruments to pass
    %                   to imProc.stack.codd.
    %                   Default is {'StackArgs',{'MeanFun',@mean, 'StdFun',@tools.math.stat.nanstd, 'Nsigma',[3 3], 'MaxIter',2}};
    %            'BackSubSizeXY' - Size [X,Y] in which background will be estimated.
    %                   Default is [128 128].
    %            'ZP' - Photomnetric ZP (mag for 1 electron), for raw photometry. Default is 25.
    %            'Threshold' - Sources detection threshold in units of S/N.
    %                   Default is 5.
    %            'ColCell' - A cell array of photometric properties that
    %                   imProc.sources.findMeasureSources will save.
    %                   Default is {'XPEAK','YPEAK',...
    %                                             'X1', 'Y1',...
    %                                             'X2','Y2','XY',...
    %                                             'SN','BACK_IM','VAR_IM',...  
    %                                             'BACK_ANNULUS', 'STD_ANNULUS', ...
    %                                             'FLUX_APER', 'FLUXERR_APER',...
    %                                             'MAG_APER', 'MAGERR_APER',...
    %                                             'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'}
    %            'findMeasureSourcesArgs' - A cell array of additional arguments to
    %                   pass to the source finding and measuring function
    %                   imProc.sources.findMeasureSources.
    %                   Default is {}.
    %            'photometricZPArgs' - A cell array of addotional arguments
    %                   to pass to the imProc.calib.photometricZP function.
    %                   Default is {}.
    %
    %            'Scale' - Pixel scale of image
%         
%         
%         % Astrometry
%         Args.Scale                            = 1.25;
%         Args.Tran                             = Tran2D('poly3');
%         Args.astrometryRefineArgs cell        = {};
%         
%         % Match against external catalog: 'MergedCat
%         Args.CoaddMatchMergedCat logical      = true;
%         Args.MergedMatchMergedCat logical     = true;
%         
%         Args.mergeCatalogsArgs cell           = {}; 
%         
%         Args.ReturnRegisteredAllSI logical    = true;  % use true if you want the return AllSI to be registered versions. If you don't care use true (should be faster/less mem)
%         
%         Args.StackMethod                      = 'sigmaclip';
%         Args.Asteroids_PM_MatchRadius         = 3;
%         Args.DeleteBackBeforeCoadd logical    = true;
%         Args.DeleteVarBeforeCoadd logical     = true;
%         
%         
%         
%         % save products
%         Args.SaveAll               = [];  % empty - check individuals
%         Args.SaveProcIm logical    = true;
%         Args.SaveProcMask logical  = true;
%         Args.SaveProcCat logical   = true;
%         Args.SaveMatchCat logical  = true;
%         Args.SaveMatchMat logical  = true;
%         Args.SaveCoaddIm logical   = true;
%         Args.SaveCoaddMask logical = true;
%         Args.SaveCoaddCat logical  = true;
%         Args.SaveAsteroids logical = true;
%         
        
    % Example: L=io.files.filelist('LAST*science.fits');
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(289:308),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(249:268),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(329:348),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(349:368),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(369:388),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(389:408),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(409:428),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(429:448),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(449:468),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(469:488),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(209:228),'CalibImages',CI);    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(489:508),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(509:528),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(129:148),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(189:208),'CalibImages',CI);
    
    
    % fails
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(169:188),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(58:77),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(529:548),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(549:568),'CalibImages',CI);
    
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(619:623),'CalibImages',CI);
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(624:627),'CalibImages',CI);
    
    
    
    % with bad images:
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2procCoadd(L(309:328),'CalibImages',CI);
    
    
    arguments
        FilesList                                               % Cell array, regexp, or AstroIamge
        Args.SubDir = NaN;  % NaN- autosubdir; '' no sub dir
        Args.BasePath = '/last02w/data1/archive'; %'/raid/eran/archive'; %'/euler/archive';

        Args.BitDictionaryName                = BitDictionary('BitMask.Image.Default')
        Args.KeySoftVer                       = 'PIPEVER';

        Args.CCDSEC                           = [];  % which CCDSEC to analuze - empty foe entire image  [xmin xmax ymin ymax]
        Args.CalibImages CalibImages          = [];
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.SubImageSizeXY                   = [1600 1600];  % empty - full image
        Args.OverlapXY                        = [64 64];
        
        Args.IdentifyBadImagesCCDSEC          = [3001 4000 3001 4000];
        
        Args.AstroImageReadArgs cell          = {};
        
        Args.SameField logical                = true;
        Args.CatName                          = 'GAIADR3'; %'GAIAEDR3';
        Args.CooOffset                        = [0 0];    % [deg]
        
        Args.singleRaw2procArgs cell          = {};
        Args.DeletePropAfterSrcFinding        = {'Back','Var'};
        %Args.DeleteBackBeforeCoadd logical    = true;
        %Args.DeleteVarBeforeCoadd logical     = true;

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
                                                 'MAG_APER', 'MAGERR_APER'};
                                                 %'FLUX_CONV', 'MAG_CONV', 'MAGERR_CONV'};
        Args.findMeasureSourcesArgs cell      = {};
        
        Args.photometricZPArgs cell           = {};
        
        % Astrometry
        Args.Scale                            = 1.25;
        Args.Tran                             = Tran2D('poly3'); %Tran2D('cheby1_3'); %Tran2D('poly3');
        %Args.astrometryCoreArgs cell          = {};
        Args.astrometrySubImagesArgs cell     = {};
        Args.astrometryRefineArgs cell        = {};
        Args.RefineSearchRadius               = 8;
        
        % Match against external catalog: 'MergedCat
        Args.CoaddMatchMergedCat logical      = true;  
        Args.MergedMatchMergedCat logical     = true; %false;  % issue 454
        
        Args.mergeCatalogsArgs cell           = {};
        
        Args.ReturnRegisteredAllSI logical    = true;  % use true if you want the return AllSI to be registered versions. If you don't care use true (should be faster/less mem)
        
        Args.StackMethod                      = 'sigmaclip';
        Args.Asteroids_PM_MatchRadius         = 3;
                
        Args.AddProjName2Header logical       = true;
        Args.AddFieldID2Header logical        = true;
        
        % PSF phot for single images
        Args.SingleImAddPSF logical                   = true;
        Args.SingleImconstructPSFArgs cell            = {'CropByQuantile',true,'Quantile',0.999}; % {}; % {'CropByQuantile',true,'Quantile',0.999};
        Args.SingleImPsfPhot logical                  = true;   
        % PSF phot for coadd images
        Args.CoaddImconstructPSFArgs                  = {'CropByQuantile',true,'Quantile',0.999}; % {}; % {'CropByQuantile',true,'Quantile',0.999};

        % save products
        Args.SaveAll               = [];  % empty - check individuals
        Args.SaveProcIm logical    = true;
        Args.SaveProcMask logical  = true;
        Args.SaveProcCat logical   = true;
        Args.SaveMatchCat logical  = true;
        Args.SaveMatchMat logical  = true;
        Args.SaveCoaddIm logical   = true;
        Args.SaveCoaddMask logical = true;
        Args.SaveCoaddCat logical  = true;
        Args.SaveCoaddPSF logical  = true;
        Args.SaveAsteroids logical = true;

        Args.SelectKnownAsteroid logical      = false;
        Args.GeoPos                           = [];
        Args.OrbEl                            = [];
        Args.INPOP                            = [];
        Args.AsteroidSearchRadius             = 10;
        
        Args.HostName              = [];

        Args.MaxFWHM               = 5;  % max of median(FWHM) - if larger stop processing
    end
    
    if ~isempty(Args.SaveAll)
        if Args.SaveAll
            Args.SaveProcIm     = true;
            Args.SaveProcMask   = true;
            Args.SaveProcCat    = true;
            Args.SaveMatchCat   = true;
            Args.SaveMatchMat   = true;
            Args.SaveCoaddIm    = true;
            Args.SaveCoaddMask  = true;
            Args.SaveCoaddCat   = true;
            Args.SaveCoaddPSF   = true;
            Args.SaveAsteroids  = true;
        else
            Args.SaveProcIm     = false;
            Args.SaveProcMask   = false;
            Args.SaveProcCat    = false;
            Args.SaveMatchCat   = false;
            Args.SaveMatchMat   = false;
            Args.SaveCoaddIm    = false;
            Args.SaveCoaddMask  = false;
            Args.SaveCoaddCat   = false;
            Args.SaveCoaddPSF   = false;
            Args.SaveAsteroids  = false;
        end
    end
    
    
    
    if isa(FilesList, 'AstroImage')
        % FileList is an AstroImage
        AI = FilesList;
%         [SizeY, SizeX] = AI(1).sizeImage;
    else
        % FileList is a cell array of images
                                                                                         
        AI = AstroImage(FilesList, Args.AstroImageReadArgs{:}, 'CCDSEC',Args.CCDSEC);
    end

    if ~isa(Args.BitDictionaryName, 'BitDictionary')
        Args.BitDictionaryName = BitDictionary(Args.BitDictionaryName);
    end


    % add ProjName to header
    if Args.AddProjName2Header
        Nfile = numel(AI);
        for Ifile=1:1:Nfile
            [~,FileNameStr] = fileparts(FilesList{Ifile});
            SplitStr = split(FileNameStr,'_');
            AI(Ifile).HeaderData.replaceVal('PROJNAME',SplitStr{1});
        end
    end
    
    % Add FieldID to header
    if Args.AddFieldID2Header
        Nfile = numel(AI);
        for Ifile=1:1:Nfile
            [~,FileNameStr] = fileparts(FilesList{Ifile});
            SplitStr = split(FileNameStr,'_');
            AI(Ifile).HeaderData.replaceVal('FIELDID',SplitStr{4});
        end
    end        
    
        
    % make sure images are in single format
    AI = AI.cast('single');

    RawHeader = astroImage2AstroHeader(AI, 'CreateNewObj',true);


    % search for bad images
    [Result,~] = imProc.stat.identifyBadImages(AI, 'CCDSEC',Args.IdentifyBadImagesCCDSEC);
    AI = AI(~[Result.BadImageFlag]);
        
    Nim = numel(AI);
    
    if Nim==0
        error('No good images found');
    end
    
    % update header with SoftVersion keyword
    VerString = tools.git.getVersion;
    AI.setKeyVal(Args.KeySoftVer,VerString);

    
    %Nsub = 24;
    %AllSI = AstroImage([Nim, Nsub]);
    AstrometricCat = [];
    for Iim=1:1:Nim
%           Iim
%         
%         if Iim==10
%             'a'
%         end
        
        if Iim==1 || ~Args.SameField || isempty(AstrometricCat)
            % need to generate AstrometricCat for field
            %tic;
            % ResultSingle(Iim) is not needed
            % AllSI(Iim,:),
            [SI, BadImageFlag, AstrometricCat] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                                      'CatName',Args.CatName,...
                                                                                      'BitDictionaryName',Args.BitDictionaryName,...
                                                                                      'CooOffset',Args.CooOffset,...
                                                                                      'DeletePropAfterSrcFinding',Args.DeletePropAfterSrcFinding,...
                                                                                      'Tran',Args.Tran,...
                                                                                      'astrometrySubImagesArgs',Args.astrometrySubImagesArgs,...
                                                                                      'astrometryRefineArgs',Args.astrometryRefineArgs,...
                                                                                      'RefineSearchRadius',Args.RefineSearchRadius,...
                                                                                      'RemoveBadImages',false,...
                                                                                      'findMeasureSourcesArgs',Args.findMeasureSourcesArgs,...
                                                                                      'ColCell',Args.ColCell,...
                                                                                      'Threshold',Args.Threshold,...
                                                                                      'AddPSF',Args.SingleImAddPSF,...
                                                                                      'constructPSFArgs',Args.SingleImconstructPSFArgs,...
                                                                                      'PsfPhot',Args.SingleImPsfPhot,...
                                                                                      Args.singleRaw2procArgs{:});
            %toc
            
        else
            %tic;
            [SI, BadImageFlag, ~] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                         'CatName',AstrometricCat,...
                                                                         'BitDictionaryName',Args.BitDictionaryName,...
                                                                         'CooOffset',Args.CooOffset,...
                                                                         'WCS',AllSI(Iim-1,:),...
                                                                         'DeletePropAfterSrcFinding',Args.DeletePropAfterSrcFinding,...
                                                                         'Tran',Args.Tran,...
                                                                         'astrometrySubImagesArgs',Args.astrometrySubImagesArgs,...
                                                                         'astrometryRefineArgs',Args.astrometryRefineArgs,...
                                                                         'RefineSearchRadius',Args.RefineSearchRadius,...
                                                                         'RemoveBadImages',false,...
                                                                         'findMeasureSourcesArgs',Args.findMeasureSourcesArgs,...
                                                                         'ColCell',Args.ColCell,...
                                                                         'Threshold',Args.Threshold,...
                                                                         'AddPSF',Args.SingleImAddPSF,...
                                                                         'constructPSFArgs',Args.SingleImconstructPSFArgs,...
                                                                         'PsfPhot',Args.SingleImPsfPhot,...
                                                                         Args.singleRaw2procArgs{:});
            %toc
            
        end
       
        % check image quality
        AllFWHM = AllSI.getStructKey('FWHM');
        if median([AllFWHM.FWHM])>Args.MaxFWHM
            error('Median FWHM in proc images is larger than threshold (%f)',median([AllFWHM.FWHM]));
        end

        io.msgLog(LogLevel.Info, '%s: multiRaw2procCoadd: image %d of %d processed: %s', ...
                  Args.HostName, Iim, Nim,AI(Iim).getStructKey('FILENAME').FILENAME);
        
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
    AllSI.deleteProp(Args.DeletePropAfterSrcFinding);
%     if Args.DeleteBackBeforeCoadd
%         AllSI.deleteProp('Back');
%     end
%     if Args.DeleteVarBeforeCoadd
%         AllSI.deleteProp('Var');
%     end
    
    
    % Save individual proc images
    AutoSubDir = false;
    if isnumeric(Args.SubDir)
        if isnan(Args.SubDir)
            AutoSubDir = true;
        else
            Args.SubDir = str2double(Args.SubDir);
        end
    end
    
    
    % DataProp = {'Image','Mask','Cat','PSF'};
    % IP = ImagePath.generateImagePathFromProduct(AllSI, 'PropFromHeader',true,...
    %                                                       'DataDirFromProjName',true,...
    %                                                       'CropID_FromInd',false,...
    %                                                       'AutoSubDir',AutoSubDir,...
    %                                                       'SetProp',{'Product','Image', 'SubDir',Args.SubDir, 'BasePath',Args.BasePath, 'DataDir',''});
    % ProjName = IP(1).ProjName;
    % 
    % Args.SubDir = IP(1).SubDir;                                                  
    % writeProduct(IP, AllSI, 'Save',Args.SaveProcIm || Args.SaveProcMask || Args.SaveProcCat,...
    %                         'SaveFields', DataProp([Args.SaveProcIm, Args.SaveProcMask, Args.SaveProcCat, false]));
    
    
    Args.ReturnRegisteredAllSI = false;
    % procMergeCoadd:
    % coadd the sub images of each field
    % generate a mask and a catalog for each coadd image
    % generate a PSF for each field.
    
    io.msgLog(LogLevel.Info, 'multiRaw2procCoadd: started coadding the sub images');
    
    [MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd, OnlyMP] = pipeline.generic.procMergeCoadd(AllSI,...
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
                                                                                             'constructPSFArgs',Args.CoaddImconstructPSFArgs,...
                                                                                             'photometricZPArgs',Args.photometricZPArgs,...
                                                                                             'ReturnRegisteredAllSI',Args.ReturnRegisteredAllSI,...
                                                                                             'StackMethod',Args.StackMethod,...
                                                                                             'Asteroids_PM_MatchRadius',Args.Asteroids_PM_MatchRadius,...
                                                                                             'DeleteBackBeforeCoadd',false,...
                                                                                             'DeleteVarBeforeCoadd',false,...
                                                                                             'SelectKnownAsteroid',Args.SelectKnownAsteroid,...
                                                                                             'GeoPos',Args.GeoPos,...
                                                                                             'OrbEl',Args.OrbEl,...
                                                                                             'INPOP',Args.INPOP,...
                                                                                             'AsteroidSearchRadius',Args.AsteroidSearchRadius,...
                                                                                             'HostName',Args.HostName);
       
                                                                                         
    % Add JD and CropID to Catalog
    Coadd = imProc.cat.insertCol(Coadd);

    % find orphans / streaks
%     [OrphansList,CleanOrphansList,Norphans] = lcUtil.findOrphansClean(MatchedS, 'BitDict',Coadd(1).MaskData.Dict, 'MaxNepochs',3);
%     % fit objects that appears on a line in the same epoch - possible streaks
%     % fit the rest
%     
%     Flag1 = [CleanOrphansList(21).Src.Ndet]==1;
%     
%     Data = [[CleanOrphansList(21).Src(Flag1).MeanRA].', [CleanOrphansList(21).Src(Flag1).MeanDec].'];
%     MeanJD = [CleanOrphansList(21).Src(Flag1).MeanJD];
%     Result = tools.math.fit.ransacLinear(Data, 'MinRMS',1./3600, 'ThresholdDist', 1./3600);
%     if Result.Npt > numel(unique(MeanJD))
%         % multiple apperances on a staright line in the same epoch
%         % likely a satellite streak
%     end
%     
%     % look for remaining orphans
%     FlagRealOrphans = ~Result.FlagGoodPt;
%     NnotStreak = numel(CleanOrphansList(21).Src(FlagRealOrphans));
% 
%     if NnotStreak>3
%         
%     end
    
 
    % Save individual coadd images
    % DataProp = {'Image','Mask','Cat','PSF'};
    % IP = ImagePath.generateImagePathFromProduct(Coadd, 'PropFromHeader',true,...
    %                                                       'DataDirFromProjName',false,...
    %                                                       'AutoSubDir',false,...
    %                                                       'CropID_FromInd',false,...
    %                                                       'SetProp',{'Product','Image', 'SubDir',Args.SubDir, 'BasePath',Args.BasePath, 'DataDir','', 'PathLevel','proc'});
    % IP.setAllVal('ProjName',ProjName);
    % IP.setAllVal('DataDir',ProjName);
    % 
    % 
    % FlagGood = ~isemptyImage(Coadd);
    % writeProduct(IP(FlagGood), Coadd(FlagGood), 'Save', Args.SaveCoaddIm || Args.SaveCoaddMask || Args.SaveCoaddCat || Args.SaveCoaddPSF,...
    %                         'SaveFields', DataProp([Args.SaveCoaddIm, Args.SaveCoaddMask, Args.SaveCoaddCat, Args.SaveCoaddPSF]));
    % 
    % % save MergedCat
    % writeProduct(IP(FlagGood), MergedCat(FlagGood), 'Save',Args.SaveMatchCat, 'Product','Cat', 'Level','merged', 'WriteFunArgs', {'FileType','fits'}, 'SaveFields',{'Cat'});
    % 
    % % Save MatchedS
    % writeProduct(IP(FlagGood), MatchedS(FlagGood), 'Save',Args.SaveMatchMat, 'Product','MergedMat', 'Level','merged', 'WriteFunArgs', {'FileType','hdf5'}, 'SaveFields',{'Cat'}, 'FileType','hdf5');
    % 
    % % save Asteroids MAT file
    % Igood = find(FlagGood, 1);
    % writeProduct(IP(Igood), ResultAsteroids, 'Save',Args.SaveAsteroids, 'Product','Asteroids', 'Level','proc');
    
end

