function [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd] = multiRaw2proc(FilesList, Args)
    % 
    % Example: L=io.files.filelist('LAST*science.fits');
    % [AllSI, MergedCat, MatchedS, Coadd, ResultSubIm, ResultAsteroids, ResultCoadd]=pipeline.generic.multiRaw2proc(L(289:308),'CalibImages',CI);
    
    
    arguments
        FilesList                                               % Cell array, regexp, or AstroIamge
        Args.CalibImages CalibImages          = [];
        Args.Dark                             = []; % [] - do nothing
        Args.Flat                             = []; % [] - do nothing
        Args.Fringe                           = []; % [] - do nothing
        Args.BlockSize                        = [1600 1600];  % empty - full image
        
        Args.AstroImageReadArgs cell          = {};
        
        
        Args.SameField logical                = true;
        Args.CatName                          = 'GAIAEDR3';
        
        Args.singleRaw2procArgs cell          = {};
        Args.DeletePropAfterSrcFinding        = {'Back','Var'};
        Args.UpdateCounter logical            = true;
        
        Args.coaddArgs cell                   = {};
        
        % Background and source finding
        Args.backgroundArgs cell              = {};
        Args.BackSubSizeXY                    = [128 128];
        Args.ZP                               = 25;
        Args.findMeasureSourcesArgs cell      = {};
        Args.photometricZPArgs cell           = {};
        
        % Astrometry
        Args.Scale                            = 1.25;
        Args.Tran                             = Tran2D('poly3');
        Args.astrometryRefineArgs             = {};
        
        Args.ReturnRegisteredAllSI logical    = false;
        
    end
    
    if isa(FilesList, 'AstroImage')
        AI = FilesList;
    else
        AI = AstroImage(FilesList, Args.AstroImageReadArgs{:});
    end
        
    Nim = numel(AI);
    
    for Iim=1:1:Nim
        %Iim
        
        if Iim==1 || ~Args.SameField
            % need to generate AstrometricCat for field
            %tic;
            [SI, AstrometricCat, ResultSingle(Iim)] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                                      'CatName',Args.CatName,...
                                                                                      Args.singleRaw2procArgs{:});
            %toc
            
        else
            %tic;
            [SI, ~, ResultSingle(Iim)] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                         'CatName',AstrometricCat,...
                                                                         'WCS',AllSI(Iim-1,:),...
                                                                         Args.singleRaw2procArgs{:});
            %toc
            
        end
       
        if Iim==1
            % alocate AstroImage for all sub images
            Nsub  = numel(SI);
            AllSI = AstroImage([Nim, Nsub]);
        end
                
        AllSI(Iim,:) = SI;
        
        % clean data that will not be used later on
        AllSI(Iim,:) = AllSI(Iim,:).deleteProp(Args.DeletePropAfterSrcFinding);

        % add keywords to Header
        if Args.UpdateCounter
            for Isub=1:1:Nsub
                AllSI(Iim,Isub).HeaderData.replaceVal({'COUNTER'}, Iim);
            end
        end
        
    end
    clear SI;
    
    
    
    % get JD
    JD = julday(AllSI(:,1));
    
    % merge catalogs
    [MergedCat, MatchedS, ResultSubIm.ResZP, ResultSubIm.ResVar, ResultSubIm.FitMotion] = imProc.match.mergeCatalogs(AllSI);
    
    % search for asteroids - proper motion channel
    [MergedCat, ResultAsteroids.AstCrop] = imProc.asteroids.searchAsteroids_pmCat(MergedCat, 'BitDict',AllSI(1).MaskData.Dict, 'JD',JD, 'PM_Radius',3, 'Images',AllSI);
    
    % search for asteroids - orphan channel
    % imProc.asteroids.searchAsteroids_orphans
    
    % cross match with external catalogs
    
    % flag orphans
    
    
    % coadd images
    Nfields = numel(MatchedS);
    ResultCoadd = struct('ShiftX',cell(Nfields,1), 'ShiftY',cell(Nfields,1), 'CoaddN',cell(Nfields,1), 'AstrometricFit',cell(Nfields,1), 'ZP',cell(Nfields,1), 'PhotCat',cell(Nfields,1)); % ini ResultCoadd struct
    Coadd       = AstroImage([Nfields, 1]);  % ini Coadd AstroImage
    for Ifields=1:1:Nfields
        ResultCoadd(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.X,1,1), 2, 'omitnan');
        ResultCoadd(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.Y,1,1), 2, 'omitnan');
    
        ShiftXY = cumsum([0 0; -[ResultCoadd(Ifields).ShiftX, ResultCoadd(Ifields).ShiftY]]);
        % no need to transform WCS - as this will be dealt later on
        RegisteredImages = imProc.transIm.imwarp(AllSI(:,Ifields),...
                                                 'ShiftXY',ShiftXY,...
                                                 'TransWCS',false,...
                                                 'FillValues',0,...
                                                 'ReplaceNaN',true,...
                                                 'CreateNewObj',~Args.ReturnRegisteredAllSI);
        
        % use sigma clipping...
        % 1. NOTE that the mean image is returned so that the effective gain
        % is now Gain/Nimages
        % 2. RegisteredImages has no header so no JD...
        [Coadd(Ifields), ResultCoadd(Ifields).CoaddN] = imProc.stack.coadd(RegisteredImages, Args.coaddArgs{:},'StackMethod','sigmaclip');
        
        % Background
        Coadd(Ifields) = imProc.background.background(Coadd(Ifields), Args.backgroundArgs{:}, 'SubSizeXY',Args.BackSubSizeXY);
    
        % Source finding
        Coadd(Ifields) = imProc.sources.findMeasureSources(Coadd(Ifields), Args.findMeasureSourcesArgs{:},...
                                                   'RemoveBadSources',true,...
                                                   'ZP',Args.ZP,...
                                                   'CreateNewObj',false);
                                           
                                           
        % astrometry    
        MeanJD = mean(JD);
        [ResultCoadd(Ifields).AstrometricFit, Coadd(Ifields), AstrometricCat] = imProc.astrometry.astrometryRefine(Coadd(Ifields), Args.astrometryRefineArgs{:},...
                                                                                                'WCS',AllSI(1,Ifields).WCS,...
                                                                                                'EpochOut',MeanJD,...
                                                                                                'Scale',Args.Scale,...
                                                                                                'CatName',Args.CatName,...
                                                                                                'Tran',Args.Tran,...
                                                                                                'CreateNewObj',false);

        
        % photometric calibration
        [Coadd(Ifields), ResultCoadd(Ifields).ZP, ResultCoadd(Ifields).PhotCat] = imProc.calib.photometricZP(Coadd(Ifields),...
                                                                                                    'CreateNewObj',false,...
                                                                                                    'MagZP',Args.ZP,...
                                                                                                    'CatName',AstrometricCat,...
                                                                                                    Args.photometricZPArgs{:});
        
        
    end
    
    
    % save products
    Args.SaveProcIm     = false;
    Args.SaveProcMask   = false;
    Args.SaveProcCat    = false;
    Args.SaveMatchCat   = false;
    Args.SaveMatchSrc   = false;
    Args.SaveCoaddIm    = false;
    Args.SaveCoaddMask  = false;
    Args.SaveCoaddCat   = false;
    
    
    
    IP  = ImagePath;
    if Args.SaveProcIm   
        Nim = numel(AllSI);
        for Iim=1:1:Nim
            IP.readFromHeader(AllSI(Iim));  
            IP.Product = 'Image';
            
            % FFU: whos is responsible for creating the dir? ImagePath?
            % FFU: the date is today - BUG!!
            AllSI(Iim).write1(IP.genFull, 'Image', 'FileType','fits',...
                                                   'WriteHeader',true,...
                                                   'Append',false,...
                                                   'OverWrite',true,...
                                                   'WriteTime',false);
           
        end        
    end
    
    if Args.SaveProcMask
        Nim = numel(AllSI);
        for Iim=1:1:Nim
            IP.readFromHeader(AllSI(Iim));  
            IP.Product = 'Mask';
            % FFU: whos is responsible for creating the dir? ImagePath?
            % FFU: the date is today - BUG!!
            AllSI(Iim).write1(IP.genFull, 'Mask', 'FileType','fits',...
                                                   'WriteHeader',true,...
                                                   'Append',false,...
                                                   'OverWrite',true,...
                                                   'WriteTime',false);
        end
    end
    
    if Args.SaveProcCat
        Nim = numel(AllSI);
        for Iim=1:1:Nim
            IP.readFromHeader(AllSI(Iim));  
            IP.Product = 'Cat';
            AllSI(Iim).write1(IP.genFull, 'Cat', 'FileType','fits',...
                                                   'WriteHeader',false,...
                                                   'Append',false,...
                                                   'OverWrite',true,...
                                                   'WriteTime',false);
        end
    end
    
    
    if Args.SaveMatchCat
        
    end
    
    if Args.SaveMatchSrc
        
    end
    
    if Args.SaveCoaddIm
        tic;
        Nim = numel(Coadd);
        for Iim=1:1:Nim
            IP.readFromHeader(Coadd(Iim));  
            IP.Product = 'Image';
            IP.Counter = 0;
            
            
            % need to update ProjName, Time, TimeZone, Filter, Type, CCDID, CROPID, FieldID,...
            
            Coadd(Iim).write1(IP.genFull, 'Image', 'FileType','fits',...
                                                   'WriteHeader',true,...
                                                   'Append',false,...
                                                   'OverWrite',true,...
                                                   'WriteTime',false);
        end
        toc
        
        
    end
    
    if Args.SaveCoaddMask
        
    end
    
    if Args.SaveCoaddCat
        
    end
    
    
    
    
    % for testing:
%     clear Coadd
%     clear ResultCoadd
%     clear RegisteredImages
%     clear MergedCat
%     clear MatchedS
%     clear ResultSubIm
%     clear AllSI
    
end

