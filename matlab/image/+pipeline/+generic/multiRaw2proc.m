function [AllSI, Result] = multiRaw2proc(FilesList, Args)
    %
    % Example: L=io.files.filelist('LAST*science.fits');
    % [AllSI,Result]=pipeline.generic.multiRaw2proc(L(289:308),'CalibImages',CI);
    
    
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
    end
    
    if isa(FilesList, 'AstroImage')
        AI = FilesList;
    else
        AI = AstroImage(FilesList, Args.AstroImageReadArgs{:});
    end
        
    Nim = numel(AI);
    
    for Iim=1:1:Nim
        Iim
        if Iim==1 || ~Args.SameField
            % need to generate AstrometricCat for field
            tic;
            [SI, AstrometricCat, Result(Iim)] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                                      'CatName',Args.CatName,...
                                                                                      Args.singleRaw2procArgs{:});
            toc
            
        else
            tic;
            [SI, ~, Result(Iim)] = pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                         'CatName',AstrometricCat,...
                                                                         'WCS',AllSI(Iim-1,:),...
                                                                         Args.singleRaw2procArgs{:});
            toc
            
        end
       
        if Iim==1
            % alocate AstroImage for all sub images
            Nsub  = numel(SI);
            AllSI = AstroImage([Nim, Nsub]);
        end
        AllSI(Iim,:) = SI;
            
    end
    
    % merge catalogs
    [MergedCat, MatchedS, ResZP, ResVar, FitMotion] = imProc.match.mergeCatalogs(AllSI);
    
    % search for asteroids - proper motion channel
    [MergedCat, AstCrop] = imProc.asteroids.searchAsteroids_pmCat(MergedCat, 'BitDict',AllSI(1).MaskData.Dict, 'JD',JD, 'PM_Radius',3, 'Images',AllSI);
    
    % search for asteroids - orphan channel
    % imProc.asteroids.searchAsteroids_orphans
    
    % cross match with external catalogs
    
    % flag orphans
    
    
    % coadd images
    Nfields = numel(MatchedS);
    for Ifields=1:1:Nfields
        Summary(Ifields).ShiftX = median(diff(MatchedS(Ifields).Data.X,1,1), 2, 'omitnan');
        Summary(Ifields).ShiftY = median(diff(MatchedS(Ifields).Data.Y,1,1), 2, 'omitnan');
    
        ShiftXY = cumsum([0 0; -[Summary(Ifields).ShiftX, Summary(Ifields).ShiftY]]);
        RegisteredImages = imProc.transIm.imwarp(AllSI(:,Ifields), 'ShiftXY',ShiftXY, 'CreateNewObj',true);
        [Coadd, CoaddN] = imProc.stack.coadd(RegisteredImages);
        
    end
end

