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
            [SI, AstrometricCat, Result(Iim)]=pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
                                                                                      'CatName',Args.CatName,...
                                                                                      Args.singleRaw2procArgs{:});
            toc
            
        else
            tic;
            [SI, ~, Result(Iim)]=pipeline.generic.singleRaw2proc(AI(Iim),'CalibImages',Args.CalibImages,...
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
    
    
end

