function [AI, TableForDB] = prePrep(Images, Args)
    % pre-preparation of astronomical images (cast, quality checks)
    %     Optional steps include:
    %       Read images from local directory or get an AstroImage object.
    %       Apply CCDSEC to images. Default is full image.
    %       Cast image type to single.
    %       Flag and remove empty images.
    %       Flag and remove images with incorrect size.
    %       Estimate the global bcakground
    %       Check that there are not too many pixels with high level
    %       Check for image histogram anomalies.
    %       Check for large number of pixels with fixed value.
    %       Estimate the PSF using the ACF.
    %       Check for bad PSF.
    %       Add file name literals to image header.
    %       Add raw image ID to header.
    % Input  : - Images - Either an AstroImage or a cell array of images,
    %            or a char array with image template name.
    %          * ...,key,val,... 
    %            
    % Output : - 
    % Author : Eran Ofek (2025 Sep) 
    % Example: [AI, TFD]=pipeline.generic.prePrep(AI);

    arguments
        Images
   
        Args.AstroImageReadArgs          = {};
        Args.CCDSEC                      = [];
        %Args.BitDictionaryName           = 'BitMask.Image.Default.yml';

        Args.ImageClass                  = 'single';

        
        Args.LogObj                      = []; % if given write log.
        Args.TableForDB                  = true; % if given then update table with header + results.

        % quality checks
        Args.MinNim                      = 10;

        Args.CheckEmpty                  = true;

        Args.RequiredSizeXY              = [6422 9600];  % [X Y]

        Args.GlobalBackLevel             = true;
        Args.backgroundLevelArgs         = {}; %{'DiluteFactor',101, 'UseMex',true, 'MaxPixFraction',0.4, 'ThresholdBack',4000};

        Args.HistAnomaly                 = true;
        Args.histAnomalyArgs             = {};

        Args.BadVal                      = 32768;  % if empty do not check
        Args.MaxNBadVal                  = 1e4;   

        Args.GlobalBadPSF                = false;
        Args.MaxRadius                   = 50;
        Args.ACF_HalfSize                = [500 500];
        Args.CCDSEC2                     = [1 1000 1 1000];   % failure region
        Args.MaxFWHM                     = 5;
        Args.UseMex                      = true;

        Args.AddFileNameLiteralsToHeader = {'ProjName','FieldID'};
        Args.AddRawImageID               = true;
        Args.KeyRawID                    = 'ID_RAW';
        Args.ClassID                     = @uint64;

        
    end
    TableForDB = Args.TableForDB;
    Nim = NaN;

    try
        % try block
        % read Images
        if isa(Images, 'AstroImage')
            AI = Images;
            % crop if needed
            AI = AI.crop(Args.CCDSEC);
            % search file names in AstroImage
            Images = AI.getFileNames;
        else
            % assume input is a list of images
            AI = AstroImage(Images, Args.AstroImageReadArgs{:}, 'CCDSEC',Args.CCDSEC);
        end
        AI = AI(:);

        if ~isempty(Args.ImageClass)
            AI.cast(Args.ImageClass);
        end

        % allocate TableForDB
        Nim = numel(AI);
        
        % allocate TableForDB:
        TableForDB=allocateTableForDB(TableForDB, Nim, Args.ClassID);
       
        % Check for empty images
        if Args.CheckEmpty
            NotEmptyImage = ~AI.isemptyImage;
            if any(~NotEmptyImage)
                TableForDB.NotEmptyImage = NotEmptyImage;            
            end
            FlagGoodImages = NotEmptyImage;
        end
    
        % check for images with wrong size
        if ~isempty(Args.RequiredSizeXY)
            [Ny, Nx] = AI.sizeImage;
            TableForDB.Nx = Nx;
            TableForDB.Ny = Ny;
            FlagCorrectSize = Args.RequiredSizeXY(1)==Nx(:) & Args.RequiredSizeXY(2)==Ny(:);
    
            TableForDB.CorrectSize = FlagCorrectSize;
    
            FlagGoodImages = FlagGoodImages & FlagCorrectSize;
        end
    
        
        % global background
        if Args.GlobalBackLevel
            % need to call it in ImProc...
             [TableForDB.GoodGlobalBack, TableForDB.FracPixAboveThreshold, TableForDB.Median] = imProc.quality.backgroundLevel(AI, Args.backgroundLevelArgs{:});
             FlagGoodImages = FlagGoodImages & TableForDB.GoodGlobalBack;
        end
    
        % histogram anomaly
        if Args.HistAnomaly
            % need an imProc version...
            [TableForDB.HistOK(FlagGoodImages)] = ~imProc.quality.histAnomaly(AI, Args.histAnomalyArgs{:});
            FlagGoodImages = FlagGoodImages & TableForDB.HistOK;
            
        end
    
        % many pixels with the same value
        if ~isempty(Args.MaxNBadVal)
            for Iim=1:1:Nim
                TableForDB.NpixWithBadVal(Iim)   = sum(AI(Iim).ImageData.Image(:)==Args.BadVal);
            end
            TableForDB.NpixWithBadValOK = TableForDB.NpixWithBadVal<Args.MaxNBadVal;
            FlagGoodImages = FlagGoodImages & TableForDB.NpixWithBadValOK;
        end
           
    
        % PSF based on image ACF
        if Args.GlobalBadPSF
            % Crop image
            for Iim=1:1:Nim
                if NotEmptyImage(Iim)
                    BackSubImage = imUtil.cut.trim(AI(Iim).ImageData.Image, [Args.ACF_HalfSize, Args.ACF_HalfSize], false, [], Args.UseMex);
                    % subtract background
                    BackSubImage = BackSubImage - TableForDB.Median(Iim);
                
                    [FWHM_ACF,~,~,ACF] = imUtil.psf.fwhm_fromACF(BackSubImage, 'CCDSEC',[], 'MaxRadius',Args.MaxRadius);
                    if FWHM_ACF>Args.MaxFWHM
                        % run it again in a different CCDSEC
                        % this may be due to satellite streaks
                        BackSubImage = imUtil.cut.trim(AI(Iim).ImageData.Image, Args.CCDSEC2, true, [], Args.UseMex);
                        % subtract background
                        BackSubImage = BackSubImage - TableForDB.Median(Iim);
                        [FWHM_ACF,~,~,ACF] = imUtil.psf.fwhm_fromACF(Image, 'CCDSEC',[], 'MaxRadius',Args.MaxRadius);
                    end
        
                    TableForDB.ACF_FWHM(Iim)     = FWHM_ACF;
                    
                end
            end
            TableForDB.GoodACF_FWHM = TableForDB.ACF_FWHM<Args.MaxFWHM;
            FlagGoodImages = FlagGoodImages & TableForDB.GoodACF_FWHM;
        end
    
        % return selected images
        TableForDB.GoodImage = FlagGoodImages;
        AI = AI(FlagGoodImages);
        NimGood = numel(AI);
        % update header
    
        % add header keywords
        if ~isempty(Args.AddFileNameLiteralsToHeader)
            AFN = AstroFileName(Images);
            
            Nlit = numel(Args.AddFileNameLiteralsToHeader);
            for Ilit=1:1:Nlit
                for Iim=1:1:NimGood
                    AI(Iim).HeaderData.replaceVal(upper(Args.AddFileNameLiteralsToHeader{Ilit}), AFN.ProjName(Iim));
                end
            end
        end
    
    
        % add raw image ID
        if Args.AddRawImageID
            % populate LEVEL and CROPID
            AI = AI.setKeyVal('LEVEL','raw');
            AI = AI.setKeyVal('CROPID',0);
            [AI, ID] = imProc.db.generateImageID(AI, 'KeyID',Args.KeyRawID);
    
            TableForDB.RawID(FlagGoodImages) = ID;
        end
    
    
        % write log
        if ~isempty(Args.LogObj)
            Nim = numel(AI);
            Msg = sprintf('prePrep quality checks: %d out of %d images passed', NimGood, Nim);
            Obj.writeLog(Msg, LogLevel.Info);
        end
    catch ME
        % allocate TableForDB:
        TableForDB = allocateTableForDB(TableForDB, Nim, Args.ClassID);
        AI         = AstroImage;
        % write catch error:
        if ~isempty(Args.LogObj)
            Obj.writeLog(ME, LogLevel.Error);
        end
    end
end

% Aux functions:
function TableForDB=allocateTableForDB(TableForDB, Nim, ClassID)
    % allocate TableForDB

    if ~isempty(TableForDB)
                if islogical(TableForDB) && TableForDB
                    % create new TableForDB
                    TableForDB = struct('NotEmptyImage',false(Nim,1),...
                                        'CorrectSize',false(Nim,1),...
                                        'Nx',nan(Nim,1),...
                                        'Ny',nan(Nim,1),...
                                        'RawID',ClassID(nan(Nim,1)),...
                                        'GoodGlobalBack',false(Nim,1),...
                                        'FracPixAboveThreshold',nan(Nim,1),...
                                        'Median',nan(Nim,1),...
                                        'HistOK',false(Nim,1),...
                                        'NpixWithBadVal',nan(Nim,1),...
                                        'NpixWithBadValOK',false(Nim,1),...
                                        'ACF_FWHM',nan(Nim,1),...
                                        'GoodACF_FWHM',false(Nim,1),...
                                        'GoodImage',false(Nim,1));
                else
                    % Add columns:
                    TableForDB.NotEmptyImage         = false(Nim,1);
                    TableForDB.CorrectSize           = false(Nim,1);
                    TableForDB.Nx                    = nan(Nim,1);
                    TableForDB.Ny                    = nan(Nim,1);
                    TableForDB.RawID                 = nan(Nim,1);
                    TableForDB.GoodGlobalBack        = false(Nim,1);
                    TableForDB.FracPixAboveThreshold = nan(Nim,1);
                    TableForDB.Median                = nan(Nim,1);
                    TableForDB.HistOK                = false(Nim,1);
                    TableForDB.NpixWithBadVal        = nan(Nim,1);
                    TableForDB.NpixWithBadValOK      = false(Nim,1);
                    TableForDB.ACF_FWHM              = nan(Nim,1);
                    TableForDB.GoodACF_FWHM          = false(Nim,1);
                    TableForDB.GoodImage             = false(Nim,1);
                end
    end
end
