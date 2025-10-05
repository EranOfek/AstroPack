function [AI, TableForDB, TableHeader] = prePrep(Images, Args)
    % pre-preparation of astronomical images (cast, quality checks)
    %     The operations are wrapped within try catch block.
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
    %       Add additional header keywords.
    %       Add file name literals to image header.
    %       Add git version to image header.
    %       Add raw image ID to header.
    % Input  : - Images - Either an AstroImage or a cell array of images,
    %            or a char array with image template name.
    %          * ...,key,val,... 
    %            'AstroImageReadArgs' - A cell array of arguments to pass
    %                   to the AstroImage reader. Default is {}.
    %            'CCDSEC' - CCDSEC for image to trim. If empty then no trim.
    %                   Default is [].
    %            'Convert2single' - Transform raw images to single class.
    %                   Default is true.
    %            'LogObj' - An optional MsgLogger object.
    %                   If non empty, then will write error and information
    %                   messages to the specified log file and standard
    %                   output.
    %                   Default is [].
    %            % ---------- Quality checks ----------
    %            'MinNim' - Minimum number of images required for success.
    %                   If fewer than this pass, AI is returned empty and
    %                   SelectedImages=false for all images. Default is 5.
    %            'CheckEmpty' - Logical indicating whether to flag empty images.
    %                   Non-empty images will have NotEmptyImage=true in
    %                   TableForDB. Default is true.
    %            'RequiredSizeXY' - If non-empty, required [X Y] image size.
    %                   Images with correct size will have CorrectSize=true
    %                   in TableForDB. Default is [6422 9600].
    %            'GlobalBackLevel' - If true, measure & check the global
    %                   background level (via imProc.quality.backgroundLevel).
    %                   Default is true.
    %            'backgroundLevelArgs' - Cell array of args forwarded to
    %                   imProc.quality.backgroundLevel (e.g.,
    %                   {'DiluteFactor',101,'UseMex',true,'MaxPixFraction',0.4,
    %                    'ThresholdBack',4000}). Default is {}.
    %            'HistAnomaly' - If true, check for histogram anomalies
    %                   (via imProc.quality.histAnomaly). Default is true.
    %            'histAnomalyArgs' - Cell array of args for histAnomaly.
    %                   Default is {}.
    %            'BadVal' - Pixel value considered “bad/fixed”. If empty,
    %                   do not check. Default is 32768.
    %            'MaxNBadVal' - Maximum allowed number of pixels equal to BadVal.
    %                   Images exceeding this are flagged. Default is 1e4.
    %            'GlobalBadPSF' - If true, estimate PSF via ACF and flag
    %                   images with too-large FWHM. Default is true.
    %            'MaxRadius' - Max radius (pixels) for ACF-based PSF measure.
    %                   Default is 50.
    %            'ACF_HalfSize' - Half-size [X Y] of the cutout used for ACF.
    %                   Default is [500 500].
    %            'CCDSEC2' - Alternate CCDSEC [X1 X2 Y1 Y2] for a second PSF
    %                   attempt if the first fails (e.g., streaks). Default is
    %                   [1 1000 1 1000].
    %            'MaxFWHM' - Maximum acceptable ACF-based FWHM (pixels).
    %                   Default is 5.
    %            'UseMex' - If true, use MEX-accelerated implementations where
    %                   available. Default is true.
    %            % ---------- Header updates & table ----------
    %            'AddFileNameLiteralsToHeader' - Cell array of literal names
    %                   (e.g., {'ProjName','FieldID'}) to inject from file
    %                   names into the FITS header. Default is {'ProjName','FieldID'}.
    %            'AddHeadKeys' - A two columns cell array of additional header kewyords
    %                   to head to the header {KeyName, KeyValue}.
    %                   Default is : {'FILTER','clear';...
    %                                        'TIMEZONE',2;...
    %                                        'CCDID',1;...
    %                                        'CROPID',0;...
    %                                        'LEVEL','raw';...
    %                                        'VERSION','1';...
    %                                        'SUBDIR','';...
    %                                        'LIGHTSEC','[1 6388 25 9600]';...
    %                                        'OVERSCAN','[6389 6422 1 9600]'};   % '[1 6354 1 9600]'};
    %            'AddGitVersion' - If true, add git version to image header.
    %                   Default is true.
    %            'KeySoftVer' - Git software version header keyword name.
    %                   Default is 'PIPEVER'.
    %            'AddRawImageID' - If true, generate and add a raw image ID
    %                   to the header (via imProc.db.generateImageID).
    %                   Default is true.
    %            'KeyRawID' - Header keyword name for the raw ID.
    %                   Default is 'ID_RAW'.
    %            'ClassID' - Function handle for ID numeric class (e.g., @uint64).
    %                   Default is @uint64.
    %            'Keys2table' - Cell array of header keys to export into the
    %                   TableHeader output. Default is
    %                   {'EXPMODE','FILTER','JD','GAIN','READNOI','CAMNAME','CAMTEMP','CAMCOOL','CAMMODE','CAMGAIN','GAMOFFS','DATE-OBS',...
    %                    'M_RA','M_DEC','M_HA','M_JRA','M_JDEC','M_JHA','AZ','ALT','AIRMASS','TRK_RA','TRK_DEC','MNTTEMP','FOCUS','PREVFOCUS'}.
    %            'TableForDB' - If true, return a table with image quality
    %                   status/flags (TableForDB). Default is true.
    %            
    % Output : - Updated AstroImage object.
    %            Contains only good images, updateds in the header,
    %            and optionally single precision format.
    %          - Table with quality status and flags per image in the
    %            original list. Images in the first output arguments were
    %            selected based on the SelectedImages column.
    %            GoodImages indicate that the image passed all the quality
    %            criteria (with the exception of Args.MinNim).
    %          - Optional table with the selected header keywords for all
    %            the images. The columns in this table corresponds to the
    %            header keyword names in the Args.Keys2table argument.
    % Author : Eran Ofek (2025 Sep) 
    % Example: [AI, TFD]=pipeline.generic.prePrep(AI);

    arguments
        Images
   
        Args.AstroImageReadArgs          = {};
        Args.CCDSEC                      = [];
        %Args.BitDictionaryName           = 'BitMask.Image.Default.yml';

        Args.Convert2single              = true;

        
        Args.LogObj                      = []; % if given write log.
        
        % quality checks
        Args.MinNim                      = 5;

        Args.CheckEmpty                  = true;

        Args.RequiredSizeXY              = [6422 9600];  % [X Y]

        Args.GlobalBackLevel             = true;
        Args.backgroundLevelArgs         = {}; %{'DiluteFactor',101, 'UseMex',true, 'MaxPixFraction',0.4, 'ThresholdBack',4000};

        Args.HistAnomaly                 = true;
        Args.histAnomalyArgs             = {};

        Args.BadVal                      = 32768;  % if empty do not check
        Args.MaxNBadVal                  = 1e4;   

        Args.GlobalBadPSF                = true;
        Args.MaxRadius                   = 50;
        Args.ACF_HalfSize                = [500 500];
        Args.CCDSEC2                     = [1 1000 1 1000];   % failure region
        Args.MaxFWHM                     = 5;
        Args.UseMex                      = true;

        Args.AddHeadKeys                 = {'FILTER','clear';...
                                            'TIMEZONE',2;...
                                            'CCDID',1;...
                                            'CROPID',0;...
                                            'LEVEL','raw';...
                                            'VERSION','1';...
                                            'SUBDIR','';...
                                            'LIGHTSEC','[1 6388 25 9600]';...
                                            'OVERSCAN','[6389 6422 1 9600]'};   % '[1 6354 1 9600]'};
        Args.AddFileNameLiteralsToHeader = {'ProjName','FieldID'};
        Args.AddGitVersion               = true;
        Args.KeySoftVer                  = 'PIPEVER';
        Args.AddRawImageID               = true;
        Args.KeyRawID                    = 'ID_RAW';
        Args.ClassID                     = @uint64;

        Args.Keys2table                  = {'EXPMODE','FILTER','JD','GAIN','READNOI','CAMNAME','CAMTEMP','CAMCOOL','CAMMODE','CAMGAIN','GAMOFFS','DATE-OBS',...
                                            'M_RA','M_DEC','M_HA',...
                                            'M_JRA','M_JDEC','M_JHA',...
                                            'AZ','ALT','AIRMASS','TRK_RA','TRK_DEC',...
                                            'MNTTEMP','FOCUS','PREVFOCUS'};
        Args.TableForDB                  = true; % if given then update table with header + results.

        
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
        Nim = numel(AI);

        if Args.Convert2single
            %AI.cast(Args.ImageClass);  % very slow

            for Iim=1:1:Nim
                AI(Iim).ImageData.Image = single(AI(Iim).ImageData.Image);
                %AI(Iim).Image = single(AI(Iim).Image);
            end
        end

        
        % allocate TableForDB:
        TableForDB=allocateTableForDB(TableForDB, Nim, Args.ClassID);
       
        % Check for empty images
        if Args.CheckEmpty
            NotEmptyImage = ~AI.isemptyImage;
            if any(~NotEmptyImage)
                TableForDB.NotEmptyImage = NotEmptyImage;            
            end
            TableForDB.NotEmptyImage = NotEmptyImage;
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
             [TableForDB.GoodGlobalBack, TableForDB.FracPixAboveThreshold, TableForDB.Median] = imProc.quality.backgroundLevel(AI, 'UseMex',Args.UseMex, Args.backgroundLevelArgs{:});
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
                    if isnan(TableForDB.Median(Iim))
                        BackImage = median(AI(Iim).ImageData.Image, 'all','omitnan');
                    else
                        BackImage = TableForDB.Median(Iim);
                    end
                    BackSubImage = imUtil.cut.trim(AI(Iim).ImageData.Image, [Args.ACF_HalfSize, Args.ACF_HalfSize], false, [], Args.UseMex);
                    % subtract background
                    BackSubImage = BackSubImage - BackImage;
                
                    
                    [FWHM_ACF,~,~,ACF] = imUtil.psf.fwhm_fromACF(BackSubImage, 'CCDSEC',[], 'MaxRadius',Args.MaxRadius, 'UseMex',Args.UseMex, 'Back',[]); %BackImage);
                    if FWHM_ACF>Args.MaxFWHM
                        % run it again in a different CCDSEC
                        % this may be due to satellite streaks
                        BackSubImage = imUtil.cut.trim(AI(Iim).ImageData.Image, Args.CCDSEC2, true, [], Args.UseMex);
                        % subtract background
                        BackSubImage = BackSubImage - TableForDB.Median(Iim);
                        [FWHM_ACF,~,~,ACF] = imUtil.psf.fwhm_fromACF(BackSubImage, 'CCDSEC',[], 'MaxRadius',Args.MaxRadius, 'UseMex',Args.UseMex, 'Back',[]); %BackImage);
                    end
        
                    TableForDB.ACF_FWHM(Iim)     = FWHM_ACF;
                    
                end
            end
            TableForDB.GoodACF_FWHM = TableForDB.ACF_FWHM<Args.MaxFWHM;
            FlagGoodImages = FlagGoodImages & TableForDB.GoodACF_FWHM;
        end
    
        % Populate GoodImages
        TableForDB.GoodImages = FlagGoodImages;
        % Populate EnoughImages (i.e., >=MinNim)
        TableForDB.SelectedImages = TableForDB.GoodImages & sum(FlagGoodImages)>=Args.MinNim;

        % return selected images
        AI = AI(TableForDB.SelectedImages);
        NimGood = numel(AI);
        % update header
    

        % UPDATE/fix header

        % add additional header keywords
        if ~isempty(Args.AddHeadKeys)
            AI.setKeyVal(Args.AddHeadKeys(:,1), Args.AddHeadKeys(:,2));
        end


        % add header keywords
        if ~isempty(Args.AddFileNameLiteralsToHeader)
            AFN = AstroFileName(Images);
            
            Nlit = numel(Args.AddFileNameLiteralsToHeader);
            for Ilit=1:1:Nlit
                for Iim=1:1:NimGood
                    AI(Iim).HeaderData.replaceVal(char(upper(Args.AddFileNameLiteralsToHeader{Ilit})), char(AFN.(Args.AddFileNameLiteralsToHeader{Ilit})(Iim)));
                end
            end
        end
    
        % update header with SoftVersion keyword
        if Args.AddGitVersion
            VerString = tools.git.getVersion;
            AI.setKeyVal(Args.KeySoftVer,VerString);
        end
        
        % add raw image ID
        if Args.AddRawImageID
            % populate LEVEL and CROPID
            AI = AI.setKeyVal('LEVEL','raw');
            AI = AI.setKeyVal('CROPID',0);
            [AI, ID] = imProc.db.generateImageID(AI, 'KeyID',Args.KeyRawID);
    
            TableForDB.RawID(FlagGoodImages) = ID;
        end
    
        TableForDB = struct2table(TableForDB);

        if nargout>2 && ~isempty(Args.Keys2table)
            TableHeader = imProc.header.headers2table(AI,'ColNameDic',Args.Keys2table);
            TableHeader.FileNames = string(Images(:));
        else
            TableHeader = [];
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
        else
            ME
            error('Failed on try catch');
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
                                'GoodImages',false(Nim,1),...
                                'SelectedImages',false(Nim,1));
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
            TableForDB.GoodImages            = false(Nim,1);
            TableForDB.SelectedImages        = false(Nim,1);
        end
    end
end
