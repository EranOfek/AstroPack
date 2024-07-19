function [Status, AI, RawHeader] = readAndQualify(FilesList, Args)
    % Read a set of images into AstroImage, update their headers, and check quality
    %       This function performs the following steps:
    %       1. Read images into AstroImage object.
    %       2. Update the image headers.
    %       3. Convert the images to single precision.
    %       4. Add Software version to header.
    %       5. Check image size.
    %       6. Check image quality using imProc.stat.identifyBadImages
    %       
    % Input  : - A cell array of files list to upload, or an AstroImage
    %            object with iamges.
    %          * ...,key,val,... 
    %            'ConfigFile'
    %            'AstroImageReadArgs' - A cell array of optional arguments
    %                   to pass to the AstroImage constructor. Default is {}.
    %            'CCDSEC' - A CCDSEC array of the region to read. If empty,
    %                   read entire image. Default is [].
    %            'identifyBadImagesArgs' - A cell array of additional
    %                   parameters to pass to imProc.stat.identifyBadImages
    %                   Default is {}.
    %            'KeyProjName' - A keyword header name for the project name.
    %                   If not empty, then read project name from file name
    %                   (see FileNames convention) and add it to the header.
    %                   Default is 'PROJNAME'.
    %            'KeyFieldID' - Like KeyProjName, but for the FieldID.
    %                   Default is 'FIELDID'.
    %            'Convert2single' - A logical indicating if to cast the
    %                   images to single precsion. Default is true.
    %            'KeySoftVer' - An haeder keyword in which to store the
    %                   pipeline version.
    %                   Default is 'PIPEVER'.
    %            'ImageSizeXY' - Required image size. If [X Y] image size
    %                   is not equal to this image then the image will be declared
    %                   as bad.
    %                   Default is [6422 9600].
    %            'MinNumGoodImages' - Minimum number of good images. If number
    %                   of good images is smaller than this number, then the
    %                   Status.Status output will be set to false.
    % Output : - A ststus structure with the following fields:
    %            .Status - A logical indicating if there are enough good
    %                   images to continue.
    %            .Msg - A cell array of messages with information about bad
    %                   images.
    %          - An AstroImage object with the good (only) images.
    %          - An AstroHeader object with all the good images headers.
    % Author : Eran Ofek (2024 Jul) 
    % Example: [Status, AI]=pipeline.generic.readAndQualify(FilesList)

    arguments
        FilesList
        Args.ConfigFile               = 'LAST.Pipeline.readAndQualify';
        Args.AstroImageReadArgs       = {};
        Args.CCDSEC                   = [];
        Args.identifyBadImagesArgs    = {};

        Args.KeyProjName              = 'PROJNAME';  % If not empty, add project name from file name to header, only for filelists input
        Args.KeyFieldID               = 'FIELDID';

        %Args.BitDictionaryName        = BitDictionary('BitMask.Image.Default');   % BitDictionary or its name

        Args.Convert2single logical   = true;
        Args.KeySoftVer               = 'PIPEVER';

        Args.ImageSizeXY              = [6422 9600];
        Args.MinNumGoodImages         = 10;

    end

    if isa(FilesList, 'AstroImage')
        AI = FilesList;
        FilesList = {AI.FileName};
    else
        % Read FileNames
        AI = AstroImage(FilesList, Args.AstroImageReadArgs{:}, 'CCDSEC',Args.CCDSEC);        
    end       


    % add ProjName to header
    if ~isempty(Args.KeyProjName)
        Nfile = numel(AI);
        for Ifile=1:1:Nfile
            [~,FileNameStr] = fileparts(FilesList{Ifile});
            SplitStr = split(FileNameStr,'_');
            AI(Ifile).HeaderData.replaceVal(Args.KeyProjName, SplitStr{1});
        end
    end
    
    % Add FieldID to header
    if ~isempty(Args.KeyFieldID)
        Nfile = numel(AI);
        for Ifile=1:1:Nfile
            [~,FileNameStr] = fileparts(FilesList{Ifile});
            SplitStr = split(FileNameStr,'_');
            AI(Ifile).HeaderData.replaceVal(Args.KeyFieldID, SplitStr{4});
        end
    end

    % update header with SoftVersion keyword
    VerString = tools.git.getVersion;
    AI.setKeyVal(Args.KeySoftVer,VerString);



    %if ~isa(Args.BitDictionaryName, 'BitDictionary')
    %    Args.BitDictionaryName = BitDictionary(Args.BitDictionaryName);
    %end

    Nim = numel(AI);

    if Args.Convert2single
        AI = AI.cast('single');
    end

    
    % check image size
    if ~isempty(Args.ImageSizeXY)
        [SizeY, SizeN] = AI.sizeImage;
        Flag.Size = all([SizeX, SizeY]==Args.ImageSizeXY, 2);
    else
        Flag.Size = true(Nim,1);
    end


    % search for bad images
    [StatBadIm,~] = imProc.stat.identifyBadImages(AI, 'CCDSEC',Args.IdentifyBadImagesCCDSEC, Args.identifyBadImagesArgs{:});
    Flag.Good = ~[StatBadIm.BadImageFlag];

    FlagBad = ~Flag.Size(:) & ~Flag.Good(:);
    if sum(FlagBad)>0
        % Bad image was identified
        Ibad = find(FlagBad);
        Nbad = numel(Ibad);
        for Ib=1:1:Nbad
            Flag.Size(Ibad(Ib))
            Status.Msg{Ib} = sprintf('readAndQualify: Image rejected (Bad=%d, Size=%d) / %s  ',Flag.Good(Ibad(Ib)), Flag.Size(Ibad(Ib)), FilesList{Ibad(Ib)});
        end

    end


    if sum(~FlagBad)>=Args.MinNumGoodImages
        Status.Status = true;
        AI = AI(~FlagBad);
    else
        AI = [];
        Status.Status = false;
        Status.Msg{end+1} = sprintf('readAndQualify: Not enough good images');
    end

    if nargout>2
        RawHeader = astroImage2AstroHeader(AI, 'CreateNewObj',true);
    end
end
