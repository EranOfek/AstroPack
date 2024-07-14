function [Status, AI, RawHeader] = readAndQualify(FilesList, Y, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Jul) 
    % Example: 

    arguments
        FilesList
        Args.ConfigFile               = 'LAST.Pipeline.readAndQualify';
        Args.AstroImageReadArgs       = {};
        Args.CCDSEC                   = [];

        Args.KeyProjName              = 'PROJNAME';  % If not empty, add project name from file name to header, only for filelists input
        Args.KeyFieldID               = 'FIELDID';

        Args.BitDictionaryName        = BitDictionary('BitMask.Image.Default');   % BitDictionary or its name

        Args.Convert2single logical   = true;

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


    if ~isa(Args.BitDictionaryName, 'BitDictionary')
        Args.BitDictionaryName = BitDictionary(Args.BitDictionaryName);
    end

    Nim = numel(AI);

    if Args.Convert2single
        AI = AI.cast('single');
    end

    if nargout>1
        RawHeader = astroImage2AstroHeader(AI, 'CreateNewObj',true);
    end

    % check image size
    if ~isempty(Args.ImageSizeXY)
        [SizeY, SizeN] = AI.sizeImage;
        Flag.Size = all([SizeX, SizeY]==Args.ImageSizeXY, 2);
    else
        Flag.Size = true(Nim,1);
    end

    % search for bad images
    [StatBadIm,~] = imProc.stat.identifyBadImages(AI, 'CCDSEC',Args.IdentifyBadImagesCCDSEC);
    Flag.Good = ~[StatBadIm.BadImageFlag];

    FlagBad = ~Flag.Size(:) & ~Flag.Good(:);
    if sum(FlagBad)>0
        % Bad image was identified
        Ibad = find(FlagBad);
        Nbad = numel(Ibad);
        for Ib=1:1:Nbad
            Flag.Size(Ibad(Ib))
            Status.Msg{Ib} = sprintf('Bad image File: %s  ',  FilesList{Ibad(Ib)});
        end

    end


    if sum(Flag.Good(:) & Flag.Size(:))>=Args.MinNumGoodImages
        ...


    AI = AI(~[Result.BadImageFlag]);
        
    if Nim==0
        error('No good images found');
    end
    
    % update header with SoftVersion keyword
    VerString = tools.git.getVersion;
    AI.setKeyVal(Args.KeySoftVer,VerString);

    


end
