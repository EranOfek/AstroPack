function [FN,SubDir]=writeProduct(Obj, FNin, Args)
    % Write AstroImage/AstroCatalog/MergedSources data products using FileNames object
    %   The file names are generated using the FileNames object
    % Input  : - An AstroImage object.
    %          - A FileNames object. The numbre of files should
    %            corresponds to the number of elements in the
    %            AstroImage object.
    %          * ...,key,val,...
    %            'Save' - A logical indicating if to save data products.
    %                   Default is true.
    %            'IsSimpleFITS' - Default is true.
    %            'Type' - FileNames type. If empty, then use the
    %                   Type provided in the FileNames object.
    %                   Default is [].
    %            'Level' - Like 'Type', but for 'Level'.
    %                   Default is [].
    %            'LevelPath' -  Seperate Level for path.
    %                   If numeric empty, then use 'Level' argument.
    %                   Default is [].
    %            'Product' - A cell array of product names to
    %                   write. Default is {'Image','Mask','Cat','PSF'}
    %            'WriteHeader' - A logical, or a vector of logicals
    %                   corresponding to each product and
    %                   indicating if to write an header for this
    %                   product. Default is [true, false, true, false]
    %            'DateFromHeader' - A logical indicating if to get
    %                   the Time from the header. Default is true.
    %            'Counter' - Like 'Type', but for 'Counter'.
    %                   Default is [].
    %            'CCDID' - Like 'Type', but for 'CCDID'.
    %                   Default is [].
    %            'CropID' - Like 'Type', but for 'CropID'.
    %                   Default is [].
    %            'FindSubDir' - A logical indicating if to find the SubDir
    %                   automatically using the nextSubDir function.
    %                   Default is false.
    %            'SubDir' - Like 'Type', but for 'SubDir'.
    %                   Default is [].
    %            'Version' - Like 'Type', but for 'Version'.
    %                   Default is [].
    %            'FileType' - Like 'Type', but for 'FileType'.
    %                   Default is [].
    %            'GetHeaderJD' - Update JD from header. Default is true.
    %            'GetHeaderCropID' - Update CropID from header.
    %                   Default is true.
    %            'GetHeaderCounter' - Update Counter from header.
    %                   Default is true.
    %            'KeyCropID' - Header keyword containing the
    %                   CropID. Default is 'CROPID'.
    %            'KeyCounter' - Header keyword containing the
    %                   Counter. Default is 'COUNTER'.
    %            'BasePath' - Like 'Type', but for 'BasePath'.
    %                   Default is [].
    %            'FullPath' - Like 'Type', but for 'FullPath'.
    %                   Default is [].
    % Output : - A FileNames object for the written files
    %            (Product='Image').
    %          - Used SubDir.
    % Author : Eran Ofek (Apr 2023)

    arguments
        Obj
        FNin FileNames
        Args.Save logical           = true;
        Args.IsSimpleFITS logical   = true;
        Args.Type                   = [];
        Args.Level                  = [];
        Args.LevelPath              = [];
        Args.Product cell           = {'Image','Mask','Cat','PSF'};
        Args.WriteHeader            = [true, false, true, false];

        Args.DateFromHeader logical = true;
        Args.Counter                = [];
        Args.CCDID                  = [];
        Args.CropID                 = [];  % ...
        Args.FindSubDir logical     = false;
        Args.SubDir                 = [];
        Args.Version                = [];
        Args.FileType               = [];

        Args.GetHeaderJD logical       = true;
        Args.GetHeaderCropID logical   = true;
        Args.GetHeaderCounter logical  = true;
        Args.KeyCropID                 = 'CROPID';
        Args.KeyCounter                = 'COUNTER';


        Args.BasePath               = [];
        Args.FullPath               = [];
    end

    if Args.Save
        % Save data products
        Nprod = numel(Args.Product);
        if numel(Args.WriteHeader)==1
            WriteHeader = repmat(Args.WriteHeader, Nprod,1);
        else
            WriteHeader = Args.WriteHeader;
            if numel(WriteHeader)~=Nprod
                error('Number of elements in WriteHeader must be 1 or equal to the number of products');
            end
        end
        Nobj  = numel(Obj);
        

        % generate a copy of FNin, so the object will not be modified
        FN = FNin.copy;


        % get SubDir auomatically
        if Args.FindSubDir
            [SubDir, FN] = FN.nextSubDir;
        else
            SubDir = FN.SubDir;
        end

        FN = FN.updateIfNotEmpty('Type',Args.Type,...
                                 'Level',Args.Level,...
                                 'Counter',Args.Counter,...
                                 'CCDID',Args.CCDID,...
                                 'CropID',Args.CropID,...
                                 'SubDir',Args.SubDir,...
                                 'Version',Args.Version,...
                                 'FileType',Args.FileType,...
                                 'BasePath',Args.BasePath,...
                                 'FullPath',Args.FullPath);

        % get CropID/Counter/Time from header
         
        if Args.GetHeaderJD || Args.GetHeaderCropID || Args.GetHeaderCounter
            FN = updateForAstroImage(FN, Obj,...
                                     'GetHeaderJD',Args.GetHeaderJD,...
                                     'GetHeaderCropID',Args.GetHeaderCropID,...
                                     'GetHeaderCounter',Args.GetHeaderCounter,...
                                     'SelectFirst',true,...
                                     'CreateNewObj',true);
        end

        Nfn   = FN.nfiles;
        if Nobj~=Nfn
            error('Number of elements in AstroImage and FileNames object must be identical');
        end

        % loop for writing the products
        switch class(Obj)
            case 'AstroImage'
                % AstroImage input

                for Iprod=1:1:Nprod
                    % generate file names
                    OutFileNames = FN.genFull('Product',Args.Product{Iprod}, 'LevelPath',Args.LevelPath);
                    for Iobj=1:1:Nobj
                            % create dir only on first file
                            Obj(Iobj).write1(OutFileNames{Iobj}, Args.Product{Iprod},...
                                             'FileType',FN.FileType{1},...
                                             'IsSimpleFITS',Args.IsSimpleFITS,...
                                             'WriteHeader',WriteHeader(Iprod),...
                                             'MkDir',Iobj==1);
                    end
                end
            case 'AstroCatalog'
                % AstroCatalog input
                for Iobj=1:1:Nobj
                    Obj(Iobj).write1(OutFileNames{Iobj},...
                                     'FileType',FN.FileType{1});
                end

            case 'MatchedSources'
                % MatchedSources input
                FN.Level = 'merged';
                for Iobj=1:1:Nobj
                    Obj(Iobj).write1(OutFileNames{Iobj},...
                                     'FileType',FN.FileType{1});
                end

            otherwise
                error('1st input class %s is not supported',class(Obj));
        end
    end  % if Args.Save
end
