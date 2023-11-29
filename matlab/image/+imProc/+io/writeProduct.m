function [FN,SubDir,Status]=writeProduct(Obj, FNin, Args)
    % Write AstroImage/AstroCatalog/MergedSources data products using FileNames object
    %   The file names are generated using the FileNames object.
    %   For input AstroImage the FileName in the ImageData property is
    %   updated with the given file name.
    % Input  : - An AstroImage/AstroCatalog/MatchedSources/struct object.
    %            A MatchedSources will be written as .hdf5 file.
    %            A struct will be saved as a .mat file.
    %          - A FileNames object. The number of files should
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
    %                   write. If one of the products is empty, then skip
    %                   this product.
    %                   Default is {'Image','Mask','Cat','PSF'}
    %            'WriteHeader' - A logical, or a vector of logicals
    %                   corresponding to each product and
    %                   indicating if to write an header for this
    %                   product. Default is [true, false, true, false]
    %            'OverWrite' - logical, true if existing files are to
    %                   be overwritten.
    %                   Default is false.
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
    %            'CropID_FromIndex' - For non AstroImage inputs,
    %                   update CropID from object element index.
    %                   Default is true.
    %            'AI_Counter_FromHeader' - Update Counter from AstroImage header.
    %                   Default is true.
    %            'Counter_Zero' - For non AstroImage inputs,
    %                   update Counter to 0.
    %                   Default is true.
    %            'KeyCropID' - Header keyword containing the
    %                   CropID. Default is 'CROPID'.
    %            'KeyCounter' - Header keyword containing the
    %                   Counter. Default is 'COUNTER'.
    %            'BasePath' - Like 'Type', but for 'BasePath'.
    %                   Default is [].
    %            'FullPath' - Like 'Type', but for 'FullPath'.
    %                   Default is [].
    %            'WriteEmpty' - Logical indicating if to write an empty
    %                   product. Default is false.
    % Output : - A FileNames object for the written files
    %            (Product='Image').
    %          - Used SubDir.
    %          - Status structure, with status per problem.
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
        Args.OverWrite              = false;

        Args.DateFromHeader logical = true;
        Args.Counter                = [];
        Args.CCDID                  = [];
        Args.CropID                 = [];  % ...
        Args.FindSubDir logical     = false;
        Args.SubDir                 = [];
        Args.Version                = [];
        Args.FileType               = [];

        Args.GetHeaderJD logical       = true;
        Args.AI_CropID_FromHeader logical  = true;
        Args.CropID_FromIndex logical      = true;
        Args.AI_Counter_FromHeader logical = true;
        Args.Counter_Zero logical          = true;

        Args.KeyCropID                 = 'CROPID';
        Args.KeyCounter                = 'COUNTER';


        Args.BasePath               = [];
        Args.FullPath               = [];
        Args.WriteEmpty logical     = false;
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
         
        switch class(Obj)
            case {'AstroImage','AstroCatalog','MatchedSources'}
                if Args.GetHeaderJD || Args.AI_CropID_FromHeader || Args.AI_Counter_FromHeader
                    FN = updateFromObjectInfo(FN, Obj,...
                                             'GetHeaderJD',Args.GetHeaderJD,...
                                             'AI_CropID_FromHeader',Args.AI_CropID_FromHeader,...
                                             'CropID_FromIndex',Args.CropID_FromIndex,...
                                             'AI_Counter_FromHeader',Args.AI_Counter_FromHeader,...
                                             'Counter_Zero',Args.Counter_Zero,...
                                             'SelectFirst',true,...
                                             'CreateNewObj',true);
                end
        end

        Nfn   = FN.nfiles;
        if Nobj~=Nfn
            error('Number of elements in AstroImage and FileNames object must be identical');
        end

        % get SubDir auomatically
        if Args.FindSubDir
            [SubDir, FN] = FN.nextSubDir;
        else
            SubDir = FN.SubDir;
        end


        % loop for writing the products
        
        Status = [];
        Istat  = 0;
        DirCreated = false;
        switch class(Obj)
            case 'AstroImage'
                % AstroImage input

                for Iprod=1:1:Nprod
                    
                    if ~isempty(Args.Product{Iprod})

                        % generate file names
                        %FN_Iobj = FN.reorderEntries(Iobj, 'CreateNewObj',true);
                        OutFileNames  = FN.genFull('Product',Args.Product{Iprod}, 'LevelPath',Args.LevelPath);
                        FlagGoodTimes = FN.validTimes;
                        for Iobj=1:1:Nobj
                            % create dir only on first file
                            
                            if (FlagGoodTimes(Iobj) && ~isempty(Obj(Iobj).Image)) || Args.WriteEmpty
                                
                                Obj(Iobj).write1(OutFileNames{Iobj}, Args.Product{Iprod},...
                                             'FileType',FN.FileType{1},...
                                             'IsSimpleFITS',Args.IsSimpleFITS,...
                                             'WriteHeader',WriteHeader(Iprod),...
                                             'MkDir',~DirCreated,...
                                             'OverWrite',Args.OverWrite);
                                DirCreated = true;
                                % Update FileName in Obj
                                Obj(Iobj).ImageData.FileName = OutFileNames{Iobj};
                            else
                                Istat = Istat + 1;
                                Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, image is empty - not saved', OutFileNames{Iobj}, Args.Product{Iprod});
                            end
                        end
                    end
                end
            case 'AstroCatalog'
                % AstroCatalog input
                %OutFileNames = FN.genFull('Product','Cat', 'LevelPath',Args.LevelPath);
                OutFileNames = FN.genFull('Product',Args.Product{1}, 'LevelPath',Args.LevelPath);
                for Iobj=1:1:Nobj
                    if ~isempty(Obj(Iobj).ColNames) || Args.WriteEmpty
                        Obj(Iobj).write1(OutFileNames{Iobj},...
                                     'FileType',FN.FileType{1});
                    else
                        Istat = Istat + 1;
                        Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, image is empty - not saved', OutFileNames{Iobj}, 'CatData');
                    end
                end

            case 'MatchedSources'
                % MatchedSources input
                FN.Level    = 'merged';
                FN.FileType = {'hdf5'};
                OutFileNames = FN.genFull('Product','MergedMat', 'LevelPath',Args.LevelPath);
                for Iobj=1:1:Nobj
                    if ~isempty(Obj(Iobj).Fields) || Args.WriteEmpty
                        Obj(Iobj).write1(OutFileNames{Iobj},...
                                     'FileType',FN.FileType{1});
                    else
                        Istat = Istat + 1;
                        Status(Istat).Msg = sprintf('FileName=%s, DataProperty=%s, image is empty - not saved', OutFileNames{Iobj}, 'MergedMat');
                    end
                end

            
            otherwise
                % save object as mat file
                FN.Level    = Args.Level;
                FN.FileType = {'mat'};
                OutFileNames = FN.genFull('Product',Args.Product, 'LevelPath',Args.LevelPath);
                for Iobj=1:1:Nobj
                    save(OutFileNames{Iobj}, 'Obj', '-v7.3');
                end

        end
    end  % if Args.Save
end
