function writeProduct(Obj, FNin, Args)
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
    %            'SubDir' - Like 'Type', but for 'SubDir'.
    %                   Default is [].
    %            'Version' - Like 'Type', but for 'Version'.
    %                   Default is [].
    %            'FileType' - Like 'Type', but for 'FileType'.
    %                   Default is [].
    %            'GetHeaderCropID' - A logical indicating if to get
    %                   the CropID from the header.
    %                   Default is true.
    %            'BasePath' - Like 'Type', but for 'BasePath'.
    %                   Default is [].
    %            'FullPath' - Like 'Type', but for 'FullPath'.
    %                   Default is [].
    % Output : null
    % Author : Eran Ofek (Apr 2023)

    arguments
        Obj
        FNin FileNames
        Args.Save logical           = true;
        Args.IsSimpleFITS logical   = true;
        Args.Type                   = [];
        Args.Level                  = [];
        Args.Product cell           = {'Image','Mask','Cat','PSF'};
        Args.WriteHeader            = [true, false, true, false];

        Args.DateFromHeader logical = true;
        Args.Counter                = [];
        Args.CCDID                  = [];
        Args.CropID                 = [];  % ...
        Args.SubDir                 = [];
        Args.Version                = [];
        Args.FileType               = [];

        Args.GetHeaderCropID logical = true;

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
        Nfn   = FN.nfiles;
        if Nobj~=Nfn
            error('Number of elements in AstroImage and FileNames object must be identical');
        end

        % generate a copy of FNin, so the object will not be modified
        FN = FNin.copy;

        % Product and CropID are set inside the write loop


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

        % get JD for all images
        if Args.dateFromHeader
            JD = Obj.julday;
        else
            JD = [];
        end

        % loop for updateing the FN object
        for Iobj=1:1:Nobj
            % for each element

            % set the FileNames time
            if ~isempty(JD)
                FN.Time(Iobj) = JD(Iobj);
            end

            % go over products
            for Iprod=1:1:Nprod
                % Update Product in FileNames object
                FN.Product = Args.Product{Iprod};

                % Update CropID in FileNames object
                if Args.GetHeaderCropID
                    FN.CropID = AI.HeaderData.getVal('CROPID');
                end
            end
        end

        % generate file names
        OutFileNames = FN.genFull;


        % loop for writing the products
        switch class(Obj)
            case 'AstroImage'
                % AstroImage input
                for Iobj=1:1:Nobj
                    for Iprod=1:1:Nprod
                        Obj(Iobj).write1(OutFileNames{Iobj}, Args.Product{Iprod},...
                                         'FileType',FN.FileType{1},...
                                         'IsSimpleFITS',Args.IsSimpleFITS.IsSimpleFITS,...
                                         'WriteHeader',WriteHeader(Iprod));

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
