function [Status, AFN] = saveProductMat(Product, FileName, Args)
    % Save products into a MAT file.
    % Input  : - Any matlab object.
    %          - Either a string/cell array of Nfile X Nprod
    %            Containing the file names per product.
    %            Or, an AstroFileName object from which the file names and
    %            paths will be generated.
    %            If this is a file name, then the path is either embeded
    %            and/or provided by the 'Path' argument.
    %          * ...,key,val,... ,
    %            'AddSubDir' - A logical indicating if to include the
    %                   SubDir (i.e., visit name) in the file path.
    %                   Default is true.
    %            'PathType' - If the object "Path" property is
    %                   populated, then the path will be retrieved
    %                   from this property. Otherwise, will be
    %                   work according to one of the following
    %                   options:
    %                   'proc' - Path of the form:
    %                           /BasePath/ProjName/YYYY/MM/DD/proc/SubDir
    %                   'raw' - Path of the form:
    %                           /BasePath/ProjName/YYYY/MM/DD/raw
    %                   'new'|'calib'|'failed' - Path of the form:
    %                           /BasePath/ProjName/<new>
    %                   'ref' - Get the reference images file names
    %                           using genRefPath.
    %                           Answer is of the form:
    %                           /RefBasePath/FieldID
    %                   Default is 'proc'.
    %            'BasePath' - AstroFileName BasePath. If empty, use object
    %                   property. Default is [].
    %            'BasePathRef' - AstroFileName BasePathRef. If empty, use object
    %                   property. Default is [].
    %            'Path' - Full Path. Default is [].
    %            'SubDirKey' - SubDir Header keyword. If not empty, then
    %                   will add the SubDir to the object header.
    %                   Default is 'SUBDIR'.
    %            'FileType' - FileType. If FileName is an AstroFileName
    %                   then this keyword is ignored.
    %                   Default is 'hdf5'.
    %            'RealIfComplex' - A logical indicating if to take
    %                   the real value (of a complex value).
    %                   This is used only if FileType=hdf5.
    %                   Default is true.
    %            'Type' - Type to cast the Data fields before writing.
    %                   If empty, then skip. Default is 'single';
    %            'SkipFields' - Fields to skip (not to cast).
    %                   Default is [].
    %            'OverWrite' - Default is false.
    %            'SanifyPath' - A logical. true can be time-consuming.
    %                   Default is false.
    %            'ProductType' - Default is 'MergedMat'.
    %            'SavedProductName' - the name of the variable to be written into a mat file 
    % Output : - Status cell array containing an error message for each
    %            failed file.
    %          - An updated AstroFileName object (if included in input).
    % Author : Eran Ofek (2025 Oct) 
    % Example: [Status,AFN]=imProc.io.saveProductMat(T,'hi.mat')
    %          [Status,AFN]=imProc.io.saveProductMat(T,AFN)


    arguments
        Product
        FileName
        %Args.OutProduct        = ["Image", "Mask", "PSF", "Cat"];
        %Args.WriteHeader       = [true, false, false, true];
        Args.AddSubDir                = true;
        Args.PathType                 = 'proc'; % 'proc'|'raw'|'new'|'calib'|'failed'|'ref'
        Args.BasePath                 = [];
        Args.BasePathRef              = [];
        Args.Path                     = [];
        Args.SubDirKey                = 'SUBDIR';
        
        Args.FileType                 = 'mat';
        Args.RealIfComplex logical    = true;
        Args.Type                     = 'single';
        Args.SkipFields               = [];

        Args.OverWrite logical        = false;
        %Args.WriteTime logical        = false;
        Args.SanifyPath               = false; 

        Args.ProductType                  = [];

        %Args.WriteMethodImages        = 'Simple';    % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        %Args.WriteMethodTables        = 'Standard';  % can be 'Standard' or 'MexHeader'
        %Args.WriteMethodImages = 'ThreadedMex';     % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        %Args.WriteMethodTables = 'MexHeader';       % can be 'Standard' or 'MexHeader'  
        Args.SavedProductName        = [];      
    end    

    if isa(FileName, 'AstroFileName')
        %FileList = FileName.genProducts('OutProduct',Args.OutProduct, 'AddPath',false);
        if isempty(Args.Path)
            Args.Path = FileName.Path;
        end
        [FileListImage,PathList,~,AFN]  = FileName.genFullPath('AddSubDir',Args.AddSubDir,...
                                                     'PathType',Args.PathType,...
                                                     'BasePath',Args.BasePath,...
                                                     'BasePathRef',Args.BasePathRef,...
                                                     'Path',Args.Path,...
                                                     'CreateNewObj',true);
        Nim = numel(FileListImage);
        FileList = strings(Nim, 1);
        FileList = FileName.genFile('Product',Args.ProductType);
        if ischar(AFN.FileType)
            FileType = AFN.FileType;
        else
            FileType = AFN.FileType{1};
        end
    else
        AFN = [];
        if ischar(FileName)
            FileName = string(FileName);
        end
        if isvector(FileName)
            FileName = FileName(:);
        end
        [Nim, NprodGiven] = size(FileName);
        
        FileType = Args.FileType;
        PathList = Args.Path;
        FileList = FileName;
    end
    FileToSave = FileList;


    %if Args.AddSubDir && isa(AI, 'AstroImage') && isa(AFN, 'AstroFileName')
    %    AI.setKeyVal(Args.SubDirKey, AFN.SubDir);
    %end


    %Nobj = numel(MS);
    %if Nim~=Nobj
    %    error('Numbder of images (%d) is not consistent with the number of file names (%d)', Nobj, Nim);
    %end

    Npath      = numel(PathList);
    Status     = {};
    ErrInd     = 0;

    % product type: Args.OutProduct{Iprod}
    % save product: AI(Iobj).(Args.OutProduct{Iprod})
    % Path: PathList{Iobj}
    % File FileList{Iobj, Iprod}

    %Ipath = min(Iobj, Npath);
    %if Ipath==0
    %    FileToSave = FileList{Iobj};
    %else
    %    FileToSave = join([PathList(Ipath), filesep, FileList{Iobj}],"",2);
    %end
   
    if isempty(Product)
    % Data = AI(Iobj).(Args.OutProduct{Iprod});
    % if isempty(Data)
        % Image is empty
        % Write error status
        ErrInd = ErrInd + 1;
        Status{ErrInd} = sprintf('Product not saved / MatchedSources: %s is empty',FileToSave);
    else
        % Create the target directory if it does not exist yet (e.g. a
        % rejected visit where no image products were written first).
        if ~isempty(PathList) && ~isfolder(PathList)
            mkdir(PathList);
        end
        % Save to the full path directly, without cd-ing into the target
        % directory, so a failing save() cannot strand the process in the
        % wrong working folder.
        FullFileToSave = fullfile(PathList, FileToSave);
        if isempty(Args.SavedProductName)
            Args.SavedProductName = 'Product';
        else
            eval([Args.SavedProductName ' = Product;']);
        end
        save(FullFileToSave, Args.SavedProductName, '-v7.3');

    end
    
end
