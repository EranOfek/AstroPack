function [Status,AFN] = saveProductImage(AI, FileName, Args)
    % Save FITS products from an AstroImage object
    %   Optionaly save the: Image, Mask, PSF, Cat
    % Input  : - An AstroImage object.
    %          - Either a string/cell array of Nfile X Nprod
    %            Containing the file names per product.
    %            Or, an AstroFileName object from which the file names and
    %            paths will be generated.
    %            If this is a file name, then the path is either embeded
    %            and/or provided by the 'Path' argument.
    %          * ...,key,val,... ,
    %            'OutProduct' - A string/cell array of products to save in the AstroImage.
    %                   Options are:
    %                       "Image" - Image
    %                       "Mask" - Mask image.
    %                       "PSF" - PSF image.
    %                       "Cat" - Catalog data.
    %                   Default is ["Image", "Mask", "PSF", "Cat"]
    %            'WriteHeader' - An array of logical flags indicating if to
    %                   save header for each of the products.
    %                   Default is [true, false, false, true].
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
    %            'AddSubDirKey' - SubDir Header keyword. If not empty, then
    %                   will add the SubDir to the object header.
    %                   Default is 'SUBDIR'.
    %            'FileType' - FileType. If FileName is an AstroFileName
    %                   then this keyword is ignored.
    %                   Default is 'fits'.
    %            'OverWrite' - Default is false.
    %            'WriteTime' - Add the write time to header.
    %                   Default is false.
    %            'SanifyPath' - A logical. true can be time-consuming.
    %                   Default is false.
    %            'WriteMethodImages' - can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
    %                   Default is 'ThreadedMex'.
    %            'WriteMethodTables' - can be 'Standard' or 'MexHeader'  
    %                   Default is 'MexHeader'.
    % Output : - Status cell array containing an error message for each
    %            failed file.
    %          - An updated AstroFileName object (if included in input).
    % Author : Eran Ofek (2025 Oct) 
    % Example: [Status,AFN]=imProc.io.saveProductImage(AI, AFN);
    %
    %          % Write a single Image product with user specified file name
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), 'MyFile.fits','OutProduct',"Image");
    %          % Write a single PSF product with user specified file name
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), 'MyPSF.fits','OutProduct','PSF');
    %          % write a single PSF product using AstroFileName
    %          AFN=AstroFileName; AFN.JD=2451545; AFN.julday2time;  
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), AFN,'OutProduct','PSF','Path','/home/eran');
    %          % or specifiy the path in the AstroFileName
    %          AFN.Path = '/home/eran';
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), AFN,'OutProduct','Cat');
    %          % save multiple images and multiple data products
    %          AFN=AstroFileName; AFN.JD=[2460000;2460001]; AFN.julday2time;  
    %          [Status,AFN]=imProc.io.saveProductImage([AI;AI], AFN,'Path','/home/eran');
    


    arguments
        AI
        FileName
        Args.OutProduct        = ["Image", "Mask", "PSF", "Cat"];
        Args.WriteHeader       = [true, false, false, true];
        Args.AddSubDir         = true;
        Args.PathType          = 'proc'; % 'proc'|'raw'|'new'|'calib'|'failed'|'ref'
        Args.BasePath                 = [];
        Args.BasePathRef              = [];
        Args.Path                     = [];
        Args.SubDirKey                = 'SUBDIR';
        Args.FileType                 = 'fits';  % If AstroFileName, use info
        
        Args.OverWrite logical        = false;
        Args.WriteTime logical        = false;
        Args.SanifyPath               = false; 

        %Args.WriteMethodImages        = 'Simple';    % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        %Args.WriteMethodTables        = 'Standard';  % can be 'Standard' or 'MexHeader'
        Args.WriteMethodImages = 'ThreadedMex';     % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        Args.WriteMethodTables = 'MexHeader';       % can be 'Standard' or 'MexHeader'  

    end    

    if ischar(Args.OutProduct)
        Args.OutProduct = string(Args.OutProduct);
    end
    Nprod = numel(Args.OutProduct);
 
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
        FileList = strings(Nim, Nprod);
        for Iprod=1:1:Nprod
            FileList(:,Iprod) = FileName.genFile('Product',Args.OutProduct{Iprod});
        end
        FileType = AFN.FileType{1};
    else
        AFN = [];
        if ischar(FileName)
            FileName = string(FileName);
        end
        if isvector(FileName)
            FileName = FileName(:);
        end
        [Nim, NprodGiven] = size(FileName);
        if Nprod~=NprodGiven
            error('INput FileName contains only %d columns, while %d products were requested', NprodGiven, Nprod);
        end
        FileType = Args.FileType;
        PathList = Args.Path;
        FileList = FileName;
    end

    if Args.AddSubDir && isa(AI, 'AstroImage') && isa(AFN, 'AstroFileName')
        AI.setKeyVal(Args.SubDirKey, AFN.SubDir);
    end


    Nobj = numel(AI);
    if Nim~=Nobj
        error('Numbder of images (%d) is not consistent with the number of file names (%d)', Nobj, Nim);
    end

    Npath      = numel(PathList);
    Status     = {};
    ErrInd     = 0;
    DirCreated = false;
    for Iobj=1:1:Nobj
        for Iprod=1:1:Nprod
            % product type: Args.OutProduct{Iprod}
            % save product: AI(Iobj).(Args.OutProduct{Iprod})
            % Path: PathList{Iobj}
            % File FileList{Iobj, Iprod}

            Ipath = min(Iobj, Npath);
            FileToSave = join([PathList(Ipath), filesep, FileList{Iobj, Iprod}],"",2);

            if strcmp(Args.OutProduct{Iprod}, 'Cat')
                Prop = 'CatData';
            else
                Prop = Args.OutProduct{Iprod};
            end
            if AI(Iobj).isemptyProperty(Prop)
            % Data = AI(Iobj).(Args.OutProduct{Iprod});
            % if isempty(Data)
                % Image is empty
                % Write error status
                ErrInd = ErrInd + 1;
                Status{ErrInd} = sprintf('Product not saved / Image: %s is empty',FileToSave);
            else
                AI(Iobj).write1(FileToSave, Args.OutProduct{Iprod},...
                                             'FileType',FileType,...
                                             'WriteHeader',Args.WriteHeader(Iprod),...
                                             'MkDir',~DirCreated,...
                                             'OverWrite',Args.OverWrite,...
                                             'WriteTime',Args.WriteTime,...
                                             'SanifyPath',Args.SanifyPath,...
                                             'WriteMethodImages',Args.WriteMethodImages,...
                                             'WriteMethodTables',Args.WriteMethodTables);

                DirCreated = true;
                % Update FileName in Obj
                %Obj(Iobj).ImageData.FileName = OutFileNames{Iobj};
                Obj(Iobj).ImageData.FileName = FileToSave;

            end
        end
    end
end
