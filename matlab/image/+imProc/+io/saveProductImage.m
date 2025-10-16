function [Status,AFN] = saveProductImage(AI, FileName, Args)
    % Save products in AstroImage object
    %   Optionaly save the: Image, Mask, PSF, Cat
    % Input  : - An AstroImage object.
    %          - Either a string/cell array of Nfile X Nprod
    %            Containing the file names per product.
    %            Or, an AstroFileName object from which the file names and
    %            paths will be generated.
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
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), 'MyFile.fits');
    %          [Status,AFN]=imProc.io.saveProductImage(AI(1), 'MyFile.fits','OutProduct',"Image");

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
        Args.AddSubDirKey             = 'SUBDIR';
        Args.FileType                 = 'fits';  % If AstroFileName, use info
        
        Args.OverWrite logical        = false;
        Args.WriteTime logical        = false;
        Args.SanifyPath               = false; 

        %Args.WriteMethodImages        = 'Simple';    % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        %Args.WriteMethodTables        = 'Standard';  % can be 'Standard' or 'MexHeader'
        Args.WriteMethodImages = 'ThreadedMex';     % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
        Args.WriteMethodTables = 'MexHeader';       % can be 'Standard' or 'MexHeader'  


       
    end    

    Nprod = numel(Args.OutProduct);
 
    if isa(FileName, 'AstroFileName')
        %FileList = FileName.genProducts('OutProduct',Args.OutProduct, 'AddPath',false);
        [FileListImage,PathList,~,AFN]  = FileName.genFullPath('AddSubDir',Args.AddSubDir,...
                                                     'PathType',Args.PathType,...
                                                     'BasePath',Args.BasePath,...
                                                     'BasePathRef',Args.BasePathRef,...
                                                     'Path',Args.Path,...
                                                     'CreateNewObj',true);
        Nim = numel(FileListImage);
        FileList = string(Nim, Nprod);
        for Iprod=1:1:Nprod
            FileList(:,Iprod) = FileName.genFile('Product',Args.OutProduct{Iprod});
        end
        FileType = AFN.FileType{1};
    else
        AFN = [];
        if isvector(FileName)
            FileName = FileName(:);
        end
        [Nim, NprodGiven] = size(FileName);
        if Nprod~=NprodGiven
            error('INput FileName contains only %d columns, while %d products were requested', NprodGiven, Nprod);
        end
        FileType = Args.FileType;
    end

    if Args.AddSubDirKey && isa(AI, 'AstroImage') && isa(AFN, 'AstroFileName')
        AI.setKeyVal(Args.SubDirKey, AFN.SubDir);
    end


    Nobj = numel(Obj);
    if Nim~=Nobj
        error('Numbder of images (%d) is not consistent with the number of file names (%d)', Nobj, Nim);
    end

    Status     = {};
    ErrInd     = 0;
    DirCreated = false;
    for Iobj=1:1:Nobj
        for Iprod=1:1:Nprod
            % product type: Args.OutProduct{Iprod}
            % save product: AI(Iobj).(Args.OutProduct{Iprod})
            % Path: PathList{Iobj}
            % File FileList{Iobj, Iprod}

            FileToSave = [PathList, FileList{Iobj, Iprod}];
            Data = AI(Iobj).(Args.OutProduct{Iprod});
            if isempty(Data)
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
