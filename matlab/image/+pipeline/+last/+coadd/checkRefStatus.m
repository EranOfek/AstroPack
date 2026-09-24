function [Result,Path] = checkRefStatus(FieldID, CamNum, CropID, Args)
    % Check that the reference image exist with all its metadata
    % Input  : - FieldID
    %          - Cam number.
    %          - Crop number
    %          * ...,key,val,...
    %            'BaseDir' - Reference base dir.
    %                   Default is '/marvin/references'.
    % Output : - A structure with the following fields:
    %            .DirExist - Logical for dir exist.
    %            .Ncoadd - N of coadd.
    %            .Nref - N of ref.
    %            .FilesCoadd
    %            .FilesRef
    % Author : Eran Ofek (2025 Jan) 
    % Example: [Result] = pipeline.last.coadd.checkRefStatus(1389,1,1)

    arguments
        FieldID
        CamNum
        CropID
        Args.BaseDir           = '/marvin/references';
        Args.Filter            = 'clear';
    end


    PWD = pwd;
    cd(Args.BaseDir);

    if isnumeric(FieldID)
        FieldIDstr = sprintf('%d',FieldID);
    else
        FieldIDstr = FieldID;
    end
    
    if isfolder(FieldIDstr)
        Result.DirExist = true;

        cd(FieldIDstr);
        Path = pwd;
        
        AFN_C = AstroFileName.dirLiteral('ProjName',sprintf('LAST.*.*.%02d',CamNum), 'FieldID',FieldIDstr, 'CropID',CropID, 'Filter',Args.Filter, 'Type','sci', 'Level','coadd', 'Product','*');

        AFN_R = AstroFileName.dirLiteral('ProjName',sprintf('LAST.*.*.%02d',CamNum), 'FieldID',FieldIDstr, 'CropID',CropID, 'Filter',Args.Filter, 'Type','sci', 'Level','ref', 'Product','*');

        Result.Ncoadd = AFN_C.nFiles;
        Result.Nref   = AFN_R.nFiles;
        if Result.Ncoadd==0
            Result.FilesCoadd = {};
        else
            Result.FilesCoadd = AFN_C.genFile;
        end
        if Result.Nref==0
            Result.FilesRef = {};
        else
            Result.FilesRef   = AFN_R.genFile;
        end

    else
        Path = [];
        Result.DirExist = false;
        Result.Ncoadd   = 0;
        Result.Nref     = 0;
        Result.FilesRef = {};
        Result.FilesCoadd = {};
    end


    cd(PWD);

end
