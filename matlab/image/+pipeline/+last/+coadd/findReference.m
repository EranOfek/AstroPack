function [File, Path, AFN] = findReference(FieldID, CumNum, CropID, Args)
    % Find reference images based on fieldid, camnum, cropid
    % Input  : - LAST fieldid (scalar).
    %          - LAST cumera number (scalar).
    %          - LAST CropID (scalar)
    %          * ...,key,val,... 
    %            'BasePath' - BasePath. If empty, then construct from host name (e.g., '/last01e/data')
    %                   Default is '/marvin'
    %            'Filter' - Filter name. Default is 'clear'.
    % Output : - Reference image file name. Return empty if not exist.
    %          - Reference image path.
    %          - AstroFileName for reference image.
    % Author : Eran Ofek (2024 Dec) 
    % Example: [File, Path, AFN] = pipeline.last.coadd.findReference(1000, 1, 10)

    arguments
        FieldID
        CumNum
        CropID
        Args.BasePath              = '/marvin'; % if empty assume LAST machine
        Args.Filter                = 'clear';
        Args.BaseProjName          = 'LAST';
    end

    Path = pipeline.last.path.constructRefDir(Args.BasePath, FieldID);

    PWD = pwd;
    if isfolder(Path)
        cd(Path);

        ProjName = sprintf('%s.*.*.%02d', Args.BaseProjName, CumNum);
        AFN  = AstroFileName.dirLiteral('ProjName',ProjName, 'Filter',Args.Filter', 'CropID',CropID, 'Level','*', 'Product','Image');
        cd(PWD);
        if isempty(AFN.Time)
            File = [];
        else
            File = AFN.genFile;
        end
    else
        File = [];
        Path = [];
        AFN  = [];
    end
    
end
