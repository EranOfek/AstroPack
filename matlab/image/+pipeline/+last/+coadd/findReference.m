function [File, Path, AFN] = findReference(FieldID, CumNum, CropID, Args)
    % Find reference images based on fieldid, camnum, cropid
    % Input  : - LAST fieldid (scalar).
    %          - LAST cumera number (scalar).
    %          - LAST CropID (scalar)
    %          * ...,key,val,... 
    %            'BasePath' - BasePath. If empty, then construct from host name (e.g., '/last01e/data')
    %                   Default is '/marvin'
    %            'Filter' - Filter name. Default is 'clear'.
    % Output : - Reference image file name.
    %          - Reference image path.
    %          - AstroFileName for reference image.
    % Author : Eran Ofek (2024 Dec) 
    % Example: 

    arguments
        FieldID
        CumNum
        CropID
        Args.BasePath              = '/marvin'; % if empty assume LAST machine
        Args.Filter                = 'clear';
    end

    Path = pipeline.last.path.constructRefDir(Args.BasePath, FieldID);
    AFN  = AstroFileName.readLiteral('ProjName',ProjName, 'Filter',Args.Filter', 'CropID',CropID, 'Level','*', 'Product','Image');
    File = AFN.genFile;
    
end
