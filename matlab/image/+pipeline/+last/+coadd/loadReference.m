function [Result] = loadReference(FieldID, CumNum, CropID, Args)
    % Load LAST reference image
    % Input  : - LAST fieldid (scalar).
    %          - LAST cumera number (scalar).
    %          - LAST CropID (scalar)
    %          * ...,key,val,... 
    %            'BasePath' - BasePath. If empty, then construct from host name (e.g., '/last01e/data')
    %                   Default is '/marvin'
    %            'Filter' - Filter name. Default is 'clear'.
    %            'ExtraOutProduct' - Additional Products to load in
    %                   addition to the 'Image' product.
    %                   Default is ["Mask", "PSF", "Cat"]
    % Output : - An AstroImage with the loaded reference image products. 
    % Author : Eran Ofek (2024 Dec) 
    % Example: pipeline.last.coadd.loadReference

    arguments
        FieldID
        CumNum
        CropID        
        Args.BasePath          = '/marvin'; % if empty assume LAST machine
        Args.Filter            = 'clear';
        Args.ExtraOutProduct      = ["Mask", "PSF", "Cat"];

    end

    [File, Path, AFN] = pipeline.last.coadd.findReference(FieldID, CumNum, CropID, 'BasePath',Args.BasePath, 'Filter',Args.Filter);
    
    Result = AstroImage.readProducts(AFN, 'Path',Path, 'ExtraOutProduct',Args.ExtraOutProduct);
        
end
