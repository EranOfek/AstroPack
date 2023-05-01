function wget_tess_ffi(Sector, FilesType, Args)
    % Get TESS Full Image Frames (FFI)
    % Description: Rquires wget
    % Input  : - TESS sector (1-63). If empty, brings all.
    %            Default is [].
    %          - Files type: ['calibrated']|'uncalibrated'
    %          * ...,key,val,...
    %            'Nwget' - Number of files to retireve simultanosuly.
    %            'BaseURL' - Default is
    %            'https://archive.stsci.edu/tess/bulk_downloads/bulk_downloads_ffi-tp-lc-dv.html'
    % Output : null
    % Author : Eran Ofek (May 2023)
    % Example: VO.TESS.wget_tess_ffi
   
    arguments
        Sector       = [];
        FilesType    = 'calibrated';  % 'calibrated'|'imcalibrated'
        
        Args.Nwget   = 10;
        Args.BaseURL = 'https://archive.stsci.edu/tess/bulk_downloads/bulk_downloads_ffi-tp-lc-dv.html';
    end
    
    switch FilesType
        case 'calibrated'
            StrFind = '_ffic';
        case 'uncalibrated'
            StrFind = '_ffir';
        otherwise
            error('Unknown FilesType option');
    end
    
    if ~isempty(Sector)
        StrFind = sprintf('sector_%d%s',Sector, StrFind);
    end
        
    List = www.find_urls(Args.BaseURL, 'strfind',StrFind);
    List = regexprep(List, 'https:/missions', 'https://archive.stsci.edu/missions');
    
    % wget the curl files
    www.pwget(List,'--no-check-certificate -U Mozilla', Args.Nwget);
   
    
end
