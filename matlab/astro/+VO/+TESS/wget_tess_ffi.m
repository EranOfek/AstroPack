function wget_tess_ffi(Sector, FilesType, Args)
    % Get TESS Full Image Frames (FFI)
    % Description: Rquires wget
    % Input  : - TESS sector (1-63). If empty, brings all.
    %            Default is [].
    %          - Files type: ['calibrated']|'uncalibrated'
    %          * ...,key,val,...
    %            'Nwget' - Number of files to retireve simultanosuly.
    %            'BaseURL' - Default is
    %                   'https://archive.stsci.edu/tess/bulk_downloads/bulk_downloads_ffi-tp-lc-dv.html'
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
    
    % get curl scripts
    Nl = numel(List);
    for Il=1:1:Nl
        CurlScript = webread(List{Il});
        CurlScript = char(CurlScript).';
        
        Str = regexprep(CurlScript, 'curl.{1,80}fits https','https');
        Str = regexprep(Str, '#!/bin/sh','');
        % conver lines of char arrays to cell
        Cell = regexp(Str,'\n','split');
        Flag = contains(Cell,'https://');
        Cell = Cell(Flag);
    
        % wget all the files
        Files = regexprep(Cell,'http.+/','');
        www.pwget(Cell,'--no-check-certificate -U Mozilla', Args.Nwget, 'OutFile',Files);
        
    end    
    
end
