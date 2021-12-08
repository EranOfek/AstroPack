function [URL,Files] = wget_manga(Plate, IFUdsgn, VerDRP3, Args)
    % wget SDSS/MaNGA spectral cube FITS files.
    % Input  : - A vector of SDSS/MaNGA plate numbers.
    %          - A vector of IFU design numbers.
    %          - A cell array of verdrp3 strings.
    %          * ...,key,val,...
    %            'CubeType' - ['LINECUBE'] | 'LOGCUBE'
    %            'DR'       - data relaese. Default is 'DR17'.
    %            'GetFiles' - Default is true.
    %            'MaxGet'   - Max parallel get. Default is 15.
    %            'wgetStr'  - Default is '--no-check-certificate -U Mozilla'.
    % Output : - A cell array of full URLs.
    %          - A cell array of file names.
    % Author : Eran Ofek (Dec 2021)
    % Example: URL = VO.SDSS.MaNGA.wget_manga(10001, 12701, 'v3_1_1')
    
    
    arguments
        Plate
        IFUdsgn
        VerDRP3
        
        Args.CubeType           = 'LINCUBE';  % 'LINCUBE' | 'LOGCUBE'
        Args.DR                 = 'dr17';
       
        Args.GetFiles logical   = true;
        Args.MaxGet             = 15;
        Args.wgetStr            = '--no-check-certificate -U Mozilla';
    end
    
    if ischar(VerDRP3)
        VerDRP3 = {VerDRP3};
    end
    %https://data.sdss.org/sas/dr17/manga/spectro/redux/v3_1_1/10001/stack/
    % manga-10001-12701-LINCUBE.fits.gz
    BaseURL = sprintf('https://data.sdss.org/sas/%s/manga/spectro/redux/',Args.DR);
    N       = numel(Plate);
    URL     = cell(N,1);
    for I=1:1:N
        URL{I}     = sprintf('%s%s/%d/stack/manga-%d-%d-%s.fits.gz',BaseURL, VerDRP3{I}, Plate(I), Plate(I), IFUdsgn(I), Args.CubeType);
    end
    
    Files = {};
    if Args.GetFiles
        Files = www.pwget(URL, Args.wgetStr, Args.MaxGet);
    end
end
    