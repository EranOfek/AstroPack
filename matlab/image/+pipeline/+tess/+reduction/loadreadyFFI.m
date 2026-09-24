function FFI=loadreadyFFI(FFIPath, Args)
    %{
    Loads a TESS Full Frame Image (FFI) FITS file into an AstroImage object
    and prepares it for downstream pipeline processing. The function reads
    the science image extension, identifies the TESS sector from the file
    name, and standardizes the FITS header by inserting sector and photometric
    zero-point information while removing non-standard header keys.
    
    Input   : - FFIPath. Path to a TESS FFI FITS file (e.g. '*_ffic.fits').
              * ...,key,val,...
                'SectorToken' - Regular expression token used to extract the
                       TESS sector number from the file name. The first
                       capturing group must correspond to the sector number.
                       Default is '-s00(\d+)-'.
                'ZP' - Photometric zero point to be added to the FITS header
                       under the key 'PH_ZP'. Default is 20.44.
    
    Output  : - FFI. AstroImage object containing the FFI image data and
                updated FITS header, including:
                  * 'Sector' keyword with the extracted TESS sector number
                  * 'PH_ZP' keyword with the supplied photometric zero point
                Non-standard FITS header keywords ('EXTVER', 'PCOUNT',
                'GCOUNT') are removed.
    
    Author  : Ruslan Konno (Jan 2026)
    Example : FFIPath = 'tess2018213055942-s0001-2-3-0120-s_ffic.fits';
              FFI = pipeline.tess.io.loadreadyFFI(FFIPath);
    %}

    arguments
        FFIPath

        Args.SectorToken = '-s00(\d+)-';
        Args.ZP = 20.44;
    end

    % Load image
    FFI = AstroImage.readImages2AstroImage(FFIPath, 'HDU', 2);
    
    % Identify sector
    Tokens = regexp(FFIPath, Args.SectorToken, 'tokens');
    Sector = str2double(Tokens{1}{1});

    % Add sector to header
    FFI.HeaderData.insertKey({'Sector',Sector});

    % Add a zero point to header
    FFI.HeaderData.insertKey({'PH_ZP',Args.ZP},inf);
    
    % Remove keys that should not be in a fits header
    FFI.HeaderData.deleteKey({'EXTVER','PCOUNT','GCOUNT'}, 'UseRegExp',false);

    % Remove leading and trailing black strips
    FFI.crop([45 2092 1 2048]);

end