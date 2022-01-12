function writeSimpleFITS(Image, FileName, Args)
    % Write a simple (single HDU) FITS file to disk
    %   This function is a few times faster compared with CFITSIO routines.
    % Input  : - An image data (e.g., matrix).
    %          - File name in which to write the FITS file.
    %          * ...,key,val,...
    %            'Header' - A 2 or 3 columns cell array containing the
    %                   header.
    %            'DataType' - Image data type. If empty, use image type.
    %                   Default is [].
    % Output : null
    % Author : Eran Ofek (Jan 2022)
    % Example: io.fits.writeSimpleFITS(AI.Image, 'try.fits','Header',AI.HeaderData.Data);

    arguments
        Image
        FileName
        Args.Header cell              = {};
        Args.DataType                 = [];
    end

    if isempty(Args.DataType)
        Args.DataType = class(Image);
    end

    if isempty(Args.Header)
        % create minimal default FITS header
        Args.Header = io.fits.defaultHeader(Args.DataType, size(Image));
    end
    HeaderStr = io.fits.generateHeaderBlocks(Args.Header);

    FID = fopen(FileName,'w');
    %fwrite(FID, HeaderStr, 'char', 0, 'b');
    fprintf(FID,'%s',HeaderStr);
    io.fits.writeImageData(FID, Image, Args.DataType);
    fclose(FID);

end