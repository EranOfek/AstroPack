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
    % update the BITPIX and BZERO key vals if needed (or, should we keep
    %  them in some particular case?)
    [BitPix,bzero]  = io.fits.dataType2bitpix(Args.DataType);
    Args.Header = imUtil.headerCell.replaceKey(Args.Header,'BITPIX',{BitPix});
    Args.Header = imUtil.headerCell.replaceKey(Args.Header,'BZERO',{bzero});

    HeaderStr = io.fits.generateHeaderBlocks(Args.Header);

    % if the class is unsigned, we must write Image-bzero as signed, and
    % change the required class.
    % To do the substraction, we have to upcast
    switch Args.DataType
        case 'uint8'
            Image=int8(int16(Image)-int16(bzero));
            Args.DataType='int8';
        case 'uint16'
            Image=int16(int32(Image)-int32(bzero));
            Args.DataType='int16';
        case 'uint32'
            Image=int32(int64(Image)-int64(bzero));
            Args.DataType='int16';
    end

    FID = fopen(FileName,'w');
    %fwrite(FID, HeaderStr, 'char', 0, 'b');
    fprintf(FID,'%s',HeaderStr);
    io.fits.writeImageData(FID, Image, Args.DataType);
    fclose(FID);

end