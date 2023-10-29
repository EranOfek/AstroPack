function writeSimpleFITS(Image, FileName, Args)
    % Write a simple (single HDU) FITS file to disk
    %   This function is a few times faster than FITS.write(). This
    %    involves tradeoff between using fwrite (slow because of endianness conversion)
    %    and growing the image header with CFITSIO routines AFTER having
    %    created the HDU and written the image.
    %
    % Input  : - An image data (e.g., matrix).
    %          - File name in which to write the FITS file.
    %          * ...,key,val,...
    %            'Header' - A 2 or 3 columns cell array containing the
    %                   header.
    %            'DataType' - Image data type. If empty, use image type.
    %                   Default is [].
    %            'UseMatlabIo' use matlab CFITS instead of fwrite. Default
    %                   is true
    %            'CompressType' which CFITS compression to use (see
    %                   'help matlab.io.fits.setComplessionType').
    %                   Default is 'NOCOMPRESS'. Only effective if
    %                   UseMatlabIo=true.
    % Output : null
    % Author : Eran Ofek (Jan 2022), Enrico Segre (Feb 2023)
    % Example: io.fits.writeSimpleFITS(AI.Image, 'try.fits','Header',AI.HeaderData.Data);

    arguments
        Image
        FileName
        Args.Header cell              = {};
        Args.DataType                 = [];
        Args.UseMatlabIo              = true;
        Args.CompressType    char     = 'NOCOMPRESS';
    end

    % sanify the file name so that it contain the absolute path
    FileName = tools.os.relPath2absPath(FileName); 
    
    if isempty(Args.DataType)
        Args.DataType = class(Image);
    end

    % if the class is unsigned, we must write Image-bzero as signed, and
    % change the required class.
    switch Args.DataType
        case 'int8'
            NewDataType='uint8';
        case 'uint16'
            NewDataType='int16';
        case 'uint32'
            NewDataType='int32';
        otherwise
            NewDataType=Args.DataType;
    end
    [BitPix,bzero]  = io.fits.dataType2bitpix(Args.DataType);
    % shift the image if unsigned
    if ~strcmp(NewDataType,Args.DataType)
        Image=reshape(typecast(bitxor(Image(:),cast(bzero,Args.DataType)),...
                      NewDataType),size(Image));
    end

   % prepare header and update the BITPIX and BZERO key vals if needed
   % (or, should we keep them in some particular case?)
   if isempty(Args.Header)
       % create minimal default FITS header
       Args.Header = io.fits.defaultHeader(Args.DataType, size(Image));
   end
   if ~Args.UseMatlabIo
       Args.Header = imUtil.headerCell.replaceKey(Args.Header,'BITPIX',{BitPix});
       Args.Header = imUtil.headerCell.replaceKey(Args.Header,'BZERO',{bzero});
       % using fwrite:
       HeaderStr = io.fits.generateHeaderBlocks(Args.Header);
       FID = fopen(FileName,'w');
       fprintf(FID,'%s',HeaderStr);
       io.fits.writeImageData(FID, Image, NewDataType);
       fclose(FID);
   else
        % using matlab.io.fits:
        
        % delete existing FileName if exist
        if isfile(FileName)
            delete(FileName);
        end
        
        % Create new FITS file
        Fptr = matlab.io.fits.createFile(FileName);
        
        % set eventual compression (and error out if unrecognized)
        matlab.io.fits.setCompressionType(Fptr,upper(Args.CompressType))

        % create Image
        matlab.io.fits.createImg(Fptr, NewDataType, size(Image));

        Header = FITS.prepareHeader(Args.Header, HEAD.HeaderField, 'WriteTime', true);
        FITS.writeHeader(Fptr, Header, HEAD.HeaderField);


        % write Image %% datatype conversion overflow if FITS.writeHeader before???
        matlab.io.fits.writeImg(Fptr, Image); %,Fpixels);
        
        % change BZERO post-facto
        matlab.io.fits.writeKey(Fptr,'BZERO',bzero);

        % Close FITS file
        matlab.io.fits.closeFile(Fptr);
    end

end