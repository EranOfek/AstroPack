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
        Args.UseMatlabIo              = false;
    end

    if isempty(Args.DataType)
        Args.DataType = class(Image);
    end

    % if the class is unsigned, we must write Image-bzero as signed, and
    % change the required class.
    % To do the substraction, we have to upcast
%     switch Args.DataType
%         case 'uint8'
%             Image=int8(int16(Image)-int16(bzero));
%             Args.DataType='int8';
%         case 'uint16'
%             Image=int16(int32(Image)-int32(bzero));
%             Args.DataType='int16';
%         case 'uint32'
%             Image=int32(int64(Image)-int64(bzero));
%             Args.DataType='int32';
%     end
%     NewDataType=Args.DataType;

    switch Args.DataType
        case 'uint8'
            NewDataType='int8';
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
   else
       Header = FITS.prepareHeader(Args.Header, HEAD.HeaderField, 'WriteTime', true);
       Header.Header = imUtil.headerCell.replaceKey(Header.Header,'BITPIX',{BitPix});
       Header.Header = imUtil.headerCell.replaceKey(Header.Header,'BZERO',{bzero});
   end
   
   if ~Args.UseMatlabIo
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
        
        % create Image
        matlab.io.fits.createImg(Fptr, NewDataType, size(Image));

        % write Image %% datatype conversion overflow???
        matlab.io.fits.writeImg(Fptr, Image); %,Fpixels);
        
        % write Header
        FITS.writeHeader(Fptr, Header, HEAD.HeaderField);

        % Close FITS file
        matlab.io.fits.closeFile(Fptr);
    end

end