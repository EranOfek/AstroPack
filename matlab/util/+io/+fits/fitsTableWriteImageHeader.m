function fitsTableWriteImageHeader(fitsFileName, imageHeaderCellArray)   
    % Append a FITS image header block to a file using a mex function
    % Input  : - File name
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.fitsTableWriteImageHeader('mytable.fits', imageHeader)

    io.fits.mex.mex_fits_table_write_image_header(imageHeaderCellArray, fitsFileName);
end
