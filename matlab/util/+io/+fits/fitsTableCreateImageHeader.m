function headerBuffer = fitsTableCreateImageHeader(imageHeaderCellArray)   
    % Create a FITS image header block in memory using a mex function
    % Input  : - A 3 column header cell array (Key, Value, Comment)
    % Output : - A uint8 row vector holding the image header block followed
    %            by an empty image block. Use char(...) to read it as text.
    % Author : Chen Tishler (March 2024)
    % Example: headerBuffer = io.fits.fitsTableCreateImageHeader(imageHeader)

    % The mex function returns the buffer when it is called with a single
    % input, and appends it to a file when a file name is given as the
    % second input - see io.fits.fitsTableWriteImageHeader
    headerBuffer = io.fits.mex.mex_fits_table_write_image_header(imageHeaderCellArray);
end
