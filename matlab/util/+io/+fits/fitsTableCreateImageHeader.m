function fitsTableCreateImageHeader(imageHeaderCellArray)   
    % Write FITS file using mex function
    % Input  : - File name
    %          - Image [I, J] size.
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : Buffer with image header (and image block)
    % Author : Chen Tishler (March 2024)
    % Example: headerBuffer = io.fits.fitsTableCreateImageHeader(imageHeader)

    io.fits.mex.mex_fits_table_write_image_header(imageHeaderCellArray, fitsFileName);
end
