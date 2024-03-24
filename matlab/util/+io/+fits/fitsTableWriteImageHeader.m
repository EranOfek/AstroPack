function fitsTableWriteImageHeader(fitsFileName, imageHeaderCellArray)   
    % Write FITS file using mex function
    % Input  : - File name
    %          - Image [I, J] size.
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.fitsTableWriteImageHeader(imageHeader, 'mytable.fits')

    io.fits.mex.mex_fits_table_write_image_header(imageHeaderCellArray, fitsFileName);
end
