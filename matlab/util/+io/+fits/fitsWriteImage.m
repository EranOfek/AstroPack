function fitsWriteImage(fitsFileName, imageMatrix, headerCellArray)   
    % Write FITS file using mex function, without cfitsio
    % Input  : - File name
    %          - Image
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.fitsWriteImage('myfile.fits', [10 100], Header)

    io.fits.mex.mex_fits_write_image(fitsFileName, imageMatrix, headerCellArray);
end
