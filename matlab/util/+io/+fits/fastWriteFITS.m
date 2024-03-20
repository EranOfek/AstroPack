function fastWriteFITS(fitsFileName, imageMatrix, headerCellArray)   
    % Write FITS file using mex function
    % Input  : - File name
    %          - Image [I, J] size.
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.fastWriteFITS('myfile.fits', [10 100], Header)

    io.fits.mex.mex_fast_write_fits(fitsFileName, imageMatrix, headerCellArray);
end
