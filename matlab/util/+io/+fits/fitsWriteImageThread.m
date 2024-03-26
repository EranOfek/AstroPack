function fitsWriteImageThread(fitsFileName, imageMatrix, headerCellArray)   
    % Write FITS file using mex function, without cfitsio
    % Input  : - File name
    %          - Image
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.fitsWriteImage('myfile.fits', [10 100], Header)

    dataKeeper = io.fits.DataKeeper(imageMatrix, headerCellArray, 5); % 5 seconds until data can be released

    % Add the DataKeeper to the DataManager   
    io.fits.DataManager.getSingleton().addDataKeeper(dataKeeper);

    io.fits.mex.mex_fits_write_image_thread(fitsFileName, imageMatrix, headerCellArray);
end
