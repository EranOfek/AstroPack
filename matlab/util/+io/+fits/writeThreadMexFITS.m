function writeThreadMexFITS(fitsFileName, imageMatrix, headerCellArray)   
    % Write FITS file using mex function, without cfitsio
    % Input  : - File name
    %          - Image
    %          - A 3 column header cell array (Key, Value, Comment)
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.writeThreadMexFITS('myfile.fits', [10 100], Header)

    % Allocate flag matrix, it will be set by the thread to 0x12345678 upon completion
    data = struct;
    data.imageMatrix = imageMatrix;
    data.headerCellArray = headerCellArray;
    data.flagMat = zeros(1, 2, 'uint32');
    dataKeeper = io.fits.DataKeeper(data, data.flagMat, 0x12345678, 30);

    % Add the DataKeeper to the DataManager   
    io.fits.DataManager.getSingleton().addDataKeeper(dataKeeper);

    io.fits.mex.mex_fits_write_image_thread(fitsFileName, dataKeeper.Data.imageMatrix, dataKeeper.Data.headerCellArray, dataKeeper.FlagMat);
end
