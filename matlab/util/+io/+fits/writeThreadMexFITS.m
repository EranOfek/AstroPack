function writeThreadMexFITS(fitsFileName, imageMatrix, headerCellArray, Args)
    % Write FITS file using mex function, without cfitsio
    % Input  : - File name
    %          - Image
    %          - A 3 column header cell array (Key, Value, Comment)
    %          * ...,key,val,...
    %            'ReportLongKeys'     - Log keywords with string values > 67 chars
    %                   (those that generate CONTINUE cards and can overflow the header buffer).
    %                   Default is false.
    %            'ReportLongKeysFile' - Log file path for the above. Default is '/tmp/fits_longkeys.log'.
    % Output : -
    % Author : Chen Tishler (March 2024)
    % Example: io.fits.writeThreadMexFITS('myfile.fits', [10 100], Header)
    arguments
        fitsFileName
        imageMatrix
        headerCellArray
        Args.ReportLongKeys logical   = false;
        Args.ReportLongKeysFile       = '/tmp/fits_longkeys.log';
    end

    % Allocate flag matrix, it will be set by the thread to 0x12345678 upon completion
    data = struct;
    data.imageMatrix = imageMatrix;
    data.headerCellArray = headerCellArray;
    data.flagMat = zeros(1, 2, 'uint32');
    dataKeeper = io.fits.DataKeeper(data, data.flagMat, 0x12345678, 30);

    % Add the DataKeeper to the DataManager
    io.fits.DataManager.getSingleton().addDataKeeper(dataKeeper);

    if Args.ReportLongKeys
        Fid = fopen(Args.ReportLongKeysFile, 'a');
        if Fid ~= -1
            fprintf(Fid, '--- %s  nkeys=%d ---\n', fitsFileName, size(headerCellArray, 1));
            for Ik = 1:size(headerCellArray, 1)
                Val = headerCellArray{Ik, 2};
                if ischar(Val) && numel(Val) > 67
                    fprintf(Fid, '  %-8s  len=%-4d  %s\n', headerCellArray{Ik,1}, numel(Val), Val(1:min(80,numel(Val))));
                end
            end
            fclose(Fid);
        end
    end

    io.fits.mex.mex_fits_write_image_thread(fitsFileName, dataKeeper.Data.imageMatrix, dataKeeper.Data.headerCellArray, dataKeeper.FlagMat);
end
