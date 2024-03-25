function test_mex_WriteTable()
    import matlab.io.*
    
    
    % Define the name of the FITS file to create
    FileName = 'temp_fits_mex_table_1.fits';
    
    % Delete the file if it already exists to start fresh
    if exist(FileName, 'file')
        delete(FileName);
    end
    
    % Create a new FITS file
    fptr = fits.createFile(FileName);
    
    % Create a binary table with 10 rows, each with 3 columns of int data
    NRows = 10;
    Col1 = (1:NRows)'; % Running index for column 1
    Col2 = (11:20)';   % Running index for column 2, just as an example
    Col3 = (21:30)';   % Running index for column 3, just as an example
    ColNames = {'Col1', 'Col2', 'Col3'};
    ColFormats = {'1J', '1J', '1J'}; % '1J' format for 32-bit integer data
    ColUnits = {'', '', ''}; % Units, empty in this case
    fits.createTbl(fptr, 'binary', NRows, ColNames, ColFormats, ColUnits, 'ExampleTable');
    
    % Write the columns to the binary table
    fits.writeCol(fptr, 1, 1, Col1);
    fits.writeCol(fptr, 2, 1, Col2);
    fits.writeCol(fptr, 3, 1, Col3);
    
    % Close the FITS file
    fits.closeFile(fptr);

    % Prepare cell array with header fields
    numFields = 300;
    headerFields = cell(numFields, 3);
    for n = 1:numFields
        headerFields{n, 1} = sprintf('FLD%03d', n);
        headerFields{n, 2} = n;  %sqrt(1.0/single(n));
        headerFields{n, 3} = [];  %Comment';
    end

    io.fits.mex.mex_fits_table_write_image_header(headerFields, FileName);

    % Display a completion message
    disp('FITS file with binary table and header has been created.');
end

