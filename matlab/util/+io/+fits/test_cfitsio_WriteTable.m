function test_cfitsio_WriteTable()

    % Define the name of the FITS file to create
    fitsFileName = 'mixed_data_table.fits';
    
    % Check if the file already exists and delete it to avoid errors
    if exist(fitsFileName, 'file') == 2
        delete(fitsFileName);
    end
    
    % Define data for 10 rows with various types using running numbers
    % Integer column (int32)
    intCol = (1:10)'; % Running index from 1 to 10
    % Float column (single) - Increasing by 1.5 for each row
    floatCol = single((1.5:1.5:15)');
    % Double precision column - Increasing by 2.5 for each row
    doubleCol = (2.5:2.5:25)';
    % Logical column (using int16 to represent boolean values, 0 or 1)
    % Alternating 0 and 1 for simplicity
    logicalCol = int16(mod(1:10, 2)'); % This will create a pattern of 1, 0, 1, 0, ...
    % String column (character array, fixed length strings are easier to handle)
    % Create strings that are easily identifiable
    stringCol = repmat(' ', 10, 10); % Initialize with spaces
    for i = 1:10
        stringCol(i, :) = sprintf('Str%07d', i);
    end
    
    % Create a FITS file and add a binary table extension
    fits.createFile(fitsFileName);
    fits.createTbl(fitsFileName, 'binary', {'IntCol', 'FloatCol', 'DoubleCol', 'LogicalCol', 'StringCol'}, ...
                   {'1J', '1E', '1D', '1L', '10A'}, 'Mixed Data Table');
    
    % Write the columns
    fits.writeCol(fitsFileName, 1, 1, intCol);
    fits.writeCol(fitsFileName, 2, 1, floatCol);
    fits.writeCol(fitsFileName, 3, 1, doubleCol);
    fits.writeCol(fitsFileName, 4, 1, logicalCol);
    fits.writeCol(fitsFileName, 5, 1, stringCol, 'T'); % The 'T' flag indicates text
    
    % Close the FITS file
    fits.closeFile(fitsFileName);
    
    % Display a success message
    fprintf('Mixed data table successfully written to %s\n', fitsFileName);

end

