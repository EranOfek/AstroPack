function test_mex_WriteTable()
    import matlab.io.*
       
    % Define the name of the FITS file to create
    FileName = 'temp_fits_mex_table_1c.fits';
    
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

    test_strings_and_comments(FileName);
    return;
   
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



function test_strings_and_comments(FileName)
    % Test & benchmark writing FITS image using MATLAB fits functions

    width = 10;
    height = 10;
    
    % int16
    %imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);
 
    % Prepare cell array with header fields
    numFields = 7;
    headerFields = cell(numFields, 3);

    % 
    headerFields{1, 1} = 'NAXIS';
    headerFields{1, 2} = 2;
    headerFields{1, 3} = 'number of data axes';

    %
    headerFields{2, 1} = 'NAXIS1';
    headerFields{2, 2} = 10;
    headerFields{2, 3} = 'length of data axis 1';

    %
    headerFields{3, 1} = 'NAXIS2';
    headerFields{3, 2} = 10;
    headerFields{3, 3} = 'length of data axis 2';    

    %
    headerFields{4, 1} = 'EXPMODE';
    headerFields{4, 2} = 'video';
    headerFields{4, 3} = '';        

    headerFields{5, 1} = 'EXPMODE2';
    headerFields{5, 2} = 'video';
    headerFields{5, 3} = 'with comment abc';            

    headerFields{6, 1} = 'FILENAME';
    headerFields{6, 2} = 'LAST.01.08.03_20221205.031558.595_clear__001_001_001_dark_proc_Image_1.fits';
    headerFields{6, 3} = 'with comment to be ignored';                

    headerFields{7, 1} = '';
    headerFields{7, 2} = '';
    headerFields{7, 3} = 'This FITS file may contain long string keyword values that are continued over multiple keywords. The HEASARC convention uses the character at the end of each substring which is then continued.';

    filename = 'temp_fits_file_table_mex_test2a.fits';
    io.fits.mex.mex_fits_table_write_image_header(headerFields, FileName);
    
    disp('done.')
end

