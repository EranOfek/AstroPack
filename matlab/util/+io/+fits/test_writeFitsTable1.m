import matlab.io.*


% Define the name of the FITS file to create
fileName = 'temp_fits_table_8.fits';

% Delete the file if it already exists to start fresh
if exist(fileName, 'file')
    delete(fileName);
end

% Create a new FITS file
fptr = fits.createFile(fileName);

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


% Create an empty image
img = true;
if img
    imageSize = [0, 0]; % An empty image
    Image = [0];
    fits.createImg(fptr, 'short', size(Image));  %imageSize);
    fits.writeImg(fptr, Image);
    
    % Add header cards with specific names and values
    %fits.writeKey(fptr, 'NAXES', 2, '');
    %fits.writeKey(fptr, 'NAXIS1', 1, '');
    %fits.writeKey(fptr, 'NAXIS2', 1, '');
    fits.writeKey(fptr, 'FLD01', 1, 'Field 1 value');
    fits.writeKey(fptr, 'FLD02', 2, 'Field 2 value');
    fits.writeKey(fptr, 'FLD03', 3, 'Field 3 value');
end


% Close the FITS file
fits.closeFile(fptr);

% Display a completion message
disp('FITS file with binary table and header has been created.');
