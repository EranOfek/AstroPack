% Number of header fields
numFields = 200;

% Initialize the cell array for header fields
headerFields = cell(numFields, 1);

% Populate the cell array with example header fields
for i = 1:numFields
    key = sprintf('KEY%d', i);
    value = sprintf('Value%d', i);
    comment = sprintf('Comment %d', i);
    
    % Construct the header field string
    % Note: The total length of each string should not exceed 80 characters
    % This example does not enforce the 80-character limit or format precisely as per FITS standard
    headerField = sprintf('%-8s= %-20s / %s', key, value, comment);
    
    % Ensure the string is exactly 80 characters (for demonstration)
    % In real applications, make sure the formatting complies with FITS standards
    headerField = [headerField, repmat(' ', 1, 80 - length(headerField))];
    
    % Assign the header field to the cell array
    headerFields{i} = headerField;
end

% Display the first few header fields for verification
disp(headerFields(1:5));

fitsFileName = 'C:\Ultrasat\AstroPack.git\matlab\temp_fits_file.fits';

% Call the MEX function with the file name and header fields
% Replace 'writeFitsHeader' with the actual name of your compiled MEX file
io.fits.mex.mex_write_fits_header(fitsFileName, headerFields);

disp('done.')
