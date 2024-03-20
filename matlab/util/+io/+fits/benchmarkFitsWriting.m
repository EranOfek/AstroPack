function benchmarkFitsWriting(Obj)
    % Benchmark the writing of a FITS file with specific requirements
    %

    iterations = 1; 
    width = 1700;
    height = 1700;

    %dataSizeMB = 120;
    %numElements = (dataSizeMB * (1024^2)) / 4; % 4 bytes per element for float32
    %width = 5120;
    %height = numElements / width;

    % Create image data
    imageData = int16(rand([height, width]));
    %imageData = single(rand([height, width]));

    % Number of header fields
    numFields = 0;  %200;
    
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

    averageTime = 0;
    tic; % Start timer
    for i = 1:iterations
        %tic; % Start timer
        filename = 'temp_fits_file.fits';  %% sprintf('!temp_fits_file_%d.fits', i); % Ensure file is overwritten
        if isfile(filename)
            delete(filename)
        end
        createAndWriteFits(filename, width, height, imageData, headerFields);
        %elapsedTime = toc; % Stop timer and get elapsed time
        %averageTime = averageTime + elapsedTime;
    end
        
    elapsedTime = toc; % Stop timer and get elapsed time
    averageTime = averageTime + elapsedTime;

    % Calculate and display the average writing time
    averageTime = averageTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);
end


function createAndWriteFits(filename, width, height, imageData, headerFields)
    % Create and write a FITS file with a specific header and data size
    import matlab.io.*

    % Define data size and dimensions
    numKeys = 200;

    % Create a new FITS file
    fptr = fits.createFile(filename);

    % Create primary array (image)
    fits.createImg(fptr,'short',[height width]);
    %fits.createImg(fptr,'single',[height width]);
    
    % Add header fields with arbitrary values
    if numel(headerFields) == 0
        for i = 1:numKeys
            key = sprintf('FIELD%d', i);
            value = sprintf('Value%d', i);
            fits.writeKey(fptr, key, value);
        end
    end
    
    % Write the image data
    fits.writeImg(fptr, imageData);

    % Close the file
    fits.closeFile(fptr);

    if numel(headerFields) > 0
        io.fits.mex.mex_write_fits_header(filename, headerFields);
    end
end

