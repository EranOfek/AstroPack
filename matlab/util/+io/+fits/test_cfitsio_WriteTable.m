function benchmarkFitsWriting_matlab(Obj)
    % Benchmark the writing of a FITS file with specific requirements
    %

    iterations = 1; 
    width = 1700;
    height = 1700;

    % Generate a matrix with a running index that wraps at 32000
    imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);
    %imageData = reshape(single(1:height*width), [height, width]);

    % Create image data
    %imageData = int16(zeros([height, width]));
    %imageData = int16(rand([height, width]));
    %imageData = single(rand([height, width]));

    % Number of header fields
    numFields = 200;
    
    % Prepare cell array with header fields
    headerFields = cell(numFields, 3);
    for n = 1:numFields
        headerFields{n, 1} = sprintf('FLD%03d', n);
        headerFields{n, 2} = sqrt(single(n));
        headerFields{n, 3} = [];  %sprintf('%s', '');
    end

    averageTime = 0;
    tic;
    for i = 1:iterations
        filename = 'temp_fits_file_matlab.fits';
        if isfile(filename)
            delete(filename)
        end
        createAndWriteFits(filename, width, height, imageData, headerFields);
    end       
    elapsedTime = toc;
    averageTime = averageTime + elapsedTime;

    % Calculate and display the average writing time
    averageTime = averageTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);
end


function createAndWriteFits(filename, width, height, imageData, headerFields)
    % Create and write a FITS file with a specific header and data size
    import matlab.io.*

    % Define data size and dimensions
    numKeys = size(headerFields, 1);

    % Create a new FITS file
    fptr = fits.createFile(filename);

    % Create primary array (image)
    fits.createImg(fptr,'short', [height width]);
    %fits.createImg(fptr,'single',[height width]);
    
    for i = 1:numKeys
        key = headerFields{i, 1};
        value = headerFields{i, 2};
        fits.writeKey(fptr, key, value);
    end
    
    % Write the image data
    fits.writeImg(fptr, imageData);

    % Close the file
    fits.closeFile(fptr);
end

