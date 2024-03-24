function test_fastWriteFITS()

    iterations = 1000; 
    width = 1700;
    height = 1700;
    imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);
    %imageData = reshape(single(1:height*width), [height, width]);
   
    % Prepare cell array with header fields
    numFields = 2;    
    headerFields = cell(numFields, 3);
    for n = 1:numFields
        headerFields{n, 1} = sprintf('FLD%03d', n);
        headerFields{n, 2} = sqrt(1.0/single(n));
        headerFields{n, 3} = [];  %sprintf('%s', '');
    end
    
    averageTime = 0;
    tic;
    for i = 1:iterations
        filename = 'temp_fits_file_mex1.fits';
        io.fits.fastWriteFITS(filename, imageData, headerFields);
    end       
    elapsedTime = toc;
    averageTime = averageTime + elapsedTime;

    % Calculate and display the average writing time
    averageTime = averageTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);
    
    disp('done.')

end
