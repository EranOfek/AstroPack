function test_mex_WriteFITS_thread()
    % Test & benchmark writing FITS image using MATLAB fits functions

    iterations = 10; 
    width = 17000;
    height = 17000;
    
    % int16
    %imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);

    % single
    imageData = reshape(single(1:height*width), [height, width]);
   
    % Prepare cell array with header fields
    numFields = 2000;
    headerFields = cell(numFields, 3);
    for n = 1:numFields
        headerFields{n, 1} = sprintf('FLD%03d', n);
        headerFields{n, 2} = sqrt(1.0/single(n));
        headerFields{n, 3} = [];  %sprintf('%s', '');
    end
    
    averageTime = 0;
    tic;
    for i = 1:iterations
        filename = sprintf('temp_fits_file_mex_thread_%03d.fits', i);
		%io.fits.mex.mex_fits_write_image(filename, imageData, headerFields);
        %io.fits.mex.mex_fits_write_image_thread(filename, imageData, headerFields);
        io.fits.fitsWriteImageThread(filename, imageData, headerFields)   
    end       
    elapsedTime = toc;
    averageTime = averageTime + elapsedTime;

    % Calculate and display the average writing time
    averageTime = averageTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);
    
    fprintf('pause...\n');
    for i=1:10
        pause(1);
        io.fits.DataManager.getSingleton().scanAndRelease();
    end

    %fprintf('%d, %d\n', numel(imageData), numel(headerFields));
    %imageData = [];
    %headerFields = [];        
    disp('done.')

end
