function test_mex_WriteFITS()
    % Test & benchmark writing FITS image using MATLAB fits functions

    test_strings_and_comments();
    %return;

    iterations = 100; 
    width = 1700;
    height = 1700;
    
    % int16
    %imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);

    % single
    imageData = reshape(single(1:height*width), [height, width]);
   
    % Prepare cell array with header fields
    numFields = 200;
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
        io.fits.mex.mex_fits_write_image(filename, imageData, headerFields);
    end       
    elapsedTime = toc;
    averageTime = averageTime + elapsedTime;

    % Calculate and display the average writing time
    averageTime = averageTime / iterations;
    fprintf('Average write time over %d iterations: %f seconds\n', iterations, averageTime);
    
    disp('done.')

end




function test_strings_and_comments()
    % Test & benchmark writing FITS image using MATLAB fits functions

    width = 10;
    height = 10;
    
    % int16
    %imageData = reshape(mod(int16(0:(height*width-1)), 32000) + 1, [height, width]);

    % single
    imageData = reshape(single(1:height*width), [height, width]);
   
    % Prepare cell array with header fields
    numFields = 200;
    headerFields = cell(numFields, 3);

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

    filename = 'temp_fits_file_mex_test2.fits';
    io.fits.mex.mex_fits_write_image(filename, imageData, headerFields);
    
    disp('done.')
end
