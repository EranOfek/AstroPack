function test_writecsv()
    % Test the basic functionality of the writeCSV function

    % Create the test data
    matrices = {rand(500000, 3), [1 2 3; 4 5 6], rand(700000, 3)};
    headers = {'A', 'B', 'C'};
    filename = 'output.csv';
    
    % Call the function
    io.files.mex.mex_matrix_writecsv_double(matrices, headers, filename);

    % Check that the file was created
    assert(exist(filename, 'file') == 2, 'Output file was not created');

    return;
    
    % Read the file
    fileContent = fileread(filename);

    % Check the header line
    assert(contains(fileContent, 'A,B,C'), 'File content does not match expected');

    % Clean up
    delete(filename);
   
    % Assuming you have an array of objects where each object contains a property 
    % which is a matrix, you can create a cell array containing references to all those matrices.
    
    % n = length(objArray);  % Number of objects in the array
    % matrices = cell(1, n);  % Preallocate cell array
    % for i = 1:n
    %     matrices{i} = objArray(i).matrixProperty;  % Replace 'matrixProperty' with the actual property name
    % end
end

