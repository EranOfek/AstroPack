%
% io.files.mex.etst_writecsv()
%

function test_writecsv()
    % Test the basic functionality of the writeCSV function

    % Create the test data
    matrices = {rand(5, 3), [1 2 3; 4 5 6], rand(7, 3)};
    headers = {'A', 'B', 'C'};
    FileName = 'output.csv';
    
    % Call the function
    io.files.mex.mex_writecsv_double(matrices, headers, FileName');

    % Call the wrapper
    io.files.writeCsv(matrices, headers, FileName);
    
    % Call the wrapper with int matrix (without cell array)
    io.files.writeCsv(int32([1 2 3; 4 5 6]), headers, FileName);    
    
    % Call the wrapper with int matrix (without cell array)
    p = 3.14159265358979323846;
    io.files.writeCsv(double([p,p,p; p,p,p]), headers, FileName, 'PrecDigits', [4, 8, 15]);

    % Clean up
    delete(FileName);
   
    % Assuming you have an array of objects where each object contains a property 
    % which is a matrix, you can create a cell array containing references to all those matrices.
    
    % n = length(objArray);  % Number of objects in the array
    % matrices = cell(1, n);  % Preallocate cell array
    % for i = 1:n
    %     matrices{i} = objArray(i).matrixProperty;  % Replace 'matrixProperty' with the actual property name
    % end
    
end

