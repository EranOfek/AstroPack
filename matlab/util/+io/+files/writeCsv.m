function Result = writeCsv(Matrices, ColNames, FileName)
    % The writeCSV function is a MATLAB MEX function that takes in an array 
    % of 2D matrices, an array of column headers, and a filename as input arguments. 
    % The function writes the content of the input matrices into a CSV file, with the 
    % given column headers. The name of the CSV file is specified by the filename argument.
    % The  function assumes that all input matrices have the same number of columns, 
    % which should match the number of column headers. 
    % If this is not the case, the behavior of the function is undefined.
    % If a value in the input matrices can be represented as an integer, the function 
    % will write it to the CSV file without a decimal point. For example, if a value in 
    % the matrix is 1.0, it will be written to the CSV file as 1.
    % 
    % Input  : - Matrices - cell array of Matrices, must be of the same type (i.e. all double or all float)
    %          - ColNames - cell array of column names
    %          - FileName - Output CSV file name
    % Output : - True on sucsess
    %
    % Author : Chen Tishler (Jul 2023)
    % Example: 
    %   mat1 = [1.0 2.0; 3.0 4.0];
    %   mat2 = [5.0 6.0; 7.0 8.0];
    %   io.files.writeCSV({mat1, mat2}, {'Header1', 'Header2'}, filename);
    %
    %----------------------------------------------------------------------
    arguments
        Matrices           	% cell array of 2D matrixes to write
        ColNames		   	% cell array of column names
        FileName            % Output CSV file name
    end

    if ~iscell(Matrices)
        Matrices = {Matrices};
    end
    
    C = class(Matrices{1});
    switch C
        case {'uint8','int8'}
             io.files.mex.mex_writecsv_int8(Matrices, ColNames, FileName);
        case {'uint16','int16'}
             io.files.mex.mex_writecsv_int16(Matrices, ColNames, FileName);
        case {'uint32','int32'}
             io.files.mex.mex_writecsv_int32(Matrices, ColNames, FileName);
        case {'uint64','int64'}
             io.files.mex.mex_writecsv_int64(Matrices, ColNames, FileName);
        case {'single'}
             io.files.mex.mex_writecsv_single(Matrices, ColNames, FileName);
        case {'double'}
             io.files.mex.mex_writecsv_double(Matrices, ColNames, FileName);
        otherwise
            error('io.files.mex.mex_writecsv.writeCsv - Unsupported data type');
    end
    
    % Just check if output file exists
    Result = isfile(FileName);
end
