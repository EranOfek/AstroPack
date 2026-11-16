function Result = writeCsv(Matrices, ColNames, FileName, Args)
    % The writeCSV function is a MATLAB MEX function that takes in an array 
    % of 2D matrices, an array of column headers, and a filename as input arguments. 
    % The function writes the content of the input matrices into a CSV file, with the 
    % given column headers. The name of the CSV file is specified by the filename argument.
    % All non-empty input matrices must have the same type and as many columns as
    % there are column headers, otherwise an error is raised. Empty matrices are skipped.
    % If a value in the input matrices can be represented as an integer, the function 
    % will write it to the CSV file without a decimal point. For example, if a value in 
    % the matrix is 1.0, it will be written to the CSV file as 1.
    % 
    % Input  : - Matrices - cell array of Matrices, must be of the same type (i.e. all double or all float)
    %          - ColNames - cell array of column names
    %          - FileName - Output CSV file name
    %           * Pairs of ...,key,val,...
    %             The following keys are available: 
    %           'PrecDigits' - for single and double numbers, array with
    %           number of precision digits for each column. Must have entry
    %           for each column, i.e. numel(ColNames) == numel(Args.PrecDigits)
    %
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
        Matrices           	 % cell array of 2D matrixes to write
        ColNames		   	 % cell array of column names
        FileName             % Output CSV file name
        Args.PrecDigits = [] % Optional - array
    end

    if ~iscell(Matrices)
        Matrices = {Matrices};
    end
    
    if ~isempty(Args.PrecDigits)
        assert(numel(ColNames) == numel(Args.PrecDigits));
    end
    
    % The mex reads numel(ColNames) columns of every matrix, with the type of
    % the matrix that selects it, and does not check either: a mismatch reads
    % past the end of the data and crashes MATLAB
    IsFilled = ~cellfun(@isempty, Matrices);
    Ncols    = cellfun(@(M) size(M,2), Matrices(IsFilled));
    if any(Ncols ~= numel(ColNames))
        error('io:files:writeCsv:ColumnMismatch', ...
              'Matrices have %d to %d columns, while there are %d column names', ...
              min(Ncols), max(Ncols), numel(ColNames));
    end

    Classes = cellfun(@class, Matrices(IsFilled), 'UniformOutput',false);
    if numel(unique(Classes)) > 1
        error('io:files:writeCsv:TypeMismatch', ...
              'Matrices must have the same type, got: %s', strjoin(unique(Classes), ', '));
    end

    if any(IsFilled)
        C = Classes{1};
    else
        C = class(Matrices{1});
    end
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
             io.files.mex.mex_writecsv_single(Matrices, ColNames, FileName, int32(Args.PrecDigits));
        case {'double'}
             io.files.mex.mex_writecsv_double(Matrices, ColNames, FileName, int32(Args.PrecDigits));
        otherwise
            error('io.files.mex.mex_writecsv.writeCsv - Unsupported data type');
    end
    
    % Just check if output file exists
    Result = isfile(FileName);
end
