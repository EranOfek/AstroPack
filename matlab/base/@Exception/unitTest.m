function Result = unitTest()
    % Exception.unitTest
    
    % Note that we cannot use here any of the msgLog() functions,
    % as we validate that LogFile works without any dependencies
    fprintf('Exception test started\n');

    Catched = false;
    try
        assert(isnumeric(idx),'MYFUN:notNumeric', 'Indexing array is not numeric.')
    catch Ex
        Catched = true;
        getReport(Ex)
    end
    assert(Catched == true);
    
    % Should NOT throw
    Catched = false;
    try
        A(1) = 1;        
        indexIntoArray(A, 1);
    catch Ex
        Catched = true;
        getReport(Ex)
        error('Should not throw');
    end
    assert(Catched == false);
    
    % Index out of range
    Catched = false;
    try
        A(1) = 1;        
        indexIntoArray(A, 2);
    catch Ex
        Catched = true;
        getReport(Ex)
    end
    assert(Catched == true);
    
    % Missing argument
    Catched = false;
    try
        A(1) = 1;        
        indexIntoArray(A);
    catch Ex
        Catched = true;
        getReport(Ex)
    end
    assert(Catched == true);
    
    % Index size
    Catched = false;
    try
        A(1) = 1;
        Idx(1) = 1;
        Idx(2) = 2;
        indexIntoArray(A, Idx);
    catch Ex
        Catched = true;
        getReport(Ex)
    end
    assert(Catched == true);
    
    
    % Done
    fprintf('Exception test passed\n');
    Result = true;
end


% From: https://www.mathworks.com/help/matlab/matlab_prog/throw-an-exception.html
function indexIntoArray(A,idx)

    % 1) Detect the error.
    try
        A(idx)
    catch

        % 2) Construct an MException object to represent the error.
        errID = 'MYFUN:BadIndex';
        msg = 'Unable to index into array.';
        baseException = MException(errID,msg);

        % 3) Store any information contributing to the error. 
        if nargin < 2 
            causeException = MException('MATLAB:notEnoughInputs','Not enough input arguments.');
            baseException = addCause(baseException,causeException);

            % 4) Suggest a correction, if possible.
            if(nargin > 1) 
                exceptionCorrection = matlab.lang.correction.AppendArgumentsCorrection('1');
                baseException = baseException.addCorrection(exceptionCorrection);
            end

            throw(baseException);
        end

        try
            assert(isnumeric(idx),'MYFUN:notNumeric', ...
                'Indexing array is not numeric.')
        catch causeException
            baseException = addCause(baseException,causeException);
        end

        if any(size(idx) > size(A))
            errID = 'MYFUN:incorrectSize';
            msg = 'Indexing array is too large.';
            causeException2 = MException(errID,msg);
            baseException = addCause(baseException,causeException2);
        end

        % 5) Throw the exception to stop execution and display an error
        % message.
        throw(baseException)
    end
end
