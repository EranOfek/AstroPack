function test_update_matrix_inplace()
    % Test & benchmark

    unittest_update_matrix_inplace();

    for i = 1:100
        %unittest_update_matrix_inplace2();    
    end;

    iterations = 100000;
	
	% Constants for matrix dimensions
    numRowsA = 1703; 
	numColsA = 1705; 
	sizeB = 25;

    % Initialize matrix A with zeros and matrix B with random values
	A = zeros(numRowsA, numColsA, 'double');
	B = rand(sizeB, 'double'); 

	averageMatlabTime = 0;
	averageMexTime = 0;
    averageAvxTime = 0;

    % Loop to perform matrix updates
    for i = 1:iterations        
        % Calculate random dimensions for the submatrix
        deltaRows = floor(sizeB/2 * rand);  % Change in number of rows for submatrix
        deltaCols = floor(sizeB/2 * rand);  % Change in number of columns for submatrix
        
        % Select a random starting point for submatrix in A
        startRowA = ceil(numRowsA/2 * rand); 
		endRowA = startRowA + deltaRows;
        startColA = ceil(numColsA/2 * rand); 
		endColA = startColA + deltaCols;

        % Select a random starting point for submatrix in B
        startRowB = ceil(sizeB/2 * rand); 
		endRowB = startRowB + deltaRows;
        startColB = ceil(sizeB/2 * rand); 
		endColB = startColB + deltaCols;
        
        % 
		tic;
        A(startRowA:endRowA, startColA:endColA) = ...
            A(startRowA:endRowA, startColA:endColA) + ...
            B(startRowB:endRowB, startColB:endColB);
			
		averageMatlabTime = averageMatlabTime + toc;
	
		tic;
		tools.array.mex.mex_update_matrix_inplace_double(A, B, int32(startColA), int32(startRowA), int32(startColB), int32(startRowB), int32(1+deltaCols), int32(1+deltaRows));
		averageMexTime = averageMexTime + toc;		

		tic;
		tools.array.mex.mex_update_matrix_inplace_double_avx2(A, B, int32(startColA), int32(startRowA), int32(startColB), int32(startRowB), int32(1+deltaCols), int32(1+deltaRows));
		averageAvxTime = averageAvxTime + toc;		        
    end

    % Calculate and display the average writing time
    averageMatlabTime = averageMatlabTime / iterations;
	averageMexTime = averageMexTime / iterations;
    averageAvxTime = averageAvxTime / iterations;
	fprintf('Average time: MATLAB: %f, MEX: %f: AVX2: %f\n', averageMatlabTime, averageMexTime, averageAvxTime);
end



function unittest_update_matrix_inplace()

    % Initialize matrix A with zeros and matrix B with random values
	A = zeros(5, 5, 'double');
    C = zeros(5, 5, 'double');
    D = zeros(5, 5, 'double');
    E = zeros(5, 5, 'double');
	B = ones(4, 4, 'double'); 
    disp(A);
    disp(B);

    C(2:4, 2:4) = A(1:3, 1:3) + B(1:3, 1:3);       
    disp(C);
	    
	tools.array.mex.mex_update_matrix_inplace_double(D, B,      int32(2), int32(2), int32(1), int32(1), int32(3), int32(3));	
    disp(D);
    assert(isequal(C, D))

	tools.array.mex.mex_update_matrix_inplace_double_avx2(E, B, int32(2), int32(2), int32(1), int32(1), int32(3), int32(3));
    disp(E);
    assert(isequal(C, E))
end



function unittest_update_matrix_inplace2()

    numRowsA = 1703; 
    numColsA = 1705; 
    sizeB = 25;
    
    % Initialize matrix A with zeros and matrix B with random values
    A = zeros(numRowsA, numColsA, 'double');
    C = zeros(numRowsA, numColsA, 'double');
    D = zeros(numRowsA, numColsA, 'double');
    E = zeros(numRowsA, numColsA, 'double');
    B = ones(sizeB, sizeB, 'double');   %rand(sizeB, 'double'); 

    % Calculate random dimensions for the submatrix
    deltaRows = floor(sizeB/2 * rand);  % Change in number of rows for submatrix
    deltaCols = floor(sizeB/2 * rand);  % Change in number of columns for submatrix
    
    % Select a random starting point for submatrix in A
    startRowA = ceil(numRowsA/2 * rand); 
	endRowA = startRowA + deltaRows;
    startColA = ceil(numColsA/2 * rand); 
	endColA = startColA + deltaCols;

    % Select a random starting point for submatrix in B
    startRowB = ceil(sizeB/2 * rand); 
	endRowB = startRowB + deltaRows;
    startColB = ceil(sizeB/2 * rand); 
	endColB = startColB + deltaCols;

    C(startRowA:endRowA, startColA:endColA) = ...
        A(startRowA:endRowA, startColA:endColA) + ...
        B(startRowB:endRowB, startColB:endColB);
	%disp(C);

    % MEX
	tools.array.mex.mex_update_matrix_inplace_double(D, B, int32(startColA), int32(startRowA), int32(startColB), int32(startRowB), int32(1+deltaCols), int32(1+deltaRows));	
    %disp(D);    
    assert(isequal(C, D));

    % AVX2
	tools.array.mex.mex_update_matrix_inplace_double_avx2(E, B, int32(startColA), int32(startRowA), int32(startColB), int32(startRowB), int32(1+deltaCols), int32(1+deltaRows));
    %disp(E);
    assert(isequal(C, E));
end
