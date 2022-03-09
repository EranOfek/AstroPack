% Test the original mex_WriteMatrix (mex_WriteMatrix.c)

function test_mex_WriteStructCsv()

	A = struct;
    for i=1:200000
        A(i).str = sprintf('string %d hello', i);
        A(i).int8 = int32(-i);
        A(i).uint8 = int32(i);
        A(i).int16 = int32(-i);
        A(i).uint16 = int32(i);    
        A(i).int32 = int32(-i);
        A(i).uint32 = int32(i);
        A(i).int64 = int32(-i);
        A(i).uint64 = int32(i);    
        A(i).fsingle = 0.1 * i;
        A(i).fdouble = 0.111 * i;        
    end
    
	% write it to 'magic.txt'
    t1 = tic();
	mex_WriteStructCsv(A, 'test1.csv');
    t2 = toc(t1);
    fprintf(' mex_WriteStructCsv: %f\n', t2); 

	% free mex function
	clear mex_WriteStructCsv;

end
