% Test the original mex_WriteMatrix (mex_WriteMatrix.c)

function test_mex_writestructcsv()

	A = struct;
	A(1).str = 'string1';
	A(1).int = 1;
	A(1).doub = 0.1;
	A(2).str = 'string2';
	A(2).int = 2;
	A(2).doub = 0.2;
	
	% write it to 'magic.txt' 
	mex_WriteStructCsv('test1.csv', A);

	% free mex function
	clear mex_WriteStructCsv;

end
