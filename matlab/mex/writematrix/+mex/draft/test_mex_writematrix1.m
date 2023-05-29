% Test the original mex_WriteMatrix (mex_WriteMatrix.c)

function test_mex_writematrix()

	% create 'magic square' matrix 5x5
	Z=magic(5);

	% write it to 'magic.txt' 
	mex_WriteMatrix('magic.txt', Z, '%10.10f', ',', 'w+');

	% append it transposed to 'magic.txt' 
	mex_WriteMatrix('magic.txt', Z', '%10.10f', ',', 'a+');

	% free mex function
	clear mex_WriteMatrix;

end
