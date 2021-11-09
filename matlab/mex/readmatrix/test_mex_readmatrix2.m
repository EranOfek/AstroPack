% Test the improved mex_WriteMatrix2 (mex_WriteMatrix2.c)
function test_mex_writematrix2()

	% create 'magic square' matrix 5x5
	Z = rand(3, 2);

    friends(1).name  = 'Jordan Robert';
    friends(1).phone = '3386';
    friends(2).name  = 'Mary Smith';
    friends(2).phone = '3912';
    friends(3).name  = 'Stacy Flora';
    friends(3).phone = '3238';

	header = 'name,phone,col1,col2,col3';
	
	% write it to 'magic.txt' 
	mex_WriteMatrix2('Z.csv', Z, '%.5f', ',', 'w+', header, friends);


	% free mex function
	clear mex_WriteMatrix2;

end
