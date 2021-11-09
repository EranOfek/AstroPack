# Title: max_readmatrix

### Chen Comments - 

MEX - accessing structure of arrays

https://www.mathworks.com/matlabcentral/answers/63174-mex-accessing-structure-of-arrays


Separate CSV to stucture array by a specified field

https://www.mathworks.com/matlabcentral/answers/249240-separate-csv-to-stucture-array-by-a-specified-field

	%read data with textscan
	fid = fopen('somefile.txt', 'rt');
	c = textscan(fid, '%f, %f, %s', 'HeaderLines', 1);  
	fclose(fid);
	%or as a table
	t = readtable('somefile.txt');

	%partition the data, 
	%using a table here. replace t.x, t.y, t.ZLEVEL, by c{1}, c{2}, c{3} if using textscan
	[levels, ~, pos] = unique(t.ZLEVEL);  %get partitioning
	xygrouped = accumarray(pos, 1:numel(pos), [], @(v) {[t.x(v), t.y(v)]}) %defer to accumarray to do the grouping
	sarray = struct('XYData', xygrouped, 'ZLEVEL', levels)  %and convert to struct


# Description

Function to write matrix contents to file. Replacement for (slow) dlmwrite in MATLAB

### Files in this folder

- mex_writematrix.md - This file
- build1.bat
- build2.bat                   
- mex_WriteMatrix0.c
- mex_WriteMatrix1.c           
- mex_WriteMatrix2.c
- mex_phonebook.c
- test_mex_writematrix1.m
- test_mex_writematrix2.m      
- test_phonebook.m
              
			  
			  
### .gitignore			  

	# Compiled MEX files
	*.mexw64

### mex_WriteMatrix

    mex_WriteMatrix(filename,matrix,format,delimiter, writemode);

Parameters:

    filename  - full path for CSV file to export 
    matrix    - matrix of type 'double' values to be exported
    format    - format of export (sprintf) , e.g. '%10.6f'
    delimiter - delimiter, for example can be ',' or ';' or sprintf('\t')
    writemode - write mode 'w+' for rewriting file 'a+' for appending
 

### mex_WriteMatrix2

    mex_WriteMatrix(filename,matrix,format,delimiter, writemode);

Parameters:

    filename  - full path for CSV file to export 
    matrix    - matrix of type 'double' values to be exported
    format    - format of export (sprintf) , e.g. '%10.6f'
    delimiter - delimiter, for example can be ',' or ';' or sprintf('\t')
    writemode - write mode 'w+' for rewriting file 'a+' for appending



## Compiling

build.bat

	call mex mex_WriteMatrix.c -lut
	call mex phonebook.c -lut
	
	
build2.bat


	call mex mex_WriteMatrix2.c -lut




## Testing

### mex_WriteMatrix()

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



### mex_WriteMatrix2 

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



