# Package: tools.latex


### tools.latex.latex_table

Create a latex table from a data given in a cell array. Package: Util.LaTex Description: Create a latex table from a data given in a cell array.


    
    Create a latex table from a data given in a cell array.  
    Package: Util.LaTex  
    Description: Create a latex table from a data given in a cell array.  
    Input  : - Output file name.  
    - Cell array of data (see colvec2cellarray.m).  
    - Header cell-array, contains header for each column.  
    - cell of strings of format for each column.  
    - Table title, default is empty string.  
    - File name containing the caption text, default is no caption.  
    - Column position (e.g., 'clrc') one character per column.  
    'c' for center; 'l' for left; 'r' for right.  
    Default is string of 'c' (in length equal to number of columns.  
    Output : The output is written to the output file name.  
    Tested : Matlab 5.3  
    By : Eran O. Ofek          October 2001  
    URL : http://wise-obs.tau.ac.il/~eran/matlab.html  
    Example: C=colvec2cellarray([1;2;3],['a';'b';'c']);  
    Header=cell(2,2);  
    Header{1,1} = 'Num'; Header{1,2}='Str'; Header{2,2}='[km]';  
    Format=cell(1,2);  
    Format{1} = '$7.2f$'; Format{2}='s';  
    Util.LaTex.latex_table('a.out',C,Header,Format,'Table 1.1','a.try','lr');  
    Reliable: 2  
      
      
