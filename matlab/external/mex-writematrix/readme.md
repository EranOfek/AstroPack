
Description:
    Function to write matrix contents to file. Replacement for (slow) dlmwrite in MATLAB

Usage:
    mex_WriteMatrix(filename,matrix,format,delimiter, writemode);

Parameters:
*    filename  - full path for CSV file to export 
*    matrix    - matrix of type 'double' values to be exported
*    format    - format of export (sprintf) , e.g. '%10.6f'
*    delimiter - delimiter, for example can be ',' or ';' or sprintf('\t')
*    writemode - write mode 'w+' for rewriting file 'a+' for appending
 
