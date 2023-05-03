% Functions and Classes list for the io package
% Author : autogenerated (May 2023)
%                     io.FuncLog - Automatic scope/function entry/exit logger
%                      io.Syslog - 
%                  io.allFunList - Functions and Classes list for the io package
%                      io.msgLog - Log message to singeton MsgLogger
%                    io.msgLogEx - Log MException message to singeton MsgLogger, call from within a 'catch' block
%                io.msgLogStruct - Log struct fields to singeton MsgLogger
%                 io.msgLogThrow - Log message to singeton MsgLogger and throw exception
%                    io.msgStyle - Log message to singleton MsgLogger, with style/color supported by cprintf.m
%                    io.unitTest - Package Unit-Test
%        io.files.addPathToFiles - Concat a path to a list of file names.
%            io.files.allFunList - Functions and Classes list for the io.files package
%               io.files.catfile - Concatenate files into a single file.
% io.files.copy_files_from_dirtree - Copy or movde all files recursively in a directory tree.
%           io.files.create_list - Create a file and a ell array containing a list of files.
%           io.files.delete_cell - Delete a list of files listed in a cell array.
%       io.files.dirSortedByDate - dir command, where the files are sorted by date
%              io.files.dir_cell - dir like command for a cell of file names.
%              io.files.file2str - Read the content of a file into a string or cell vector.
%              io.files.filelist - Generate a cell array array of files list from file name/regular expression
%         io.files.files_arrived - Check if all files in a list arrived to disk (i.e., size not increasing).
%         io.files.files_by_date - Select files by date
%         io.files.for_each_file - Execute a function on a list of files.
%                   io.files.fpf - Easy to use fprintf, with automatic formatting.
%          io.files.fprintf_cell - An fprintf command for a cell vector.
%                io.files.gunzip - Execute gunzip on files, or a cell array of files
%                  io.files.gzip - Execute gzip on files, or a cell array of files
%            io.files.ini2struct - Convert ini file into a structure
%               io.files.inifile - INIFILE Creates, reads, or writes data from/to a standard ini (ascii)
%           io.files.isEmptyFile - Check if files in a list are empty (i.e., have zero size).
%   io.files.list_fun_in_package - Find all functions in a matlab package.
%                 io.files.load2 - Load a mat file into a variable
%            io.files.load_check - Load, but check if variable exist in workspace.
%         io.files.load_from_zip - Extract and load files from a zip file.
%                 io.files.loadh - Load a matrix from HDF5 file.
%                 io.files.mgrep - grep-like utility for MATLAB. Search for substrings in a text file.
%             io.files.moveFiles - Move a list of files with options.
%                  io.files.rdir - recursive dir function
%    io.files.read_delimted_head - Read delimited file with header
%        io.files.read_formatted - Read text with column position format.
%   io.files.read_user_pass_file - Read user/password from file
%        io.files.removeFilePath - Remove path from full file name
%       io.files.replaceFilePath - Replace the path name in a full file name
%                 io.files.saveh - Save a matrix into HDF5 file.
%   io.files.searchNewFilesInDir - Check if file template exist in dir. Also check if additional files
%              io.files.superdir - A 'dir'-like function that can deal with more types of wild cards
%              io.files.unitTest - Package Unit-Test
%                    io.files.wc - Apply the Unix wc (word count) command for a file name.
%                   io.files.wcl - Count the number of lines in a file.
%             io.files.which_dir - Return the directory in which a matlab program resides.
%             io.fits.allFunList - Functions and Classes list for the io.fits package
%        io.fits.dataType2bitpix - Convert class name to BITPIX keyword value number.
%          io.fits.defaultHeader - Generate a default (minimal) FITS header
%   io.fits.generateHeaderBlocks - Given a 3 column cell array of an header, create a string containing
%    io.fits.unittestWriteSimple - Unit test for FITS.writeSimpleFITS(), using in fact io.fits.writeSimpleFITS()
%         io.fits.writeImageData - Write FITS image data matrix to an open file
%        io.fits.writeSimpleFITS - Write a simple (single HDU) FITS file to disk
 help io.allFunList