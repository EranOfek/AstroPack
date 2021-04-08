function List=filelist(FileName,UseRegExp)
% Generate a cell array array of files list from file name/regular expression
% Package: @imUtil.util
% Input  : - A file name, a file name containing wild
%            cards or regular expression, a cell array of
%            file names, or a structure arrawy which is the
%            output of the dir command.
%          - A logical indicating if to use regular expression (true) or
%            wild cards (false). Default is false.
% Output : - A cell array of file names.
% Author : Eran Ofek (Apr 2020)
% Example: List=io.files.filelist('\w*.fits',true);

arguments
    FileName
    UseRegExp(1,1) logical      = false;
end

if ischar(FileName) || isstring(FileName)
    if UseRegExp
        Files = dir('*');
        FilesCell = fullfile({Files.folder},{Files.name});
        Tmp  = regexp(FilesCell,FileName,'match');
        Flag = ~cellfun(@isempty,Tmp);
        List = FilesCell(Flag);    
    else
        Files = dir(FileName);
        List  = fullfile({Files.folder},{Files.name});
    end
    
elseif isstruct(FileName)
    List = {FileName.name};
elseif iscell(FileName)
    List = FileName;
else
    error('Unknown file names input');
end
