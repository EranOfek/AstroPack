function List=filelist(FileName,Method)
% Generate a cell array array of files list from file name/regular expression
% Package: @imUtil.util
% Input  : - A file name, a file name containing wild
%            cards or regular expression, a cell array of
%            file names, or a structure arrawy which is the
%            output of the dir command.
%          * ...,key,val,...
%            'Method' - Either: 'regexp', or 'wild'.
%               'wild' allows for simple wild cards (default).
%               'regexp' allows for full regular expressions.
%            'StringOutput' - Indicate if to convert the output into a
%               string array (otherwise a cell array). Default is true.
% Output : - A cell array of file names.
% Author : Eran Ofek (Apr 2020)
% Example: List=io.files.filelist('\w*.fits','regexp');

arguments
    FileName
    Method char          = 'wild';
end

if ischar(FileName) || isstring(FileName)
    switch lower(Method)
        case 'wild'
            Files = dir(FileName);
            List  = fullfile({Files.folder},{Files.name});
        case 'regexp'
            Files = dir('*');
            FilesCell = fullfile({Files.folder},{Files.name});
            Tmp  = regexp(FilesCell,FileName,'match');
            Flag = ~cellfun(@isempty,Tmp);
            List = FilesCell(Flag);
        %case 'superdir'
            
        otherwise
            error('Unknown Method option - need to be regexp | superdir');
    end
elseif isstruct(FileName)
    List = {FileName.name};
elseif iscell(FileName)
    List = FileName;
else
    error('Unknown file names input');
end
