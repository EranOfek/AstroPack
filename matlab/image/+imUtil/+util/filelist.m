function List=filelist(FileName,Method)
% Generate a cell array of files list from file name/regular expression
% Package: @imUtil.util
% Input  : - A file name, a file name containing wild
%            cards or regular expression, a cell array of
%            file names, or a structure arrawy which is the
%            output of the dir command.
%          - Either: 'regexp', or 'wild'.
%            'wild' allows for simple wild cards (default).
%            'regexp' allows for full regular expressions.
% Output : - A cell array of file names.
% Example: List=imUtil.util.filelist('\w*.fits','regexp');

if nargin<2
    Method = 'wild';
end

if ischar(FileName)
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