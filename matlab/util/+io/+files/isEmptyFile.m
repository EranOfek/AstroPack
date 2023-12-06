function IsEmpty = isEmptyFile(List)
    % Check if files in a list are empty (i.e., have zero size).
    % Input  : - A cell array of file names or a char array of file name
    %            template (e.g., '*.m').
    % Output : - A logical array indicating, for each file, if it is of
    %            size zero or doesnot exist.
    % Author : Eran Ofek (Dec 2022)
    % Example: IsEmpty = io.files.isEmptyFile('*.mat')
    %          IsEmpty = io.files.isEmptyFile({'a'})

    arguments
        List
    end

    if ischar(List)
        List = io.files.filelist(List);
    end

    DC = io.files.dir_cell(List);

    DC = tools.struct.structEmpty2NaN(DC, 'bytes');

    Bytes = [DC.bytes];
    IsEmpty = Bytes==0 | isnan(Bytes);

end