function NewFile = removeFilePath(File)
    % Remove path from full file name
    % Input  : - Full file name or a cell of file names.
    % Output : - A cell array of file names without the path.
    % Author : Eran Ofek (Sep 2022)
    % Example: io.files.removeFilePath('/a/a/as')
    
    Pat = sprintf('(?<=\\%s)[^\\%s]+$', filesep, filesep);
    %NewFile = regexp(File,'(?<=\\)[^\\]+$','match');
    NewFile = regexp(File,Pat,'match');

    
    
end