function NewFile = replaceFilePath(File,NewPath)
    % Replace the path name in a full file name
    % Input  : - Full file name or a cell of full file names.
    %          - New path.
    % Output : - New file names with new path.
    % Author : Eran Ofek (Sep 2022)
    % Example: io.files.replaceFilePath('/a/v/d/aaa.fits','/b/b/')
    %          io.files.replaceFilePath({'/a/v/d/aaa.fits','/a/e/d/bbb.fits'},'/b/b/')
   
    
    [Path] = fileparts(File);
    NewFile = regexprep(File, Path, NewPath);
    NewFile = regexprep(NewFile,sprintf('%s%s',filesep,filesep),filesep);
    
end