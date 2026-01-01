function [Result] = copyFileOverNFS(FileNames, RemoteDirName, Args)
    % copy files to a network NFS drive 
    %     May fail if the file name list is too long (> 8-10 thousand file names)
    % Input  : - file name 
    %          - the target directory name
    %          * ...,key,val,... 
    % Output : - none
    % Author : A.M. Krassilchtchikov (2026 Jan) 
    % Example: tools.os.copyFileOverNFS('myfile.fits', '/mnt/euclid/catsHTM/NewCats/', ... 
    %                                   'RemoteUser', 'euclid', 'RemoveOrigin', true);
    % 
    arguments
        FileNames
        RemoteDirName
        Args.RemoteUser        = [];
        Args.LocalDirName      = '.';
        Args.RemoveOrigin      = false;
    end
    %
    FN = '';
    for IFiles = numel(FileNames)
        NewName = sprintf('%s/%s',Args.LocalDirName,FileNames(IFiles));
        FN = strcat(FN,' ',NewName);
    end
    CopyFile = sprintf('su - %s -c "cp -f %s %s"',Args.RemoteUser,FN,RemoteDirName);
    [~, Result] = system(CopyFile);
    if isempty(Result) && Args.RemoveOrigin
        RemoveLocalFile = sprintf('rm %s',FN);
        [~, Result] = system(RemoveLocalFile);
    end
end
