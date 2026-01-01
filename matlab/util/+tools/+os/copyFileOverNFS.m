function [Result] = copyFileOverNFS(FileNames, RemoteDirName, Args)
    % copy files to a network NFS drive with user spoofing
    %     May fail if the file name list is too long (> 8-10 thousand file names)
    % Input  : - a file name or a cell array of file names
    %          - the target directory name
    %          * ...,key,val,... 
    %          'RemoteUser' - the local name of the target directory owner (UID local = UID remote)
    %          'LocalDirName' - the name of the local directory (current by default)
    %          'RemoveOrigin' - move files instead of copy (false by default)
    %          'MakeRemoteDir' - make the remote dir if it does not exist
    % Output : - copied (or moved) files
    % Author : A.M. Krassilchtchikov (2026 Jan) 
    % Example: tools.os.copyFileOverNFS('myfile.fits', '/mnt/euclid/catsHTM/NewCats/', 'RemoteUser', 'euclid');
    %          FileNames = {'file1.hdf5', 'file2.hdf5', 'file3.hdf5'}; 
    %          TargetDir = '/mnt/euclid/catsHTM/NewCats/GAIA/DR3var/';
    %          tools.os.copyFileOverNFS(FileNames, TargetDir, 'RemoteUser', 'euclid', 'RemoveOrigin', true);
    arguments
        FileNames
        RemoteDirName
        Args.RemoteUser        = [];
        Args.LocalDirName      = '.';
        Args.RemoveOrigin      = false;
        Args.MakeRemoteDir     = false;
    end
    %
    FN = '';
    for IFiles = 1:numel(FileNames)
        NewName = sprintf('%s/%s ',Args.LocalDirName,FileNames{IFiles});
        FN = strcat(FN," ",NewName);
    end
    if Args.MakeRemoteDir
        MakeDir = sprintf('su %s -c "mkdir -p %s"',Args.RemoteUser,RemoteDirName);
        [~, Error] = system(MakeDir);
        if ~isempty(Error)
            error('Remote directory cannot be created');
        end
    end
    CopyFile = sprintf('su %s -c "cp -f %s %s"',Args.RemoteUser,FN,RemoteDirName);
    [~, Result] = system(CopyFile);
    if isempty(Result) && Args.RemoveOrigin
        RemoveLocalFile = sprintf('rm %s',FN);
        [~, Result] = system(RemoveLocalFile);
    end
end
