function NameAbs = relPath2absPath(NameRel)
    % make the path of a file or of a directory absolute if it is relative
    % Input: - a file name or a directory name
    % Output: -- the same as input but with a full path
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: FN = '~/matlab/data/a.fits'; FN1 = tools.os.relPath2absPath(FN)
    %          DN = '~/matlab/data/../'; DN1 = tools.os.relPath2absPath(DN)
    %
    if isfolder(NameRel)
        NameAbs=strtrim(ls('-d',NameRel));
    else 
       [Fdir,Fname,Fext] = fileparts(NameRel);
       FullDir = strtrim(ls('-d',Fdir));
       NameAbs = strcat(FullDir,'/',Fname,Fext);
    end
    
end
