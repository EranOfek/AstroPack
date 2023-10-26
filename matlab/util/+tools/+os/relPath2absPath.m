function NameAbs = relPath2absPath(NameRel)
    % make the path of a file or of a directory absolute if it is relative
    % Input: - a file name or a directory name
    % Output: -- the same as input but with a full path
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: FN = '~/matlab/data/spec/../a.fits'; FN1 = tools.os.relPath2absPath(FN)
    %          DN = '~/matlab/data/../AstroPack/../data/'; DN1 = tools.os.relPath2absPath(DN)
    %
    if isfolder(NameRel)
%         NameRel=strtrim(ls('-d',NameRel));
        cd(NameRel); NameAbs = pwd;
    else 
       [Fdir,Fname,Fext] = fileparts(NameRel);
       FullDir = strtrim(ls('-d',Fdir));
       cd(FullDir); FullDir = pwd;
       NameAbs = strcat(FullDir,'/',Fname,Fext);
    end    
end
