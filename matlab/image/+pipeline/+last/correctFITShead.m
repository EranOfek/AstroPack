function [Result] = correctFITShead(RootDir, FileNameTemplate, Keys, Args)
    % correct FITS file headers in the LAST archive
    %     Optional detailed description
    % Input  : - root directory from where to check the headers
    %          - template of a file name
    %          - a cell array of keywords and values 
    %          * ...,key,val,... 
    % Output : - corrected keywords in the FITS file headers
    % Author : A.M. Krassilchtchikov (2024 Nov) 
    % Example: RootDir = '/Data1/LAST.01.01.01/'; 
    %          Template = '*coadd*Ima*fits';
    %          Keys = {'NODENUMB', 1, 'node number' ; 'MOUNTNUM', 1, 'mount number'};
    %          pipeline.last.correctFITShead(RootDir,Template,Keys)
    %
    arguments
        RootDir
        FileNameTemplate        
        Keys                   
        Args.ProcDirTemplate   = '*/*/*/proc/*';        
    end
    % find all the directories according to the template
    D = dir(fullfile(RootDir, Args.ProcDirTemplate));
    Dirs = D([D.isdir]);
    Dirs = Dirs(~ismember({Dirs.name}, {'.', '..'})); 
    % 
    N = numel(Dirs);
    for i = 1:N
        ProcFolder = strcat(Dirs(i).folder,'/',Dirs(i).name);
        FITS.correctHeaders(ProcFolder,FileNameTemplate,Keys,'CheckKeyExist',true);
    end

end
