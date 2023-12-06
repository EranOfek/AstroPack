function PackNames = identifySubPackagesInFolder(Folder, KeepPlus)
    % Identify all packages hierarchy in a folder name
    % Input  : - A folder name (a char array). Default is pwd.
    %          - A logical indicating if to keep the '+' in the package
    %            name. Default is false.
    % Output : - A cell array of packages identified in the folder name.
    % Author : Eran Ofek (May 2022)
    % Example: PackNames = tools.code.identifySubPackagesInFolder(Folder)
    
    arguments
        Folder             = pwd;
        KeepPlus logical   = false;
    end
    
    SpTxt  = regexp(Folder, filesep , 'split');
    IsPack = contains(SpTxt,'+');
    PackNames = SpTxt(IsPack);
    if ~KeepPlus
        PackNames = strrep(PackNames,'+','');
    end
end