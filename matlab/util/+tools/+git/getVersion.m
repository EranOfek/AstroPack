function VerString=getVersion(Args)
    % Return a string with the branch, tag and commit number for AstroPack git
    % Input  : * ...,key,val,...
    %            See code
    % Output : - A string with: <branch>:<tag><commit>
    % AUthor : Eran Ofek (Jun 2023)
    % Example: VerString=tools.git.getVersion

    arguments
        Args.Path                 = 'matlab/AstroPack';
        Args.AddBranch logical    = true;
        Args.AddTag logical       = true;
        Args.AddCommit logical    = true;
    end

    PWD = pwd;
    
    HomePath = tools.os.get_userhome;

    cd(fileparts(mfilename('fullpath')));
    %cd(sprintf('%s%s%s',HomePath,filesep,Args.Path));

    if Args.AddBranch
        [~,Str1] = system('git branch --show-current');
    else
        Str1 = '';
    end
    
    if Args.AddTag
        if Args.AddCommit
            [~,Str2] = system('git describe --tags');
        else
            [~,Str2] = system('git describe --tags --abbrev=0');
        end
    else
        Str2 = '';
    end

    Str1 = regexprep(Str1,'\n','');
    Str2 = regexprep(Str2,'\n','');
    VerString = sprintf('%s:%s',Str1,Str2);
    
    cd(PWD);

end