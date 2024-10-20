function genPackage_allFunList(Args)
    % generate a file named allFunList in each subpackage containing the list of functions 
    % Input  : * ...,key,val,...
    %            'FileName_allFunList' - File name of file containing the
    %                   list of functions.
    %                   Default is 'allFunList.m'.
    % Author : Eran Ofek (May 2022)
    % Example: tools.code.genPackage_allFunList
    
    arguments
        Args.FileName_allFunList = 'allFunList.m';
    end
        
    DateNow = datestr(now,'mmm yyyy');
    
    PWD = pwd;
    
    AllPack = tools.code.classifyAllFiles('FileTemplate','+*');
    Npack = numel(AllPack);
    for Ipack=1:1:Npack
        if AllPack(Ipack).isdir
            FullPath = sprintf('%s%s%s', AllPack(Ipack).folder, filesep, AllPack(Ipack).name);
            
            if isempty(AllPack(Ipack).PackNames)
                PackName = AllPack(Ipack).name(2:end);
            else
                PackName = tools.cell.sprintf_concatCell('.', AllPack(Ipack).PackNames, AllPack(Ipack).name(2:end));
            end
            
            AllFiles = tools.code.classifyAllFiles('Path',FullPath);
            Flag     = ~[AllFiles.isdir] & strcmp({AllFiles.Extension},'.m');
            AllFiles = AllFiles(Flag);
            Nfiles   = numel(AllFiles);
            Text  = sprintf('%% Functions and Classes list for the %s package\n%% Author : autogenerated (%s)\n',PackName,DateNow);
            for Ifiles=1:1:Nfiles
                FunName = tools.cell.sprintf_concatCell('.',AllFiles(Ifiles).PackNames, AllFiles(Ifiles).name(1:end-2));
                Text = sprintf('%s%% %30s - %s\n', Text, FunName, AllFiles(Ifiles).DescriptionLine);
            end
            Text = sprintf('%s help %s',Text, PackName);
            cd(FullPath);
            FID = fopen(Args.FileName_allFunList,'w');
            fprintf(FID,'%s.%s',Text, Args.FileName_allFunList(1:end-2));
            fclose(FID);
        end
    end
    
    cd(PWD);

end
