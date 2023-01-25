function List=getAllFun(Args)
    % Generate a list of all functions and methods
    % Input  : * ...,key,val,...
    %            'RemoveObsolete' - Remove functions in an 'obsolete' dir.
    %                   Default is true.
    %            'RemoveUnitTest' - Remove unitTest files.
    %                   Default is true.
    %            'RemoveTesting' - Remove 'testing' directory.
    %                   Default is true.
    % Output : - A structure array with a list of all functions and methods
    %            in the AstroPack directory.
    % Author : Eran Ofek (May 2022)
    % Example: List = tools.code.getAllFun
    
    arguments
        Args.RemoveObsolete logical    = true;
        Args.RemoveUnitTest logical    = true;
        Args.RemoveTesting logical     = true;
    end
    
    % get all files
    AllFiles = tools.code.classifyAllFiles;
    
    % remove obsolete
    if Args.RemoveObsolete
        IsO = contains({AllFiles.folder}, 'obsolete');
        AllFiles = AllFiles(~IsO);
    end
    
    % remove unitTest
    if Args.RemoveUnitTest
        IsU = contains({AllFiles.name}, 'unitTest');
        AllFiles = AllFiles(~IsU);
    end
    
    % remove testing
    if Args.RemoveTesting
        IsT = contains({AllFiles.folder}, 'testing');
        AllFiles = AllFiles(~IsT);
    end
    
    % select mat files
    IsM = strcmp({AllFiles.Extension},'.m');
    AllFiles = AllFiles(IsM);
    
    % remove directories %kra: already not needed as we selected ".m" files?
    Flag     = ~[AllFiles.isdir];
    AllFiles = AllFiles(Flag);
    
    
    IsClass = [AllFiles.IsClass];
    AllClasses = AllFiles(IsClass);
    AllFuns    = AllFiles(~IsClass);
    
    % prep all funs
    Nfuns = numel(AllFuns);
    
    I = 0;
    for Ifuns=1:1:Nfuns
        I = I + 1;
        Report = AllFuns(I);
        
        List(I).FullPath        = tools.cell.sprintf_concatCell('/',Report.folder, Report.name); 
        List(I).FunName         = Report.name; 
        % remove .m
        List(I).FunName = regexprep(List(I).FunName, '.m$',''); 
        List(I).FunFullName     = tools.cell.sprintf_concatCell('.',Report.PackNames, Report.name);
        % remove .m
        List(I).FunFullName = regexprep(List(I).FunFullName, '.m$','');
        
        if isempty(strfind(Report.folder, '@'))
            List(I).ClassName       = '';
        else
            % a possible class
            Tmp = regexp(Report.folder, '/(?<ClassName>@\w+)','names');
            List(I).ClassName = Tmp.ClassName(2:end);
        end
        
        List(I).DescriptionLine = Report.DescriptionLine;
        List(I).Help            = Report.Help;
        List(I).Author          = Report.Author;
        List(I).Month           = Report.Month;
        List(I).Year            = Report.Year;
    end
        
    % prep all methods within main class file
    Nclass = numel(AllClasses);
    for Iclass=1:1:Nclass
        for Ifun=1:1:numel(AllClasses(Iclass).ClassFuns)
            I = I + 1;
            Report = AllClasses(Iclass).ClassFuns(Ifun);
            ReportClass = AllClasses(Iclass); 
            
            List(I).FunName         = Report.MainFunName;
            
            if isempty(strfind(AllClasses(Iclass).folder, '@'))
                List(I).ClassName       = '';
            else
                % a possible class
                Tmp = regexp(AllClasses(Iclass).folder, '/(?<ClassName>@\w+)','names');
                List(I).ClassName = Tmp.ClassName(2:end);
            end
            
            List(I).FullPath        = tools.cell.sprintf_concatCell('/',ReportClass.folder, ReportClass.name); 
            List(I).FunFullName     = tools.cell.sprintf_concatCell('.',AllClasses(Iclass).PackNames, ['@', List(I).ClassName], List(I).FunName);
            % remove .m
            List(I).FunFullName = regexprep(List(I).FunFullName, '.m$','');
        
            List(I).DescriptionLine = Report.DescriptionLine;
            List(I).Help            = Report.Help;
            List(I).Author          = Report.Author;
            List(I).Month           = Report.Month;
            List(I).Year            = Report.Year;
        end
    end
    
    
end
