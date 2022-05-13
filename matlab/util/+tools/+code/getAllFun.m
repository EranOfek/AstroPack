function List=getAllFun
    % Generate a list of all functions and methods
    % Output : - A structure array with a list of all functions and methods
    %            in the AstroPack directory.
    % Author : Eran Ofek (May 2022)
    % Example: List = tools.code.getAllFun
    
    % get all files
    AllFiles = tools.code.classifyAllFiles;
    
    % select mat files
    IsM = strcmp({AllFiles.Extension},'.m');
    AllFiles = AllFiles(IsM);
    
    % remove directories
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
        
        List(I).FunName         = Report.name;
        List(I).FunFullName     = tools.cell.sprintf_concatCell('.',Report.PackNames, Report.name);
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
            Report = AllClasses(Iclass).ClassFuns(Ifun);
            
            
            List(I).FunName         = AllClasses(Iclass).name;
            
            if isempty(strfind(AllClasses(Iclass).folder, '@'))
                List(I).ClassName       = '';
            else
                % a possible class
                Tmp = regexp(AllClasses(Iclass).folder, '/(?<ClassName>@\w+)','names');
                List(I).ClassName = Tmp.ClassName(2:end);
            end

            List(I).FunFullName     = tools.cell.sprintf_concatCell('.',AllFiles(Iclass).PackNames, '@', List(I).ClassName, AllClasses(Iclass).name);
            
            List(I).DescriptionLine = Report.DescriptionLine;
            List(I).Help            = Report.Help;
            List(I).Author          = Report.Author;
            List(I).Month           = Report.Month;
            List(I).Year            = Report.Year;
        end
    end
    
    
end
