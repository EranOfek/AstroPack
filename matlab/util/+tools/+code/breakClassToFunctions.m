function AllFun = breakClassToFunctions(FileName)
    % Parse all functions from a class m file
    % Input  : - File name or a cell array containing the file content
    %            (file line per cell element).
    % Output : - A structure array of all functions found in class.
    %            The function text in a cell format is in the .Text field.
    % Author : Eran Ofek (May 2022)
    % Example: AllFun = tools.code.breakClassToFunctions(F)
    
    arguments
        FileName
        
    end
    
    % Read the file
    if iscell(FileName)
        SpStr = FileName;
    else
        SpStr = io.files.file2str(FileName, 'cell');
    end
    
    Matched  = regexp(SpStr, '^\s*function', 'match');
    Report.StartPosFunctions = find(~cellfun(@isempty, Matched));
    Nfun     = numel(Report.StartPosFunctions);
    
    MatchedEnd  = regexp(SpStr, '^\s*end', 'match');
    
    LinesFun = find(~cellfun(@isempty, Matched));
    Iend = find(~cellfun(@isempty, MatchedEnd));
    LineEnd = Iend(1:end-2);
    
    for Ifun=Nfun:-1:1
        FunLineStart = LinesFun(Ifun);
        if Ifun==Nfun
            FunLineEnd   = LineEnd(end);
        else
            FunLineEnd   = LineEnd(find(LinesFun(Ifun+1)>LineEnd, 1, 'last'));
        end
        
        AllFun(Ifun).Text = SpStr(FunLineStart:FunLineEnd);
        
    end
    if Nfun==0
        AllFun = struct('Text',cell(0,1));
    end
        
end
