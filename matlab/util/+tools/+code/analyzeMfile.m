function Report = analyzeMfile(FileName, Args)
    % Analyze m files. Count lines of code, identify functions, help, authors etc.
    % Input  : - File name or a cell array containing the file content
    %            (file line per cell element).
    %          * ...,key,val,...
    %            'CountLines' - A logical indicating if to count lines.
    %            'HelpInHead' - A logical indicating if the elpmsection is
    %                   in the first line of the file (true), or after the
    %                   function statement (false). Default is false.
    % Output : - A structure containing the report, including the following
    %            fields:
    %            .IsClass - Is a class.
    %            .StartPosFunctions
    %            .NumFunctions - Number of functions in file.
    %            .Lines - A structure containing line counting parameters.
    %            .DescriptionLine
    %            .Help
    %            .HelpNumLines
    % Author : Eran Ofek (May 2022)
    % Report = tools.code.analyzeMfile(a)
   
    arguments
        FileName
        Args.CountLines logical       = true;
        %Args.HelpInHead logical       = false;
    end
    
    % Read the file
    if iscell(FileName)
        SpStr = FileName;
    else
        SpStr = io.files.file2str(FileName, 'cell');
    end
    %SpStr = regexp(StrFile,'\n','split');
    
    % check if a class
    Report.IsClass = tools.code.isClass(SpStr);
    
    % count number of functions
    if ~iscellstr(SpStr)
        error('Error in file: %s',FileName);
    end
    Matched  = regexp(SpStr, '^\s*function', 'match');
    Report.StartPosFunctions = find(~cellfun(@isempty, Matched));
    Report.NumFunctions      = sum(~cellfun(@isempty, Matched));
    
    if isempty(SpStr)
        % empty file?
        Report.MainFunName = '';
    else
        if Report.IsClass
            % look for classdef name
            MatchedName  = regexp(SpStr, '^\s*classdef\s*(?<FunName>\w+)', 'names');
            MatchedName  = MatchedName(~cellfun(@isempty, MatchedName));
            if isempty(MatchedName)
                Report.MainFunName = '';
            else
                Report.MainFunName = MatchedName{1}.FunName;
            end
        else
            % look for fun name in the first line (including get/set)
            MatchedName  = regexp(SpStr{1}, '^\s*function\s*\[?.*\]?\s*=\s*(?<FunName>\w+\.?\w*)', 'names');
            if isempty(MatchedName)
                MatchedName  = regexp(SpStr{1}, '^\s*function\s*(?<FunName>\w+\.?\w*)', 'names');
                if isempty(MatchedName)
                    Report.MainFunName = '';
                else
                    Report.MainFunName = MatchedName.FunName;
                end
            else
                Report.MainFunName = MatchedName.FunName;
            end
        end
    end
    
    % count number of lines
    if Args.CountLines
        Report.Lines.Total    = numel(SpStr);
        Report.Lines.NonEmpty = sum(~cellfun(@isempty, SpStr));
        Report.Lines.Comments = sum(~cellfun(@isempty,regexp(SpStr,'\s*%','match')));
        Report.Lines.Code     = Report.Lines.NonEmpty - Report.Lines.Comments;
    end
    
    % get the entire help
    if Report.IsClass
        HelpStartLine = 1;
    else
        HelpStartLine = 2;
    end
    Matched = regexp(SpStr,'\s*%', 'match');
    HelpEndLine = find(diff(~cellfun(@isempty, Matched(HelpStartLine:end)))~=0, 1, 'first')+1;
    Report.Help = SpStr(HelpStartLine:HelpEndLine);
    Report.Help = strrep(Report.Help,'%',' ');
    Report.HelpNumLines = numel(Report.Help);
        
    if Report.NumFunctions>0
    
        
        % search for author name
        %Author = regexp(Report.Help,'Author : (?<Author>\w+\s?\w+\.? \w+)\s+\((?<Month>\w+)\s(?<Year>\d+)\)', 'names');
        Author = regexp(Report.Help,'Author : (?<Author>[\w\s\&\.]+)\s+\((?<Month>\w+)\s(?<Year>\d+)\)', 'names');
        Flag   = ~cellfun(@isempty,Author);
        if ~any(Flag)
            %Author = regexp(Report.Help,'By : (?<Author>\w+\s?\w+\.? \w+)\s{2,100}(?<Month>\w+)\s(?<Year>\d+)', 'names');
            Author = regexp(Report.Help,'By : (?<Author>\w+\s?\w+\.? \w+)\s{2,100}(?<Month>\w+)\s(?<Year>\d+)', 'names');
            Flag   = ~cellfun(@isempty,Author);
        end
        if ~any(Flag)
            Info.Author = '';
            Info.Month  = '';
            Info.Year   = '';
        else
            Info = Author{Flag};
        end
      
        Report.Author = Info.Author;
        Report.Month  = Info.Month;
        Report.Year   = Info.Year;
    else
        Info.Author = '';
        Info.Month  = '';
        Info.Year   = '';
        Report.Author = Info.Author;
        Report.Month  = Info.Month;
        Report.Year   = Info.Year;
    end
   
    % parse first line of help
    if Report.IsClass
        %isempty(Report.StartPosFunctions)
        Report.DescriptionLine = SpStr{1};
    else
        if isempty(Report.StartPosFunctions)
            % a script
            if isempty(SpStr)
                % empty file
                Report.DescriptionLine = '';
            else
                Report.DescriptionLine = SpStr{1};
            end
        else
            % a function
            Report.DescriptionLine = SpStr{Report.StartPosFunctions(1)+1};
        end
    end
    Report.DescriptionLine = strrep(Report.DescriptionLine,'%',' ');
    Report.DescriptionLine = strtrim(Report.DescriptionLine);
    
end
