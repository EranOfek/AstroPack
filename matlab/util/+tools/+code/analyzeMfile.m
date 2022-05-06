function Report = analyzeMfile(FileName, Args)
    % Analyze m files. Count lines of code, identify functions, help, authors etc.
    % Input  : - File name.
    %          * ...,key,val,...
    %            'CountLines' - A logical indicating if to count lines.
    % Output : - A structure containing the report, including the following
    %            fields:
    %            .IsClass - Is a class.
    %            .StartPosFunctions
    %            .NumFunctions - Number of functions in file.
    %            .Lines - A structure containing line counting parameters.
    %            .DescriptionLine
    %            .Help
    % Author : Eran Ofek (May 2022)
    % Report = tools.code.analyzeMfile(a)
   
    arguments
        FileName
        Args.CountLines logical       = true;
    end
    
    % Read the file
    SpStr = io.files.file2str(FileName, 'cell');
    %SpStr = regexp(StrFile,'\n','split');
    
    % check if a class
    Matched = regexp(SpStr, '[^%]\s*[^'']classdef', 'match');
    if sum(~cellfun(@isempty, Matched))>0
        Report.IsClass = true;
    else
        Report.IsClass = false;
    end
    
    % count number of functions
    Matched  = regexp(SpStr, '^\s*function', 'match');
    Report.StartPosFunctions = find(~cellfun(@isempty, Matched));
    Report.NumFunctions      = sum(~cellfun(@isempty, Matched));
    
    % count number of lines
    if Args.CountLines
        Report.Lines.Total    = numel(SpStr);
        Report.Lines.NonEmpty = sum(~cellfun(@isempty, SpStr));
        Report.Lines.Comments = sum(~cellfun(@isempty,regexp(SpStr,'\s*%','match')));
        Report.Lines.Code     = Report.Lines.NonEmpty - Report.Lines.Comments;
    end
    
    % parse first line of help
    if isempty(Report.StartPosFunctions)
        Report.DescriptionLine = '';
    else
        Report.DescriptionLine = SpStr{Report.StartPosFunctions(1)+1};
        Report.DescriptionLine = strrep(Report.DescriptionLine,'%',' ');
        Report.DescriptionLine = strtrim(Report.DescriptionLine);
    end
    
    if Report.NumFunctions>0
    
        % get the entire help
        Matched = regexp(SpStr,'\s*%', 'match');
        HelpEndLine = find(diff(~cellfun(@isempty, Matched(2:end)))~=0, 1, 'first')+1;
        Report.Help = SpStr(2:HelpEndLine);
        Report.Help = strrep(Report.Help,'%',' ');

        % search for author name
        Author = regexp(Report.Help,'Author : (?<Author>\w+\s?\w+\.? \w+)\s+\((?<Month>\w+)\s(?<Year>\d+)\)', 'names');
        Flag   = ~cellfun(@isempty,Author);
        if ~any(Flag)
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
    end
end
