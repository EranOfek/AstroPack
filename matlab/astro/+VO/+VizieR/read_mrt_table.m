function [T, Meta] = read_mrt_table(FileName, Args)
    % Read CDS/VizieR MRT (Machine Readable Table) fixed-width tables.
    %   Description:
    %       MRT files are fixed-width ASCII tables commonly used by CDS,
    %       VizieR, ApJ, A&A and other astronomical journals. The format
    %       includes a header section describing each column in a
    %       "Byte-by-byte Description" block.
    %
    %       This function:
    %           1. Locates the column definition header
    %           2. Parses byte ranges and column metadata
    %           3. Detects the start of the data block
    %           4. Extracts fixed-width columns
    %           5. Automatically converts numeric fields
    % Input  : - File name.
    %          * ...,key,val,...
    %            'TrimStrings' - Logical (default: true)
    %                   Remove leading and trailing whitespace from
    %                   character columns.
    %
    %            'EmptyNumericAsNaN' - Logical (default: true)
    %                   Convert empty numeric fields to NaN.
    %
    % Output : - table.
    %          - Structure containing table columns meta data.
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: [T, Meta] = VO.VizieR.readMRT("apjs486025t1_mrt.txt");
            
    arguments
        FileName (1,1) string
        Args.TrimStrings (1,1) logical = true
        Args.EmptyNumericAsNaN (1,1) logical = true
    end
    
    %% Read file
    Lines = readlines(FileName);
    Lines = string(Lines);
    
    %% -------------------------------------------------
    % 1. Locate column definition header
    %% -------------------------------------------------
    
    HeaderIdx = find(contains(Lines,"Bytes") & ...
                     contains(Lines,"Format") & ...
                     contains(Lines,"Label"),1);
    
    if isempty(HeaderIdx)
        error("MRT header not found.")
    end
    
    % Column definitions start two lines later
    DefStart = HeaderIdx + 2;
    
    % Find end of definition block (next dashed separator)
    DefEnd = [];
    for I = DefStart:numel(Lines)
        if startsWith(strtrim(Lines(I)),"---")
            DefEnd = I - 1;
            break
        end
    end
    
    if isempty(DefEnd)
        error("Could not determine end of column definitions.")
    end
    
    DefBlock = Lines(DefStart:DefEnd);
    
    %% -------------------------------------------------
    % 2. Parse column definitions
    %% -------------------------------------------------
    
    StartByte = [];
    EndByte   = [];
    Format    = strings(0);
    Units     = strings(0);
    Label     = strings(0);
    Explain   = strings(0);
    
    for I = 1:numel(DefBlock)
    
        Line = strtrim(DefBlock(I));
        if Line == ""
            continue
        end
    
        % General MRT pattern:
        % 1- 11 F11.5 cm-1 WaveNum Explanation
        Tokens = regexp(Line, ...
            '^\s*(\d+)\s*-?\s*(\d*)\s+(\S+)\s+(\S+)\s+(\S+)\s*(.*)', ...
            'tokens');
    
        if isempty(Tokens)
            continue
        end
    
        Tok = Tokens{1};
    
        SB = str2double(Tok{1});
    
        if Tok{2} == ""
            EB = SB;
        else
            EB = str2double(Tok{2});
        end
    
        StartByte(end+1,1) = SB;
        EndByte(end+1,1)   = EB;
        Format(end+1,1)    = string(Tok{3});
        Units(end+1,1)     = string(Tok{4});
        Label(end+1,1)     = string(Tok{5});
    
        if numel(Tok) >= 6
            Explain(end+1,1) = string(Tok{6});
        else
            Explain(end+1,1) = "";
        end
    end
    
    if isempty(StartByte)
        error("Column parsing failed.")
    end
    
    %% -------------------------------------------------
    % 3. Detect data start
    %% -------------------------------------------------
    
    % Data begins after the final dashed separator
    LastDash = find(startsWith(strtrim(Lines),"---"),1,'last');
    
    if isempty(LastDash)
        error("Could not detect data block.")
    end
    
    DataStart = LastDash + 1;
    
    % Remove blank lines at top
    while DataStart <= numel(Lines) && strlength(strtrim(Lines(DataStart))) == 0
        DataStart = DataStart + 1;
    end
    
    DataLines = Lines(DataStart:end);
    
    % Remove empty lines
    Mask = strlength(strtrim(DataLines)) > 0;
    DataLines = DataLines(Mask);
    
    %% -------------------------------------------------
    % 4. Read fixed-width data
    %% -------------------------------------------------
    
    Nrow = numel(DataLines);
    Ncol = numel(Label);
    
    Columns = cell(Ncol,1);
    
    for C = 1:Ncol
        Columns{C} = strings(Nrow,1);
    end
    
    for I = 1:Nrow
    
        Line = DataLines(I);
        LineLen = strlength(Line);
    
        for C = 1:Ncol
    
            SB = StartByte(C);
            EB = EndByte(C);
    
            if SB <= LineLen
                EB = min(EB,LineLen);
                Columns{C}(I) = extractBetween(Line,SB,EB);
            else
                Columns{C}(I) = "";
            end
    
        end
    end
    
    %% -------------------------------------------------
    % 5. Convert numeric columns
    %% -------------------------------------------------
    
    for C = 1:Ncol
    
        if startsWith(Format(C),"F") || startsWith(Format(C),"I")
    
            Numeric = str2double(Columns{C});
    
            if Args.EmptyNumericAsNaN
                Numeric(isnan(Numeric)) = NaN;
            end
    
            Columns{C} = Numeric;
    
        else
    
            if Args.TrimStrings
                Columns{C} = strtrim(Columns{C});
            end
    
        end
    end
    
    %% -------------------------------------------------
    % 6. Build output table
    %% -------------------------------------------------
    
    T = table;
    
    for C = 1:Ncol
        VarName = matlab.lang.makeValidName(Label(C));
        T.(VarName) = Columns{C};
    end
    
    %% -------------------------------------------------
    % 7. Metadata
    %% -------------------------------------------------
    
    Meta = struct;
    Meta.StartByte   = StartByte;
    Meta.EndByte     = EndByte;
    Meta.Format      = Format;
    Meta.Units       = Units;
    Meta.Label       = Label;
    Meta.Description = Explain;

    T.Properties.VariableUnits = {Units{:}};

end