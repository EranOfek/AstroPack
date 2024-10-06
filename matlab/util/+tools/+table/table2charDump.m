function [OutString] = table2charDump(T,Args)
    % Format table in characters dump for database insert operation
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: 

    arguments
        T
        Args.NumFormat      = '%.18g';  % Adjust precision as needed
        Args.Delimiter      = ',';
        Args.LineStartChar  = '(';
        Args.LineEndChar    = ');';
        Args.LineTerminator = ''; %'\n';
        Args.CharInQuote logical = true;
    end
    
    ColNames = T.Properties.VariableNames;
    
    % Convert the table to a cell array
    C = table2cell(T);
    SizeC = size(C);
    
    % Get the variable types for each column
    VarTypes = varfun(@class, T, 'OutputFormat', 'cell');
    
    % Initialize an empty string to store the result
    OutString = '';

    % Define a format for numeric precision
    
    
    % Iterate over each row in the table
    for I = 1:SizeC(1)
        RowString = '';  % To store the row as a tab-separated string
        
        % Iterate over each column in the row
        for J = 1:SizeC(2)
            switch VarTypes{J}
                case {'single','double'}
                    ValStr = sprintf(Args.NumFormat, C{I,J});
                case {'char','string'}
                    if Args.CharInQuote
                        ValStr = sprintf('''%s''', C{I,J});
                    else
                        ValStr = sprintf('%s', C{I,J});
                    end
                otherwise
                    % assume this is an integer or logical
                    ValStr = sprintf('%d',C{I,J});
            end
            if J==SizeC(2)
                RowString = sprintf('%s%s',RowString,ValStr);
            else
                RowString = sprintf('%s%s%s ',RowString, ValStr, Args.Delimiter);
            end            
        end
        OutString = sprintf('%s %s %s %s %s', OutString, Args.LineStartChar, RowString, Args.LineEndChar, Args.LineTerminator);
    end
                
end
