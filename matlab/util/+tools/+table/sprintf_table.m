function [Text] = sprintf_table(T, Args)
    % sprintf table values with format and latex options
    % Input  : - A table.
    %          * ...,key,val,... 
    %            'Format' - A cell array of formats per column.
    %            'Cols' - Optional column names.
    %            'IsLatex' - print latex table. Default is false.
    % Output : - A char array.
    % Author : Eran Ofek (2025 Apr) 
    % Example: tools.table.sprintf_table(table([1;2], ["aa";"bb"]),'Format', {'%d','%3s'})
    %          tools.table.sprintf_table(table([1;2], ["aa";"bb"]),'Format', {'%d','%3s'}, 'IsLatex',true)

    arguments
        T
        Args.Format            = [];
        Args.Cols              = [];
        Args.IsLatex           = false;
    end

    [Nrow, Ncol] = size(T);
    
    if isempty(Args.Cols)
        Args.Cols = T.Properties.VariableNames;
    end


    for Icol=1:1:Ncol
        if Args.IsLatex
            if strcmp(Args.Format{Icol}(end),'f') || strcmp(Args.Format{Icol}(end),'d')
                Args.Format{Icol} = sprintf('$%s$ ', Args.Format{Icol});
            else
                Args.Format{Icol} = sprintf('%s ',Args.Format{Icol});
            end
            if Icol==Ncol
                Args.Format{Icol} = sprintf('%s \\\\\\\\ \n', Args.Format{Icol});
            else
                Args.Format{Icol} = sprintf('%s & ', Args.Format{Icol});
            end
        else
            Args.Format{Icol} = sprintf('%s ',Args.Format{Icol});
            if Icol==Ncol
                Args.Format{Icol} = sprintf('%s \n',Args.Format{Icol});
            end
        end
    end
    
    Text = '';
    for Irow=1:1:Nrow
        for Icol=1:1:Ncol
            Format = sprintf('%%s %s',Args.Format{Icol});
            Text   = sprintf(Format, Text, T.(Args.Cols{Icol})(Irow));
        end
    end


end
