function [Text] = sprintf_table(T, Args)
    % sprintf table values with format and latex options
    % Input  : - A table.
    %          * ...,key,val,... 
    %            'Format' - A cell array of formats per column.
    %            'Cols' - Optional column names.
    %            'IsLatex' - print latex table. Default is false.
    %            'AddHeader' - Add VizieR style header. Default is false.
    % Output : - A char array.
    % Author : Eran Ofek (2025 Apr) 
    % Example: tools.table.sprintf_table(table([1;2], ["aa";"bb"]),'Format', {'%d','%3s'})
    %          tools.table.sprintf_table(table([1;2], ["aa";"bb"]),'Format', {'%d','%3s'}, 'IsLatex',true)

    arguments
        T
        Args.Format            = [];
        Args.Cols              = [];
        Args.IsLatex           = false;
        Args.AddHeader         = false;
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

    if Args.AddHeader
        % Add VizieR style header

        Header = '';
        Header = sprintf('%sTitle: \n',Header);
        Header = sprintf('%sAuthors: \n',Header);
        Header = sprintf('%sTable: (Number) Name \n',Header);
        Header = sprintf('%s================================================================================\n',Header);
        Header = sprintf('%sByte-by-byte Description of file: <FileName>\n',Header);
        Header = sprintf('%s--------------------------------------------------------------------------------\n',Header);
        Header = sprintf('%s   Bytes Format Units Label  Explanations\n',Header);
        Header = sprintf('%s--------------------------------------------------------------------------------\n',Header);
        Iend = 1;
        for Icol=1:1:Ncol
            
            Args.Format{Icol} = deblank(Args.Format{Icol});
            Number = Args.Format{Icol}(2:end-1);
            switch Args.Format{Icol}(end)
                case 'd'
                    FormatLetter = 'I';
                    NN = str2double(Number);
                case 'f'
                    FormatLetter = 'F';
                    TmpNum = split(Number,'.');
                    NN = str2double(TmpNum{1});
                otherwise
                    FormatLetter = 'X';
                    % do nothing
            end
            
            Istart = Iend + 1;
            Iend   = Istart + NN;

            Format = sprintf('%s%s', FormatLetter, Number);
            Header = sprintf('%s %3d-%3d %-6s UNITS %s EXPLANATION\n',Header, Istart,Iend, Format, Args.Cols{Icol});
        end
        Header = sprintf('%s--------------------------------------------------------------------------------\n',Header);
        Header = sprintf('%s--------------------------------------------------------------------------------\n',Header);

        Text   = sprintf('%s%s',Header, Text);

    end

end
