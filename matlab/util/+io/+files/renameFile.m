function renameFile(Template, InExp, OutExp)
    % Rename  file names using regular expression replacments
    % Input  : - File name template. Only file names with this templates
    %            will be selected.
    %          - Input regular expression.
    %          - Output expression.
    % Output : null
    % Author : Eran Ofek (2023 Dec) 
    % Example: io.files.renameFile('LAST*.fits','_WD_','_WD')

    arguments
        Template
        InExp
        OutExp
    end

    Files   = dir(Template);
    NewNames = regexprep({Files.name}, InExp, OutExp);
    io.files.moveFiles({Files.name}, NewNames);
    
end
