function renameFile(Template, InExp, OutExp)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
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
