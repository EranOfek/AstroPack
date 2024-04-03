function save(V, File, Var)
    % save function to avoid Transparency violation error. Use in parfor.
    % Input  : - Version.
    %          - File name
    %          - Version
    % Output : null
    % Author : Eran Ofek (2024 Apr) 
    % Example: io.files.save('-v7.3','MergedOnlyMP.mat',MergedOnlyMP)

    arguments
        V
        File
        Var
    end

    save(V, File, "Var");

end
