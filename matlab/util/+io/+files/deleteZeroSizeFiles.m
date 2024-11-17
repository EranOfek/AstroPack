function [List] = deleteZeroSizeFiles(Del)
    % Delete from current directory files with zero size.
    % Input  : - Flag indicate if to delete files (true) or only return their
    %            names (false). Default is true.
    % Output : - A cell array of files with zero size.
    % Author : Eran Ofek (2024 Nov) 
    % Example: io.files.deleteZeroSizeFiles

    arguments
        Del logical     = true;
    end

    F = dir;
    Flag = ~F.isdir & F.bytes==0;
    List = {F(Flag).name};
    
    if Del
        io.files.delete_cell(List);
    end
    
end
