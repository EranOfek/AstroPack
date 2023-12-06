function List = listClasses
    % find and display all classes in the AstroPack toolbox.
    % Output : - A cell array of classes, including package location.
    % Author : Eran Ofek (May 2022)
    % Example: List = tools.code.listClasses
   
    All  = tools.code.classifyAllFiles('FileTemplate','@*');
    Flag = [All.isdir];
    All  = All(Flag);
    
    N    = numel(All);
    List = cell(N,1);
    for I=1:1:N
        Npack = numel(All(I).PackNames);
        List{I} = '';
        for Ipack=1:1:Npack
            List{I} = sprintf('%s%s.',List{I}, All(I).PackNames{Ipack});
        end
        List{I} = sprintf('%s%s',List{I}, All(I).name(2:end));
    end
    
end
