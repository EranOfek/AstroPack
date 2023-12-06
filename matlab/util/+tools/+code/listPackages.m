function List = listPackages
    % find and display all packages and subpackaes in the AstroPack toolbox.
    % Output : - A cell array of packages, including all subpackages.
    % Author : Eran Ofek (May 2022)
    % Example: List = tools.code.listPackages
   
    All  = tools.code.classifyAllFiles('FileTemplate','+*');
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
