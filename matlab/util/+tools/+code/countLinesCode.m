function [Lines,AllFiles]=countLinesCode()
    % Count lines of .m files code under some directory
    % Input  : null
    % Output : - A structure of lines of codes under current directory.
    %          - A structure of all identified files as returned by 
    %            tools.code.classifyAllFiles
    % Author : Eran Ofek (Oct 2023)
    % Example: [Lines,AllFiles]=tools.code.countLinesCode
    
    
    AllFiles = tools.code.classifyAllFiles;

    K=0;
    for I=1:numel(AllFiles)
        if ~isempty(AllFiles(I).Lines)
            K=K+1; 
            Total(K) = AllFiles(I).Lines.Total;
            NonEmpty(K) = AllFiles(I).Lines.NonEmpty;
            Comments(K) = AllFiles(I).Lines.Comments;
            Code(K) = AllFiles(I).Lines.Code;
        end
    end
    
    Lines.Total    = sum(Total);
    Lines.NonEmpty = sum(NonEmpty);
    Lines.Comments = sum(Comments);
    Lines.Code     = sum(Code);
end