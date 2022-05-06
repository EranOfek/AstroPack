function classifyAllFiles
    %
   
    
    
    
    arguments
        Args.Path           = [];
        Args.FileTemplate   = '*';
    end
    
    if isempty(Args.Path)
        Args.Path = 
    
    PWD = pwd;
    cd(Args.Path);
    
    AllFiles = io.files.rdir(Args.FileTemplate);
    
    cd(PWD);
end
