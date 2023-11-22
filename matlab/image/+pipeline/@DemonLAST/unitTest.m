function Result = unitTest(Args)
    % a unit test for DemonLast
    % NB: if running at a LAST node with Args.Insert2DB  = true,
    % use the last_operational credentials in Args.AstroDBArgs ! 
    % 
    % Author: A.M. Krassilchtchikov (Oct 2023)
    arguments
        Args.RestoreNew = true;  % copy the raw data back to new 
        Args.Insert2DB  = false; % whether to perform the DB part
        Args.AstroDBArgs cell  = {'Host','socsrv','DatabaseName','lastdb','Port',5432};
%         Args.AstroDBArgs cell  = {'Host','10.23.1.25','DatabaseName','last_operational','Port',5432};  % use this when running on a LAST node
        Args.DB_ImageBulk   logical = false; % whether to use bulk or direct injection method
        Args.DB_CatalogBulk logical = true;  % whether to use bulk or direct injection method
    end
    
    I = Installer;
    BaseDir = I.getDataDir('LASTpipelineUnitTest');
    
    CatsHTMdir = strcat(BaseDir,'/catsHTM/');
    startup('AstroPack_CatsHTMPath',CatsHTMdir)
    
    D = pipeline.DemonLAST;
    D.setPath(BaseDir);
%     D.RefPath = strcat(BaseDir,'/reference/');   % not needed?  

    D.main('StopButton',false,'StopWhenDone',true,'HostName','last08w',...
           'Insert2DB',Args.Insert2DB,'AstroDBArgs',Args.AstroDBArgs,...
           'DB_ImageBulk',Args.DB_ImageBulk,'DB_CatalogBulk',Args.DB_CatalogBulk);
    
    if Args.RestoreNew % copy the raw data back to new
        % NB: this is hard-coded, because the particular observation
        % used for the unitTest and distributed with Installer is of 2023/06/16 
        CurrentDir = pwd; cd(BaseDir);
        !cp 2023/06/16/raw/LAST*fits new/  
        cd(CurrentDir);
    end
    
    Result = 0;
    
end