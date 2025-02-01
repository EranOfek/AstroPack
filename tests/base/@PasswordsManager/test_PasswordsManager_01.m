function Result = unitTest()
    % unitTest for PasswordManager class
   
    
    C=Configuration;
    C.reloadSysConfig

    PM = PasswordsManager;
    [Result]=PM.search('ptf') 
    [Result]=PM.search('VO.PTF.wget_corrim','fun',false)

    Result = true;
end