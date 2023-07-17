function Result = unitTest()
    % unit test for DemonLast
    
    D = pipeline.DemonLAST;
    
    D.BasePath  = '/last08e/data2/archive/LAST.01.08.01/';
%     D.CamNumber = 2;
%     D.StartJD   = [20 4 2023];
%     D.EndJD     = [27 4 2023];

%     D.main('RegenCalib',true);
    
    D.main('Insert2DB',true,'DB_Table_Raw','test_raw_images','DB_Table_Proc','test_proc_images',...
            'DB_Table_Coadd','test_coadd_images', 'DB_ImageBulk', true)

    Result = 0;
    
end