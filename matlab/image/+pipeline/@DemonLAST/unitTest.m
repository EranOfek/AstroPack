function Result = unitTest()
    % unit test for DemonLast
    
    startup('AstroPack_CatsHTMPath','~/matlab/data/pipeline/LAST/catsHTM')
    D = pipeline.DemonLAST;
    D.setPath('~/matlab/data/pipeline/LAST/')
%     D.RefPath = '/home/sasha/LAST/pipeline/reference/';
    D.main('StopButton',0)
    
%     D.BasePath  = '/last10e/data2/archive/LAST.01.10.02/';
% %     D.CamNumber = 2;
% %     D.StartJD   = [20 4 2023];
% %     D.EndJD     = [27 4 2023];
% 
% %     D.main('RegenCalib',true);
%     
%     D.main('Insert2DB',true,'DB_Table_Raw','test_raw_images','DB_Table_Proc','test_proc_images',...
%             'DB_Table_Coadd','test_coadd_images', 'DB_ImageBulk', false)

    Result = 0;
    
end