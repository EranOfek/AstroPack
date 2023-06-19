function Result = unitTest()
    % unit test for DemonLast
    
    D = pipeline.DemonLAST;
    
    D.BasePath  = '/last08e/data1/archive/LAST.01.08.01/';
%     D.CamNumber = 2;
%     D.StartJD   = [20 4 2023];
%     D.EndJD     = [27 4 2023];

%     D.main('RegenCalib',true);
    
    D.main('Insert2DB',true)

    Result = 0;
    
end