    function Result = testDBlast0 (Args)
            
            arguments
                Args.Table = 'test_raw_images';
                Args.DataDir = '/last01e/data1/archive/LAST.01.01.01/2023/04/';
            end
            
            A = db.AstroDb('Host','10.23.1.25','DatabaseName','last_operational', ...
                           'UserName','postgres','Password','postgres','Port',5432);
            
            A.TnRawImages   = 'test_raw_images';
            A.TnProcImages  = 'test_proc_images';
            A.TnCoaddImages = 'test_coadd_images';
            A.TnSrcCatalog  = 'test_src_catalog';
            A.Dname        = 'last_operational';
            
%           createTables(A); % if the tables do not exist as of yet, need to create them
            
            drawnow('update'); tic % ~ 6000 files at the speed about 1 file/sec -- too slow?
            
            if strcmp(Args.Table,'test_raw_images') 
                RawTuples = A.addImages2DB('DataDir',Args.DataDir, ...
                                             'InputImages','LAST*sci*raw_Image*.fits', ...
                                             'DBname','last_operational', 'DBtable','test_raw_images'); 

            elseif strcmp(Table,'test_proc_images')
                ProcTuples = A.addImages2DB('DataDir',Args.DataDir, ...
                                             'InputImages','LAST*sci_proc_Image*.fits', ...
                                             'DBname','last_operational', 'DBtable','test_proc_images'); 
                                         
            elseif strcmp(Table,'test_coadd_images')
                CoaddTuples = A.addImages2DB('DataDir',Args.DataDir, ...
                                             'InputImages','LAST*sci_coadd_Image*.fits', ...
                                             'DBname','last_operational', 'DBtable','test_coadd_images'); 
            end

            toc
            
            Result = true;
                        
    end