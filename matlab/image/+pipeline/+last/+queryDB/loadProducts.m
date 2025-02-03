function [AI, AllPaths, AllFiles] = loadProducts(T, Level, Product, Args)
    % Given a table output from last_visits query, load all the data products belonging to some Level/Product.
    %    See also: pipeline.last.queryDB.table2path
    % Input  : - A table which is the output of a query of the
    %            last.last_visits DB table.
    %            Alternatively, a cell array or string array of visit paths
    %            from which to extract the products.
    %          - Level name. Default is 'coadd'.
    %          - Product name. Default is 'Image+'.
    %          * ...,key,val,... 
    %            'Load' - Logical indicating if to load files.
    %                   Default is true.
    %            'table2pathArgs' - A cell array of additional arguments to
    %                   pass to pipeline.last.queryDB.table2path.
    %                   Default is {}.
    %            'ExtraOutProduct' - When Product='Image+', the Image will be
    %                   loaded along with the products specified in this
    %                   strings array.
    %                   Default is ["Mask", "PSF", "Cat"].
    %            'Ncounter' - When Level='proc' and Product='Cat', all
    %                   images for the specific CropID will returned. This is the
    %                   number of expected images in visit directory (i.e., the
    %                   Counter). Default is 20.
    % Output : - An array of either AstroImage, AstroCatalog, tables, 
    %            MatchedSources object, or MovingSources object.
    %          - A string array of all paths.
    %          - A string array of all verified files.
    % Author : Eran Ofek (2024 Dec) 
    % Example: AI=pipeline.last.queryDB.loadProducts(T)
    %          AI=pipeline.last.queryDB.loadProducts(T,'coadd','Image');
    %          AI=pipeline.last.queryDB.loadProducts(T,'coadd','Asteroids');
    %          AI=pipeline.last.queryDB.loadProducts(T,'coadd','Cat');
    %          AI=pipeline.last.queryDB.loadProducts(T,'proc','Cat');
    %          AI=pipeline.last.queryDB.loadProducts(T,'merged','MergedMat');
    %          AI=pipeline.last.queryDB.loadProducts(T,'merged','Cat');
    %          AI=pipeline.last.queryDB.loadProducts(T,'merged','Asteroids');
    %          AI=pipeline.last.queryDB.loadProducts(T,'coadd.zogyD','Image');

    arguments
        T
        Level                  = 'coadd';
        Product                = 'Image+';
        Args.Load logical      = true;
        Args.table2pathArgs    = {};
        Args.ExtraOutProduct   = ["Mask", "PSF", "Cat"];
        Args.Ncounter          = 20;

        Args.CropID            = [];

    end

    if istable(T)
        Nextra = numel(Args.ExtraOutProduct);
    
        [AFN] = pipeline.last.queryDB.table2path(T, Args.table2pathArgs{:});
    
        AllPaths      = AFN.genPath([],'AddSubDir',true);
    % elseif iscell(T) || isstring(T)
    %     AllPaths = T;
    else
        error('Unknown 1st input type');
    end
    

    %Level: 'proc'|'coadd'|'merged'|proc.zogyD|coadd.zogyD
    %Product: MergedMat|Asteroids|TransientsCat|Image|Cat|PSF|Mask|Image+

    PWD = pwd;

    Npath = numel(AllPaths);
    AllFiles = strings(Npath,1);
    %AI    = AstroImage([Npath,1]);
    for Ipath=1:1:Npath
        cd(AllPaths(Ipath));

        switch Level
            case 'coadd'

                switch Product
                    case 'Image'
        
                        AllFiles      = AFN.genFile(Ipath, 'Time','*','Counter','*');
            
                        AFND = AstroFileName.dir(AllFiles);
                        if AFND.nFiles==0
                            warning('File not found : %s%s%s',AFND.genPath,filesep,AFND.genFile);
                        else
                            AllFiles{Ipath} = AFND.genFile{1};
                            if Args.Load
                                AI(Ipath) = AstroImage(AllFiles{Ipath});
                            end
                        end

                    case 'Image+'

                            
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*');
            
                        AFND = AstroFileName.dir(FileTemp);
                        Files = AFND.genProducts([], 'OutProduct',["Image", Args.ExtraOutProduct]);
                        AllFiles(Ipath) = Files{1};

                        CellArgs = cell(1,2.*Nextra);
                        I = 0;
                        for Iextra=1:1:Nextra
                            I = I + 1;
                            CellArgs{I}   = Args.ExtraOutProduct{Iextra};
                            I = I + 1;
                            CellArgs{I} = Files{1+Iextra};
                        end
            
                        if Args.Load
                            AI(Ipath) = AstroImage({Files{1}}, CellArgs{:});
                        end

                    case 'Cat'
                        
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level, 'Product',Product);
                        AFND = AstroFileName.dir(FileTemp);
                        AllFiles(Ipath) = AFND.genFile;
                        if Args.Load
                            AI(Ipath) = AstroCatalog(AllFiles{Ipath});
                        end


                    case 'Asteroids'
                        FA = dir('*coadd_Asteroids_*.mat');
                        if numel(FA)==1
                            AllFiles{Ipath} = FA(1).name;
                            if Args.Load
                                AI(Ipath) = io.files.load2(AllFiles{Ipath});
                            end
                        else
                            error('Found %d Asteroids files in %s',numel(FA),pwd);
                        end
                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end
            case 'coadd.zogyD'
                FA = dir('*coadd.zogyD_Image.mat');
                numel(FA)
                if numel(FA)==1
                    AllFiles{Ipath} = FA(1).name;
                    if Args.Load
                        AI(Ipath) = io.files.load2(AllFiles{Ipath});
                    end
                else
                    warning('Found %d zogyD files in %s',numel(FA),pwd);
                end


            case 'proc'

                switch Product
                    case 'Cat'
                        if Ipath==1
                            if Args.Load
                                AI = AstroCatalog([Args.Ncounter, Npath]);
                            end
                            AllFiles = strings(Args.Ncounter, Npath);
                        end
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level, 'Product',Product, 'FileTYpe','fits*');
                        AFND = AstroFileName.dir(FileTemp);
                        
                        Icounter = str2double(AFND.Counter);

                        AllFiles(Icounter, Ipath) = AFND.genFile;
                        if Args.Load
                            AI(Icounter, Ipath) = AstroCatalog({AllFiles{Icounter, Ipath}});
                        end


                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end

            case 'merged'
                switch Product
                    case 'MergedMat'
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level, 'Product',Product, 'FileTYpe','hdf5');
                        AFND = AstroFileName.dir(FileTemp);
                        AllFiles(Ipath) = AFND.genFile;
                        if Args.Load
                            AI(Ipath) = MatchedSources.read(AllFiles{Ipath});
                        end

                    case 'Cat'
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level, 'Product',Product, 'FileTYpe','fits');
                        AFND = AstroFileName.dir(FileTemp);
                        AllFiles(Ipath) = AFND.genFile;
                        if Args.Load
                            AI(Ipath) = AstroCatalog(AllFiles{Ipath});
                        end

                    case 'Asteroids'
                        FA = dir('*merged_Asteroids_*.mat');
                        if numel(FA)==1
                            AllFiles{Ipath} = FA(1).name;
                            if Args.Load
                                AI(Ipath) = io.files.load2(AllFiles{Ipath});
                            end
                        else
                            error('Found %d Asteroids files in %s',numel(FA),pwd);
                        end


                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end

            otherwise
                error('Unsupported option Level=%s, Product=%s', Level, Product);
        end

    end

    cd(PWD);

end
