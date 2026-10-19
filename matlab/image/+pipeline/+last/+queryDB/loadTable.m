function [AI, AllPaths, AllFiles] = loadTable(T, Level, Product, Args)
    % Given a table output from last_visits query, load all the data products belonging to some Level/Product.
    %    See also: pipeline.last.queryDB.table2path
    % Input  : - A table which is the output of a query of the
    %            last.last_visits DB table.
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
    %                   The epoch catalogs are read from the visit's MAT file
    %                   (written by pipelineI instead of the proc_Cat FITS
    %                   files, issue #1417) if there is one; AllFiles then
    %                   holds the MAT file name.
    %            'CatMatTemplate' - File template of that MAT file.
    %                   Default is '*_sci_proc_Cat_1.mat'.
    % Output : - An array of either AstroImage, AstroCatalog, tables, 
    %            MatchedSources object, or MovingSources object.
    %          - A string array of all paths.
    %          - A string array of all verified files.
    % Author : Eran Ofek (2024 Dec) 
    % Example: AI=pipeline.last.queryDB.loadTable(T)
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd','Image');
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd','Asteroids');
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd','Cat');
    %          AI=pipeline.last.queryDB.loadTable(T,'proc','Cat');
    %          AI=pipeline.last.queryDB.loadTable(T,'merged','MergedMat');
    %          AI=pipeline.last.queryDB.loadTable(T,'merged','Cat');
    %          AI=pipeline.last.queryDB.loadTable(T,'merged','Asteroids');
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd.zogyD');

    arguments
        T
        Level                  = 'coadd';
        Product                = 'Image+';
        Args.Load logical      = true;
        Args.table2pathArgs    = {};
        Args.ExtraOutProduct   = ["Mask", "PSF", "Cat"];
        Args.Ncounter          = 20;
        Args.CatMatTemplate    = '*_sci_proc_Cat_1.mat';

    end

    Nextra = numel(Args.ExtraOutProduct);

    [AFN] = pipeline.last.queryDB.table2path(T, Args.table2pathArgs{:});

    AllPaths      = AFN.genPath([],'AddSubDir',true);
    

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
        
                        FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*');
            
                        AFND = AstroFileName.dir(FileTemp);
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
                        AllFiles{Ipath} = Files{1};

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
                FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level);
                AFND = AstroFileName.dir(FileTemp);
                if AFND.nFiles==1
                    AllFiles(Ipath) = AFND.genFile;
                    if Args.Load
                        AI(Ipath) = AstroImage(AllFiles{Ipath});
                    end
                else
                    warning('Found %d zogyD files in %s',AFND.nFiles,pwd);
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
                        if isempty(dir(Args.CatMatTemplate))
                            FileTemp      = AFN.genFile(Ipath, 'Time','*','Counter','*', 'Level',Level, 'Product',Product, 'FileTYpe','fits*');
                            AFND = AstroFileName.dir(FileTemp);
                            
                            Icounter = str2double(AFND.Counter);

                            AllFiles(Icounter, Ipath) = AFND.genFile;
                            if Args.Load
                                AI(Icounter, Ipath) = AstroCatalog({AllFiles{Icounter, Ipath}}).';
                            end
                        else
                            % one MAT file with all the epochs and crops of the visit (issue #1417)
                            [Cat, ~, InfoMat] = pipeline.last.load.loadEpochCatMat(pwd, 'CatTemplate',Args.CatMatTemplate, ...
                                                                                   'CropID',str2double(string(AFN.CropID(Ipath))));
                            Icounter = InfoMat.Counter;
                            [~, MatName, MatExt] = fileparts(InfoMat.CatFile);
                            AllFiles(Icounter, Ipath) = MatName + MatExt;
                            if Args.Load
                                AI(Icounter, Ipath) = Cat;
                            end
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
