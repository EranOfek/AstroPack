function [AI] = loadTable(T, Level, Product, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Dec) 
    % Example: AI=pipeline.last.queryDB.loadTable(T)
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd','Image');
    %          AI=pipeline.last.queryDB.loadTable(T,'coadd','Asteroids');
    %          

    arguments
        T
        Level                  = 'coadd';
        Product                = 'Image+';
        Args.table2pathArgs    = {};
        Args.ExtraOutProduct   = ["Mask", "PSF", "Cat"];
    end

    Nextra = numel(Args.ExtraOutProduct);

    [AFN] = pipeline.last.queryDB.table2path(T, Args.table2pathArgs{:});

    AllPaths      = AFN.genPath([],'AddSubDir',true);
    

    %Level: 'proc'|'coadd'|'merged'|proc.zogyD|coadd.zogyD
    %Product: MergedMat|Asteroids|TransientsCat|Image|Cat|PSF|Mask|Image+

    PWD = pwd;

    Npath = numel(AllPaths);
    %AI    = AstroImage([Npath,1]);
    for Ipath=1:1:Npath
        cd(AllPaths(Ipath));

        switch Level
            case 'coadd'

                switch Product
                    case 'Image'
        
                        AllFiles      = AFN.genFile([], 'Time','*','Counter','*');
            
                        AFND = AstroFileName.dir(AllFiles{Ipath});
                        if AFND.nFiles==0
                            warning('File not found : %s%s%s',AFND.genPath,filesep,AFND.genFile);
                        else
                            AI(Ipath) = AstroImage(AFND.genFile{1});
                        end

                    case 'Image+'

                        AllFiles      = AFN.genFile([], 'Time','*','Counter','*');
            
                        AFND = AstroFileName.dir(AllFiles{Ipath});
                        Files = AFND.genProducts([], 'OutProduct',["Image", Args.ExtraOutProduct]);
            
                        CellArgs = cell(1,2.*Nextra);
                        I = 0;
                        for Iextra=1:1:Nextra
                            I = I + 1;
                            CellArgs{I}   = Args.ExtraOutProduct{Iextra};
                            I = I + 1;
                            CellArgs{I} = Files{1+Iextra};
                        end
            
                        AI(Ipath) = AstroImage({Files{1}}, CellArgs{:});

                    case 'Asteroids'
                        FA = dir('*coadd_Asteroids_*.mat');
                        if numel(FA)==1
                            AI(Ipath) = io.files.load2(FA(1).name);
                        else
                            error('Found %d Asteroids files in %s',numel(FA),pwd);
                        end
                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end
            case 'proc'

                switch Product
                    case 'Cat'


                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end

            case 'merged'
                switch Product
                    case 'MergedMat'

                    case 'Cat'

                    case 'Asteroids'

                    otherwise
                        error('Unsupported option Level=%s, Product=%s', Level, Product);
                end

            otherwise
                error('Unsupported option Level=%s, Product=%s', Level, Product);
        end

    end

    cd(PWD);

end
