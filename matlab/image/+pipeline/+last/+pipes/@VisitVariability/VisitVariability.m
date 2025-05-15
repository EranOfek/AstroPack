% p
% ipeline.last.pipes.VisitVariability - A class for searching

%
% Properties :
%
% Functionality :
%
%




classdef VisitVariability < Component
    properties (Dependent) % Access image data directly
        
    end

    properties (SetAccess = public)
        BaseDir   = '/marvin';
        DB        = db.Db;
        
    end
    
    methods % Constructor
        function Obj=VisitVariability(Args)
            % Constructor for VisitVariability
            
            Obj.DB.connect;
            Obj.DB.useDB('last');
        end
            
    end
    
    
    methods (Static) % main functionality
        function [MS, AC, Table] = searchVarMatchedSourcesFile(FileName, Path, Args)
            % Search for variable sources in a single MatchedSources file
            % Input  : - FileName or MatchedSources object.
            %          - Path. If empty, read from current dir.
            %            Default is [].
            %          * ...,key,val,...
            %            'varAnalysisArgs' - A cell array of additional
            %                   arguments to pass to: lcUtil.variabilityAnalysis
            %                   Default is {}.
            % Output : - MatchedSources object.
            %          - AstroCatalog object with output variability table.
            %          - Output variability table.
            % Author : Eran Ofek (Mar 2025)
            % Example: [MS,AC,Table]=pipeline.last.pipes.VisitVariability.searchVarMatchedSourcesFile(MS)
            
            arguments
                FileName
                Path      = [];
                Args.varAnalysisArgs = {};
            end
            
            PWD = pwd;
            if ~isempty(Path)
                cd(Path);
            end
            
            if isa(FileName, 'MatchedSources')
                MS = FileName;
            else
                MS = MatchedSources.read({FileName});
            end
            cd(PWD);
            AC = lcUtil.variabilityAnalysis(MS, Args.varAnalysisArgs{:});
        
            if nargout>2
                Table = AC.Catalog;
            end
            
        end
        
        function searchVisitDir(Path, Args)
            % Analyze (search for variability) in all MergedMat files in a visit dir, and write product to the visit dir.
            % Input  : - Path for visit to analyze.
            %            If empty, use current dir. Default is [].
            %          * ...,key,val,...
            %            'varAnalysisArgs' - A cell array of additional
            %                   arguments to pass to: lcUtil.variabilityAnalysis
            %                   Default is {}.
            %            'FileTemp' - Template of MergedMat file names to
            %                   read. Default is 'LAST*_MergedMat*.hdf5'.
            %            'WriteProduct' - A logical indicating if to write
            %                   product to visit dir.
            %            'WriteDB' - A logical indicating if to write
            %                   results to DB. Default is true.
            %            'AstIndex' - Index of last asteroid candidate
            %                   detected. Default is 0.
            %            'DB' - DB object (must be supplied).
            % Output : - An AstroCatalog object, with element per MatchedSources file
            %            (i.e., cropID) with all variable source candidates found
            %            in all cropIDs in visit.
            %          - An AstroCatalog object, with element per MatchedSources file
            %            (i.e., cropID) with all fast moving asteroid
            %            candidates.
            %          - AstIndex of latest found asteroid.
            % Example:
            % [TV,TA]=pipeline.last.pipes.VisitVariability.searchVisitDir('/marvin/LAST.01.03.02/2025/03/12/proc/184350v0');
            
            arguments
                Path                      = [];
                Args.varAnalysisArgs      = {};
                Args.FileTemp             = 'LAST*_MergedMat*.hdf5';
                Args.WriteProduct logical = true; 
                Args.AstIndex             = 0;

                Args.INPOP             = celestial.INPOP.init;
                Args.OrbEl             = [];

                Args.SearchVar logical  = true;
                Args.SearchAst logical  = true;

                Args.HealpixType   = 'nested';
                Args.HealpixLevel  = 2.^[3, 8, 16];   % diamater ~ 13 deg, 0.4 deg, 5.7"
                Args.ColHealpix    = ["UPIX_PARTITION", "UPIX_LOW", "UPIX_HIGH"];
                Args.UniqueID logical = true;
                Args.DB               = [];

                Args.VarTableName      = [];
                Args.AstTableName      = [];

            end
           
            FunJD = @(jd) jd;
            AstIndex = Args.AstIndex;

            PWD = pwd;
            if ~isempty(Path)
                cd(Path);
            end
            
            Files = dir(Args.FileTemp);
            Nf    = numel(Files);
            %AstC  = [];
            %AC    = [];
            for If=1:1:Nf
                
                MS = MatchedSources.read({Files(If).name});
                if MS.Nepoch>14 && MS.Nsrc>100

                    MS.addSrcData;
    
                    DataPath = string(pwd);
                    Tmp      = split(DataPath,'/');
                    ProjName = Tmp{3};
                    Visit    = Tmp(end);
    
                    if Args.SearchAst
                        %'motion'
                        %tic;
                        [~,AstAC, AstIndex] = lcUtil.fitFastMotion(MS, 'AstIndex',AstIndex, 'OutType','AstroCatalog', 'INPOP',Args.INPOP, 'OrbEl',Args.OrbEl, 'Visit',Visit);

                        %toc
                        if ~isempty(AstAC)
                            if numel(AstAC)>1
                                % merge AstAC
                                AstAC = AstAC.merge('IsTable',true);
                            end
                            if AstAC.sizeCatalog>0
                                %AstCm=AstC.merge('IsTable',true);
                                AstAC.Catalog=db.util.insertHealpixIndex2table(AstAC.Catalog, 'ColRA','RA', 'ColDec','Dec', 'CooUnits','deg',...
                                                  'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                                  'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);
                                AstAC.Catalog.Flags = uint32(AstAC.Catalog.Flags);
                                AstAC.Catalog = db.util.insertIntegerTime2table(AstAC.Catalog, 'ColJD', celestial.time.julday(), 'ColIntTime','insertion_time_jd', 'IntTimeFun',FunJD);
                                
                                if ~isempty(Args.AstTableName)
                                    Args.DB.insertCharDump(Args.AstTableName, AstAC.Catalog);
                                end
                            end
                            
                        end
                      
                    end
    
                    if Args.SearchVar
                        % Note that the following function may modify MS
                        %'var'
                        %tic;
                        VarAC = lcUtil.variabilityAnalysis(MS, Args.varAnalysisArgs{:}, 'Visit',Visit);
                        %toc
    
                        if ~isempty(VarAC) && VarAC.sizeCatalog>0
                            %VarAC = VarAC.merge('IsTable',true);
                            VarAC.Catalog=db.util.insertHealpixIndex2table(VarAC.Catalog, 'ColRA','RA', 'ColDec','Dec', 'CooUnits','deg',...
                                                  'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                                  'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);
                            VarAC.Catalog.FLAGS = uint32(VarAC.Catalog.FLAGS);
                            VarAC.Catalog = db.util.insertIntegerTime2table(VarAC.Catalog, 'ColJD', celestial.time.julday(), 'ColIntTime','insertion_time_jd', 'IntTimeFun',FunJD);
                                
                            if ~isempty(Args.VarTableName)
                                Args.DB.insertCharDump(Args.VarTableName, VarAC.Catalog);
                            end
                            
                        end
                    end
                end

            end
            
            % if Args.WriteProduct
            %     FN = FileNames.generateFromFileName(Files(1).name);
            %     FN.Product  = 'VariablesCat';
            %     FN.FileType = 'mat';
            %     OutFileName = FN.genFile;
            % 
            %     save('-v7.3', OutFileName, 'AC');
            % end
            
            cd(PWD);
            
            
        end
    end
    
    
    methods % run on data functionality
        function [ACVar,ACAst]=analayzeAllData(Obj, Args)
            %
            % Example: VV=pipeline.last.pipes.VisitVariability;
            %          DB=db.Db, DB.User='socsrv/root'; DB, DB.connect; DB.useDB('last');
            %          T = DB.query("SELECT jd_start, mountnum, camnum, subdir, any(ccdid) AS ccdid, any(fieldid) AS fieldid, any(filter) AS filter, any(nodenumb) AS nodenumb, any(id_visit) AS id_visit, any(cropid) AS cropid, any(ra) AS ra, any(dec) as dec FROM visit_images GROUP BY jd_start, mountnum, camnum, subdir");
            %          Ind = (1:1000)';
            %          VV.analayzeAllData('DB',DB, 'T',T, 'Ind',Ind);
            %          VV.analayzeAllData('DB',DB, 'T','VisitImages.mat', 'Ind',Ind);
            %          VV.analayzeAllData('DB',DB);
            %          VV.analayzeAllData('DB',DB,'Mount',8,'IngestionTime',[2460708 2460740])

            arguments
                Obj
                Args.T                 = [];
                Args.Ind               = [];
                Args.Mount             = 1;
                Args.IngestionTime     = [-Inf 2460708]; %66.5];

                Args.INPOP             = celestial.INPOP.init;
                Args.OrbEl             = celestial.OrbitalEl.loadSolarSystem('merge');
                Args.DB                = []; % must be supplied
                Args.VarTableName      = 'last.mergedmat_var';
                Args.AstTableName      = 'last.fastmoving_asteroids';

                Args.FailedFile        = '~/varSearchFailed.txt';
                Args.StartInd          = 1;
            end

            if isempty(Args.T)
                if isempty(Args.DB)
                    Args.DB = db.Db;
                    Args.DB.User = 'socsrv/root';
                    Args.DB.User
                    Args.DB.connect;
                    Args.DB.useDB('last');
                end

                T = Args.DB.query("SELECT jd_start, mountnum, camnum, subdir, any(ccdid) AS ccdid, any(fieldid) AS fieldid, any(filter) AS filter, any(nodenumb) AS nodenumb, any(id_visit) AS id_visit, any(cropid) AS cropid, any(ra) AS ra, any(dec) as dec, min(ingestion_time) as ingestion_time FROM visit_images GROUP BY jd_start, mountnum, camnum, subdir");

                if ~isempty(Args.Mount)
                    Flag = T.mountnum==Args.Mount;
                    T    = T(Flag,:);
                end

                JD_ingest = convert.time(T.ingestion_time, 'StrDate', 'JD');
                Flag      = JD_ingest>=Args.IngestionTime(1) & JD_ingest<Args.IngestionTime(2);
                T         = T(Flag,:);

            else
                T = T(Args.Ind,:);
            end

            %Npool = 15;
            Nt = size(T,1);
            %VecI = (1:1:Nt).';
            %IndPool = ceil(VecI./(Nt./15));

            %VecNotDone = true(Nt,1);

            % Cont = true;
            % Counter = 0;
            % K       = 0;
            % KA      = 0;
            ACAst   = [];
           
            % delete(gcp('nocreate'))
            %if isempty(gcp('nocreate'))
            %    parpool(Npool)
            %end

            %parfor Ipool=1:Npool
            for It=Args.StartInd:1:Nt

                [It, Nt] %, Args.Ind(It)]
                
                FN=pipeline.last.queryDB.table2path(T(It,:));
                Path = FN.genPath('AddSubDir',true);
            
                tic;
                try
                    AstIndex = 0;
                    pipeline.last.pipes.VisitVariability.searchVisitDir(Path,'WriteProduct',false,'AstIndex',AstIndex, 'INPOP',Args.INPOP, 'OrbEl',Args.OrbEl, 'DB',Args.DB, 'VarTableName',Args.VarTableName, 'AstTableName',Args.AstTableName);
                catch ME
                    'a'
                    FID = fopen(Args.FailedFile,'w');
                    fprintf(FID,'%d %13.5f %13.5f %d\n', Args.Mount, Args.IngestionTime(1), Args.IngestionTime(2), It);
                    fclose(FID);
                end
                toc
              
                
            end

            
        end
        
    end

    methods (Static)% select interesting candidates
        function select1(T)
            %

            BD = BitDictionary;
            Fbad = BD.findBit(T.flags, 'Saturated');
            Fsel = T.ndet>15 & abs(T.corrc_mag_best_dec)<0.5 & abs(T.corrc_mag_best_ra)<0.5 & T.nfound<3 & ~Fbad;
            Tg   = T(Fsel,:);
            Color = Tg.gaia_bp-Tg.gaia_rp;
            AbsMag = Tg.gaia_bp - (5.*log10(1000./Tg.gaia_plx) - 5);

            F = Tg.poly5_residstd./Tg.poly1_residstd<0.5;
            R=pipeline.last.pipes.VisitVariability.getLC(Tg(F,:));

            
            F = abs(Tg.rm_minsn_win3)>5;

        end
    

    end
        
    methods (Static) % data retrival functions from DB
        function [AFN,AllPath]=getLocationFromDB(T, Args)
            % Given a table of variable/fast moving - generate visit path.
            % Input  : - A table which is the output of a DB query of the
            %            fast moving or variables in visit.
            %          * ...,key,val,...
            %            'ColProjName' - Column in table containing ProjName.
            %                   Default is 'projname'.
            %            'ColJD' - Column in table containing JD.
            %                   If a string array then will chose the first
            %                   existing column name in table.
            %                   Default is ["jd", "midjd", "pm_jd"]
            %            'ColSubDir' - Column in table containing
            %                   SubDir/visit name. Default is 'visit'.
            %            'ColCropID' - Column of CropID number.
            %                   Default is 'cropid'.
            % Output : - An AstroFileName object with the path information.
            %          - A string array of paths per table entry.
            % Author : Eran Ofek (Mar 2025)
            % Example:
            % AFN=pipeline.last.pipes.VisitVariability.getLocationFromDB(T(1:10,:))
            
            arguments
                T
                Args.ColProjName = 'projname';
                Args.ColJD       = ["jd", "midjd", "pm_jd"];
                Args.ColSubDir   = 'visit';
                Args.ColCropID   = 'cropid';

            end

            [~, Args.ColJD] = tools.table.isColumn(T, Args.ColJD);

            Nt = size(T,1);
            AFN = AstroFileName;
            AFN.JD = T.(Args.ColJD);
            AFN.julday2time;
            AFN.SubDir = T.(Args.ColSubDir);

            AFN.CropID   = T.(Args.ColCropID);
            AFN.ProjName = T.(Args.ColProjName);

            if nargout>1
                AllPath = AFN.genPath([], 'AddSubDir',true);
            end

        end
    
        function Result=getProductFromDB(T, Level, Args)
            % Given a fast moving or visit variability entry - get coadd or MatchedSources product.
            % Input  : - A table which is the output of a DB query of the
            %            fast moving or variables in visit.
            %          - Product Level: 'coadd' | 'merged'.
            %            Default is 'coadd'.
            %          * ...,key,val,...
            %            'ColProjName' - Column in table containing ProjName.
            %                   Default is 'projname'.
            %            'ColJD' - Column in table containing JD.
            %                   If a string array then will chose the first
            %                   existing column name in table.
            %                   Default is ["jd", "midjd", "pm_jd"]
            %            'ColSubDir' - Column in table containing
            %                   SubDir/visit name. Default is 'visit'.
            %            'ColCropID' - Column of CropID number.
            %                   Default is 'cropid'.
            % Example:
            % R=pipeline.last.pipes.VisitVariability.getProductFromDB(T(1,:),'merged')
            % R=pipeline.last.pipes.VisitVariability.getProductFromDB(T(1,:),'coadd')

            arguments
                T
                Level            = 'coadd';
                Args.ColProjName = 'projname';
                Args.ColJD       = ["jd", "midjd", "pm_jd"];
                Args.ColSubDir   = 'visit';
                Args.ColCropID   = 'cropid';
            end

            [AFN, AllPath] = pipeline.last.pipes.VisitVariability.getLocationFromDB(T, 'ColProjName',Args.ColProjName, 'ColJD',Args.ColJD, 'ColSubDir',Args.ColSubDir, 'ColCropID',Args.ColCropID);

            AFN.Level  = Level;
            PWD = pwd;
            Npath = numel(AllPath);
            for Ipath=1:1:Npath
                cd(AllPath(Ipath));
                switch Level
                    case 'coadd'
                        Product  = 'Image';
                        FileType = 'fits';
                        TempFileName = AFN.genFile(Ipath,'Time','*', 'Filter','*', 'FieldID','*', 'Counter','*', 'CCDID','*', 'Product',Product, 'Version', '*', 'FileType',FileType);
                        F = dir(TempFileName);
                        if isempty(F)
                            error('File %s not found','TempFileName');
                        end
                        %Result(Ipath) = AstroImage.readProducts(F(1).name, 'Path',[], 'Level',Level, 'ExtraOutProduct',["Mask", "PSF", "Cat"]);
                        Result(Ipath) = AstroImage.readFileNamesObj(F(1).name, 'Path',[], 'AddProduct',{'Mask', 'PSF', 'Cat'});
                    case 'merged'
                        Product  = 'MergedMat';
                        FileType = 'hdf5';
                        TempFileName = AFN.genFile(Ipath,'Time','*', 'Filter','*', 'FieldID','*', 'Counter','*', 'CCDID','*', 'Product',Product, 'Version', '*', 'FileType',FileType);
                        F = dir(TempFileName);
                        if isempty(F)
                            error('File %s not found','TempFileName');
                        end
                        Result(Ipath) = MatchedSources.read(F(1).name);
                    otherwise
                        error('Unknown product Level option');
                end
            end
            cd(PWD);

        end

        function [Result, Found, MS]=getLC(T, Args)
            % Given a table of variables - get objects LC.
            % Input  : - A table which is the output of a DB query of the
            %            fast moving or variables in visit.
            %          * ...,key,val,...
            %            See code for options.
            % Output : - A structure array of LCs with fields:
            %            .JD
            %            .Mag
            %          - A structure array of found objects in
            %            MatchedSources objects (i.e., source index).
            %          - A MatchedSources objects from which the LCs were
            %            retrieved.
            % Author : Eran Ofek (Mar 2025)
            % Example: R=pipeline.last.pipes.VisitVariability.getLC(T(1:2));

            arguments
                T
                Args.SearchRadius      = 3;
                Args.SearchRadiusUnits = 'arcsec';
                Args.ColRA             = 'ra';
                Args.ColDec            = 'dec';
                Args.FieldRA           = 'RA';
                Args.FieldDec          = 'Dec';
                Args.CooUnits          = 'deg';

                Args.FieldMag          = {'MAG_BEST', 'MAG_PSF', 'MAG_APER_3'};
            end
            RAD = 180./pi;

            MS = pipeline.last.pipes.VisitVariability.getProductFromDB(T, 'merged');
            TargetRA  = T.(Args.ColRA);
            TargetDec = T.(Args.ColDec);

            N = numel(TargetRA);
            for I=1:1:N
                [Found(I)] = coneSearch(MS(I), TargetRA(I), TargetDec(I), Args.SearchRadius, 'SearchRadiusUnits',Args.SearchRadiusUnits, 'CooUnits',Args.CooUnits);

                [Result(I).JD, Result(I).Mag] = MS(I).getLC_ind(Found(I).Ind, Args.FieldMag);
                Result(I).TargetRA  = TargetRA(I);
                Result(I).TargetDec = TargetDec(I);
                [Result(I).URL] = VO.search.simbad_url(Result(I).TargetRA./RAD, Result(I).TargetDec./RAD);

            end


        end
        
        function [Result, Found, MS]=plotLC(T, Args)
            % Given a table line, plot LC
            % Input  : - A table which is the output of a DB query of the
            %            fast moving or variables in visit.
            %          * ...,key,val,...
            %            See code for options.
            % Output : - A structure array of LCs with fields:
            %            .JD
            %            .Mag
            %          - A structure array of found objects in
            %            MatchedSources objects (i.e., source index).
            %          - A MatchedSources objects from which the LCs were
            %            retrieved.
            % Author : Eran Ofek (Mar 2025)
            % Example: R=pipeline.last.pipes.VisitVariability.plotLC(T(1,:));

            arguments
                T
                Args.SearchRadius      = 3;
                Args.SearchRadiusUnits = 'arcsec';
                Args.ColRA             = 'ra';
                Args.ColDec            = 'dec';
                Args.FieldRA           = 'RA';
                Args.FieldDec          = 'Dec';
                Args.CooUnits          = 'deg';

                Args.FieldMag          = {'MAG_BEST', 'MAG_PSF', 'MAG_APER_3'};

                Args.AssignToBase      = [];  % Variable name - if given, then assign the MS into this variable in the base session
            end


            [Result, Found, MS] = pipeline.last.pipes.VisitVariability.getLC(T, 'SearchRadius',Args.SearchRadius,...
                                                                                'SearchRadiusUnits',Args.SearchRadiusUnits,...
                                                                                'ColRA',Args.ColRA,...
                                                                                'ColDec',Args.ColDec,...
                                                                                'FieldRA',Args.FieldRA,...
                                                                                'FieldDec',Args.FieldDec,...
                                                                                'CooUnits',Args.CooUnits,...
                                                                                'FieldMag',Args.FieldMag);

            I = 1;
            plot((Result(I).JD-min(Result(I).JD)).*1440, Result(I).Mag, 'ko', 'MarkerFaceColor','k');
            plot.invy;
            H = xlabel('Time [min]');
            H.FontSize = 16;
            H.Interpreter = 'latex';
            H = ylabel('Mag');
            H.FontSize = 16;
            H.Interpreter = 'latex';

            if ~isempty(Args.AssignToBase)
                % assign the Table into the base session
                assignin('base', Args.AssignToBase, MS);
                fprintf('Variable %s containing MatchedSources object is assigned to base\n',Args.AssignToBase);
            end
        end
    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
