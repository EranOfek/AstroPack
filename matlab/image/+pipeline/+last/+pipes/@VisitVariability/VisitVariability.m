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
        
        function [AC,AstC,AstIndex] = searchVisitDir(Path, Args)
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
            end
           

            AstIndex = Args.AstIndex;

            PWD = pwd;
            if ~isempty(Path)
                cd(Path);
            end
            
            Files = dir(Args.FileTemp);
            Nf    = numel(Files);
            AstC  = [];
            for If=1:1:Nf
                MS = MatchedSources.read({Files(If).name});
                MS.addSrcData;

                DataPath = string(pwd);
                Tmp      = split(DataPath,'/');
                ProjName = Tmp{3};
                Visit    = Tmp(end);

                if Args.SearchAst
                    try
                        [~,TmpAst, AstIndex] = lcUtil.fitFastMotion(MS, 'AstIndex',AstIndex, 'OutType','AstroCatalog', 'INPOP',Args.INPOP, 'OrbEl',Args.OrbEl, 'Visit',Visit);
                    
                        if ~isempty(TmpAst)
                            if isempty(AstC)
                                AstC = TmpAst;
                            else
                                AstC(end+1) = TmpAst;
                            end
                        end
                    catch ME
                        TmpAst = [];
                        AstC   = [];
                        AstIndex = [];
                    end
    
                else
                    AstC     = [];
                    AstIndex = [];
                end

                if Args.SearchVar
                    % Note that the following function may modify MS
                    try
                        TmpVar = lcUtil.variabilityAnalysis(MS, Args.varAnalysisArgs{:}, 'Visit',Visit);
                        if ~isempty(TmpVar)
                            AC(If) = TmpVar;
                        end
                    catch
                        TmpVar = [];
                        AC     = [];
                    end
                    
                else
                    AC = [];
                end

            end
            
            if ~exist('AC','Var')
                AC = [];
            end
            if ~exist("AstC","var")
                AstC = [];
            end

            if Args.WriteProduct
                FN = FileNames.generateFromFileName(Files(1).name);
                FN.Product  = 'VariablesCat';
                FN.FileType = 'mat';
                OutFileName = FN.genFile;
                
                save('-v7.3', OutFileName, 'AC');
            end
            
            cd(PWD);
            
            if ~isempty(Args.DB)
                %fprintf('Write to DB not operational yet\n');

                if ~isempty(AstC)
                    AstCm=AstC.merge('IsTable',true);
                    AstCm.Catalog=db.util.insertHealpixIndex2table(AstCm.Catalog, 'ColRA','RA', 'ColDec','Dec', 'CooUnits','deg',...
                                          'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                          'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);
                    AstCm.Catalog.Flags = uint32(AstCm.Catalog.Flags);

                    Args.DB.insertCharDump('fastmoving_asteroids1',AstCm.Catalog);
                end

                if ~isempty(AC)
                    ACm=AC.merge('IsTable',true);
                    ACm.Catalog=db.util.insertHealpixIndex2table(ACm.Catalog, 'ColRA','RA', 'ColDec','Dec', 'CooUnits','deg',...
                                          'HealpixType',Args.HealpixType, 'HealpixLevel',Args.HealpixLevel,...
                                          'ColHealpix',Args.ColHealpix, 'UniqueID',Args.UniqueID);
                    ACm.Catalog.FLAGS = uint32(ACm.Catalog.FLAGS);
                    Args.DB.insertCharDump('mergedmat_var1',ACm.Catalog);
                end
            end
            
        end
    end
    
    
    methods % run on data functionality
        function [ACVar,ACAst]=analayzeAllData(Obj, Args)
            %
            % Example: VV=pipeline.last.pipes.VisitVariability;
            %          [Tvar, Tast] = VV.analayzeAllData('DB',DB);
           

            arguments
                Obj
                Args.INPOP             = celestial.INPOP.init;
                Args.OrbEl             = celestial.OrbitalEl.loadSolarSystem('merge');
                Args.DB                = []; % must be supplied
            end

            AstIndex = 0;
            
            JD = celestial.time.julday([5 1 2025]);
            T = Obj.DB.query(sprintf('SELECT * FROM visit_images WHERE midjd>%10.1f',JD));
            Nt = size(T,1);
            VecNotDone = true(Nt,1);

            Cont = true;
            Counter = 0;
            K       = 0;
            KA      = 0;
            ACAst   = [];
            while Cont  % && Counter<100
                
                K = K + 1;
                [Counter, K]
                
                I = find(VecNotDone ,1, 'first');

                FN=pipeline.last.queryDB.table2path(T(I,:));
                Path = FN.genPath('AddSubDir',true);
            
                
                %tic;
                
                [TV,TA,AstIndex] = pipeline.last.pipes.VisitVariability.searchVisitDir(Path,'WriteProduct',false,'AstIndex',AstIndex, 'INPOP',Args.INPOP, 'OrbEl',Args.OrbEl, 'DB',Args.DB);
                %toc

                if nargout>0
                    if K==1
                        ACVar = TV(:);
                    else
                        if ~isempty(TV)
                            ACVar = [ACVar; TV(:)];
                        end
                    end
    
                    if ~isempty(TA)
                        KA = KA + 1;
                        
    
                        if isempty(ACAst)
                            ACAst = TA;
                        else
                            Nex = numel(ACAst);
                            Nad = numel(TA);
                            ACAst(Nex+1:Nex+Nad) = TA(:);
                        end
                    end
                end

                Idone = strcmp(T.subdir,T.subdir{I}) & T.midjd==T.midjd(I) & T.mountnum==T.mountnum(I) & T.camnum==T.camnum(I);
                VecNotDone(Idone) = false;
                Counter = Counter + 1;
                
            end

            
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
            %                   Default is 'jd'.
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
                Args.ColJD       = 'jd';
                Args.ColSubDir   = 'visit';
                Args.ColCropID   = 'cropid';

            end

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
            %                   Default is 'jd'.
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
                Args.ColJD       = 'jd';
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

        %function [LC, MS]=getLC
        %end
        
        
    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
