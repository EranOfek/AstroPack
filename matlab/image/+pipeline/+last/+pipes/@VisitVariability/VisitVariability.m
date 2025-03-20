% pipeline.last.pipes.VisitVariability - A class for searching for variable
% sources in the visit level data.
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
        
        function [Table,TableAst] = searchVisitDir(Path, Args)
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
            % Output : - A table with all variable source candidates found
            %            in all cropIDs in visit.
            % Example:
            % [TV,TA]=pipeline.last.pipes.VisitVariability.searchVisitDir('/marvin/LAST.01.03.02/2025/03/12/proc/184350v0');
            
            arguments
                Path                      = [];
                Args.varAnalysisArgs      = {};
                Args.FileTemp             = 'LAST*_MergedMat*.hdf5';
                Args.WriteProduct logical = true; 
                Args.WriteDB logical      = true;
                Args.AstIndex             = 0;
            end
           
            AstIndex = Args.AstIndex;

            PWD = pwd;
            if ~isempty(Path)
                cd(Path);
            end
            
            Files = dir(Args.FileTemp);
            Nf    = numel(Files);
            for If=1:1:Nf
                MS = MatchedSources.read({Files(If).name});

                [~,TmpAst, AstIndex] = lcUtil.fitFastMotion(MS, 'AstIndex',AstIndex);

                % Note that the following function may modify MS
                AC(If) = lcUtil.variabilityAnalysis(MS, Args.varAnalysisArgs{:});
        

                if If==1
                    Table = AC(If).Catalog;
                    TableAst = TmpAst;
                else
                    Table = [Table; AC(If).Catalog];
                    TableAst = [TableAst; TmpAst];
                end
            end
            
            if Args.WriteProduct
                FN = FileNames.generateFromFileName(Files(1).name);
                FN.Product  = 'VariablesCat';
                FN.FileType = 'mat';
                OutFileName = FN.genFile;
                
                save('-v7.3', OutFileName, 'Table');
            end
            
            cd(PWD);
            
            if Args.WriteDB
                %fprintf('Write to DB not operational yet\n');
            end
            
        end
    end
    
    
    methods % run on data functionality
        function OutTable=analayzeAllData(Obj)
            %
            % Example: VV=pipeline.last.pipes.VisitVariability;
            %          Tout = VV.analayzeAllData;
           
            
            JD = celestial.time.julday([1 1 2025]);
            T = Obj.DB.query(sprintf('SELECT * FROM visit_images WHERE midjd>%10.1f',JD));
            Nt = size(T,1);
            VecNotDone = true(Nt,1);

            Cont = true;
            Counter = 0;
            K       = 0;
            while Cont && Counter<500
                K = K + 1;
                K
                
                I = find(VecNotDone ,1, 'first');

                FN=pipeline.last.queryDB.table2path(T(I,:));
                Path = FN.genPath('AddSubDir',true);
            
                
                %tic;
                TV = pipeline.last.pipes.VisitVariability.searchVisitDir(Path,'WriteProduct',false);
                %toc

                Idone = strcmp(T.subdir,T.subdir{I}) & T.midjd==T.midjd(I) & T.mountnum==T.mountnum(I) & T.camnum==T.camnum(I);
                VecNotDone(Idone) = false;
                Counter = Counter + 1;
                
                if Counter==1
                    OutTable = TV;
                else
                    OutTable = [OutTable; TV];
                end
            end

            
        end
        
    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
