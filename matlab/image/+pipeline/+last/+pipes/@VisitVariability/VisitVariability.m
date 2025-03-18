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
        function [MS, AC, Table] = analyzeMatchedSourcesFile(FileName, Path, Args)
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
            % Example: [MS,AC]=pipeline.last.pipes.VisitVariability.analyzeMatchedSourcesFile(MS)
            
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
        
        function [Table] = analyzeVisitDir(Path, Args)
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
            % Output : 
            
            arguments
                Path                      = [];
                Args.varAnalysisArgs      = {};
                Args.FileTemp             = 'LAST*_MergedMat*.hdf5';
                Args.WriteProduct logical = true; 
                Args.WriteDB logical      = true;
            end
           
            PWD = pwd;
            if ~isempty(Path)
                cd(Path);
            end
            
            Files = dir(Args.FileTemp);
            Nf    = numel(Files);
            for If=1:1:Nf
                MS = MatchedSources.read({Files(If).name});
                AC = lcUtil.variabilityAnalysis(MS, Args.varAnalysisArgs{:});
        
                if If==1
                    Table = AC(If).Catalog;
                else
                    Table = [Table; AC(If).Catalog];
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
                fprintf('Write to DB not operational yet');
            end
            
        end
    end
    
    
    methods % run on data functionality
        function analayzeAllData
            %
           
            
            JD = celestial.time.julday([1 1 2025]);
            T = DB.query(sprintf('SELECT * FROM visit_images WHERE midjd>%10.1f',JD));

            %%% MODIFY
            
            PWD = pwd;

            tic;
            K = 0;
            for I=1:1:100
                %FN = pipeline.last.queryDB.table2path(T(I,:));
                %Dir = FN.genPath('AddSubDir',1);
                %cd(Dir);

                MS=pipeline.last.queryDB.loadProducts(T(I,:),'merged','MergedMat');

                [AC(I)] = lcUtil.variabilityAnalysis(MS);

                if AC(I).sizeCatalog>0
                    K = K + 1;
                    if K==1
                        Table = AC(I).Catalog;
                    else
                        Table = [Table; AC(I).Catalog];
                    end
                end

            end
            toc


            
        end
        
    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
