%

classdef MatchedSources < Component
    properties
        Data(1,1) struct  % each field [Nepoch, Nsrc]
    end
    
    properties (Dependent)
        Fields cell
    end
    properties
        Nsrc(1,1)
        Nepoch(1,1)
    end
    
    properties (Constant)
        DimEpoch = 1;
        DimSrc   = 2;
    end
   
    methods % constructor
        
    end
    
    methods % setters/getters
        function Result = get.Nsrc(Obj)
            % getter for Nsrc
            FN = fieldnames(Obj.Data);
            if isempty(FN)
                Result = [];
            else
                Result = size(Obj.Data.(FN{1}), Obj.DimSrc);
            end
        end
        
        function Result = get.Nepoch(Obj)
            % getter for Nepoch
            FN = fieldnames(Obj.Data);
            if isempty(FN)
                Result = [];
            else
                Result = size(Obj.Data.(FN{1}), Obj.DimEpoch);
            end
        end
        
        function Result = get.Fields(Obj)
            % getter for Fields
            Result = fieldnames(Obj.Data);
        end
    end
    
    methods (Static) % static
        
    end
    
    methods % read/write
        
    end
    
    methods  % functions
        function Obj = addMatrix(Obj, Matrix, FieldName)
            % Add matrix into the MatchedSources Data
            % Obj = addMatrix(Obj, Matrix, FieldName)
            % Input  : - A single element MatchedSources object.
            %          - One of the following inputs:
            %            1. A numeric matrix. In this case, FieldName must
            %            be a char array with field name in which to store
            %            the matrix.
            %            2. A cell array of matrices. In this case
            %            FieldName must be a cell array of field names, per
            %            matrix.
            %            3. A struct. In this case the struct will be
            %            copied to the Data field.
            %            4. A matched AstroTable/AstroCatalog object
            %            In this case, the columns will be selected from
            %            FieldName.
            %          - A field name.
            %            If the field already exist, then replace the
            %            current content with the new content.
            % Output : - A MatchedSources object.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources;
            %          MS.addMatrix(rand(100,200),'FLUX')
            %          MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            %          MS = MatchedSources;
            %          St.Flux=rand(100,200); MS.addMatrix(St);
            
            
            arguments
                Obj(1,1)
                Matrix
                FieldName   = [];
            end
           
            if isnumeric(Matrix)
                % matrix is numeric - add
                if ~ischar(FieldName)
                    error('For numeric matrix FieldName must be a char array');
                end
                Obj.Data.(FieldName) = Matrix;
            elseif isstruct(Matrix)
                % store the struct as is in Data
                Obj.Data = Matrix;
            elseif iscell(Matrix)
                Ncell = numel(Matrix);
                for Icell=1:1:Ncell
                    Obj.Data.(FieldName{Icell}) = Matrix{Icell};
                end
            elseif isa(Matrix, 'AstroTable')
                % Assume input is an array of matched AstroTables
                [Nrow, Ncol] = Matrix.sizeCatalog;
                if ~all(Nrow==Nrow(1))
                    error('For AstroTable/AstroCatalog input, all catalogs must have the same number of rows');
                end
                if ischar(FieldName)
                    FieldName = {FieldName};
                end
                
                [Res, Summary, N_Ep] = imProc.match.matched2matrix(Matrix, FieldName, true);
                Obj.addMatrix(Res);
%                 
%                 Nepoch = numel(Matrix);
%                 Nfn = numel(FieldName);
%                 for Ifn=1:1:Nfn
%                     Obj.Data.(FieldName{Ifn}) = nan(Nepoch, Nrow(1));
%                     for Iepoch=1:1:Nepoch
%                         ColVal = getCol(Matrix(Iepoch), FieldName{Ifn});
%                         Obj.Data.(FieldName{Ifn})(Iepoch,:) = ColVal.';
%                     end
%                 end
                        
            else
                error('Unknown Matrix input option');
            end
            
        end
        
        function varargout = getMatrix(Obj, FieldNames)
            % Get matrix using field name
            % Input  : - A single element MatchedSources object.
            %          - Field name, or a cell array of field names.
            % Output : - A mtrix of [Epoch X Source] for each requested
            %            field.
            % Author : Eran Ofek (Jun 2021)
            % Example: MS = MatchedSources; MS.Data.FLUX = rand(100,200);
            %          A = MS.getMatrix('FLUX');
            
            arguments
                Obj(1,1)
                FieldNames
            end
           
            if ischar(FieldNames)
                FieldNames = {FieldNames};
            end
            
            Nfn = numel(FieldNames);
            if nargout>Nfn
                error('Number of output arguments is larger than the number of requested fields');
            end
            varargout = cell(1, nargout);
            for Ifn=1:1:nargout
                varargout{Ifn} = Obj.Data.(FieldNames{Ifn});
            end
            
            
        end
    end
    
    methods (Static) % unitTest
        function Result = unitTest()
            % unitTest for MatchedSources class
           
            % addMatrix
            MS = MatchedSources;
            MS.addMatrix(rand(100,200),'FLUX')
            MS.addMatrix({rand(100,200), rand(100,200), rand(100,200)},{'MAG','X','Y'})
            MS = MatchedSources;
            
            St.Flux=rand(100,200);
            MS.addMatrix(St);
            
            % match sources for addMatrix:
            AC = AstroCatalog;
            AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
            AC.ColNames = {'RA','Dec'};
            AC.ColUnits = {'rad','rad'};
            AC.getCooTypeAuto
            AC2 = AstroCatalog;
            AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0];
            AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
            AC2.getCooTypeAuto
            [MC,UM,TUM] = imProc.match.match(AC,AC2,'Radius',0.01,'RadiusUnits','rad');
            [MC,UM,TUM] = imProc.match.match([AC;AC2; AC; AC2],AC2,'Radius',0.01,'RadiusUnits','rad');
            % run addMatrix with AstroCatalog input
            MS = MatchedSources;
            MS.addMatrix(MC,{'RA','Dec'});
            
            
            Result = true;
        end
    end
end