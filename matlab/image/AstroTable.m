% AstroTable handle class
% Package: @AstroTable
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroTable < Component %ImageComponent
    % Component should contain:
    % UserData
    % Config
    
    properties (SetAccess = public)
        Catalog                                                = [];
        ColNames cell                                           = {};
        ColUnits cell                                          = {};
        ColDesc cell                                           = {};
        SortByCol                                              = [];
        IsSorted(1,1) logical                                  = false;
    end
    
    methods % Constructor
       
        function Obj = AstroTable(AnotherObj, Args)
            % Constrt an AstroTable object or transform AstCat/struct/ to AstroTable
            % Input  : - If empty then construct and empty AstroTable object.
            %            If array/table, then construct an AstroTable object
            %               with this array in the Catalog property.
            %            If an AstCat/catCl object then convert it to
            %               AstroTable object.
            %            If file name or a cell array of file names, then
            %               attempt read data from files. Cell array
            %               contains a list of file names, while a single
            %               file may contain wild cards or gegular
            %               expressions.
            %          * ...,Key,Val,...
            %            'ColNames' - A cell array of column names.
            %                   If empty, try to use other inputs.
            %                   Default is {}.
            %            'ColUnits' - A cell array of column units.
            %                   If empty, try to use other inputs.
            %                   Default is {}.
            %            'UseRegExp' - Logical indicating if to use regexp
            %                   when using io.files.filelist.
            %                   Default is false.
            %            'FileType' - File type from which to read data:
            %                   'fits' - FITS table. Default.
            %                   'hdf5' - HDF5 file (dataset is indicated by
            %                           HDU).
            %                   'ipac' | 'txt' | 'mat' | ...
            %            'TableType' - FITS table type: ['auto'] | 'bintable' | 'table'
            %            'HDU' - FITS HDU number or HDF5 dataset name.
            %            'ArgsreadTable1' - A cell array of additional
            %                   arguments to pass to FITS.readTable1.
            %                   Default is {}.
            %            'ConvertTable2array' - When eading a FITS table,
            %                   attempt to convert the table to an array (only of
            %                   all columns are of class double).
            %                   Default is true.
            % Output : - An AstroTable object.
            % Author : Eran Ofek (Mar 2021)
            % Example: AC=AstroTable(array2table(rand(10,2)));
            %          AC=AstroTable(rand(10,2),'ColNames',{'RA','Dec'});
            %          A = AstCat; A(1).Cat=rand(10,2); A(2).Cat=rand(10,2);
            %          AC = AstroTable(A);
            %          AC = AstroTable(A,'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
            %          AC=AstroTable('asu.fit','HDU',2); % read from FITS table
            %          
            
            arguments
                AnotherObj                    = [];
                Args.ColNames cell            = {};
                Args.ColUnits cell            = {};
                Args.UseRegExp(1,1) logical   = false;
                Args.FileType                 = 'fits'; % 'fits' | 'hdf5' | ...
                Args.TableType                = 'auto'; % 'auto'|'bintable'|'table' for FITS.readTable1
                Args.HDU                      = 1;  % HDU or dataset name
                Args.ArgsreadTable1           = {};
                Args.ConvertTable2array       = true;  % only if all columns are double
            end
            
            % FFU: use ImageIO instead!!!
            
            if isempty(AnotherObj)
                Obj.Catalog = [];
            else
                if ischar(AnotherObj) || iscellstr(AnotherObj) || isstring(AnotherObj)
                    % read from files
                    List  = io.files.filelist(AnotherObj, Args.UseRegExp);
                    Nlist = numel(List);
                    for Ilist=1:1:Nlist
                        % create a single empty AstroTable
                        Obj(Ilist) = AstroTable;
                        switch lower(Args.FileType)
                            case 'fits'
                                % read FITS table
                                Table = FITS.readTable1(List{Ilist}, 'HDU',Args.HDU,...
                                                                     'TableType',Args.TableType,...
                                                                     Args.ArgsreadTable1{:});
                                Obj(Ilist).Catalog = Table;
                                if Args.ConvertTable2array
                                    ColTypes = varfun(@class,Obj(Ilist).Catalog,'OutputFormat','cell');
                                    if all(strcmp(ColTypes, 'double'))
                                        % conversion to matrix is doable
                                        Obj(Ilist).Catalog  = table2array(Obj(Ilist).Catalog);
                                        Obj(Ilist).ColNames  = Table.Properties.VariableNames;
                                        Obj(Ilist).ColUnits = Table.Properties.VariableUnits;
                                    end
                                end
                                
                            case {'hdf5','h5'}
                                % read from HDF5
                                % FFU
                                error('FFU: Unsupported FileType');
                                
                            case {'ipac'}
                                % read IPAC text table
                                error('FFU: Unsupported FileType');
                                
                            case {'mat'}
                                % read from a mat file containing one of
                                % the supported formats
                                error('FFU: Unsupported FileType');
                                % NOT TESTED
                                %Obj = AstroTable(io.files.load2(List{Ilist}));
                                
                            case {'txt'}
                                % attempt to read from text file
                                error('FFU: Unsupported FileType');
                                
                            otherwise
                                error('Unknown FileType option');
                        end
                    end
                        
                elseif isnumeric(AnotherObj) || istable(AnotherObj)
                    % read table into AstroTable
                    Obj(1).Catalog = AnotherObj;
                    if isempty(Args.ColNames)
                        if istable(AnotherObj)
                            Obj(1).ColNames = AnotherObj.Properties.VariableNames;
                        end
                    else
                        Obj(1).ColNames = Args.ColNames;
                    end
                    if isempty(Args.ColUnits)
                        if istable(AnotherObj)
                            Obj(1).ColUnits = AnotherObj.Properties.VariableUnits;
                        end
                    else
                        Obj(1).ColUnits = Args.ColUnits;
                    end
                elseif isa(AnotherObj,'AstCat') || isa(AnotherObj,'catCl')
                    % read AstCat or catCl objects
                    Nobj = numel(AnotherObj);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroTable;
                        Obj(Iobj).Catalog  = AnotherObj(Iobj).Cat;
                        if isempty(Args.ColNames)
                            Obj(Iobj).ColNames  = AnotherObj(Iobj).ColNames;
                        else
                            Obj(Iobj).ColNames  = Args.ColNames;
                        end
                        if isempty(Args.ColUnits)
                            Obj(Iobj).ColUnits = AnotherObj(Iobj).ColUnits;
                        else
                            Obj(Iobj).ColUnits = Args.ColUnits;
                        end

                    end
                    
                elseif isa(AnotherObj,'AstroTable')
                    % already in AstroTable
                    Obj = AnotherObj;
                else
                    error('First input argument is of unsupported class');
                end
            end
                   
        end

    end
 
%     methods % subsref
%         function varargout = subsref(Obj,S)
%             % FOR SMALL TABLES THIS SLOWS DOWN PERFORMNCES BY FACTOR OF 4!
%             % tic; for I=1:1:1000, aa=AC.Catalog; end; toc

%             if numel(S)>1 && strcmp(S(end-1).subs,'CatCol')
%                 % identified a CatColumn requesy
%                 if strcmp(S(end).type,'.')
%                     ColInd = colname2ind(Obj, S(end).subs);
%                     varargout{1} = Obj.Catalog(:,ColInd);
%                     if istable(varargout{1})
%                         varargout{1} = table2array(varargout{1});
%                     end
%                 end
%             else
%                 if nargout==0
%                     Nargout = 1;
%                 else
%                     Nargout = nargout;
%                 end
%                 [varargout{1:Nargout}] = builtin('subsref',Obj,S);
%             end
%             
%         end
%     end

 
    methods % Setters/Getters
        function set.Catalog(Obj, Data)
            % setter for catalog - set also column names and units if table
            % and available
            
            if istable(Data)
                Obj.Catalog = Data;
                %Obj.ColNames = Data.Properties.VariableNames; % result in
                %infinte recyusrion
                
            elseif isnumeric(Data)
                Obj.Catalog = Data;
            elseif iscell(Data)
                Obj.Catalog = cell2table(Data);
            else
                error('Catalog must be numeric, cell, or table');
            end
            Obj.IsSorted = false;
        end
        
        function Result = get.Catalog(Obj)
            % getter for Image - get image from ImageData property
            Result = Obj.Catalog;
        end        
        
        function set.ColNames(Obj, CellColName)
            % setter for ColNames - input is either a cell or a string array
            % if the catalog is in table format it will also set its
            % Properties.VariableNames (but only if the size is consistent)
            Obj.ColNames = CellColName;
            if istable(Obj.Catalog)
                % set the column names also in the table
                if numel(CellColName)==size(Obj.Catalog,2)
                    Obj.Catalog.Properties.VariableNames = CellColName;
                end
            end
        end
        
        function CellColName = get.ColNames(Obj)
            % getter for ColNames
           
            CellColName = Obj.ColNames;
            if isempty(CellColName) && ~isempty(Obj.Catalog) && istable(Obj.Catalog)
                % get ColNames from table properties
                CellColName = Obj.Catalog.Properties.VariableNames;
            end
        end
            
        function set.ColUnits(Obj, CellColUnits)
            % setter for ColUnits - input is either a cell or a string array
            % if the catalog is in table format it will also set its
            % Properties.VariableUnits (but only if the size is consistent)
            Obj.ColUnits = CellColUnits;
            if istable(Obj.Catalog)
                % set the column names also in the table
                if numel(CellColUnits)==size(Obj.Catalog,2)
                    Obj.Catalog.Properties.VariableUnits = CellColUnits;
                end
            end
        end
        
        function CellColUnits = get.ColUnits(Obj)
            % getter for ColUnits
           
            CellColUnits = Obj.ColUnits;
            if isempty(CellColUnits) && ~isempty(Obj.Catalog) && istable(Obj.Catalog)
                % get ColUnits from table properties
                CellColUnits = Obj.Catalog.Properties.VariableUnits;
            end
        end
       
        function set.SortByCol(Obj,Val)
            % Setter for SortByCol, also set IsSorted to false - sue sortrows to sort
            
            Obj.SortByCol = Val;
            Obj.IsSorted  = false;
        end
        
    end
    
    methods (Static)  % static methods
       function VarNames = defaultColNames(Ncol)
            % create a default ColNames with N columns
            % Package: @AstroTable (Static)
            % Input  : - Number of columns
            % Example: AstroTable.defaultColNames(5)
            
            VarNames = cell(1,Ncol);
            for Icol=1:1:Ncol
                VarNames{Icol} = sprintf('Var%d',Icol);
            end
       end
        
       function Ans = compareColNames(ColNames1,ColNames2)
            % Compare two ColNames's
            % Package: @AstroTable (Static)
            % Description: Given two cell array of strings compare their
            %              content. Return a vector of logical of length
            %              equal to the longer ColNames.
            % Input  : - First ColNames
            %          - Second ColNames
            % Output : - a vector of logical of length
            %            equal to the longer ColNames. True if two elements
            %            are identical.
            % Example: AstroTable.compareColNames({'a','b'},{'a','b'})
            
            N1 = numel(ColNames1);
            N2 = numel(ColNames2);
            
            MaxN = max(N1,N2);
            MinN = min(N1,N2);
            Ans  = false(1,MaxN);
            Ans(1:MinN) = ~tools.cell.isempty_cell(regexp(ColNames1(1:MinN),ColNames2(1:MinN),'match'));
            
       end
        
       function NewArray = insertColumn(OldArray,NewData,ColInd)
            % Insert a single column into a matrix, table, or a cell array
            % Package: @AstroTable (Static)
            % Description: Insert a single column into a matrix, table,
            %              or a cell array.
            % Input  : - A matrix, cell array, or a table.
            %          - A multiple column array, table, or cell array, in
            %            which the number of rows is equal to the number of
            %            rows in the first input argument.
            %            The type must be equalt to the type of the first
            %            input.
            %          - A column index in which to insert the new column.
            %            This is the index of the new column after
            %            insertion. For example, 1 will insert the new
            %            column as the first column. If Inf, will insert in
            %            the last column position.
            % Output : - The new array.
            % Example: AstroTable.insertColumn({'a','b','c'},{'d','e'},2)
            %          AstroTable.insertColumn(ones(5,3),zeros(5,2),2)
            %          TT=array2table(zeros(5,2));
            %          TT.Properties.VariableNames={'a','b'};
            %          AstroTable.insertColumn(array2table(ones(5,3)),TT,2)
            
            [Nrow,Ncol]   = size(OldArray);
            [NrowI,NcolI] = size(NewData);
            
            if ~strcmp(class(OldArray),class(NewData))
                error('First two input arguments must be of the same class');
            end
            
            if  (Nrow~=NrowI)
                error('Number of rows in column to insert must equal to the number of rows in array');
            end
            if isinf(ColInd)
                ColInd = Ncol+1;
            end
            if ColInd>(Ncol+1) || ColInd<1
                error('Column index in which to insert the column must be between 1 and number of columns +1');
            end
            
            
            NewArray = [OldArray(:,1:(ColInd-1)), NewData, OldArray(:,ColInd:end)];
       end
        
       function [Name, IndInCell, IndInSynonym] = searchSynonym(Cell, SynonymCell, Args)
           % Find the first appearance of a synonym in a cell array.
           % Input  : - A cell array of names.
           %          - A cell array of synonyms.
           %          * ...,key,val,...
           %            'CaseSens' - Case sensetive search. Default is
           %                    false.
           % Output : - The found name as appear in the Cell of names.
           %          - The index of the name in the cell of names.
           %          - The index of the name as appear in the cell of
           %            synonyms.
           % Example: [Name, IndInCell, IndInSynonym] = AstroTable.searchSynonym({'Number','ra','dec','Flux','Color'},{'ALPHA_J2000','RAJ2000','RA','ALPHA'});
           
           arguments
               Cell 
               SynonymCell
               Args.CaseSens(1,1) logical            = false;
           end
           
           if Args.CaseSens
               [Flag,IndInSyn] = ismember(Cell, SynonymCell);
           else
               [Flag,IndInSyn] = ismember(lower(Cell), lower(SynonymCell));
           end
           
            if all(IndInSyn==0)
                % not found
                Name          = {};
                IndInCell     = [];
                IndInSynonym  = [];
            else             
                IndInCell     = find(IndInSyn>0, 1, 'first');
                IndInSynonym  = IndInSyn(IndInCell);
                Name          = Cell(IndInCell);
            end

        end
    end
    
    
    methods % get general info
        function [Nrow,Ncol] = sizeCatalog(Obj)
            % Return the number o rows and columns in all elements in an AstroTable object
            % Example: [Nrow,Ncol] = sizeCatalog(Obj)
            
            Nrow = zeros(size(Obj));
            Ncol = zeros(size(Obj));
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                [Nrow(Iobj), Ncol(Iobj)] = size(Obj(Iobj).Catalog);
            end
                
        end
        
        function Result = isemptyCatalog(Obj)
            % Return true if Catalog data in AstroTable object is empty, otherwise false.
            % example: Result = isemptyCatalog(Obj)
            
            Result = true(size(Obj));
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj) = isempty(Obj(Iobj).Catalog);
            end
            
        end
        
        function Result = isColumn(Obj,ColName)
            % Return true if a name is an existing column name in an AstroTable object
            % Example: Result = isColumn(Obj,'RA')
            
            arguments
                Obj
                ColName char
            end
            
            Result = false(size(Obj));
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Result(Iobj) = any(strcmp(ColName, Obj(Iobj).ColNames));
            end
            
        end
            
        function ColInd = colname2ind(Obj, ColName, FillValue)
            % Convert column names to column indices
            % Input  : - A single element AstroTable object.
            %          - A column name, a cell array of column names or an
            %            array of column indices.
            %            If empty, then return all columns.
            %            Default is empty.
            %          - If the colun name is char or cell, and it doesn't
            %            exist, this is the fill value for the column
            %            index. If empty, will fail. Default is NaN.
            % Output : - A vector of column indices corresponding to the column names.
            % Author : Eran Ofek (Mar 2021)
            % Example: colname2ind(AC, {'Var1','aa'})
            %          colname2ind(AC, [1 2])
            
            arguments
                Obj(1,1)
                ColName        = [];
                FillValue      = NaN;
            end
           
            if isempty(ColName)
                ColInd = (1:1:numel(Obj.ColNames));
            else
                if isnumeric(ColName)
                    % assumes columns are already column index
                    ColInd = ColName;
                elseif ischar(ColName)
                    ColInd = find(strcmp(Obj.ColNames, ColName));
                    Tmp = find(strcmp(Obj.ColNames, ColName));
                    if isempty(Tmp)
                        if isempty(FillValue)
                            error('Column %s not found',ColName);
                        else
                            ColInd = FillValue;
                        end
                    else
                        ColInd = Tmp;
                    end

                elseif iscell(ColName) || isstring(ColName)
                    Ncol   = numel(ColName);
                    ColInd = nan(1,Ncol);
                    for Icol=1:1:Ncol
                        Tmp = find(strcmp(Obj.ColNames, ColName{Icol}));
                        if isempty(Tmp)
                            if isempty(FillValue)
                                error('Column %s not found',ColName{Icol});
                            else
                                ColInd(Icol) = FillValue;
                            end
                        else
                            ColInd(Icol) = Tmp;
                        end
                    end
                end
            end
        end
        
        function ColName = colind2name(Obj, ColInd)
            % Return column names corresponding to column indices
            % Input  : - A single element AstroCatlog object
            %          - A vector of column indices.
            % Output : - A cell array of column names corresponding to the
            %            column indices.
            % Example: colind2name(AC,[2 1])
           
            if iscell(ColInd) || isstring(ColInd)
                % assume already in cell format
                ColName = ColInd;
            else
                ColName = Obj.ColNames(ColInd);
            end
        end
        
        function St = col2struct(Obj)
            % return structure array with column names in field and index in values.
            % Example: col2struct(A)
            
            Nobj = numel(Obj);
            Iobj = 1;
            St   = tools.struct.struct_def(Obj(Iobj).ColNames,size(Obj));
            for Iobj=1:1:Nobj
                St(Iobj) = cell2struct(num2cell(1:1:numel(Obj(Iobj).ColNames)), Obj(Iobj).ColNames,2);
            end
            
        end
        
        function Result = isColIdentical(Obj, ColNames)
            % Check if ColNames in an AstroTable object is identical to another ColNames
            % Package: @AstroTable
            % Descriptio: For each element in a AstroTable object check if the
            %             ColNames property is equal to a reference ColNames.
            % Input  : - An AstroTable object.
            %          - A cell array of column names (i.e., a reference
            %            ColNames).
            % Output : - A vector lf logical indicating if ColNames's are
            %            identical
            % Example:  Ans=isColIdentical(C,C(4).ColNames)
            
            Nobj   = numel(Obj);
            Result = false(size(Obj));
            for Iobj=1:1:Nobj
                Result(Iobj) = all(AstroTable.compareColNames(Obj(Iobj).ColNames, ColNames));
            end
        end
        
    end
    
    methods  % columns get/edit
        function Result = getCol(Obj, Columns, OutputIsTable, UpdateAstroTable, Args)
            % Get a catalog columns by index or names
            % Input  : - A single element AstroTable object.
            %          - A vector of column indices, or a column name, or a
            %            cell array of column names.
            %          - If true will convert the output to table,
            %            otherwise will attempt to convert to a matrix.
            %            Default is false.
            %          - If true than in addition to returning the sub
            %            catalog, will store this sub catalog in the
            %            AstroTable object.
            %            Default is false.
            %          - Attempt to use dictionary for column names.
            % Output : - A matrix or a table containing the selected
            %            columns.
            % Author : Eran Ofek (Mar 2021)
            % Example: AC.getCol({'Var1','Var3'},true)
            %          AC.getCol([1 2])
            %          AC.getCol({'Var1','Var3'},true,true)
            
            arguments
                Obj(1,1)
                Columns
                OutputIsTable(1,1) logical         = false;
                UpdateAstroTable(1,1) logical    = false;
                Args.UseDict(1,1) logical          = true;
                Args.DictName                      = {};
                Args.DictFamily char               = '';
            end
                
            
%             if Args.UseDict
%                 error('FFU: Dictinary is not implemented yet');
%             end
            ColInd = colname2ind(Obj, Columns, []);
            if istable(Obj.Catalog)
                if OutputIsTable
                    Result = Obj.Catalog(:,ColInd);
                else
                    Result = table2array(Obj.Catalog(:,ColInd));
                end
            else
                if OutputIsTable
                    Result = array2table(Obj.Catalog(:,ColInd));
                    Result.Properties.VariableNames = Obj.ColNames;
                    Result.Properties.VariableUnits = Obj.ColUnits;
                else
                    Result = Obj.Catalog(:,ColInd);
                end
            end
            
            if UpdateAstroTable
                Obj.Catalog = Result;
            end
            
        end
        
        function Obj = array2table(Obj)
            % Convert catalog data in AstroTable to table format
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if ~istable(Obj(Iobj).Catalog)
                    Obj(Iobj).Catalog = array2table(Obj(Iobj).Catalog);
                    Obj(Iobj).Catalog.Properties.VariableNames = Obj(Iobj).ColNames;
                    Obj(Iobj).Catalog.Properties.VariableUnits = Obj(Iobj).ColUnits;
                end
            end
        end
        
        function Obj = table2array(Obj)
            % Convert catalog data in AstroTable to array format
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if istable(Obj(Iobj).Catalog)
                    Obj(Iobj).Catalog = table2array(Obj(Iobj).Catalog);
                end
            end
        end
        
        function Obj = insertCol(Obj, Data, Pos, NewColNames, NewColUnits)
            % Insert columns to AstroTable object
            % Input  : - An AstroTable object
            %          - Array, cell array, table, or another AstroTable
            %            object to insert.
            %          - Either number, or column name before which to insert
            %            the new columns.
            %          - Cell array of new column names. Default is {}.
            %            If empty, then use default names.
            %          - Cell array of new column units. Default is {}.
            %            If empty, then use ''.
            % Output : - The AstroTable object with the new columns.
            % Example: A=AstroTable; A.Catalog=rand(10,3); A.ColNames={'a','b','c'}; insertCol(A,ones(10,2),'c')     

            arguments
                Obj
                Data
                Pos
                NewColNames                           = {};
                NewColUnits                           = {};
            end
            
            if ~iscell(NewColNames) && ~isstring(NewColNames)
                NewColNames = {NewColNames};
            end
           
            
            Nobj = numel(Obj);
            if isa(Data,'AstroTable')
                Nobj2 = numel(Data);
                for Iobj=1:1:Nobj
                    if isempty(Obj(Iobj).ColUnits)
                        Ncol = numel(Obj(Iobj).ColNames);
                        [Obj(Iobj).ColUnits{1:Ncol}] = deal('');
                    end
                    Iobj2             = min(Nobj,Nobj2);
                    ColInd            = colname2ind(Obj(Iobj), Pos);
                    Obj(Iobj).Catalog = AstroTable.insertColumn(Obj(Iobj).Catalog, Data(Iobj2).Catalog, ColInd);
                    if isempty(NewColNames)
                        % attempt to copy ColNames from Data
                        Obj(Iobj).ColNames = AstroTable.insertColumn(Obj(Iobj).ColNames, Data(Iobj2).ColNames, ColInd);
                        Obj(Iobj).ColUnits = AstroTable.insertColumn(Obj(Iobj).ColUnits, Data(Iobj2).ColUnits, ColInd);
                    else
                        Obj(Iobj).ColNames = AstroTable.insertColumn(Obj(Iobj).ColNames, NewColNames, ColInd);
                        Obj(Iobj).ColUnits = AstroTable.insertColumn(Obj(Iobj).ColUnits, NewColUnits, ColInd);
                    end
                    
                end
            else
                if isempty(NewColNames)
                    NewColNames = AstroTable.defaultColNames(size(Data,2));
                    
                end
                if isempty(NewColUnits)
                    NcolInsert = size(Data,2);
                    [NewColUnits{1:NcolInsert}] = deal('');
                end
                
                for Iobj=1:1:Nobj
                    if isempty(Obj(Iobj).ColUnits)
                        Ncol = numel(Obj(Iobj).ColNames);
                        [Obj(Iobj).ColUnits{1:Ncol}] = deal('');
                    end
                    ColInd            = colname2ind(Obj(Iobj), Pos);
                    Obj(Iobj).Catalog = AstroTable.insertColumn(Obj(Iobj).Catalog, Data, ColInd);
                   
                    Obj(Iobj).ColNames = AstroTable.insertColumn(Obj(Iobj).ColNames, NewColNames, ColInd);
                    Obj(Iobj).ColUnits = AstroTable.insertColumn(Obj(Iobj).ColUnits, NewColUnits, ColInd);
                end
            end
        end
        
        function Obj = replaceColNames(Obj,OldNames,NewNames)
            % Replace column names in AstroTable object
            % Input  : - An AstroTable object.
            %          - A cell array of column names in the existing input
            %            AstroTable object, or a vector of indices.
            %          - A cell array of new names to replace the old
            %            column names.
            % Output : - An AstroTable object with the updated column
            %            names.
            % Example: AC.replaceColNames([1 2],{'RA','Dec'})
            
            if numel(OldNames)~=numel(NewNames)
                error('Number of old and new names should be identical');
            end
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                ColInd = colname2ind(Obj(Iobj), OldNames);
                Obj(Iobj).ColNames(ColInd) = NewNames;
            end
            
        end
        
        function Obj = replaceCol(Obj, NewData, ColNames)
            % replace columns in AstroTable
            % Input  : - An AstroTable object.
            %          - Data of columns to replace. This may be a matrix,
            %            cell array, a table or another AstroTable
            %            object. However, the data types should be
            %            consistent.
            %            If AstroTable then numver of elements should be
            %            1 or equal to the number of elements in the first
            %            input.
            % Example: AC = AstroTable;
            %          AC(1).Catalog = rand(100,3);
            %          AC(1).ColNames={'a','b','c'};
            %          AC=AC.replaceCol(nan(100,2),{'a','b'});
            
            arguments
                Obj
                NewData
                ColNames                 = [];
            end
            
            Nobj   = numel(Obj);
            if isa(NewData, 'AstroTable')
                % Data is in AstroTable format
                Nobj2 = numel(NewData);
                if ~(Nobj2==1 || Nobj2==Nobj)
                    error('Number of elements in second AstroTable must be 1 or equal to the number in the first input');
                end
                for Iobj=1:1:Nobj
                    ColInd   = colname2ind(Obj(Iobj),ColNames);
                    %NcolData = size(Obj(Iobj2).Catalog,2);
                    Iobj2    = min(Nobj2,Iobj);
                    Obj(Iobj).Catalog(:,ColInd) = NewData(Iobj2).Catalog;
                end
            else
                % 
                for Iobj=1:1:Nobj
                    ColInd   = colname2ind(Obj(Iobj),ColNames);
                    Obj(Iobj).Catalog(:,ColInd) = NewData;
                end
            end
        end
        
        function Obj = deleteCol(Obj, Columns)
            % Delete columns fron an AstroTable object.
            % Input  : - An AstroTable object.
            %          - Column name, cell array of columns or column
            %            indices to remove from Catalog.
            % Output : - An AstroTable object with the deleted columns.
            % Example: AC.Catalog=array2table(rand(10,3));
            %          AC.ColNames={'RA','Dec','flux'}; AC.deleteCol('Dec')
            %          AC.Catalog=(rand(10,3));
            %          AC.ColNames={'RA','Dec','flux'}; AC.deleteCol({'RA','Dec'})

            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                ColInd = colname2ind(Obj(Iobj), Columns);
                Obj(Iobj).Catalog(:,ColInd) = [];
                if ~isempty(Obj(Iobj).ColNames)
                    Obj(Iobj).ColNames(ColInd) = [];
                end
                if ~isempty(Obj(Iobj).ColUnits)
                    Obj(Iobj).ColUnits(ColInd) = [];
                end
            end
                
                
                
            
            
        end
       
        function NewObj = merge(Obj,Columns,Args)
            % Merge table/matrices in multiple AstroTable elements
            % Input  : - An AstroTable object.
            %          - Columns to merge. Either column names or column
            %            indices. If empty, merge all columns.
            %          * ...,key,val,...
            %            'IsTable' - Attempt to merge the catalogs as
            %                   tables. Default is false.
            % Output : - A new AstroTable object containing the merged
            %            catalog.
            % Author : Eran Ofek (Mar 2021)
            % Example: AC = AstroTable; AC(1).Catalog=rand(10,3);
            % AC(1).ColNames={'a','b','c'}; AC(2).Catalog=rand(10,2);
            % AC(2).ColNames={'a','c'};
            % NAC=merge(AC,{'a','c'})
            %
            % AC = AstroTable; AC(1).Catalog=rand(10,3);
            % AC(1).ColNames={'a','b','c'}; AC(2).Catalog=rand(10,3); AC(2).ColNames={'a','b','c'};
            % NAC=merge(AC)
            
            arguments
                Obj
                Columns                       = [];
                Args.IsTable(1,1) logical     = false;
            end
            
            Nobj     = numel(Obj);
            ColNames = colind2name(Obj(1), Columns);
            
            Ncol     = numel(ColNames);
            NewObj   = AstroTable;
            NewObj.ColNames = ColNames;
            NewObj.Catalog = zeros(0,Ncol);
            for Iobj=1:1:Nobj
                ColInd   = colname2ind(Obj(Iobj), Columns);
                NewObj.Catalog = [NewObj.Catalog; getCol(Obj(Iobj), ColInd, Args.IsTable, false)];
            end
            if isempty(ColNames)
                NewObj.ColNames = Obj(1).ColNames;
            else
                NewObj.ColNames = ColNames;
            end
            if ~isempty(Obj(1).ColUnits)
                NewObj.ColUnits = Obj(1).ColUnits;
            end
              
        end
        
    end
    
    methods  % sort and flip
        
        function Obj = sortrows(Obj,SortByColumn)
            % Sort AstroTable objects by some column names/indices
            % Input  : - An AstroTable object (multiple elements is possible).
            %          - A column/s names or indices by which to sort the
            %            catalogs. If empty, will attempt to use the
            %            SortByCol property. Default is empty.
            % Output : - An object in which the catalogs are sorted.
            % Author : Eran Ofek (Mar 2021)
            % Example: AC=AstroTable; AC.Catalog = rand(100,3); AC=sortrows(AC,2);
            
            arguments
                Obj
                SortByColumn       = [];
            end
           
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if isempty(SortByColumn)
                    if isempty(Obj(Iobj).SortByCol)
                        error('Sort column is not specified as input or in SortByCol property');
                    end
                    SortByColumnInd           = colname2ind(Obj(Iobj), Obj(Iobj).SortByCol);
                else
                    SortByColumnInd           = colname2ind(Obj(Iobj), SortByColumn);
                end
                
                if ~Obj(Iobj).IsSorted || any(SortByColumnInd~=Obj(Iobj).SortByCol)
                    Obj(Iobj).Catalog   = sortrows(Obj(Iobj).Catalog, SortByColumnInd);
                    Obj(Iobj).SortByCol = SortByColumnInd;
                    Obj(Iobj).IsSorted  = true;
                end
            end
        end
                
        function Obj = flipud(Obj)
            % flip up-down all catalogs in AstroTable object (set IsSorted to false)
            % Example: flipud(Obj)
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Catalog = flipud(Obj(Iobj).Catalog);
            end
            
        end
        
    end
    
    methods % applay functions and overloads
            
        function Result = fun_unary(Obj, Operator, OperatorArgs, Columns, UpdateObj)
            % Apply an unary function to columns of a catalog
            % Input  : - An AstroTable object.
            %          - Unary operator handle (e.g., @sin).
            %          - A cell array of additional arguments to pass to
            %            the operator. Default is {}.
            %          - List of columns names or indices on which to apply
            %            the operator. If empty, apply to all columns.
            %            Default is empty.
            %          - A logical indicating if to update the object (in
            %            addition to returning the output).
            %            Default is false.
            % Output : - The result (matrix or table).
            % Author : Eran Ofek (Mar 2021)
            % Example: AC=AstroTable; AC.Catalog = rand(100,3); AC.ColNames={'a','n','s'}; AC.fun_unary(@sin)
           
            arguments
                Obj
                Operator function_handle
                OperatorArgs cell           = {};
                Columns                     = [];
                UpdateObj(1,1) logical      = false;
            end
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                %
                ColInd = colname2ind(Obj(Iobj), Columns);  % If columns empty - return all columns
                if istable(Obj(Iobj).Catalog)
                    Ncol  = numel(ColInd);
                    Nrows = size(Obj(Iobj).Catalog,1); 
                    Result = nan(Nrows,Ncol);
                    for Icol=1:1:Ncol
                        ColName = Obj(Iobj).ColNames(ColInd(Icol));
                        Result(:,Icol)  = Operator(Obj(Iobj).Catalog.(ColName), OperatorArgs{:});
                        if UpdateObj
                            Obj(Iobj).Catalog.(ColName) = Result(:,Icol);
                        end
                    end
                else
                    Result = Operator(Obj(Iobj).Catalog(:,ColInd), OperatorArgs{:});
                    if UpdateObj
                        Obj(Iobj).Catalog(:,ColInd) = Result;
                    end
                end
            end
            
            
        end
        
        function Result = min(Obj, Columns)
            % Calculate the min value of all requested columns names/indices. Default is
            % for all columns.
            % Input  : - An AstroTable object.
            %          - Column names or indices. If empty, then use all.
            %            Default is empty.
            % Output : - An array of minimum values. Line per object
            %            element, column per Catalog column.
            % Example: AC=AstroTable('asu.fit','HDU',2);
            % AC.min
            % AC.min({'mag1','mag2'})
            % min([AC;AC],{'mag1','mag2'})
            
            arguments
                Obj
                Columns         = [];
            end
            
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Tmp = fun_unary(Obj, @min, {}, Columns, false);
                if Iobj==1
                    Result = zeros(Nobj,numel(Tmp));
                end
                Result(Iobj,:) = Tmp;
            end
        end
        
        function Result = max(Obj, Columns)
            % Calculate the max value of all requested columns names/indices. Default is
            % for all columns.
            % Input  : - An AstroTable object.
            %          - Column names or indices. If empty, then use all.
            %            Default is empty.
            % Output : - An array of maximum values. Line per object
            %            element, column per Catalog column.
            % Example: AC=AstroTable('asu.fit','HDU',2);
            % AC.max
            % AC.max({'mag1','mag2'})
            % max([AC;AC],{'mag1','mag2'})
            
            arguments
                Obj
                Columns         = [];
            end
            
            Nobj   = numel(Obj);
            for Iobj=1:1:Nobj
                Tmp = fun_unary(Obj, @max, {}, Columns, false);
                if Iobj==1
                    Result = zeros(Nobj,numel(Tmp));
                end
                Result(Iobj,:) = Tmp;
            end
        end
        
        function [Result,Flag] = query(Obj, QueryStringUser, Args)
            % Query an AstroTable object
            % Input  : - An AstroTable object.
            %          - A query string with operators on column names
            %            (e.g., 'mag1>8 & mag2<10'),
            %          * ...,key,val,...
            %            'CreateNewObj' - Output is a new object, rather
            %                   than writing the output lines to the same
            %                   object. Default is true.
            % Output : - An AstroTable object with the selected lines.
            % Author : Eran Ofek (Feb 2016)
            % Example: AC=AstroTable('asu.fit','HDU',2);
            %          BC=query(AC,'mag1>8 & mag2<10');
            %          query(AC,'mag1>8 & mag2<10','CreateNewObj',false); % modifies AC
            %          AC=AstroTable('asu.fit','HDU',2);
            %          BC=query([AC;AC],'mag1>8 & mag2<10');
            %          AC=array2table(AC)
            %          BC=query(AC,'mag1>8 & mag2<10');
            
            arguments
                Obj
                QueryStringUser char
                Args.CreateNewObj(1,1) logical        = true;
            end
            
            Nobj = numel(Obj);
            
            if Args.CreateNewObj
                Result = AstroTable(size(Obj));
            else
                Result = Obj;
            end
            
            for Iobj=1:1:Nobj
                % for each AstroTable element
                
                % for table:  'RA>1' -> 'AstC.(CatField).(AstC.Col.RA)'
                % for matrix: 'RA>1' -> 'AstC.(CatField)(:,AstC.Col.RA)'

                ColStrLength = cellfun(@numel, Obj(Iobj).ColNames);
                Ncol   = numel(Obj(Iobj).ColNames);
                % need to go over column names from longest to shortest
                [~,SI] = sort(ColStrLength,'descend');

                QueryString = sprintf(' %s',QueryStringUser);
                if istable(Obj(Iobj).Catalog)
                    % catalog is stored as table
                    for Icol=1:1:Ncol
                        ColName = Obj(Iobj).ColNames{SI(Icol)};
                        QueryString = regexprep(QueryString, sprintf('(?<=[^.])%s',ColName), sprintf(' Obj(Iobj).Catalog.%s', ColName));
                    end
                else
                    % catalog is stored as array
                    for Icol=1:1:Ncol
                        ColName = Obj(Iobj).ColNames{SI(Icol)};
                        QueryString = regexprep(QueryString, sprintf('(?<=[^.])%s',ColName), sprintf(' getCol(Obj(Iobj), ''%s'')', ColName));
                    end
                end
                Flag = eval(QueryString);
                
                Result(Iobj).Catalog   = Obj(Iobj).Catalog(Flag,:);
                Result(Iobj).ColNames   = Obj(Iobj).ColNames;
                Result(Iobj).ColUnits  = Obj(Iobj).ColUnits;
                Result(Iobj).ColDesc   = Obj(Iobj).ColDesc;
                Result(Iobj).SortByCol = Obj(Iobj).SortByCol;
                Result(Iobj).IsSorted  = Obj(Iobj).IsSorted;
                
            end
            
        end
        
    end
    
    methods % plots
        function varargout = plotFun(Obj, PlotFun, Columns, varargin)
            % Operate a graphical function on AstroTable
            % Input  : - An AstroTable object. If multiple elements, then
            %            perform the plot for each element, and hold on.
            %            The hold state will return to its original state.
            %          - A plot handle function. e.g., @plot.
            %            Default is @plot.
            %          - A single column name, a cell array of column
            %            names, or a vector of column indices.
            %            Default is [1 2].
            %          * Additional arguments to pass to thre plot
            %            function.
            % Output : - The plot handle. If multiple elemnts, then only
            %            the handle of the lastes plot will be returned. 
            % Author : Eran Ofek (Apr 2021)
            % Example: AT = AstroTable; AT.Catalog=rand(10,4);
            % AT.ColNames={'a','b','c','d'};
            % AT.plotFun(@plot,{'a','c'},'o')
            % AT.plotFun(@plot,[1 2],'o','Color','b')
            % AT.plotFun(@hist,{'c'})
            % AT.plotFun(@hist,'d')
            % AT.plotFun(@plot3,[1 2 4],'o','Color','b')
            
            if nargin<3
                Columns = [1 2];
                if nargin<2
                    PlotFun = @plot;
                end
            end
            
            GH = groot; % graphic handle without properties
            FH = get(groot,'CurrentFigure');
            if isempty(FH)
                % figure doesn't exist
                HoldOn = false;
            else
                HoldOn = ishold;
            end
            
            Ncol = numel(Columns);
            if isnumeric(Columns)
                Columns = num2cell(Columns);
            else
                if ischar(Columns)
                    Columns = {Columns};
                end
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % columns
                Data = cell(1,Ncol);
                for Icol=1:1:Ncol
                    Data{Icol} = getCol(Obj(Iobj), Columns{Icol});
                end
                
                [varargout{1:1:nargout}] = PlotFun(Data{:}, varargin{:});
                hold on;
            end
            if ~HoldOn
                % return hold to original state
                hold off;
            end
            
            
        end
        
        function varargout = plot(Obj, ColXY, varargin)
            % plot function for AstroTable objects
            % Input  : - An AstroTable object (multiple elements is supported).
            %            If more then one element, then will plot all and
            %            return the hold state to its original state.
            %          - A vector of [X, Y] colum indices, or a cell array
            %            of X, Y column names.
            %          * Additional arguments to pass to the plot function
            %            e.g., 'o','Color',[1 1 0],'MarkerFaceColor',[1 1 0].
            % Output : - An handle for the last plot.
            % Author : Eran Ofek (Apr 2021)
            % Example: AT = AstroTable; AT.Catalog=[(1:1:10).',(1:1:10).', rand(10,1)]
            %          AT(2).Catalog = [(1:1:10).', (10:-1:1).', rand(10,1)];
            %          AT(1).ColNames={'X','Y','Flux'};
            %          AT(2).ColNames={'X','Y','Flux'};
            %          AT.plot({'X','Y'},'o','MarkerFaceColor','r');
            
            [varargout{1:1:nargout}] = plotFun(Obj, @plot, ColXY, varargin{:});
        end
        
    end
    

    methods (Static)  % Unit-Test
        function Result = unitTest()
            
            % Create an empty AstroTable
            AC = AstroTable;
            
            % read from FITS table
            AC = AstroTable('asu.fit','HDU',2);
            
            % Create AstroTable with table
            AC = AstroTable(array2table(rand(10,2)));
            AC = AstroTable(rand(10,2),'ColNames',{'RA','Dec'});
            
            % Create AstCat and convert to AstroTable 
            A = AstCat; A(1).Cat=rand(10,2);
            A(2).Cat=rand(10,2);
            AC = AstroTable(A);
            % The same with column names:
            AC = AstroTable(A,'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
            
            % merge selected columns of AstroTable
            MAC = merge(AC,{'Dec'});
            % merge two AstroTable (all columns)
            MAC = merge(AC);
            
            % Sort by second column
            sortrows(MAC,2)
            if ~(MAC.IsSorted && issorted(MAC.Catalog(:,2)))
                error('Problem with sort flagging');
            end
            
            % get column
            getCol(MAC,1)
            getCol(MAC,'Dec')
            getCol(MAC,{'Dec','RA'})
            % output as table
            getCol(MAC,{'Dec','RA'},true)
            % store result in original AstroTable
            Result = getCol(MAC,{'Dec','RA'},false,true);
            if ~all(Result == MAC.Catalog,'all')
                error('Result should be identical');
            end
            
            % fun_unary
            fun_unary(AC(1),@sin)
            fun_unary(AC(1),@sin,{},'RA')
            
            AC=AstroTable('asu.fit','HDU',2);
            min([AC;AC],{'mag1','mag2'})
            max([AC;AC],{'mag1','mag2'})
            max([AC;AC],[1 2])
            max([AC;AC])
            
            % insert columns
            
            
            % delete columns
            
            % ...
            
            
            % query
            AC=AstroTable('asu.fit','HDU',2);
            BC=query(AC,'mag1>8 & mag2<10');
            query(AC,'mag1>8 & mag2<10','CreateNewObj',false); % modifies AC
            AC=AstroTable('asu.fit','HDU',2);
            BC=query([AC;AC],'mag1>8 & mag2<10');
            AC=array2table(AC);
            BC=query(AC,'mag1>8 & mag2<10');
            
            % plot
            AT = AstroTable;
            AT.Catalog=rand(10,4);
            AT.ColNames={'a','b','c','d'};
            AT.plotFun(@plot,{'a','c'},'o')
            AT.plotFun(@plot,[1 2],'o','Color','b')
            AT.plotFun(@hist,{'c'})
            AT.plotFun(@hist,'d')
            AT.plotFun(@plot3,[1 2 4],'o','Color','b')
            
            AT = AstroTable;
            AT.Catalog = [(1:1:10).',(1:1:10).', rand(10,1)];
            AT(2).Catalog = [(1:1:10).', (10:-1:1).', rand(10,1)];
            AT(1).ColNames = {'X','Y','Flux'};
            AT(2).ColNames = {'X','Y','Flux'};
            AT.plot({'X','Y'},'o','MarkerFaceColor','r');
            

            Result = true;
        end
    end
    

end

            
