% AstroTable handle class
% Package: @AstroTable
% Description:
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example :
% Reliable: 2
%--------------------------------------------------------------------------
% See https://www.stsci.edu/instruments/wfpc2/Wfpc2_dhb/intro_ch23.html
%
% #functions (autogen)
% AstroTable - Constrt an AstroTable object or transform AstCat/struct/ to AstroTable
% array2table - Convert catalog data in AstroTable to table format
% col2struct - return structure array with column names in field and index in values. Example: col2struct(A)
% colind2name - Return column names corresponding to column indices
% colname2ind - Convert column names to column indices
% colnameDict2ind - Given a list of column names, select the first that appear in Table
% compareColNames - Compare two ColNames's Package: @AstroTable (Static) Description: Given two cell array of strings compare their content. Return a vector of logical of length equal to the longer ColNames.
% csvWrite - who wrote this? missing help
% defaultColNames - create a default ColNames with N columns Package: @AstroTable (Static)
% deleteCatalog - delete the content of an AstroTable object Example: Result = deleteCatalog(Obj)
% deleteCol - Delete columns fron an AstroTable object.
% flipud - flip up-down all catalogs in AstroTable object (set IsSorted to false) Example: flipud(Obj)
% funUnary - Apply an unary function to columns of a catalog
% get.Catalog - getter for Image - get image from ImageData property
% get.ColNames - getter for ColNames
% get.ColUnits - getter for ColUnits
% getCol - Get a catalog columns by index or names
% getCol2struct - Get columns from AstroTable and return them in a structure array.
% getColDic - get a single Column data from a dictionary of column names
% help - show help in manuals.AstroTable
% insertCol - Insert columns to AstroTable object
% insertColumn - Insert a single column into a matrix, table, or a cell array Package: @AstroTable (Static) Description: Insert a single column into a matrix, table, or a cell array.
% insertLines - Insert rows from Matrix2 to Matrix1, including optional NaN filling and reordering. There are 2 related functions: insertLines and selectLines: selectLines is useful for operations like: Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
% insertRows - Insert rows from Obj2 into Obj1 (e.g., Obj1(FlagInd,: ) = Obj2) There are 2 related functions: insertRows and selectRows: AstroTable/selectRows is useful for operations like: Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
% isColIdentical - Check if ColNames in an AstroTable object is identical to another ColNames Package: @AstroTable Descriptio: For each element in a AstroTable object check if the ColNames property is equal to a reference ColNames.
% isColumn - Return true if a name is an existing column name in an AstroTable object Example: Result = isColumn(Obj,'RA')
% isemptyCatalog - Return true if Catalog data in AstroTable object is empty, otherwise false. example: Result = isemptyCatalog(Obj)
% max - Calculate the max value of all requested columns names/indices. Default is for all columns.
% merge - Merge table/matrices in multiple AstroTable elements
% min - Calculate the min value of all requested columns names/indices. Default is for all columns.
% plot - plot function for AstroTable objects
% plotFun - Operate a graphical function on AstroTable
% query - Query an AstroTable object
% queryRange - Query catalog by columns in range (without using eval). See also query
% replaceCol - replace (or insert) columns in AstroTable
% replaceColNames - Replace column names in AstroTable object
% searchSynonym - Find the first appearance of a synonym in a cell array.
% selectLines - Select rows from Matrix, including optional NaN filling and reordering. There are 2 related functions: insertLines and selectLines: selectLines is useful for operations like: Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
% selectRows - Select rows from AstroTable object, including optional NaN filling and reordering. There are 2 related functions: insertRows and selectRows: AstroTable/selectRows is useful for operations like: Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
% set.Catalog - setter for catalog - set also column names and units if table and available
% set.ColNames - setter for ColNames - input is either a cell or a string array if the catalog is in table format it will also set its Properties.VariableNames (but only if the size is consistent)
% set.ColUnits - setter for ColUnits - input is either a cell or a string array if the catalog is in table format it will also set its Properties.VariableUnits (but only if the size is consistent)
% set.SortByCol - Setter for SortByCol, also set IsSorted to false - sue sortrows to sort
% sizeCatalog - Return the number o rows and columns in all elements in an AstroTable object Example: [Nrow,Ncol] = sizeCatalog(Obj)
% sortrows - Sort AstroTable objects by some column names/indices
% table2array - Convert catalog data in AstroTable to array format
% #/functions (autogen)
%

classdef AstroTable < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (SetAccess = public)
        Catalog                                                = [];
        Table                                                  = [];
        ColNames cell                                          = {};
        ColUnits cell                                          = {};
        ColDesc cell                                           = {};
        SortByCol                                              = [];
        IsSorted(1,1) logical                                  = false;
    end

    properties (Hidden)
        Reference
    end
    
    methods % Constructor
       
        function Obj = AstroTable(FileName, Args)
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
            % Example: AC = AstroTable
            %          AC = AstroTable([2 2])
            %          AC = AstroTable({rand(10,2),rand(10,2)})
            %          AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
            %          AC = AstroTable({rand(10,2),rand(10,2)},'ConvertTable2array',false)
            %          AC=AstroTable({array2table(rand(10,2))});
            %          AC=AstroTable({rand(10,2)},'ColNames',{'RA','Dec'});
            %          A = AstCat; A(1).Cat=rand(10,2);
            %          A(2).Cat=rand(10,2); A(1).ColCell={'RA','Dec'};
            %          A(1).ColUnits={'rad','rad'};
            %          AC = AstroTable(A);
            %          AC = AstroTable(A,'ColNames',{'RA','Dec'},'ColUnits',{'rad','rad'});
            %          AC=AstroTable('asu.fit','HDU',2); % read from FITS table
            
            arguments
                FileName                      = [];
                Args.ColNames cell            = {};
                Args.ColUnits cell            = {};
                
                Args.UseRegExp(1,1) logical   = false;
                Args.FileType                 = []; % 'fits' | 'hdf5' | ...
                Args.HDU                      = 1;  % HDU or dataset name
                Args.TableType                = 'auto'; % 'auto'|'bintable'|'table' for FITS.readTable1
                Args.readTableArgs            = {};
                Args.ConvertTable2array       = true;  % only if all columns are double
            end
            
            % FFU: use ImageIO instead!!!
            
            if isempty(FileName)
                Obj.Catalog = [];
            else
                if isa(FileName, 'AstroTable')
                    Obj = FileName;
                elseif isa(FileName, 'AstCat')
                    % read AstCat or catCl objects
                    Nobj = numel(FileName);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroTable;
                        Obj(Iobj).Catalog  = FileName(Iobj).Cat;
                        if isempty(Args.ColNames)
                            Obj(Iobj).ColNames  = FileName(Iobj).ColCell;
                        else
                            Obj(Iobj).ColNames  = Args.ColNames;
                        end
                        if isempty(Args.ColUnits)
                            Obj(Iobj).ColUnits = FileName(Iobj).ColUnits;
                        else
                            Obj(Iobj).ColUnits = Args.ColUnits;
                        end

                    end
                else
                    Args.readTableArgs = [Args.readTableArgs, 'TableType', Args.TableType];
                    ImIO = ImageIO(FileName, 'HDU',Args.HDU,...
                                             'FileType',Args.FileType,...
                                             'IsTable',true,...
                                             'UseRegExp',Args.UseRegExp,...
                                             'readTableArgs',Args.readTableArgs);
                                         
                    Nobj = numel(ImIO);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = AstroTable([]);
                        
                        if ~isempty(ImIO(Iobj).Data)
                            % otherwise generate an empty object
                            Obj(Iobj).Catalog  = ImIO(Iobj).Data;
                            if isempty(Args.ColNames)
                                Args.ColNames = Obj(Iobj).Catalog.Properties.VariableNames;
                            end
                            if isempty(Args.ColUnits)
                                Args.ColUnits = Obj(Iobj).Catalog.Properties.VariableUnits;
                            end
                            
                            if Args.ConvertTable2array && istable(Obj(Iobj).Catalog)
                                Obj(Iobj).Catalog = table2array(Obj(Iobj).Catalog);
                            end
                            if ~isempty(Args.ColNames)
                                Obj(Iobj).ColNames = Args.ColNames;
                            end
                            if ~isempty(Args.ColUnits)
                                Obj(Iobj).ColUnits = Args.ColUnits;
                            end
                        end
                        
                    end
                    Obj = reshape(Obj, size(ImIO));
                    
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

 
    methods % Setter/Getters
        function Result=get.Table(Obj)
            % getter for dependent property Table
            % create a table from the catalog
            if istable(Obj.Catalog)
                Result = Obj.Catalog;
            else
                Result = array2table(Obj.Catalog);
                Result.Properties.VariableNames = Obj.ColNames;
                Result.Properties.VariableUnits = Obj.ColUnits;
            end
        end

        function Result=set.Table(Obj,Data)
            % setter for dependent property Table

            % currently do nothing
            
        end

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
                % treat different classes input
                if istable(OldArray) && isnumeric(NewData)
                    NewData = array2table(NewData);
                elseif istable(OldArray) && iscell(NewData)
                    NewData = cell2table(NewData);
                elseif isnumeric(OldArray) && istable(NewData)
                    OldArray = array2table(OldArray);
                elseif isnumeric(OldArray) && iscell(NewData)
                    OldArray = array2table(OldArray);
                    NewData = cell2table(NewData);
                elseif isnumeric(OldArray) && isstring(NewData)
                    OldArray = array2table(OldArray);
                    NewData  = table(NewData);
                else
                    error('First two input argumnets must be of compatible classes');
                end
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
                IndIndInSyn = find(IndInSyn>0);
                [~,II] = min(IndInSyn(IndIndInSyn));
                IndInCell = IndIndInSyn(II);
                %IndInCell     = find(IndInSyn>0, 1, 'first'); % BUG
                IndInSynonym  = IndInSyn(IndInCell);
                Name          = Cell(IndInCell);
            end

        end

       function Result = selectLines(Obj, FlagI, Args)
            % Select rows from Matrix, including optional NaN filling and reordering.
            %       There are 2 related functions: insertLines and
            %       selectLines:
            %       selectLines is useful for operations like:
            %           Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
            %           Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
            %       insertLines is useful for operations like:
            %           Obj1(FlagInd,:) = Obj2(FlagInd2,:)
            %           Obj1 = NaN; Obj1(FlagInd,:) = Obj2(FlagInd2,:) insert NaN to Obj1 when appear in FlagInd
            % Input  : - A matrix.
            %          - A vector of logicals or indices.
            %            In the case that this is a vector of logicals, no
            %            NaNs are allowed, and the function will return the
            %            lines with logical true.
            %            In case this is a vector of indices (NaNs are
            %            allowed), the corresponding lines will be
            %            returned.
            %          * ...,key,val,...
            %            'IgnoreNaN' - A logical indicating if to ignore
            %                   the NaNs in the vector of indices.
            %                   Default is false.
            % Output : - Matrix with selected (and optionaly filled with
            %            NaNs) lines.
            % Author : Eran Ofek (Sep 2021)
            % Example : Table = (1:1:5).'; Table = [Table, rand(size(Table))];
            %           Result = AstroTable.selectLines(Table, [1;2])
            %           Result = AstroTable.selectLines(Table, [2 3 2])
            %           Result = AstroTable.selectLines(Table, [true, true, false, true, false])
            %           Result = AstroTable.selectLines(Table, [2 3 2 NaN],'IgnoreNaN',true)
            %           Result = AstroTable.selectLines(Table, [2 3 NaN 2 NaN],'IgnoreNaN',false)

            arguments
                Obj
                FlagI
                Args.IgnoreNaN(1,1) logical   = false;
            end

            if any(isnan(FlagI))
                % NaN may be treated by two differeny ways:
                % 1. ignored
                % 2. filled by lines with NaNs

                if Args.IgnoreNaN
                    % remove NaNs from FlagI
                    FlagI = FlagI(~isnan(FlagI));
                    % Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
                    Result = Obj(FlagI,:);
                else
                    % do not ignore NaNs
                    % Lines with NaNs will be filled with NaNs rows
                    [~,Ncol] = size(Obj);
                    NFI   = numel(FlagI);
                    %Result   = nan(NFI, Ncol);

                    % remove NaNs from FlagI
                    IndF  = ~isnan(FlagI);
                    FlagI = FlagI(IndF);
                    % Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
                    % initialzie Result with NaNs
                    Result         = nan(NFI, Ncol);
                    Result(IndF,:) = Obj(FlagI, :);
                end
            else
                % Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
                Result = Obj(FlagI,:);
            end
        end

       function Result = insertLines(Obj1, Obj2, FlagI, FlagI2, Args)
            % Insert rows from Matrix2 to Matrix1, including optional NaN filling and reordering.
            %       There are 2 related functions: insertLines and
            %       selectLines:
            %       selectLines is useful for operations like:
            %           Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
            %           Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
            %       insertLines is useful for operations like:
            %           Obj1(FlagInd,:) = Obj2(FlagInd2,:) % keep original values in Obj1
            %           Obj1 = NaN; Obj1(FlagInd,:) = Obj2(FlagInd2,:) insert NaN to Obj1 when appear in FlagInd
            % Input  : - A first matrix.
            %          - A second matrix in which the number of coloumns is
            %            equal or larger than the number of columns in the
            %            first matrix.
            %          - Indices in first matrix in which to put the lines
            %            from the second matrix.
            %          - Indices of lines in the second matrix to use.
            %          * ...,key,val,...
            %            'FillNaN' - A logical indicating if to fill
            %                   missing values with NaNs or to use orginal
            %                   values as appear in the 1st object.
            %                   Default is true.
            % Output : - A matrix which size is like that of the first
            %            matrix, in which lines from the second matrix were
            %            inserted.
            %            The number of columns in the output matrix is
            %            equal to the number of columns in the first
            %            matrix.
            % Author : Eran Ofek (Sep 2021)
            % Example: Table = (1:1:5).'; Table = [Table, rand(size(Table))];
            %          Table2 = (1:1:4).'; Table2 = [Table2, rand(size(Table2,1),2)];
            %          Result = AstroTable.insertLines(Table, Table2(3:4,:), [1 2])
            %          Result = AstroTable.insertLines(Table, Table2(3:4,:), [1 2],'FillNaN',false)

            arguments
                Obj1
                Obj2
                FlagI
                FlagI2                        = [];  % p[ - use all
                Args.FillNaN(1,1) logical     = true;  % false - use original values in Obj1
            end

            % remove NaNs
            FlagI = FlagI(~isnan(FlagI));

            if isempty(FlagI2)
                FlagI2 = true(size(Obj2,1), 1);
            end

            Ncol1 = size(Obj1,2);
            Ncol2 = size(Obj2,2);
            
            if Args.FillNaN
                Result           = nan(size(Obj1));
                Result(FlagI, 1:Ncol1) = Obj2(FlagI2, 1:Ncol1);
            else
                if Ncol2<Ncol1
                    error('When FillNaN=false, number of columns in second matrix must be >/>= from number of columns in first matrix');
                end
                Result = Obj1;
                Result(FlagI, :) = Obj2(FlagI2, 1:Ncol1);
            end

       end
       
    end
    
    
    methods % get general info
        function Result = getColUnits(Obj, Fields)
            % get column units
            % Input  : - A single element AstroTable object.
            %          - A char or cell array of column names.
            % Output : - A char or cell (according to input) of units.
            % Author : Eran Ofek (Nov 2021)
            % Example: Result = getColUnits(Obj, 'FLUX')            
            
            arguments
                Obj(1,1)
                Fields
            end
           
            if ischar(Fields)
                ColInd = colname2ind(Obj, Fields);
                if ColInd>numel(Obj.ColUnits)
                    Result = '';
                else
                    Result = Obj.ColUnits{ColInd};
                end
            elseif iscell(Fields)
                Nf = numel(Fields);
                Result = cell(1,Nf);
                ColInd = colname2ind(Obj, Fields);
                for If=1:1:Nf
                    if ColInd(If)>numel(Obj.ColUnits)
                        Result{If} = '';
                    else
                        Result{If} = Obj.ColUnits(ColInd(If));
                    end
                end
            else
                error('Unknown Fields type - must be a char or cell');
            end
            
                
            
        end
        
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
    
        function Obj = deleteCatalog(Obj)
            % delete the content of an AstroTable object
            % Example: Result = deleteCatalog(Obj)
           
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Obj(Iobj).Catalog   = [];
                Obj(Iobj).ColNames  = {};
                Obj(Iobj).ColUnits  = {};
                Obj(Iobj).ColDesc   = {};
                Obj(Iobj).SortByCol = [];
                Obj(Iobj).IsSorted  = false;
            end
        end
    end
    
    
    methods % column names and exist
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
            %          - If the column name is char or cell, and it doesn't
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
        
        function [ColInd, ColName, IndOfSelectedName] = colnameDict2ind(Obj, ColNames)
            % Given a list of column names, select the first that appear in Table
            % Input  : - An AstroTable (single element)
            %          - A cell array of column names
            % Output : - The selected column index.
            %          - The selected column name.
            %          - The index of the selected column in the input
            %            ColNames.
            % Author : Eran Ofek (May 2021)
            % Example: AC = AstroTable({rand(10,2),rand(10,2)},'ColNames',{'a','b'});
            %          [ColInd, ColName, IndOfSelectedName] = colnameDict2ind(AC(1),{'X','Y','a','Z'})
            
            arguments
                Obj(1,1)
                ColNames
            end
                        
            ColInd = colname2ind(Obj, ColNames, NaN);
            
            IndOfSelectedName = find(~isnan(ColInd),1);
            ColInd            = ColInd(IndOfSelectedName);
            ColName           = colind2name(Obj, ColInd);
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
        function [Result, Units, ColInd] = getCol(Obj, Columns, OutputIsTable, UpdateAstroTable, Args)
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
            %          * ...,key,val,...
            %            ' UseDict' - unsupported
            %            'SelectRows' - A vector of indices or logicals of
            %                   rows to select. If NaN, select all rows.
            %                   Default is NaN.
            %                   This is not using the selectRows function.
            % Output : - A matrix or a table containing the selected
            %            columns.
            %          - A cell array of units corresponding to the
            %            requested columns.
            %          - Selected column index.
            % Author : Eran Ofek (Mar 2021)
            % Example: AC.getCol({'Var1','Var3'},true)
            %          AC.getCol([1 2])
            %          AC.getCol({'Var1','Var3'},true,true)
            
            arguments
                Obj(1,1)
                Columns
                OutputIsTable(1,1) logical         = false;
                UpdateAstroTable(1,1) logical      = false;
                Args.UseDict(1,1) logical          = true;
                Args.SelectRows                    = NaN;
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
                    Result.Properties.VariableNames = Obj.ColNames(ColInd);
                    if ~isempty(Obj.ColUnits)
                        Result.Properties.VariableUnits = Obj.ColUnits(ColInd);
                    end
                else
                    Result = Obj.Catalog(:,ColInd);
                end
            end
            if nargout>1
                if ColInd>numel(Obj.ColUnits)
                    Units = {''};
                else
                    Units = Obj.ColUnits(ColInd);
                end
            end
            
            if UpdateAstroTable
                Obj.Catalog = Result;
            end
            
            if ~isnan(Args.SelectRows)
                Result = Result(Args.SelectRows, :);
            end
        end
        
        function Result = getCol2struct(Obj, ColNames, Args)
            % Get columns from AstroTable and return them in a structure array.
            % Input  : - An AstroTable object.
            %          - A cell array of column names.
            %          * ...,key,val,...
            %            'SelectRows'  - A vector of indices or logicals of
            %                   rows to select. If NaN, select all rows.
            %                   Default is NaN.
            % Output : - A structure array (element per AstroTable element)
            %            with field for each selected column. Each field
            %            contains thr column data.
            % Author : Eran Ofek (Sep 2021)
            % Example: AT = AstroTable({rand(1000,2)},'ColNames',{'a','b'});
            %          S  = getCol2struct(AT, {'a'});

            arguments
                Obj
                ColNames cell
                Args.SelectRows    = NaN;
            end

            Ncol = numel(ColNames);
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Data = getCol(Obj, ColNames, 'SelectRows',Args.SelectRows);
                for Icol=1:1:Ncol
                    Result(Iobj).(ColNames{Icol}) = Data(:,Icol);
                end
            end
    
        end

        function [Result, Units, Ind, ColName] = getColDic(Obj, Columns)
            % get a single Column data from a dictionary of column names
            % Input  : - A single-element AstroTable object.
            %          - A cell array of columns. The first exitsing column will be selected.
            % Output : - A vector of the column content.
            %          - Column units.
            %          - Selected column index.
            %          - Selected column name.
            % Author : Eran Ofek (Jul 2021)
            % Example: [Result, Units] = getColDic(MatchedCat,Args.CatColNamesX)
            
            arguments
                Obj(1,1)
                Columns
            end
                        
            [Ind, ColName]             = colnameDict2ind(Obj, Columns);
            if isempty(Ind)
                Result = [];
                Units  = '';
            else
                [Result, Units] = getCol(Obj, Ind);
                if isempty(Units)
                    Units = '';
                else
                    Units = Units{1};
                end
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
                    Obj(Iobj).ColNames = Obj(Iobj).Catalog.Properties.VariableNames;
                    Obj(Iobj).ColUnits = Obj(Iobj).Catalog.Properties.VariableUnits;
                    Obj(Iobj).Catalog  = table2array(Obj(Iobj).Catalog);
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
            if ~iscell(NewColUnits) && ~isstring(NewColUnits)
                NewColUnits = {NewColUnits};
            end
           
            % delete before insertion
            Obj = deleteCol(Obj, NewColNames);
            
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
                   
                    Obj(Iobj).ColNames = AstroTable.insertColumn(Obj(Iobj).ColNames(:).', NewColNames, ColInd);
                    Obj(Iobj).ColUnits = AstroTable.insertColumn(Obj(Iobj).ColUnits(:).', NewColUnits, ColInd);
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
        
        function Obj = replaceCol(Obj, NewData, ColNames, Pos, ColUnits)
            % replace (or insert) columns in AstroTable
            % Input  : - An AstroTable object.
            %          - Data of columns to replace. This may be a matrix,
            %            cell array, a table or another AstroTable
            %            object. However, the data types should be
            %            consistent.
            %            If AstroTable then numver of elements should be
            %            1 or equal to the number of elements in the first
            %            input.
            %          - Column names to replace insert.
            %          - Position for new insertion. Default is Inf.
            %          - Optional ColUnits cell for new columns.
            %            Default is {}.
            % BUGS   : The function doesn't handle a mix of insert/replace.
            % Example: AC = AstroTable;
            %          AC(1).Catalog = rand(100,3);
            %          AC(1).ColNames={'a','b','c'};
            %          AC=AC.replaceCol(nan(100,2),{'a','b'});
            
            arguments
                Obj
                NewData
                ColNames                 = [];
                Pos                      = Inf;
                ColUnits                 = {};
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
                    if all(isnan(ColInd))
                        % ColName doesn't exist - add
                        insertCol(Obj(Iobj), NewData, Pos, ColNames, ColUnits);
                    else
                        %NcolData = size(Obj(Iobj2).Catalog,2);
                        Iobj2    = min(Nobj2,Iobj);
                        Obj(Iobj).Catalog(:,ColInd) = NewData(Iobj2).Catalog;
                    end
                end
            else
                %
                for Iobj=1:1:Nobj
                    ColInd   = colname2ind(Obj(Iobj),ColNames);
                    if all(isnan(ColInd))
                        % ColName doesn't exist - add
                        insertCol(Obj(Iobj), NewData, Pos, ColNames, ColUnits);
                    else
                        Obj(Iobj).Catalog(:,ColInd) = NewData;
                    end
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
                ColInd = ColInd(~isnan(ColInd));
                Obj(Iobj).Catalog(:,ColInd) = [];
                if ~isempty(Obj(Iobj).ColNames) && ~isempty(ColInd)
                    Obj(Iobj).ColNames(ColInd) = [];
                end
                if ~isempty(Obj(Iobj).ColUnits) && ~isempty(ColInd)
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
                Args.AddEntryPerElement       = [];
                Args.AddColNames cell         = {};
            end
            
            Nobj     = numel(Obj);
            [~,Nc] = Obj.sizeCatalog;
            [~,Imax] = max(Nc);
            if isempty(Columns)
                Columns = Obj(Imax).ColNames;
            end
            ColNames = colind2name(Obj(Imax), Columns);
            ColIndC  = colname2ind(Obj(Imax), Columns);
            
            Ncol     = numel(ColNames);
            if isa(Obj, 'AstroCatalog')
                NewObj   = AstroCatalog;
            else
                % assume AstroTable
                NewObj   = AstroTable;
            end
            
            NextraCol    = size(Args.AddEntryPerElement,2);

            NewObj.ColNames = [ColNames, Args.AddColNames];
            NewObj.Catalog = zeros(0,Ncol+NextraCol);
            if Args.IsTable
                NewObj.Catalog = array2table(NewObj.Catalog);
                NewObj.Catalog.Properties.VariableNames = NewObj.ColNames;
            end
            for Iobj=1:1:Nobj
                Nrow = size(Obj(Iobj).Catalog,1);
                if Nrow>0
                    ColInd   = colname2ind(Obj(Iobj), Columns);
                    if isempty(Args.AddEntryPerElement)
                        NewObj.Catalog = [NewObj.Catalog; getCol(Obj(Iobj), ColInd, Args.IsTable, false)];
                    else
                        ExtraCols = repmat(Args.AddEntryPerElement(Iobj,:),Nrow,1);
                        if Args.IsTable
                            NewObj.Catalog = [NewObj.Catalog; [getCol(Obj(Iobj), ColInd, Args.IsTable, false), array2table(repmat(ExtraCols, Nrow, 1), 'VariableNames',Args.AddColNames)]];
                        else
                            NewObj.Catalog = [NewObj.Catalog; [getCol(Obj(Iobj), ColInd, Args.IsTable, false), repmat(ExtraCols, Nrow, 1)]];
                        end
                    end
                end
            end
            % if isempty(NewObj.ColNames)
            %     NewObj.ColNames = Obj(Imax).ColNames;
            % else
            %     NewObj.ColNames = ColNames;
            % end
            %if ~isempty(Obj(Imax).ColUnits)
            %    NewObj.ColUnits = Obj(Imax).ColUnits(ColIndC);
            %end
              
        end
        
    end
    
    methods  % sort, flip, select, interp
        
        function [Obj,Ind] = sortrows(Obj,SortByColumn)
            % Sort AstroTable objects by some column names/indices
            % Input  : - An AstroTable object (multiple elements is possible).
            %          - A column/s names or indices by which to sort the
            %            catalogs. If empty, will attempt to use the
            %            SortByCol property. Default is empty.
            % Output : - An object in which the catalogs are sorted.
            %          - Vector of sorted indices for lats catalog only.
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
                    [Obj(Iobj).Catalog, Ind]   = sortrows(Obj(Iobj).Catalog, SortByColumnInd);
                    Obj(Iobj).SortByCol = SortByColumnInd;
                    Obj(Iobj).IsSorted  = true;
                else
                    Ind = (1:1:size(Obj(Iobj).Catalog,1)).';
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
        
        function Result = insertRows(Obj1, Obj2, FlagInd, FlagInd2, Args)
            % Insert rows from Obj2 into Obj1 (e.g., Obj1(FlagInd,: ) = Obj2)
            %       There are 2 related functions: insertRows and
            %       selectRows:
            %       AstroTable/selectRows is useful for operations like:
            %           Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
            %           Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
            %       AstroTable/insertRows is useful for operations like:
            %           Obj1(FlagInd,:) = Obj2(FlagInd2,:)
            %           Obj1 = NaN; Obj1(FlagInd,:) = Obj2(FlagInd2,:) insert NaN to Obj1 when appear in FlagInd
            % Input  : - An AstroTable object (multi elements supported).
            %          - An AstroTable object (multi elements supported).
            %            The insertion of columns is between corresponding
            %            elements of the two AstroTable objects.
            %            Either one to one, or one to many.
            %          - Indices in first matrix in which to put the lines
            %            from the second matrix.
            %            Alternatively, this can be a cell array of vectors
            %            in which each cell corresponds to an element in
            %            the input object.
            %            Or, this can be a structure array, in which each
            %            element corresponds to an element in the input object.
            %            The structure field containing the vector is
            %            specified by the 'StructFieldName' key/val
            %            argument.
            %          - Indices of lines in the second matrix to use.
            %            Alternatively, a structure, or cell.
            %          * ...,key,val,...
            %            'FillNaN' - A logical indicating if to fill
            %                   missing values with NaNs or to use orginal
            %                   values as appear in the 1st object.
            %                   Default is true.
            %            'StructFieldName' - If the second input argument is
            %                   a structure, then this is the field name
            %                   that contains the vector of indices.
            %                   Default is 'Obj2_IndInObj1'.
            % Output : - An AstroTable/AstroCatalog object (same class as
            %            first input object), which contain the resulted
            %            object (new copy).
            % Author : Eran Ofek (Sep 2021)
            % Example: Table = (1:1:5).'; Table = [Table, rand(size(Table))];
            %          AT = AstroTable({Table});
            %          Table2 = (1:1:4).'; Table2 = [Table2, rand(size(Table2,1),2)];
            %          AT2 = AstroTable({Table2});
            %          Result = insertRows(AT, AT2, [1 2 3 4 5],[4 3 2 1 1]);
            %          Result = insertRows(AT, AT2, [1 2 3 4 ],[4 3 2 1 ]);

            arguments
                Obj1
                Obj2
                FlagInd
                FlagInd2                      = [];  % p[ - use all
                Args.FillNaN(1,1) logical     = true;  % false - use original values in Obj1
                Args.StructFieldName         = 'Obj2_IndInObj1';
            end


            N1     = numel(Obj1);
            N2     = numel(Obj2);
            Nmax   = max(N1, N2);
            if isa(Obj1, 'AstroCatalog')
                Result = AstroCatalog([Nmax,1]);
            else
                Result = AstroTable([Nmax,1]);
            end

            Nflag  = numel(FlagInd);
            Nflag2 = numel(FlagInd2);
            for Iobj=1:1:Nmax
                I1 = min(Iobj, N1);
                I2 = min(Iobj, N2);

                % for each element in the AstroTable
                % Treat FlagInd (struct array, cell, vector)
                if isstruct(FlagInd)
                    Iflag = min(Iobj, Nflag);
                    FlagI = FlagInd(Iflag).(Args.StructFieldName);
                elseif iscell(FlagInd)
                    Iflag = min(Iobj, Nflag);
                    FlagI = FlagInd{Iflag};
                else
                    % vector
                    FlagI = FlagInd;
                end

                if isstruct(FlagInd2)
                    Iflag = min(Iobj, Nflag2);
                    FlagI2 = FlagInd2(Iflag).(Args.StructFieldName);
                elseif iscell(FlagInd2)
                    Iflag = min(Iobj, Nflag2);
                    FlagI2 = FlagInd2{Iflag};
                else
                    % vector
                    FlagI2 = FlagInd2;
                end

                Result(Iobj).Catalog = AstroTable.insertLines(Obj1(I1).Catalog, Obj2(I2).Catalog, FlagI, FlagI2, 'FillNaN',Args.FillNaN);
                
            end
        
        end

        function Result = insertMatrix(Obj, Mat)
            % concat a matrix into a single element AstroTable
            % Input  : - An AstroTable object.
            %          - A matrix in which the number of columns is equial
            %            to the number of columns in the AstroTable object
            %            catalog data.
            % Output : - The original AstroTable object in which the matrix
            %            was concatenated.
            % Author : Eran Ofek (Dec 2022)
            % Example: AT=AstroTable({rand(10,3)});
            %          AT.insertMatrix(rand(2,3));

            arguments
                Obj(1,1)
                Mat
            end

            [~,NcolMat] = size(Mat);
            [~,Ncol]    = Obj.sizeCatalog;
            if Ncol~=NcolMat
                error('Number of columns in matrix and catalog must be the same');
            end

            Obj.Catalog = [Obj.Catalog; Mat];

        end

        function Result = selectRows(Obj, FlagInd, Args)
            % Select rows from AstroTable object, including optional NaN filling and reordering.
            %       There are 2 related functions: insertRows and
            %       selectRows:
            %       AstroTable/selectRows is useful for operations like:
            %           Result = Obj(FlagInd, :) w/o NaN in FlagInd or IgnoreNaN=true
            %           Result = NaN; Result(Ind,:) = Obj(FlagInd, :) insert NaN to Result when appear in FlagInd
            %       AstroTable/insertRows is useful for operations like:
            %           Obj1(FlagInd,:) = Obj2(FlagInd2,:)
            %           Obj1 = NaN; Obj1(FlagInd,:) = Obj2(FlagInd2,:) insert NaN to Obj1 when appear in FlagInd
            % Input  : - An AstroTable object (multi elements supported).
            %          - A vector of logicals or indices.
            %            In the case that this is a vector of logicals, no
            %            NaNs are allowed, and the function will return the
            %            lines with logical true.
            %            In case this is a vector of indices (NaNs are
            %            allowed), the corresponding lines will be
            %            returned.
            %            Alternatively, this can be a cell array of vectors
            %            in which each cell corresponds to an element in
            %            the input object.
            %            Or, this can be a structure array, in which each
            %            element corresponds to an element in the input object.
            %            The structure field containing the vector is
            %            specified by the 'StructFieldName' key/val
            %            argument.
            %          * ...,key,val,...
            %            'IgnoreNaN' - A logical indicating if to ignore
            %                   the NaNs in the vector of indices.
            %                   Default is false.
            %            'StructFieldName' - If the second input argument is
            %                   a structure, then this is the field name
            %                   that contains the vector of indices.
            %                   Default is 'Obj2_IndInObj1'.
            %            'CreateNewObj' - true|false. Create new object.
            %                   Default is true.
            % Output : - An AstroTable object with the selected (and optionaly filled with
            %            NaNs) lines.
            % Author : Eran Ofek (Sep 2021)
            % Example: Table = (1:1:5).'; Table = [Table, rand(size(Table))];
            %          AT = AstroTable({Table});
            %          Result = selectRows(AT, [1;2])
            %          Result = selectRows(AT, [2 3 2])
            %          Result = selectRows(AT, [true, true, false, true, false])
            %          Result = selectRows(AT, [2 3 2 NaN],'IgnoreNaN',true)
            %          Result = selectRows(AT, [2 3 NaN 2 NaN],'IgnoreNaN',false)
            
            arguments
                Obj
                FlagInd
                Args.IgnoreNaN(1,1) logical  = false;
                Args.StructFieldName         = 'Obj2_IndInObj1';
                Args.CreateNewObj logical    = true;
            end

            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end

            Nflag = numel(FlagInd);
            Nobj  = numel(Obj);
            for Iobj=1:1:Nobj
                % for each element in the AstroTable
                % Treat FlagInd (struct array, cell, vector)
                if isstruct(FlagInd)
                    Iflag = min(Iobj, Nflag);
                    FlagI = FlagInd(Iflag).(Args.StructFieldName);
                elseif iscell(FlagInd)
                    Iflag = min(Iobj, Nflag);
                    FlagI = FlagInd{Iflag};
                else
                    % vector
                    FlagI = FlagInd;
                end

                Result(Iobj).Catalog = AstroTable.selectLines(Obj(Iobj).Catalog, FlagI, 'IgnoreNaN',Args.IgnoreNaN);
            end
        end
    
        function Result = interp1(Obj, InterpColX, InterpColY, NewX, Args)
            % Interpolate columns in AstroTable.
            % Input  : - A single element Astrotable/AstroCatalog object.
            %          - Column name or index in the AstroTable object
            %            that contains the "X" argument of the
            %            interpolation.
            %          - Column nmaes (char array of a single column or a
            %            cell array of multiple columns) that contains the
            %            "Y" values of the interpolation.
            %          - (NewX) Vector of new X values for the interpolation.
            %          * ...,key,val,...
            %            'Sort' - Sort Table by InterpColX prior to
            %                   interpolation. If true, this will modify
            %                   the input object.
            %                   Default is true.
            %            'InterpMethod' - Interpolation method.
            %                   See interp1 for options.
            %                   Default is 'linear'.
            % Output : - An AstroTable/AstroCatalog object of the
            %            interpolated data.
            %            The new table contains the columns of InterpColX
            %            and InterpColY. The InterpColX will contain the
            %            NewX values, while the columns of InterpColY will
            %            contain the interpolated values at "NewX".
            % Author : Eran Ofek (Jan 2023)
            % Example: AT = AstroTable({rand(100,4)},'ColNames',{'JD','RA','Dec','U'},'ColUnits',{'day','deg','deg',''});
            %          R = AT.interp1('JD',{'RA','Dec'}, (0.1:0.1:0.9)');

            arguments
                Obj(1,1)
                InterpColX
                InterpColY
                NewX
                Args.Sort logical          = true;
                Args.InterpMethod          = 'linear';
            end
            
            NewX = NewX(:);
            
            if ischar(InterpColY)
                InterpColY = {InterpColY};
            end

            ColIndX = Obj.colname2ind(InterpColX);
            ColIndY = Obj.colname2ind(InterpColY);

            if Args.Sort
                Obj = Obj.sortrows(InterpColX);
            end

            if istable(Obj.Catalog)
                NewY = interp1(table2array(Obj.Catalog(:,ColIndX)), table2array(Obj.Catalog(:,ColIndY)), NewX, Args.InterpMethod);
            else
                NewY = interp1(Obj.Catalog(:,ColIndX), Obj.Catalog(:,ColIndY), NewX, Args.InterpMethod);
            end
            
            if isa(Obj, 'AstroCatalog')
                Result = AstroCatalog({[NewX, NewY]});
            else
                Result = AstroTable({[NewX, NewY]});
            end
            Result.ColNames = [InterpColX, InterpColY];
            if numel(Obj.ColUnits)==numel(Obj.ColNames)
                Result.ColUnits = Obj.ColUnits([ColIndX, ColIndY]);
            end
            
        end

        
    end
    
    methods % applay functions and overloads
            
        function Result = funUnary(Obj, Operator, Args)
            % Apply an unary function to columns of a catalog
            % Input  : - An AstroTable object.
            %          - Unary operator handle (e.g., @sin).
            %          * ...,key,val,...
            %            'OpArgs' - A cell array of additional arguments to pass to
            %                   the operator. Default is {}.
            %            'Columns' - List of columns names or indices on which to apply
            %                   the operator. If empty, apply to all columns.
            %                   Default is empty.
            %            'UpdateObj' - A logical indicating if to update the object (in
            %                   addition to returning the output).
            %                   Default is false.
            % Output : - The result (matrix or table).
            % Author : Eran Ofek (Mar 2021)
            % Example: AC=AstroTable; AC.Catalog = rand(100,3); AC.ColNames={'a','n','s'}; AC.funUnary(@sin)
           
            arguments
                Obj
                Operator function_handle
                Args.OpArgs cell            = {};
                Args.Columns                = [];
                Args.UpdateObj(1,1) logical = false;
            end
            
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                %
                ColInd = colname2ind(Obj(Iobj), Args.Columns);  % If columns empty - return all columns
                if istable(Obj(Iobj).Catalog)
                    Ncol  = numel(ColInd);
                    Nrows = size(Obj(Iobj).Catalog,1);
                    Result = nan(Nrows,Ncol);
                    for Icol=1:1:Ncol
                        ColName = Obj(Iobj).ColNames(ColInd(Icol));
                        Result(:,Icol)  = Operator(Obj(Iobj).Catalog.(ColName), Args.OpArgs{:});
                        if Args.UpdateObj
                            Obj(Iobj).Catalog.(ColName) = Result(:,Icol);
                        end
                    end
                else
                    Result = Operator(Obj(Iobj).Catalog(:,ColInd), Args.OpArgs{:});
                    if Args.UpdateObj
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
                Tmp = funUnary(Obj, @min, 'OpArgs',{}, 'Columns',Columns, 'UpdateObj',false);
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
                Tmp = funUnary(Obj, @max, 'OpArgs',{}, 'Columns',Columns, 'UpdateObj',false);
                if Iobj==1
                    Result = zeros(Nobj,numel(Tmp));
                end
                Result(Iobj,:) = Tmp;
            end
        end
        
        function [Result, Flag] = query(Obj, QueryStringUser, Args)
            % Query an AstroTable object
            % Input  : - An AstroTable object.
            %          - A query string with operators on column names
            %            (e.g., 'mag1>8 & mag2<10'),
            %          * ...,key,val,...
            %            'CreateNewObj' - Indicating if the output
            %                   is a new copy of the input (true), or an
            %                   handle of the input (false).
            %                   If empty (default), then this argument will
            %                   be set by the number of output args.
            %                   If 0, then false, otherwise true.
            %                   This means that IC.fun, will modify IC,
            %                   while IB=IC.fun will generate a new copy in
            %                   IB.
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
                Args.CreateNewObj              = [];
            end
            
            if isempty(Args.CreateNewObj)
                if nargout>0
                    Args.CreateNewObj = true;
                else
                    Args.CreateNewObj = false;
                end
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
        
        function [Result, Flag] = queryRange(Obj, varargin)
            % Query catalog by columns in range (without using eval).
            %   See also query
            % Input  : - An AstroTable object.
            %          * ...,key,val,... of column name and column range.
            %            Column name may be a cell array of column names
            %            from which the first to appear will be selected
            %            (dictionary), or a single column name.
            %            Column range may be a range of one or two numbers,
            %            and only values within the range (>=, <=) will be
            %            selected.
            % Output : - An AstroTable object, with the selected rows.
            %            If nargout>0, then a new copy is created, if
            %            nargout=0, the original object is modified.
            %          - A vector of logical of the selected rows, for the
            %          last element of the AstroTable input.
            % Author : Eran Ofek (Jun 2021)
            % Example: AT=AstroTable({rand(100,2)},'ColNames',{'x','y'});
            %          [Result, Flag] = queryRange(AT, 'x',[0.2 0.3],'y',[0.0 0.5])
            %          [Result, Flag] = queryRange(AT, {'x_win','x'},[0.2 0.3],'y',[0.0 1])
           
            % CreateNewObj treatment
            if nargout==0
                CreateNewObj = false;
            else
                CreateNewObj = true;
            end
            
            if CreateNewObj
                Result = Obj.copy();
            else
                Result = Obj;
            end
            
            Narg = numel(varargin);
            if mod(Narg,2)~=0
                error('Must supply pairs of col name and range');
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                Ikey = 0;
                Flag = true(1,1);
                for Iarg=1:2:Narg-1
                    Ikey = Ikey + 1;
                    ColInd = colnameDict2ind(Obj(Iobj), varargin{Iarg});
                    Val    = getCol(Obj(Iobj), ColInd, false, false);
                    Flag   = Flag & (Val>=min(varargin{Iarg+1}) & Val<=max(varargin{Iarg+1}));
                end
                
                Result(Iobj).Catalog = Result(Iobj).Catalog(Flag,:);
            end
            
        end
        
        function [Result, Flag, FlagAll] = queryFun(Obj, ColsFun, Args)
            % Query an AstroTable by applying functions on columns.
            % Input  : - An AstroTable object.
            %          - A cell array of Column_Name, Function,...
            %            Function must get a single vector, and return a
            %            vector of logicals.
            %          * ...,key,val,...
            %            'SameCols' - A logical indicating if all the
            %                   AstroTable elements has the same column
            %                   names/indices. This can be used to expedite
            %                   the run time. Default is false.
            %            'FunFlags' - Function to apply on all the flags
            %                   returns from all the columns.
            %                   (e.g., @all, @any). This is always applied
            %                   on the 2nd dimension. Default is @all.
            %            'IgnoreNaN' - A logical indicating if to ignore
            %                   the NaNs in the vector of indices.
            %                   Default is false.
            %            'ReturnResult' - A logical indicating if to return
            %                   the result. If false, then the Result
            %                   output will be identical to the input.
            %                   Default is true.
            %            'CreateNewObj' - true|false. Create new object.
            %                   Default is false.
            % Output : - An AstroTable with the selected rows.
            %          - A vector of logicals per each line in the last
            %            AstroTable element indicating the selected lines.
            %          - A mtrix of logicals per each line and column in
            %            the last AstroTable element.
            % Author : Eran Ofek (Nov 2021)
            % Example: AT = AstroTable({rand(10,2)},'ColNames',{'X','Y'});
            %          T = queryFun(AT, {'X', @(x) x>0.1, 'Y', @(x) x>0.1}, 'CreateNewObj',true);
           
            arguments
                Obj
                ColsFun cell
                Args.SameCols logical          = false;
                Args.FunFlags function_handle  = @all;
                Args.IgnoreNaN logical         = false;
                Args.ReturnResult logical      = true;
                Args.CreateNewObj logical      = false;
            end
            
            if Args.CreateNewObj
                Result = Obj.copy;
            else
                Result = Obj;
            end
            
            Nfuns = numel(ColsFun);
            Nobj  = numel(Obj);
            
            for Iobj=1:1:Nobj
                Nlines    = size(Obj(Iobj).Catalog,1);
                Iflag     = 0;
                FlagAll   = false(Nlines, Nfuns.*0.5);
                
                for Ifuns=1:2:Nfuns
                    Iflag = Iflag + 1;
                    Ind = colname2ind(Obj(Iobj), ColsFun{Ifuns});
                    if Ifuns==1 && Args.SameCols
                        % assume all catalogs have the same coloums
                        ColsFun{Ifuns} = Ind;
                    end
                    FlagAll(:,Iflag) = ColsFun{Ifuns+1}(Obj(Iobj).Catalog(:,Ind));
                end
                Flag = all(FlagAll, 2);
                if Args.ReturnResult
                    Result(Iobj) = selectRows(Obj(Iobj), Flag, 'IgnoreNaN',Args.IgnoreNaN, 'CreateNewObj',false);
                end
            end
        end
    end

    methods % read/write
        function Result = csvWrite(Obj, FileName)
            % who wrote this? missing help
            csvwrite(FileName, Obj.Catalog);
            Result = true;
        end
        
%         Obsoleted by AstroCatalog.writeLargeCSV
%
%         function Result = writeCSV(Obj, FileName, Args)
%             % write an AstroCatalog to a csv text file
%             % Input  : - An AstroCatalog object or a vector of AC objects
%             %          - name of the file to write to
%             %        * ...,key,val,...
%             %        'Append'   - append to an existing CSV file (no need to
%             %        make a new file and write a line with column names
%             %        'Delimiter' - field delimiter
%             %        'Format' - output format and precision
%             %        'Parallel' - parallel execution is efficient for large volumes
%             % Output : - a csv file
%             % Author : A. Krassilchtchikov (Jun 2023)
%             % Example: Files  = dir ( fullfile('./', '**', '*Cat*') );
%             %          NData  = numel(Files); Data   = repmat({''}, NData, 1);
%             %          for IData = 1:1:NData
%             %              Data{IData} = fullfile(Files(IData).folder, Files(IData).name);
%             %          end
%             %          AC = AstroCatalog(Data);
%             %          AC.writeCSV('/home/ocs/cat.csv');
%             arguments
%             Obj
%             FileName            = 'astrocatalog.csv' % output file name
%             Args.Append logical = false % append or overwrite
%             Args.Delimiter      = ',' % '\t' is tab 
%             Args.Format         = '%10.6e' % output format and precision
%             Args.Parallel logical = false % parallel execution is efficient for large volumes
%             end
%             
%             if ~Args.Append 
%                 FirstLine = Obj(1).ColNames; 
%                 writecell(FirstLine,FileName,'Delimiter',Args.Delimiter);
%             end
%             
%             FileID = fopen(FileName,'a+'); 
%              
% %             formatSpecifier = strcat(Args.Format,Args.Delimiter); 
% %             numRepetitions = size(Obj(1).Catalog,2); 
% %             formatString = [repmat(formatSpecifier, 1, numRepetitions-1), strcat(Args.Format,'\n')]; 
%             
%             for Iobj = 1:numel(Obj)
% %                   fprintf(FileID,formatString,Obj(Iobj).Catalog');
%                   writematrix(Obj(Iobj).Catalog,FileName,'WriteMode','append'); 
%             end
%                         
%             if Args.Parallel 
%                 parfor Iobj = 1:numel(Obj)
%                     dlmwrite(FileName, Obj(Iobj).Catalog, 'delimiter', Args.Delimiter, ...
%                              'precision',Args.Format,'-append')                     
%                 end
%             else
%                 for Iobj = 1:1:numel(Obj)
%                     dlmwrite(FileName, Obj(Iobj).Catalog, 'delimiter', Args.Delimiter, ...
%                          'precision',Args.Format,'-append')
%                 end
%             end
%             
%             fclose(FileID);
%             
%             Result = 0;
% 
%         end
        
        function write1(Obj, FileName, Args)
            % Write an AstroTable to a FITS/HDF5 file.
            % Input  : - An AstroTable/AstroCatalog object.
            %          - File name to save.
            %          * ...,key,val,...
            %            'FileType' - - File type to save:
            %                   'fits'
            %                   {'hdf5','h5z','h5','hd5'}
            %                   'matflat' - save Data and Header in flat
            %                           mat file.
            %                   'matstruct' - save structure with data and
            %                           Header fields.
            %                   {'jpg','tif','tiff','gif','png','bmp','hdf','jp2','jpx','jpeg','pcx','pgm'}
            %                           Use imwrite.m
            %            'Header' -  A 3 columns cell array with header.
            %                       Default is {}.
            %            'Append' - Append image as a multi extension to an
            %                      existing FITS file. Default is false.
            %            'OverWrite'- Overwrite an existing image. Default
            %                       is false.
            %            'WriteTime'- Add creation time to image header.
            %                       Default is false.
            % Output : null
            % Author : Eran Ofek (Jan 2022)
            % Example: 
            
            arguments
                Obj(1,1)
                FileName
                Args.FileType                 = 'fits';
                Args.Header                   = {};
                Args.HDU                      = 1;
                Args.Append logical           = false;
                Args.OverWrite logical        = false;
                Args.WriteTime logical        = false;
            end
            
            FITS.writeTable1(Obj, FileName, 'Header',Args.Header,...
                                               'ColNames',Obj.ColNames,...
                                               'ColUnits',Obj.ColUnits,...
                                               'HDU',Args.HDU,...
                                               'Append',Args.Append,...
                                               'OverWrite',Args.OverWrite,...
                                               'WriteTime',Args.WriteTime);
                                                                                
        end
        
        function Result = toTable(Obj)
            % Convert a single element AstroTable into a matlab table object
            % Input  : - A single element AstroTable/AstroCatalog
            % Output : - A matlab table object.
            % Author : Eran Ofek
            % Example: AT = AstroTable({rand(10,2)},'ColNames',{'a','b'});
            %          T  = AT.toTable
            
            arguments
                Obj(1,1)
            end
           
            if istable(Obj.Catalog)
                Result = Obj.Catalog;
            else
                Result = array2table(Obj.Catalog);
                Result.Properties.VariableNames = Obj.ColNames;
                Result.Properties.VariableUnits = Obj.ColUnits;
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
    
    methods (Static) % help
        function help
            % show help in manuals.AstroTable
            
            open manuals.classes.AstroTable
        end
    end

    methods (Static)  % Unit-Test
        Result = unitTest()
    end
    
end

            
