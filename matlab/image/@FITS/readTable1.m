
function [Out,HeadCell,Col] = readTable1(TableName,Args)
    % Read binary or ascii single FITS table
    % Package: @FITS (Static)
    % Description: Read binary or ascii FITS tables into a table.
    % Input  : - A FITS tables to read.
    %          * Arbitrary number of pairs of arguments: ...,keyword,value,...
    %            where keyword are one of the followings:
    %            'TableType'- FITS table type {'auto'|'bintable'|'table'}.
    %                         Default is 'auto'.
    %                         'auto' will attempt to read the table type
    %                         from the 'XTENSION' header keyword.
    %            'HDUnum'   - HDU number in which the FITS table header is.
    %                         Default is 2.
    %            'ModColName' - If the program failed because some columns
    %                         have name starting with numbers or invalid signs
    %                         set this to true (default is false).
    %                         This will modify the column names.
    %            'OutTable' - Type of table output:
    %                         'table' - Default
    %            'XTENkey'  - Header keyword from which to read the table type.
    %                         Default is 'XTENSION'.
    %            'StartRow' - First row to read from FITS table. Default is [].
    %                         If empty, then read the entire table.
    %            'NRows'    - Number of rows to read from FITS table.
    %                         Default is []. If empty, then read the entire
    %                         table.
    %            'OutClass' - A function to use in order to force all the
    %                         columns to be of the same class (e.g., @single).
    %                         Default is @double. If empty, then will keep
    %                         the original class. This option shoyld be used
    %                         if you want to read the data into a matrix.
    %            'NullVal'  - If the column is of numeric type will attempt
    %                         to replace the FITS null value with this value.
    %                         Default is NaN.
    %            'BreakRepCol'- {true|false}. If true and FITS table columns
    %                         are repeating then will change column information
    %                         according to the matrix column count.
    %                         Default is true.
    % Output : - A table containing the FITS table content.
    %          - The FITS file header.
    %          - A structure array of additional columns
    %            information, like format.
    % Tested : Matlab R2014a
    %     By : Eran O. Ofek                    Jan 2015
    %    URL : http://weizmann.ac.il/home/eofek/matlab/
    % Example: [Out,Head,Col]=FITS.readTable1('asu.fit');
    % Reliable: 2
    %--------------------------------------------------------------------------

    arguments
        TableName
        Args.TableType char           = 'auto';    % {'auto'|'bintable'|'table'}
        Args.HDUnum                   = 2;
        Args.ModColName(1,1) logical  = false;
        Args.OutTable char            = 'table';  % {'astcat'|'astcat_t'|...}
        Args.XTENkey char             = 'XTENSION';
        Args.StartRow                 = [];
        Args.NRows                    = [];
        Args.OutClass function_handle = @double;
        Args.NullVal                  = NaN;       % [] do nothing
        Args.BreakRepCol(1,1) logical = true;
    end

    CatField        = AstCat.CatField;
    ColField        = AstCat.ColField;
    ColCellField    = AstCat.ColCellField;
    ColUnitsField   = AstCat.ColUnitsField';

    % prep list of fits table names
    [ListTableName] = io.files.filelist(TableName);
    Nfile = numel(ListTableName);

    %Out = AstCat(Nfile,1);
    Col = tools.struct.struct_def({'Col','Cell','Units','TypeChar','Repeat','Scale','Zero','Nulval','Tdisp','Data'},Nfile,1);

    Ifile = 1;
    % get header
    HeadCell    = FITS.readHeader1(ListTableName{1},Args.HDUnum);

    % identify table type
    switch lower(Args.TableType)
        case 'auto'
            Ixten    = find(strcmp(HeadCell(:,1),Args.XTENkey));
            if (isempty(Ixten))
                error('Automatic table identification mode was unable to access FITS table type');
            end
            Args.TableType = HeadCell{Ixten,2};
        otherwise
            % do nothing
    end

    % Table type
    switch lower(Args.TableType)
        case 'bintable'
            Fun_getColParms = @matlab.io.fits.getBColParms;
        case 'table'
            Fun_getColParms = @matlab.io.fits.getAColParms;
        otherwise
            error('Unknown TableType option');
    end

    if (isempty(Args.StartRow) || isempty(Args.NRows))
        CellRowPar = {};
    else
        CellRowPar = {Args.StartRow,Args.NRows};
    end

    % for each fits table
    Fptr = matlab.io.fits.openFile(ListTableName{Ifile});
    matlab.io.fits.movAbsHDU(Fptr,Args.HDUnum);
    Ncol = matlab.io.fits.getNumCols(Fptr);
    %[ttype,tunit,typechar,repeat,scale,zero,nulval,tdisp]= fits.getBColParms(Fptr,2);
    Col(Ifile).Cell     = cell(1,Ncol);
    Col(Ifile).Units    = cell(1,Ncol);
    Col(Ifile).TypeChar = cell(1,Ncol);
    Col(Ifile).Repeat   = cell(1,Ncol);
    Col(Ifile).Scale    = cell(1,Ncol);
    Col(Ifile).Zero     = cell(1,Ncol);
    Col(Ifile).Nulval   = cell(1,Ncol);
    Col(Ifile).Tdisp    = cell(1,Ncol);
    Col(Ifile).Data     = cell(1,Ncol);
    for Icol=1:1:Ncol

        switch lower(Args.TableType)
            case 'bintable'
                [Col(Ifile).Cell{Icol},Col(Ifile).Units{Icol},Col(Ifile).TypeChar{Icol},...
                               Col(Ifile).Repeat{Icol},Col(Ifile).Scale{Icol},Col(Ifile).Zero{Icol},...
                               Col(Ifile).Nulval{Icol},Col(Ifile).Tdisp{Icol}]= Fun_getColParms(Fptr,Icol);
            case 'table'
                [Col(Ifile).Cell{Icol},Col(Ifile).TypeChar{Icol},Col(Ifile).Units{Icol},...
                               Col(Ifile).Repeat{Icol},Col(Ifile).Scale{Icol},Col(Ifile).Zero{Icol},...
                               Col(Ifile).Nulval{Icol},Col(Ifile).Tdisp{Icol}]= Fun_getColParms(Fptr,Icol);
            otherwise
                error('Unknown TableType option');
        end


        [Col(Ifile).Data{Icol}] = matlab.io.fits.readCol(Fptr,Icol,CellRowPar{:});
        if (~isempty(Args.OutClass))
            Col(Ifile).Data{Icol} = Args.OutClass(Col(Ifile).Data{Icol});
        end
        if (~isempty(Args.NullVal) && ~isempty(Col(Ifile).Nulval{Icol}) && isnumeric(Col(Ifile).Data{Icol}))
            Col(Ifile).Data{Icol}(Col(Ifile).Data{Icol}==Col(Ifile).Nulval{Icol}) = Args.NullVal;

        end
        % override ColRepeat using the actual data
        Col(Ifile).Repeat{Icol} = size(Col(Ifile).Data{Icol},2);
    end
    matlab.io.fits.closeFile(Fptr);

    % deal with repeating columns
    if (Args.BreakRepCol)
        Nnc  = sum(cell2mat(Col(Ifile).Repeat));
        NewCol.Cell = cell(Nnc,1);

        Icol1 = 1;
        for Icol=1:1:Ncol
            IcolN = Icol1 + Col(Ifile).Repeat{Icol} - 1;
            %Icol1 = Icol1 + Col.Repeat{Icol}; % at the end of the loop
            for Irep=1:1:Col(Ifile).Repeat{Icol}
                if (Col(Ifile).Repeat{Icol}>1)
                    NewCol.Cell{Icol1+Irep-1} = sprintf('%s_%d_',Col(Ifile).Cell{Icol},Irep);
                else
                    NewCol.Cell{Icol1+Irep-1} = Col(Ifile).Cell{Icol};
                end
            end
            [NewCol.Units{Icol1:IcolN}] = deal(Col(Ifile).Units{Icol});
            [NewCol.TypcChar{Icol1:IcolN}] = deal(Col(Ifile).TypeChar{Icol});
            [NewCol.Repeat{Icol1:IcolN}] = deal(1);
            [NewCol.Scale{Icol1:IcolN}] = deal(Col(Ifile).Scale{Icol});
            [NewCol.Zero{Icol1:IcolN}] = deal(Col(Ifile).Zero{Icol});
            [NewCol.Tdisp{Icol1:IcolN}] = deal(Col(Ifile).Tdisp{Icol});
            Icol1 = Icol1 + Col(Ifile).Repeat{Icol}; % at the end of the loop

        end
        Col(Ifile).Cell     = NewCol.Cell;
        Col(Ifile).Units    = NewCol.Units;
        Col(Ifile).TypeChar = NewCol.TypcChar;
        Col(Ifile).Repeat   = NewCol.Repeat;
        Col(Ifile).Scale    = NewCol.Scale;
        Col(Ifile).Zero     = NewCol.Zero;
        Col(Ifile).Tdisp    = NewCol.Tdisp;
    end
    if (Args.ModColName)
        % modify column names to legal variable names
        Col(Ifile).Cell = regexprep(Col(Ifile).Cell,{'-','/','(',')','&','@','#','^','%','*','~'},'');
        Col(Ifile).Cell = strcat('T',Col(Ifile).Cell);
    end
    Col(Ifile).Col      = cell2struct(num2cell(1:1:length(Col(Ifile).Cell)),Col(Ifile).Cell,2);

    % output
    switch lower(Args.OutTable)
        case 'table'
           Out = table(Col(Ifile).Data{:});
           Out.Properties.VariableNames = Col(Ifile).Cell;
           Out.Properties.VariableUnits = Col(Ifile).Units;

        case {'astrocatalog','astrotable'}
            Out(Ifile) = AstroCatalog;
            Out(Ifile).Catalog     = [Col(Ifile).Data{:}];
            Out(Ifile).ColNames    = Col(Ifile).Cell;
            Out(Ifile).ColUnits    = Col(Ifile).Units;



        case 'astcat'
            Out(Ifile).(CatField)     = [Col(Ifile).Data{:}];
            Out(Ifile).(ColField)     = Col(Ifile).Col;
            Out(Ifile).(ColCellField) = Col(Ifile).Cell;
            Out(Ifile).(ColUnitsField)= Col(Ifile).Units;

        case 'astcat_t'
            Out(Ifile).(CatField)     = table(Col(Ifile).Data{:});
            Out(Ifile).(ColField)     = Col(Ifile).Col;
            Out(Ifile).(ColCellField) = Col(Ifile).Cell;
            Out(Ifile).(ColUnitsField)= Col(Ifile).Units;

        otherwise
            error('Unknown OuTable option');
    end

end

