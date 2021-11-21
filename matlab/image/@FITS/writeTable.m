
function Obj = writeTable(Obj,FileName,HDUnum,Args)
    % Write binary or ascii multiple FITS table into a FITS object.
    % Package: @FITS (Static)
    % Description: Read binary or ascii FITS tables into a a FITS object.
    % Input  : - A FITS object.
    %          - A single FITS tables anem to read. If not provided
    %            then will use the file name in the object.
    %          - HDU number. If empty use the one in the object.
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
    %                         'table' - Default.
    %                         'astcat' - AstCat object.
    %                         'astcat_t' - AstCat object in which
    %                                      the data is stored as a table.
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
    % Output : - The updated FITS file(s).
    %--------------------------------------------------------------------------

    arguments
        Obj
        FileName                       = [];
        HDUnum                         = [];
        Args.TableType                 = 'auto';
        Args.HDUnum                    = 2;
        Args.ModColName(1,1) logical   = false;
        Args.OutTable                  = 'table';
        Args.XTENkey                   = 'XTENSION';
        Args.StartRow                  = [];
        Args.NRows                     = [];
        Args.OutClass                  = @double;
        Args.NullVal                   = NaN;
        Args.BreakRepCol               = true;
    end

    if ~isempty(FileName)
        if numel(Obj)>1
            error('if file name is given than Obj must contain single element');
        else
            Obj.File   = FileName;
            Obj.HDU    = HDUnum;
        end

    end

    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % write each FITS file
        if ~isempty(Obj(Iobj).File)
            KeyVal = tools.struct.struct2keyval(Args);
            [Obj(Iobj).Data,Obj(Iobj).Header] = ...
                FITS.writeTable1(Obj(Iobj).File, 'HDUnum',Obj.HDU, KeyVal{:});
        end
    end
end        
