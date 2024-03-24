function Result = writeTable1(Table, FileName, Args)
    % Write or append a table into FITS file.
    % Static function
    %     The table may have N-dimensions.
    %     Append will write multi extension FITS image.
    % Input  : - Table      - Array to save as FITS image, supported types:
    %                         AstroTable, AstroCatalog, and any type accepted by 
    %                         AstroTable constructor
    %          - FileName   - FITS file name to save.
    %          * Arbitrary number of ...,key,val,... pairs.
    %            Following keywords are available:
    %            'TableType'- FITS table type {'bintable'|'binary','table','ascii'}.
    %                         Default is 'binary'. 
    %                         CURRENTLY ONLY 'binary' IS SUPPORTED.
    %            'HDUnum'   - HDU number in which the FITS table header is.
    %                         Default is 2.    
    %            'DataType' - Default data type for columns, used when ColDataType is empty.
    %                         Default is '1D' (double)
    %            'Append'   - Append image as a multi extension to an
    %                         existing FITS file. Default is false.
    %            'OverWrite'- Overwrite an existing file. Default is false.
    %            'ExtName'  - Extension name. Default is 'fits'
    %            'StartRow' - If set, data is written starting this row. default is []
    %            'NRows'    - If set, limit the number of rows written. default is []
    %            'ColNames'    - cell, list of column names. default is []
    %            'ColDataType' - cell, list of data type of each column, if empty,
    %                            'DataType' is used . default is []
    %            'ColUnits'    - cell. default is []
    %
    %            'Header'   - Additonal header, Cell array of {key,val,comment} header or AstroHeader
    %                         Default is [].
    %            'HeaderHDUnum' - HDU number of the additional header.
    %                             Default is [] which will be 'HDUnum'+1
    %            'WriteTime'    - Add creation time to Header. Default is false.
    %
    % Example: 
    %   AC = AstroTable({rand(10, 2)}, 'ColNames', {'RA','Dec'});      
    %   FITS.writeTable1(AC, FileName, 'ExtName', 'MyExtName');
    %
    arguments
        Table
        FileName
        Args.TableType      = 'binary';
        Args.HDUnum         = 1;        
        Args.DataType       = '1D';
        Args.Append         = false;
        Args.OverWrite      = false;
        Args.WriteTime      = false;
        Args.ExtName        = 'fits';               
        Args.StartRow       = 1;
        Args.NRows          = [];                
        Args.ColNames       = [];
        Args.ColDataType    = [];
        Args.ColUnits       = [];
        Args.HeaderHDUnum   = [];
        Args.Header         = {};
        Args.UseMex         = false;
    end
    
    % sanify the file name so that it contain the absolute path
    FileName = tools.os.relPath2absPath(FileName); 

    % Try to convert Table to AstroTable
    if ~isa(Table, 'AstroTable') && ~isa(Table, 'AstroCatalog')
        Table = AstroTable(Table);
    end
    
    % Support both 'bintable' / 'binary' and 'table' / 'ascii' 
    if strcmp(Args.TableType, 'bintable')
        Args.TableType = 'binary';
    end
    if strcmp(Args.TableType, 'table')
        Args.TableType = 'ascii';
    end 
    
    % Overwrite existing FITS file, delete existing FileName if exist
    if (Args.OverWrite)
        if isfile(FileName)
            delete(FileName);
        end
    end
    
    % Get data size
    TableSize = size(Table.Catalog);        
    Ncol = TableSize(2);
    
    % Prepare header
    % Still unused, discuss with Eran what to do about the Header and
    % 'auto'
    %HeaderField = HEAD.HeaderField;
    
   
    if (Args.Append)
        % append to existing FITS file
        Fptr = matlab.io.fits.openFile(FileName, 'READWRITE');
    else
        % Create new FITS file
        Fptr = matlab.io.fits.createFile(FileName);
    end
    
    if isempty(Args.NRows)
        Args.NRows = TableSize(1);
    end
    
    % Get column names
    if isempty(Args.ColNames)
        Args.ColNames = Table.ColNames;
    end    
    
    % Get columns data type
    if isempty(Args.ColDataType)
        Args.ColDataType = cell(1, Ncol);
        Args.ColDataType(:) = {Args.DataType};        
    end        

    % Get column units
    if isempty(Args.ColUnits)
        Args.ColUnits = Table.ColUnits;
    end
    
    if isempty(Args.ColUnits)        
        Args.ColUnits = cell(1, Ncol);
        Args.ColUnits(:) = {'Unit'};
    end        
    
    % Move to specified HDU number
    matlab.io.fits.movAbsHDU(Fptr, Args.HDUnum);
    
    % create Table
    % createTbl(fptr,tbltype,nrows,ttype,tform,tunit,extname)    
    matlab.io.fits.createTbl(Fptr, Args.TableType, Args.NRows,...
        Args.ColNames, Args.ColDataType, Args.ColUnits, Args.ExtName);
                   
    % Process each column
    for Icol=1:1:Ncol       
        % Write elements into ASCII or binary table column
        % writeCol(fptr,colnum,firstrow,coldata)
        Data = Table.Catalog(:,Icol);        
        matlab.io.fits.writeCol(Fptr, Icol, Args.StartRow, Data); 
    end
            
    % Write optional header
    if ~isempty(Args.Header)
        
        HeaderField = HEAD.HeaderField;
        Header = FITS.prepareHeader(Args.Header, HeaderField, 'WriteTime', Args.WriteTime);
        
        if isempty(Args.HeaderHDUnum)
            Args.HeaderHDUnum = Args.HDUnum + 1;
        end
        
        % Use mex function to append the image header, file must be close
        if Args.UseMex
            matlab.io.fits.closeFile(Fptr);
            Result = (sign(Fptr) == 1);
            io.fits.mex.mex_fits_table_write_image_header(Header, FileName);
            return;
        end

        matlab.io.fits.movAbsHDU(Fptr, Args.HeaderHDUnum);

        % create empty Image
        % Note that createImg() requires 'Expected NAXES to be positive'
        % meaning that Image must be at least one byte
        Image = [0];
        matlab.io.fits.createImg(Fptr, 'single', size(Image));
        matlab.io.fits.writeImg(Fptr, Image);
        FITS.writeHeader(Fptr, Header, HeaderField);           
    end

    
    % Close FITS file
    matlab.io.fits.closeFile(Fptr);
    Result = (sign(Fptr) == 1);
end
