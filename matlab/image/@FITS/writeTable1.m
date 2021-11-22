function Result = writeTable1(Table, FileName, Args)
    % Write or append a table into FITS file.
    % Static function
    %     The table may have N-dimensions.
    %     Append will write multi extension FITS image.
    % Input  : - Array to save as FITS image.
    %          - FITS file name to save.
    %          * Arbitrary number of ...,key,val,... pairs.
    %            Following keywords are available:
    %            'TableType'- FITS table type {'auto'|'bintable'|'binary','table'}.
    %                         Default is ??? 'auto'.
    %                         'auto' will attempt to read the table type
    %                         from the 'XTENSION' header keyword.
    %            'HDUnum'   - HDU number in which the FITS table header is.
    %                         Default is 2.    
    %            'Header' - Cell array of {key,val,comment} header
    %                       or an HEAD object to write into the
    %                       FITS file.
    %            'DataType' - Data type - default is 'single'
    %                       precision.
    %            'Append' - Append image as a multi extension to an
    %                       existing FITS file. Default is false.
    %            'OverWrite'- Overwrite an existing image. Default
    %                       is false.
    %            'WriteTime'- Add creation time to image header.
    %                       Default is false.
    % Example: Flag=FITS.write(rand(100,100),'Try.fits');
    %          Flag=FITS.write(rand(10,10,3),'Try.fits');
    %

    arguments
        Table
        FileName
        Args.TableType                = 'binary';
        Args.Header cell              = {};
        Args.DataType                 = 'single';
        Args.Append(1,1) logical      = false;
        Args.OverWrite(1,1) logical   = false;
        Args.WriteTime(1,1) logical   = false;

        Args.XTENkey char             = 'XTENSION';
        Args.StartRow                 = 1;
        Args.NRows                    = [];        
        
        Args.ColNames                 = [];
        Args.ColDataType              = [];
        Args.ColUnits                 = [];
        Args.HDUnum                   = 1;
        Args.ExtName                  = '';
    end

    %HeaderField = HEAD.HeaderField;

    % Set FITS DataType
    %DataType = FITS.getDataType(Args.DataType);

    % Replace
    if strcmp(Args.TableType, 'bintable')
        Args.TableType = 'binary';
    end
    
    % Overwrite existing FITS file
    if (Args.OverWrite)
        % delete existing FileName if exist
        if isfile(FileName)
            delete(FileName);
        end
    end
    
    % Get data size
    TableSize = size(Table.Catalog);        
    Ncol = TableSize(2);
    
    % Prepare header
    %Header = FITS.prepareHeader(Args.Header, HeaderField, 'WriteTime', Args.WriteTime);

    if (Args.Append)
        % append to existing FITS file
        Fptr = matlab.io.fits.openFile(FileName, 'READWRITE');
    else
        % Create new FITS file
        Fptr = matlab.io.fits.createFile(FileName);
    end

    matlab.io.fits.movAbsHDU(Fptr, Args.HDUnum);
    
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
        Args.ColDataType(:) = {'1D'};        
    end        

    % Get column units
    if isempty(Args.ColUnits)
        Args.ColUnits = Table.ColUnits;
    end
    
    if isempty(Args.ColUnits)        
        Args.ColUnits = cell(1, Ncol);
        Args.ColUnits(:) = {'Unit'};
    end        
    
    % create Table
    % createTbl(fptr,tbltype,nrows,ttype,tform,tunit,extname)    
    matlab.io.fits.createTbl(Fptr, Args.TableType, Args.NRows,...
        Args.ColNames, Args.ColDataType, Args.ColUnits, Args.ExtName);
                   
    % write Header
    %FITS.writeHeader(Fptr, Header, HeaderField);           
    
    % Process each column
    for Icol=1:1:Ncol
        Data = Table.Catalog(:,Icol);
        
        % Write elements into ASCII or binary table column
        % writeCol(fptr,colnum,firstrow,coldata)
        matlab.io.fits.writeCol(Fptr, Icol, Args.StartRow, Data); 

    end
            
    % Close FITS file
    matlab.io.fits.closeFile(Fptr);
    Result = (sign(Fptr) == 1);
end

%--------------------------------------------------------------------------

function sample1()

    import matlab.io.*
    fptr = fits.createFile('myfile.fits');
    
    ttype = {'Col1','Col2','Col3','Col4'};
    tform = {'2L','3X','1D','1PC'};
    tunit = {'','kg/m^3','candela','parsec'};  
    fits.createTbl(fptr,'binary',0,ttype,tform,tunit,'my-table');
    
    fits.writeCol(fptr,1,1,[false false; true false]);
    fits.writeCol(fptr,2,1,int8([0 1 1; 1 1 1; 1 1 1; 1 0 1]));
    fits.writeCol(fptr,3,1,[1; 2; 3; 4]);
    
    data = cell(4,1);
    data{1} = single(1);
    data{2} = single(1+2j);
    data{3} = single([1j 2 3+j]);
    data{4} = single([1 2+3j 3 4]);
    fits.writeCol(fptr,4,1,data);
    
    fits.closeFile(fptr);
    fitsdisp('myfile.fits','index',2,'mode','full');
    
end
