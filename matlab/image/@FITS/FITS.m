% FITS
%
% Use FV to view FITS files:
% https://heasarc.gsfc.nasa.gov/docs/software/ftools/fv/
%
% list of needed functionality
% write
%
% NOTE: This file uses obsolete/HEAD.m 
%--------------------------------------------------------------------------
% #functions (autogen)
% FITS - FITS object constructor
% delete_keys - Delete a lits of keywords from a list of FITS headers Package: @FITS (Static) Description: Delete a list of header keywords from a list of FITS images.
% get_keys - Get keywords value from a single FITS header Package: @FITS (Static function) Description: Get the values of specific keywords from a single FITS file header. Use only for existing keywords.
% mget_keys - Get header keywords value from multiple FITS Package: @FITS (Static) Description: Get the values of specific keywords from a list of FITS files header.
% numHDU - return the number of HDUs in a FITS file
% numHDU1 - return the number of HDUs in a single FITS file A static function of FITS class
% read - Read all FITS file to a FITS object
% read1 - Read a single image from a FITS file A static function of FITS class
% read2cube - Read a list of FITS images into a cube (multiple file names or multiple HDUs) Static function
% read2sim - Description: Read FITS images into SIM object. Can read N-dimensional images. Can also read multi extension files.
% readHeader - Read an header into a three column cell array
% readHeader1 - Read a single header from a FITS file (Static) A static function of FITS class
% readTable - Read binary or ascii multiple FITS table into a FITS object. Package: @FITS (Static) Description: Read binary or ascii FITS tables into a a FITS object.
% readTable1 - Read binary or ascii single FITS table Package: @FITS (Static) Description: Read binary or ascii FITS tables into a table.
% write - Write or append an image into FITS file. Static function The image may have N-dimensions. Append will write multi extension FITS image.
% write_keys - Insert or update FITS header keywords Package: @FITS (Static) Description: Insert new, or update existing FITS header keywords in a list of FITS images.
% #/functions (autogen)
%

classdef FITS < handle

    properties
        Data                                      = [];         % free format
        Header                                    = cell(0,3);  % free format
        File     {mustBeA(File,{'string','char','cell'})}  = '';
        HDU(1,1)                                  = 1;
        CCDSEC double                             = [];
    end
    
   
    methods % constructor
        function Obj = FITS(FileName, ListHDU)
            % FITS object constructor
            % Input  : - A cell array of file names, a file name or a file name with wild cards.
            %          - A vector of HDU numbers with size 1, or equal in
            %            size to the file list.
            % Output : - A FITS object with the File and HDU properties
            %            populated.
            
            arguments
                FileName    = '';
                ListHDU     = 1;
            end
            
            % file names
            if iscell(FileName)
                % convert to a string array
                List = convertCharsToStrings(FileName);
            elseif ischar(FileName) || isstring(FileName)
                % read into cell of files
                List = io.files.filelist(FileName);
            else
                error('Unknown FileName type');
            end
            
            % HDUs
            Nlist = numel(List);
            Nhdu  = numel(ListHDU);
            if Nhdu==1 || Nlist==Nhdu
                ListHDU = ListHDU + zeros(size(List));
                Nhdu  = numel(ListHDU);
            else
                error('Number of HDUs must be 1 or equal to the number of files');
            end
            
            
            for Ilist=1:1:Nlist
                Ihdu            = min(Ilist,Nhdu);
                Obj(Ilist).File = List{Ilist};
                Obj(Ilist).HDU  = ListHDU(Ihdu);
            end
            
        end
    end
    

    
    methods (Static)
        function Nhdu = numHDU1(FileName)
            % return the number of HDUs in a single FITS file
            % A static function of FITS class
            % Input  : - FITS file name.
            % Output : - Number of HDUs in FITS file
            % Author : Eran Ofek
            % Example: Nhdu=FITS.numHDU1(FileName)
            
            Fptr = matlab.io.fits.openFile(FileName);
            Nhdu = matlab.io.fits.getNumHDUs(Fptr);
            matlab.io.fits.closeFile(Fptr);
        end
        
        
        function [HeadCell,Nhdu] = readHeader1(FileName, HDUnum)
            % Read a single header from a FITS file (Static)
            % A static function of FITS class
            % Input  : - FITS file name
            %          - HDU number. Default is 1
            % Output : - A 3 column cell array of header entries
            %            [Key, Value, Comment]
            %          - Number of HDUs in FITS file
            % Author : Eran Ofek
            % Example: [HeadCell,Nhdu] = FITS.readHeader1(FileName,HDUnum)
            
            arguments
                FileName char
                HDUnum               = 1;
            end
                        
            KeyPos = 9;
            ComPos = 32;
            
            Fptr = matlab.io.fits.openFile(FileName);
            Nhdu = matlab.io.fits.getNumHDUs(Fptr);
            if HDUnum > Nhdu
                error('Request for HDU %d failed because there are %d HDUs in file',HDUnum,Nhdu)
            else
                Htype = matlab.io.fits.movAbsHDU(Fptr,HDUnum);

                Nkey = matlab.io.fits.getHdrSpace(Fptr);
                HeadCell = cell(Nkey,3);
                for Ikey = 1:1:Nkey
                   Card     = matlab.io.fits.readRecord(Fptr,Ikey);
                   LenCard = length(Card);
                   if (LenCard>=9)

                       if strcmpi(Card(KeyPos),'=') || strcmpi(Card(1:8),'CONTINUE') 
                           HeadCell{Ikey,1}  = tools.string.spacedel(Card(1:KeyPos-1));
                           % Normally, the comment should start at column
                           %  32. However, Value may be a long string, and
                           %  the delimiting slash may be moved further.
                           %  Moreover, the long string may contain itself
                           %  a slash (e.g., in a path). In this case the 
                           %  string starts with a quote, and we
                           %  must first search for the closing quote.
                           PosAp = strfind(Card(KeyPos+1:end),'''');
                           % Update comment position due to over flow
                           Islash = strfind(Card(1:end),'/');
                           if (isempty(Islash)) && length(PosAp)<2
                               UpdatedComPos = ComPos;
                           else
                               if length(PosAp)>=2
                                   if isempty(Islash)
                                       UpdatedComPos = max(ComPos,KeyPos+1+PosAp(2));
                                   else
                                       UpdatedComPos = Islash(Islash>KeyPos+1+PosAp(2));
                                   end
                               else
                                   UpdatedComPos = Islash(1);
                               end
                           end
                           if ~isempty(UpdatedComPos)
                               Value = Card(KeyPos+1:min(LenCard,UpdatedComPos-1));
                           else
                               % long string and no comment (e.g.
                               %  continuing)
                               Value = Card(KeyPos+1:end);
                           end
                           PosAp = strfind(Value,'''');

                           if (isempty(PosAp))
                               if contains('TF',upper(strtrim(Value)))
                                   % a boolean
                                   Value=upper(strtrim(Value))=='T';
                               else
                                   % possible number
                                   %Value = str2double(Value);
                                   Value = real(str2doubleq(Value));  % faster
                               end
                           else
                               if (length(PosAp)>=2)
                                   % a string-am
                                   Value = strtrim(Value(PosAp(1)+1:PosAp(2)-1));
                               else
                                   Value = Card(PosAp(1)+10:end);
                               end
                           end

                           HeadCell{Ikey,2}  = Value; %Card(KeyPos+1:min(LenCard,ComPos-1));
                           if (LenCard>UpdatedComPos)
                               HeadCell{Ikey,3}  = strtrim(Card(UpdatedComPos+1:end));
                           else
                               HeadCell{Ikey,3}  = '';
                           end

                       end
                   end

                   % look for history and comment keywords
                   if numel(Card) > 6
                       if (strcmpi(Card(1:7),'HISTORY'))
                           HeadCell{Ikey,1} = 'HISTORY';
                           HeadCell{Ikey,2} = Card(KeyPos:end);
                           HeadCell{Ikey,3} = '';
                       end
                       if (strcmpi(Card(1:7),'COMMENT'))
                           HeadCell{Ikey,1} = 'COMMENT';
                           HeadCell{Ikey,2} = Card(KeyPos+2:end);
                           HeadCell{Ikey,3} = '';
                       end
                       % HEASARCH (sic) continuation lines, append content
                       %   to the previous record, and then empty the
                       %   current one (which will be removed later)
                       % I think this will work only for string values
                       if (strcmpi(Card(1:8),'CONTINUE'))
                           ValuePart = HeadCell{LastBegunKey,2};
                           if strcmp(ValuePart(end),'&')
                               ValuePart=ValuePart(1:end-1);
                           end
                           CommPart = HeadCell{LastBegunKey,3};
                           if ~isempty(CommPart) && strcmp(CommPart(end),'&')
                               CommPart=CommPart(1:end-1);
                           end
                           HeadCell{LastBegunKey,2} = strcat(ValuePart,HeadCell{Ikey,2});
                           HeadCell{LastBegunKey,3} = strcat(CommPart,HeadCell{Ikey,3});
                           HeadCell{Ikey,1} = [];
                           HeadCell{Ikey,2} = [];
                           HeadCell{Ikey,3} = [];
                       else
                           LastBegunKey = Ikey;
                       end
                       
                   end
                end

            end
            matlab.io.fits.closeFile(Fptr);

            % remove HeadCell records which are all empty, which result
            %  either from blank lines in the header, comments with no key,
            %  or after continuing lines have been joined
            %  -- How, compactly?
        end
        
        function [Image, HeadCell, Nhdu] = read1(FileName, HDUnum, Args)
            % Read a single image from a FITS file
            % A static function of FITS class
            % Input  : - FITS file name.
            %          - HDU number. default is 1.
            %          * ...,key,val,...
            %            'CCDSEC' - [xmin xmax ymin ymax] of image to read.
            %                   If empty read entire image.
            %                   Default is empty.
            % Output : - Image.
            %          - A 3 column cell array of header entries.
            % Author : Eran Ofek
            % Example: [Image,HeadCell,Nhdu]=FITS.read1(FileName,HDUnum)
            
            arguments
                FileName char
                HDUnum             = 1;
                Args.CCDSEC        = [];
            end
                         
            Fptr = matlab.io.fits.openFile(FileName);
            %Fptr = matlab.io.fits.openDiskFile(FileName);
            matlab.io.fits.movAbsHDU(Fptr, HDUnum);

            if isempty(Args.CCDSEC) || all(isinf(Args.CCDSEC))
                % read full image
                Image = matlab.io.fits.readImg(Fptr);
            else
                % read image section
                % set up start/end pixel positions
                EndPix   = fliplr(Args.CCDSEC([2,4]));
                StartPix = fliplr(Args.CCDSEC([1,3]));

                Image = matlab.io.fits.readImg(Fptr,StartPix,EndPix);
            end
            if nargout>1
                % read header
                [HeadCell,Nhdu] = FITS.readHeader1(FileName,HDUnum);
            end

            matlab.io.fits.closeFile(Fptr);

        end
        
        function [Cube] = read2cube(List,HDUnum,Args)
            % Read a list of FITS images into a cube (multiple file names or multiple HDUs)
            % Static function
            % Input  : - An image name with wild cards or a cell array of
            %            image names.
            %          - A scalar or a vector of HDU numbers.
            %          * ...,key,val,...
            %            'CCDSEC' - [xmin xmax ymin ymax] of image to read.
            %                   If empty read entire image.
            %                   Default is empty.
            % Output : - A cube of images. Image index is in 3rd dimension.
            % Author : Eran Ofek
            % Example: [Cube]=read2cube(List,HDUnum);
            
            arguments
                List
                HDUnum         = 1;
                Args.CCDSEC    = [];
            end
            if ~iscell(List)
                List = io.files.filelist(List);
            end
            
            Nlist = numel(List);
            Nhdu  = numel(HDUnum);
            Nmax  = max(Nlist,Nhdu);
            for Imax=1:1:Nmax
                Ilist = min(Nlist,Imax);
                Ihdu  = min(Nhdu,Imax);
                [Image] = FITS.read1(List{Ilist},HDUnum(Ihdu),'CCDSEC',Args.CCDSEC);
                if Imax==1
                    SizeIm = size(Image);
                    Cube = zeros(SizeIm(1),SizeIm(2), Nmax);
                end
                Cube(:,:,Imax) = Image;
            end
            
        end
        
        %------------------------------------------------------------------        
        function [Out,HeadCell,Col] = readTable1(TableName, Args)
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
            %            'OutTable' - Type of table output: {'table'|'astrocatalog'|'astrotable'}
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
            %            'IdentifyNaN' - A logical indicating if to replace
            %                         -9.1191e-36 with NaN.
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
            % @Todo: Fix to return the same cell form as AstroCatalog.ColNames            
            
            arguments
                TableName
                Args.TableType char           = 'auto';    % {'auto'|'bintable'|'table'}
                Args.HDUnum                   = 2;
                Args.ModColName(1,1) logical  = false;
                Args.OutTable char            = 'table';
                Args.XTENkey char             = 'XTENSION';
                Args.StartRow                 = [];
                Args.NRows                    = [];
                Args.OutClass                 = @double;
                Args.NullVal                  = NaN;       % [] do nothing
                Args.BreakRepCol(1,1) logical = true;
                Args.IdentifyNaN logical      = true;
            end
                      
            % get header as cell array
            HeadCell = FITS.readHeader1(TableName, Args.HDUnum);

            % identify table type
            switch lower(Args.TableType)
                case 'auto'
                    % Find extension type field in the header
                    Ixten = find(strcmp(HeadCell(:,1), Args.XTENkey));
                    if (isempty(Ixten))
                        error('Automatic table identification mode was unable to access FITS table type');
                    end
                    
                    % Get extension type (the value of key XTENkey)
                    Args.TableType = HeadCell{Ixten,2};
                    
                case 'table'
                case 'bintable'
                    % Do nothing
                    
                otherwise
                    error('Unknown TableType option');
            end
            
            %
            if (isempty(Args.StartRow) || isempty(Args.NRows))
                CellRowPar = {};
            else
                CellRowPar = {Args.StartRow, Args.NRows};
            end

            % Open file and move to absolute HDU number
            Fptr = matlab.io.fits.openFile(TableName);
            matlab.io.fits.movAbsHDU(Fptr, Args.HDUnum);
            
            % Get colunms definition
            Col = FITS.getTableCol(Fptr, Args.TableType);
            
            % Process each column
            Ncol = numel(Col.Cell);
            for Icol=1:1:Ncol
                
                % Read rows of ASCII or binary table column
                [Col.Data{Icol}] = matlab.io.fits.readCol(Fptr, Icol, CellRowPar{:});
                
                % replace -eps with NaN
                if Args.IdentifyNaN
                    Flag = Col.Data{Icol}<-9.1e-36 & Col.Data{Icol}>-9.2e-36;
                     
                    if ~islogical(Col.Data{Icol})
                        Col.Data{Icol}(Flag) = NaN;
                    end
                   
                end
                
                % Optionally convert value to specified type
                if (~isempty(Args.OutClass))
                    Col.Data{Icol} = Args.OutClass(Col.Data{Icol});
                end
                
                % Optionally replace null values with specified value
                if (~isempty(Args.NullVal) && ~isempty(Col.Nulval{Icol}) && isnumeric(Col.Data{Icol}))
                    Col.Data{Icol}(Col.Data{Icol} == Col.Nulval{Icol}) = Args.NullVal;
                end
                
                % Override ColRepeat using the actual data
                Col.Repeat{Icol} = size(Col.Data{Icol}, 2);
            end
            
            % Close file
            matlab.io.fits.closeFile(Fptr);

            % break repeating columns
            if (Args.BreakRepCol)
                Col = FITS.breakRepCol(Col);
            end
            
            if (Args.ModColName)
                % modify column names to legal variable names
                Col.Cell = regexprep(Col.Cell, {'-','/','(',')','&','@','#','^','%','*','~'},'');
                Col.Cell = strcat('T', Col.Cell);
            end
            
            %
            Col.Col = cell2struct(num2cell(1:1:length(Col.Cell)), Col.Cell, 2);

            % Set output
            switch lower(Args.OutTable)
                case 'table'
                   Out = table(Col.Data{:});
                   try
                   Out.Properties.VariableNames = Col.Cell;
                   catch
                       'a'
                   end
                   Out.Properties.VariableUnits = Col.Units;

                case {'astrocatalog', 'astrotable'}
                    Out = AstroCatalog;
                    Out.Catalog     = [Col.Data{:}];
                    Out.ColNames    = Col.Cell;
                    Out.ColUnits    = Col.Units;                                      

                otherwise
                    error('Unknown OutTable option');
            end
        
        end

        % old version
        function [Out, Col] = readTableOld(TableName,varargin)
            % Read binary or ascii FITS tables.
            % Package: @FITS
            % Description: Read binary or ascii FITS tables.
            % Input  : - List of FITS tables to read. Any input valid to Util.files.create_list.m.
            %            If multiple files are provided then all the files hould be of
            %            the same type (e.g., fits binary table).
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
            % Output : - AstCat object containing the FITS table.
            %          - A structure array of additional columns
            %            information, like format.
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jan 2015
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: [Out,Col]=FITS.read_table('asuWDs.fit','ModColName',true);
            % Reliable: 2
            %--------------------------------------------------------------------------

            HeaderField     = HEAD.HeaderField;
            CatField        = AstCat.CatField;
            ColField        = AstCat.ColField;
            ColCellField    = AstCat.ColCellField;
            ColUnitsField   = AstCat.ColUnitsField';


            DefV.TableType      = 'auto';    % {'auto'|'bintable'|'table'}
            DefV.HDUnum         = 2;
            DefV.ModColName     = false;
            DefV.OutTable       = 'astcat';  % {'astcat'|'astcat_t'|...}
            DefV.XTENkey        = 'XTENSION';
            DefV.StartRow       = [];
            DefV.NRows          = [];
            DefV.OutClass       = @double;
            DefV.NullVal        = NaN;       % [] do nothing
            DefV.BreakRepCol    = true;   

            InPar = InArg.populate_keyval(DefV,varargin,mfilename);

            % prep list of fits table names
            [~,ListTableName] = i.files.create_list(TableName,NaN);
            Nfile = numel(ListTableName);
            
            
            Out = AstCat(Nfile,1);
            Col = tools.struct.struct_def({'Col','Cell','Units','TypeChar','Repeat','Scale','Zero','Nulval','Tdisp','Data'},Nfile,1);
            
            import matlab.io.*
            for Ifile=1:1:Nfile
                
                % get header
                %Head     = FITS.get_head(ListTableName{1},InPar.HDUnum);
                Head     = FITS.readHeader1(ListTableName{1},InPar.HDUnum);
                % populate header
                Out(Ifile).(HeaderField) = Head.(HeaderField);
                
                % identify table type
                switch lower(InPar.TableType)
                    case 'auto'
                        HeadCell = Head.(HeaderField);
                        Ixten    = find(strcmp(HeadCell(:,1),InPar.XTENkey));
                        if (isempty(Ixten))
                            error('Automatic table identification mode was unable to access FITS table type');
                        end
                        InPar.TableType = HeadCell{Ixten,2};
                    otherwise
                        % do nothing
                end

                % Table type
                switch lower(InPar.TableType)
                    case 'bintable'
                        Fun_getColParms = @fits.getBColParms;
                    case 'table'
                        Fun_getColParms = @fits.getAColParms;
                    otherwise
                        error('Unknown TableType option');
                end

                if (isempty(InPar.StartRow) || isempty(InPar.NRows))
                    CellRowPar = {};
                else
                    CellRowPar = {InPar.StartRow,InPar.NRows};
                end

                
                % for each fits table
                Fptr = fits.openFile(ListTableName{Ifile});
                fits.movAbsHDU(Fptr,InPar.HDUnum);
                Ncol = fits.getNumCols(Fptr);
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
                    [Col(Ifile).Cell{Icol},Col(Ifile).Units{Icol},Col(Ifile).TypeChar{Icol},...
                                           Col(Ifile).Repeat{Icol},Col(Ifile).Scale{Icol},Col(Ifile).Zero{Icol},...
                                           Col(Ifile).Nulval{Icol},Col(Ifile).Tdisp{Icol}]= Fun_getColParms(Fptr,Icol);
                                       
                                       
                    [Col(Ifile).Data{Icol}] = fits.readCol(Fptr,Icol,CellRowPar{:});
                    if (~isempty(InPar.OutClass))
                        Col(Ifile).Data{Icol} = InPar.OutClass(Col(Ifile).Data{Icol});
                    end
                    if (~isempty(InPar.NullVal) && ~isempty(Col(Ifile).Nulval{Icol}) && isnumeric(Col(Ifile).Data{Icol}))
                        Col(Ifile).Data{Icol}(Col(Ifile).Data{Icol}==Col(Ifile).Nulval{Icol}) = InPar.NullVal;

                    end
                    % override ColRepeat using the actual data
                    Col(Ifile).Repeat{Icol} = size(Col(Ifile).Data{Icol},2);
                end
                fits.closeFile(Fptr);

                % deal with repeating columns
                if (InPar.BreakRepCol)
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
                if (InPar.ModColName)
                    % modify column names to legal variable names
                    Col(Ifile).Cell = regexprep(Col(Ifile).Cell,{'-','/','(',')','&','@','#','^','%','*','~'},'');
                    Col(Ifile).Cell = strcat('T',Col(Ifile).Cell);
                end
                Col(Ifile).Col      = cell2struct(num2cell(1:1:length(Col(Ifile).Cell)),Col(Ifile).Cell,2);

                % output
                switch lower(InPar.OutTable)
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
        
        end 
        
        
        function Col = getTableCol(Fptr, TableType)
            % Table type
            % Note that these function have different return values and order
            % [ttype,tbcol,tunit,tform,scale,zero,nulstr,tdisp] = getAColParms(fptr,colnum)
            % [ttype,tunit,typechar,repeat,scale,zero,nulval,tdisp] = getBColParms(fptr,colnum)
            switch lower(TableType)
                case 'bintable'
                    Fun_getColParms = @matlab.io.fits.getBColParms;
                case 'table'
                    Fun_getColParms = @matlab.io.fits.getAColParms;
                otherwise
                    error('Unknown TableType option');
            end
            
            % Prepare colunms definition
            Col = tools.struct.struct_def({'Col','Cell','Units','TypeChar','Repeat','Scale','Zero','Nulval','Tdisp','Data'});

            % Get the number of columns in the current FITS table
            Ncol = matlab.io.fits.getNumCols(Fptr);

            % Allocate cells for all columns
            Col.Cell     = cell(1, Ncol);   % Column name
            Col.Units    = cell(1, Ncol);
            Col.TypeChar = cell(1, Ncol);
            Col.Repeat   = cell(1, Ncol);
            Col.Scale    = cell(1, Ncol);
            Col.Zero     = cell(1, Ncol);
            Col.Nulval   = cell(1, Ncol);
            Col.Tdisp    = cell(1, Ncol);
            Col.Data     = cell(1, Ncol);   %
            
            % Process each column
            for Icol=1:1:Ncol
                
                % Get column definition
                % (@NoamSegev), fixed bug with order of values returned
                % from the two functions for ascii/binary tables (18/11/2021)
                switch lower(TableType)
                    case 'bintable'
                        % [ttype,tunit,typechar,repeat,scale,zero,nulval,tdisp] = getBColParms(fptr,colnum)                        
                        [Col.Cell{Icol}, Col.Units{Icol}, Col.TypeChar{Icol},...
                            Col.Repeat{Icol}, Col.Scale{Icol}, Col.Zero{Icol},...
                            Col.Nulval{Icol}, Col.Tdisp{Icol}] = Fun_getColParms(Fptr, Icol);
                        
                    case 'table'
                        % [ttype,tbcol,tunit,tform,scale,zero,nulstr,tdisp] = getAColParms(fptr,colnum)
                        [Col.Cell{Icol}, Col.TypeChar{Icol}, Col.Units{Icol},...
                            Col.Repeat{Icol}, Col.Scale{Icol}, Col.Zero{Icol},...
                            Col.Nulval{Icol}, Col.Tdisp{Icol}] = Fun_getColParms(Fptr,Icol);
                        
                    otherwise
                        error('Unknown TableType option');
                end
            end           
            
        end
        
        function Col = breakRepCol(Col)
            % Deal with repeating table columns            
            % Input:  Col - struct array of columns
            % Output: Col - struct array 
            Ncol = numel(Col.Cell);
            Nnc = sum(cell2mat(Col.Repeat));
            NewCol.Cell = cell(Nnc, 1);
            Icol1 = 1;
            for Icol=1:1:Ncol
                IcolN = Icol1 + Col.Repeat{Icol} - 1;
                %Icol1 = Icol1 + Col.Repeat{Icol}; % at the end of the loop
                for Irep=1:1:Col.Repeat{Icol}
                    if (Col.Repeat{Icol}>1)
                        NewCol.Cell{Icol1+Irep-1} = sprintf('%s_%d_',Col.Cell{Icol},Irep);
                    else
                        NewCol.Cell{Icol1+Irep-1} = Col.Cell{Icol};
                    end
                end
                [NewCol.Units{Icol1:IcolN}]     = deal(Col.Units{Icol});
                [NewCol.TypcChar{Icol1:IcolN}]  = deal(Col.TypeChar{Icol});
                [NewCol.Repeat{Icol1:IcolN}]    = deal(1);
                [NewCol.Scale{Icol1:IcolN}]     = deal(Col.Scale{Icol});
                [NewCol.Zero{Icol1:IcolN}]      = deal(Col.Zero{Icol});
                [NewCol.Tdisp{Icol1:IcolN}]     = deal(Col.Tdisp{Icol});
                Icol1 = Icol1 + Col.Repeat{Icol}; % at the end of the loop
            end
            
            % Replace with new values
            Col.Cell     = NewCol.Cell;
            Col.Units    = NewCol.Units;
            Col.TypeChar = NewCol.TypcChar;
            Col.Repeat   = NewCol.Repeat;
            Col.Scale    = NewCol.Scale;
            Col.Zero     = NewCol.Zero;
            Col.Tdisp    = NewCol.Tdisp;           
        end
        
        %------------------------------------------------------------------        
        
        Result = writeTable1(Table, FileName, Args)
            % Currently implemented in writeTable1.m
            
        %------------------------------------------------------------------
        function [KeysVal,KeysComment,Struct] = get_keys(Image,Keys,HDUnum,Str)
            % Get keywords value from a single FITS header
            % Package: @FITS (Static function)
            % Description: Get the values of specific keywords from a single
            %              FITS file header. Use only for existing keywords.
            % Input  : - FITS image name.
            %          - Cell array of keys to retrieve from the image header.
            %          - HDU number. Default is 1.
            %            If NaN, then set to 1.
            %          - Check if the keyword value is char and try to convert to
            %            a number {false|true}. Default is false.
            % Output : - Cell array of keyword values.
            %          - Cell array of keyword comments.
            %          - Structure containing the keyword names (as fields)
            %            and their values.
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jul 2014
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: [KeysVal,KeysComment,Struct]=FITS.get_keys('A.fits',{'NAXIS1','NAXIS2'});
            % Reliable: 2
            %--------------------------------------------------------------------------

            arguments
                Image char
                Keys
                HDUnum        = 1;
                Str logical   = false;
            end
            
            if (isnan(HDUnum))
                HDUnum = 1;
            end

            Fptr = matlab.io.fits.openFile(Image);
            N = matlab.io.fits.getNumHDUs(Fptr);
            if HDUnum > N
                matlab.io.fits.closeFile(Fptr);
                error('requested HDUnum does not exist');
            end
            matlab.io.fits.movAbsHDU(Fptr,HDUnum);

            if (ischar(Keys))
                Keys = {Keys};
            end

            Nkey = numel(Keys);


            KeysVal     = cell(size(Keys));
            KeysComment = cell(size(Keys));
            for Ikey=1:1:Nkey
                [KeysVal{Ikey},KeysComment{Ikey}] = matlab.io.fits.readKey(Fptr,Keys{Ikey});
                if (ischar(KeysVal{Ikey}) && Str)
                    %Tmp = str2double(KeysVal{Ikey});
                    Tmp = real(str2doubleq(KeysVal{Ikey}));
                    if (isnan(Tmp))
                        % do nothing - keep as a string
                    else
                        KeysVal{Ikey} = Tmp;
                    end
                end

            end
            matlab.io.fits.closeFile(Fptr);

            if nargout > 2
               Struct = cell2struct(KeysVal,Keys,2);
            end
        end
        
        function [KeysVal,KeysComment,Struct,List] = mget_keys(Images,Keys,HDUnum,Str)
            % Get header keywords value from multiple FITS
            % Package: @FITS (Static)
            % Description: Get the values of specific keywords from a list of
            %              FITS files header.
            % Input  : - List of FITS image names. See io.files.filelist.m for options.
            %          - Cell array of keys to retrieve from the image header.
            %          - HDU number. Default is 1.
            %          - Check if the keyword value is char and try to convert to
            %            a number {false|true}. Default is false.
            % Output : - Cell array (per image) of cell array of keyword values.
            %          - Cell array (per image) of cell array of keyword comments.
            %          - Structure array (element per image) containing the keyword
            %            names (as fields) and their values.
            %          - Cell array containing the list of images.
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jul 2014
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example:
            % [KeysVal,KeysComment,Struct,List]=FITS.mget_keys('PTF_201202*.fits',{'NAXIS1','NAXIS2'});
            % Reliable: 2
            %--------------------------------------------------------------------------

            arguments
                Images char
                Keys
                HDUnum        = 1;
                Str logical   = false;
            end
            
            if (isnan(HDUnum))
                HDUnum = 1;
            end
            
            List = io.files.filelist(Images);
            Nim = numel(List);
            KeysVal     = cell(size(List));
            KeysComment = cell(size(List));
            for Iim=1:1:Nim
               [KeysVal{Iim},KeysComment{Iim},Struct(Iim)] = FITS.get_keys(List{Iim},Keys,HDUnum,Str);
            end

        end
        
        function delete_keys(ImageName,Keywords)
            % Delete a lits of keywords from a list of FITS headers
            % Package: @FITS (Static)
            % Description: Delete a list of header keywords from a list of
            %              FITS images.
            % Input  : - List of FITS image names to read. See io.files.filelist.m for
            %            options.
            %          - Cell array of keyword names to delete.
            % Output : null
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jun 2014
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: FITS.delete_keys('A.fits',{'PTFPID','OBJECT'})
            % Reliable: 2
            %--------------------------------------------------------------------------
            
            if (~iscell(Keywords))
                Keywords = {Keywords};
            end
            Nkey = numel(Keywords);

            [~,List] = io.files.create_list(ImageName,NaN);
            Nim = numel(List);

            for Iim=1:1:Nim
                Fptr = matlab.io.fits.openFile(List{Iim},'readwrite');
                for Ikey=1:1:Nkey
                    matlab.io.fits.deleteKey(Fptr,Keywords{Ikey});
                end
                matlab.io.fits.closeFile(Fptr);
            end
        end
        
        function write_keys(ImageName,KeyCell)
            % Insert or update FITS header keywords
            % Package: @FITS (Static)
            % Description: Insert new, or update existing FITS header keywords in
            %              a list of FITS images.
            % Input  : - List of FITS image names to edit. See io.files.filelist.m for
            %            options.
            %          - A cell array of two or three columns of key/cal/comments to
            %            add to FITS header.
            % Output : null
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jun 2014
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: FITS.write_keys('A.fits',{'try','A','comm';'try2',6,'what'});
            % Reliable: 2
            %--------------------------------------------------------------------------

            Nkey = size(KeyCell,1);

            List = io.files.filelist(ImageName);
            
            Nim = numel(List);

            for Iim=1:1:Nim
                Fptr = matlab.io.fits.openFile(List{Iim},'readwrite');
                for Ikey=1:1:Nkey
                    %KeyCell{Ikey,:}
                    if isempty(KeyCell{Ikey,3})
                        KeyCell{Ikey,3} = ' ';
                    end
                    if (strcmp(KeyCell{Ikey,1},'SIMPLE'))
                        KeyCell{Ikey,2} = true;
                    end
                    if (~isempty(KeyCell{Ikey,1}))
                        matlab.io.fits.writeKey(Fptr,KeyCell{Ikey,:});
                    end
                end
                matlab.io.fits.closeFile(Fptr);
            end
        end
        
        function Result = write(Image, FileName, Args)
            % Write or append an image into FITS file.
            % Static function
            %     The image may have N-dimensions.
            %     Append will write multi extension FITS image.
            % Input  : - Array to save as FITS image.
            %          - FITS file name to save.
            %          * Arbitrary number of ...,key,val,... pairs.
            %            Following keywords are available:
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
            %            'SanifyPath' - whether to sanify the file name
            % Example: Result = FITS.write(rand(100,100),'Try.fits');
            %          Result = FITS.write(rand(10,10,3),'Try.fits');
            %
            
            arguments
                Image
                FileName
                Args.Header cell              = {};
                Args.DataType                 = 'single';
                Args.Append(1,1) logical      = false;
                Args.OverWrite(1,1) logical  = false;
                Args.WriteTime(1,1) logical   = false;
                Args.SanifyPath logical       = true;
            end
            
            HeaderField = HEAD.HeaderField;
            
            % Set FITS DataType
            DataType = FITS.getDataType(Args.DataType);
            
            % Overwrite existing FITS file
            if (Args.OverWrite)
                % delete existing FileName if exist
                if isfile(FileName)
                    delete(FileName);
                end
            end
            
            % sanify the file name so that it contain the absolute path
            if Args.SanifyPath
                if strcmp(FileName(1),'!') % need this for the case when overwrite is requested
                    FileName = strcat('!',tools.os.relPath2absPath(FileName(2:end)));
                else
                    FileName = tools.os.relPath2absPath(FileName);
                end
            end
            
            % Prepare header
            Header = FITS.prepareHeader(Args.Header, HeaderField, 'WriteTime', Args.WriteTime);

            if (Args.Append)
                % append to existing FITS file
                Fptr = matlab.io.fits.openFile(FileName, 'READWRITE');
            else
                % Create new FITS file
                Fptr = matlab.io.fits.createFile(FileName);
            end
            
            % create Image
            matlab.io.fits.createImg(Fptr, DataType, size(Image));
            
            % write Image
            matlab.io.fits.writeImg(Fptr, Image); %,Fpixels);
                        
            % write Header
            FITS.writeHeader(Fptr, Header, HeaderField);           
            
            % Close FITS file
            matlab.io.fits.closeFile(Fptr);
            Result = (sign(Fptr) == 1);

        end % write()
        
        function writeSimpleFITS(Image, FileName, Args)
            % Write a simple (single HDU) FITS file to disk
            % using io.fits.writeSimpleFITS, which streamlines correctly
            %   the CFITSIO routines, and is thus several times faster
            %   than FITS.write. That routine also takes care of setting
            %   BZERO properly for writing unsigned images with signed
            %   types.
            % Input  : - An image data (e.g., matrix).
            %          - File name in which to write the FITS file.
            %          * ...,key,val,...
            %            'Header' - A 2 or 3 columns cell array containing the
            %                   header.
            %            'DataType' - Image data type. If empty, use image type.
            %                   Default is [].
            %            'CompressType' which CFITS compression to use (see
            %                   'help matlab.io.fits.setCompressionType' and
            %                   https://heasarc.gsfc.nasa.gov/docs/software/fitsio/compression.html).
            %                   Default is 'NOCOMPRESS'. Among the valid types:
            %                  'GZIP', 'GZIP1', 'GZIP_1', 'GZIP2', 'GZIP_2',
            %                  'RICE', 'RICE1', 'RICE_1',
            %                  'PLIO', 'PLIO1', 'PLIO_1',
            %                  'HCOMPRESS', 'HCOMPRESS1', 'HCOMPRESS_1'
            %                  (the latter are lossy, and can use a scale or
            %                   smooth parameter, for which  we don't bother)
            %                 All algorithms are said to be lossless for
            %                 integer images, but PLIO works only for
            %                 positive integer values.
            %             'SanifyPath' - whether to sanify the file path (may appear slow)
            %
            % Output : null
            % Author : Eran Ofek (Jan 2022)
            % Example: FITS.writeSimpleFITS(AI.Image, 'try.fits','Header',AI.HeaderData.Data);

            arguments
                Image
                FileName
                Args.Header cell              = {};
                Args.DataType                 = [];
                Args.CompressType  char       = 'NOCOMPRESS';
                Args.SanifyPath logical       = true;
            end
            
            io.fits.writeSimpleFITS(Image, FileName, 'Header',Args.Header,...
                                     'DataType',Args.DataType,'UseMatlabIo',true,...
                                     'CompressType',Args.CompressType,'SanifyPath',Args.SanifyPath);
            
        end
        
        function DataType = getDataType(ArgDataType)
            % Get FITS DataType 
            % Input  : - ArgDataType: 'int8', 8, 'uint16', 'int16', 16,
            %            'uint32', 'int32', 32, 'int64', 64, 
            %            'single', 'float32', -32, 'double', 'float64', -64
            % Output : - 'uint8', 'int16', 'int32', 'int64', 'single', 'double'
            % Example: DataType = FITS.getDataType(16);
            switch ArgDataType
                 case {'int8',8}
                    DataType = 'uint8';
                 case {'uint16','int16',16}
                     % apparently uint16 is not supported?! in 2017b
                    DataType = 'int16';
                 case {'uint32','int32',32}
                     % apparently uint16 is not supported?! in 2017b
                    DataType = 'int32';
                 case {'int64',64}
                    DataType = 'int64';
                 case {'single','float32',-32}
                    DataType = 'single';
                 case {'double','float64',-64}
                    DataType = 'double';
                 otherwise
                    error('Unknown DataType option');
            end
        end
        
        function Header = prepareHeader(ArgsHeader, HeaderField, Args)
            % Prepare header before write
            % Input  : - ArgsHeader  : HEAD or AstroHeader object
            %            HeaderField : Name of header field, see HEAD.HeaderField
            %            'WriteTime' : If true, set current time
            % Output : - HEAD object
            % Example: H = FITS.prepareHeader(Header, HeaderField, 'WriteTime', WriteTime)
            arguments
                ArgsHeader
                HeaderField
                Args.WriteTime = false;
            end
            
            % Prepare header
            if (HEAD.ishead(ArgsHeader))
                % already HEAD object
                Header = ArgsHeader;
            else
                % convert to HEAD object
                Header = HEAD;
                
                % Convert from AstroHeader
                if isa(ArgsHeader, 'AstroHeader')
                    Header.(HeaderField) = ArgsHeader.Data;
                else
                    Header.(HeaderField) = ArgsHeader;
                end
            end
           
            % Set the FITS "mandatory" keywords, add BSCALE and BZERO
            % Check if BZERO and BSCALE are already in HeaderInfo
            Header = replace_key(Header,'BZERO',   single(0),  'offset data range to that of unsigned short',...
                                        'BSCALE',  single(1),  'default scaling factor');
                               
            % Write creation date to header
            if (Args.WriteTime)
                Time = celestial.time.get_atime([],0,0); % Na'ama, 20180516
                Header = replace_key(Header,'CRDATE',  Time.ISO,'Creation date of FITS file',...
                                            'COMMENT', 'File created by MATLAB FITS.write.m written by E. Ofek', '');                                        
            end
        end
        
        function Result = writeHeader(Fptr, Header, HeaderField)            
            % write Header            
            % Input  : - Fptr - File pointer returned by matlab.io.fits.createFile or
            %                   matlab.io.fits.openFile()
            %            Header - HEAD object
            %            HeaderField - See HEAD.HeaderField
            % Output : true on sucess
            % Example: FITS.writeHeader(Fptr, Header, HeaderField)
            % @Todo: Check performance with big header (keys > 300)
            
            %t = tic;
            Nline = size(Header.(HeaderField),1);            
            for Inl=1:1:Nline
                if (~isempty(Header.(HeaderField){Inl,1}))
                    switch lower(Header.(HeaderField){Inl,1})
                        case 'comment'
                            matlab.io.fits.writeComment(Fptr,Header.(HeaderField){Inl,2});
                        case 'history'
                            matlab.io.fits.writeHistory(Fptr,Header.(HeaderField){Inl,2});
                        case {'extname', 'xtension'} % Na'ama, 20180905
                            % do nothing
                        case {'simple','bitpix','naxis','naxis1','naxis2','naxis3','naxis4'}
                            % do nothing - these keywords are written by
                            % the FITS creator
                         case {'bscale', 'bzero', 'datamax', 'datamin', 'epoch', 'equinox'} % Na'ama, 2018-06-06
                            % convert to floating point, required by matlab.io.fits.writeKey
                            if (ischar(Header.(HeaderField){Inl,2}))
                                Header.(HeaderField){Inl,2} = double(eval(Header.(HeaderField){Inl,2}));
                            end
                            if isempty(Header.(HeaderField){Inl,3})
                                Header.(HeaderField){Inl,3} = ' ';
                            end
                            matlab.io.fits.writeKey(Fptr,Header.(HeaderField){Inl,1},...
                                               Header.(HeaderField){Inl,2},...
                                               Header.(HeaderField){Inl,3});
                        case 'end'
                            % do nothing
                        otherwise
                            if any(isnan(Header.(HeaderField){Inl,2}))
                                Header.(HeaderField){Inl,2} = ' ';
                            end
                            if (isempty(Header.(HeaderField){Inl,3}))
                                Header.(HeaderField){Inl,3} = ' ';
                            end
%                             Header.(HeaderField){Inl,:}
%                             string(Header.(HeaderField){Inl,3})
%                             regexprep(Header.(HeaderField){Inl,3},'\W&\S','')
                            % dela with non-standard keywords
                            if isempty(Header.(HeaderField){Inl,2})
                                Header.(HeaderField){Inl,2} = ' ';
                            end
                            %if any(strcmp(HeaderField){Inl,2},{'uint16','uint32','int16','int32'}))

                            matlab.io.fits.writeKey(Fptr,Header.(HeaderField){Inl,1},...
                                               Header.(HeaderField){Inl,2},...
                                               Header.(HeaderField){Inl,3});
                    end
                end
            end            
            %time = toc - t;
            %fprintf('writeHeader time: %0.6f', time);
            Result = true;                        
        end
        
        % read to SIM
        function Sim = read2sim(Images,Args)
            % Description: Read FITS images into SIM object.
            %              Can read N-dimensional images.
            %              Can also read multi extension files.
            % Input  : - List of images to read. See io.files.create_list.m for
            %            details.
            %          * Arbitrary number of pairs of ...,key,val,...
            %            input arguments. Available keywords are:
            %            'HDUnum' - HDU to read. If multiple numbers are
            %                       given then will attempt to read multiple
            %                       HDU each one into different SIM
            %                       element.
            %                       If empty, then will attemp to read all
            %                       extensions.
            %                       Default is 1.
            %            'CCDSEC' - A four column matrix of CCDSEC
            %                       [xmin xmax ymin ymax] to read.
            %                       Either line per image or a single line.
            %            'Sim'    - An existing SIM into to write the FITS
            %                       images. If empty, then create a new
            %                       SIM object. Default is empty.
            %            'ExecField'- SIM field into which to write the
            %                       FITS images. Default is 'Im'.
            %            'ReadHead'- Read header into SIM. Default is true.
            %            'HDUnum' - Index of HDU. Default is 1.
            %            'PopWCS' - Populate WCS. Default is true.
            % Output: - A SIM object with the FITS images.
            % Example: S=FITS.read2sim('Image*.fits');
            %          S=FITS.read2sim('Image6[15-28].fits');
            %          S=FITS.read2sim('@list');
            %          S=FITS.read2sim('Image,fits','CCDSEC',[1 10 1 100]);
            % Reliable: 2
            
            arguments
                Images
                Args.HDUnum               = 1;
                Args.CCDSEC               = [];  % section to read
                Args.Sim                  = [];  % read into existing SIM
                Args.ExecField            = SIM.ImageField;   % read into field
                Args.ReadHead             = true;
                Args.PopWCS(1,1) logical  = true;
            end
            
            HeaderField = HEAD.HeaderField;
            FileField   = SIM.FileNameField;
            WCSField    = 'WCS';
            
            
            [~,ListIm] = io.files.create_list(Images,NaN);
            Nim = numel(ListIm);
            
            if (isempty(Args.Sim))
                % allocate SIM
                Sim = SIM(Nim,1);
            else
                % Use existing SIM
                Sim = Args.Sim;
            end
            
            % number of lines in CCDSEC
            Nccdsec = size(Args.CCDSEC,1);
            
            % for each image
            Isim = 0; % image (including extensions) index
            for Iim=1:1:Nim
                % get number of HDU in FITS image
                if (isempty(Args.HDUnum))
                    Nhdu   = FITS.num_hdu(ListIm{Iim});
                    HDUnum = (1:1:Nhdu);
                else
                    HDUnum = Args.HDUnum;
                    Nhdu   = numel(HDUnum);
                end
                % for each HDU
                for Ihdu=1:1:Nhdu
                    Isim = Isim + 1;
                    % Read image to SIM
                    if (isempty(Args.CCDSEC))
                        Sim(Isim).(Args.ExecField) = FITS.read1(ListIm{Iim},HDUnum(Ihdu));
                    else
                        Sim(Isim).(Args.ExecField) = FITS.read1(ListIm{Iim},HDUnum(Ihdu),'CCDSEC',Args.CCDSEC(min(Iim,Nccdsec),:));
                    end
                    Sim(Isim).(FileField) = ListIm{Iim};

                    % read header
                    if (Args.ReadHead)
                        %H = FITS.get_head(ListIm{Iim},HDUnum(Ihdu),Args.PopWCS);
                        H = FITS.readHeader1(ListIm{Iim},HDUnum(Ihdu),Args.PopWCS);
                        Sim(Isim).(HeaderField) = H.(HeaderField);
                        Sim(Isim).(WCSField)    = H.(WCSField);
                    end
                end
            end
            
        end
        
    end

    methods (Static)
        function fpack(List, Command, Args)
            % Compress FITS files using fpack
            % Input  : - A file name, a file name containing wild
            %            cards or regular expression, a cell array of
            %            file names, or a structure arrawy which is the
            %            output of the dir command.
            %          - fpack command string.
            %            Default is 'fpack -r'.
            %          * ...,key,val,...
            %            'RegExp' - A logical indicating if to use regular expression (true) or
            %                   wild cards (false). Default is false.
            %            'ListList' - If this argument is true, and the file name in the first
            %                   argument starts with '@', then will read the file names from
            %                   this files (lines start with % and # ignored).
            %                   Default is true.
            % Output : null.
            % Author : Eran Ofek (Dec 2023)

            arguments
                List
                Command                       = 'fpack -r';
                Args.RegExp logical        = false;
                Args.ListList logical      = true;
            end

            List = io.files.filelist(List, Args.RegExp, Args.ListList);

            Nlist = numel(List);

            % check if external code installed
            Res = system(Command);
            if Res~=255
                error('fpack not installed - in linux use: apt-get install libcfitsio-bin')
            end

            for Ilist=1:1:Nlist
                system(sprintf('%s %s',Command, List{Ilist}));
            end

        end

        function funpack(List, Command, Args)
            % Uncompress FITS files using fpack
            % Input  : - A file name, a file name containing wild
            %            cards or regular expression, a cell array of
            %            file names, or a structure arrawy which is the
            %            output of the dir command.
            %          - fpack command string.
            %            Default is 'funpack'.
            %          * ...,key,val,...
            %            'RegExp' - A logical indicating if to use regular expression (true) or
            %                   wild cards (false). Default is false.
            %            'ListList' - If this argument is true, and the file name in the first
            %                   argument starts with '@', then will read the file names from
            %                   this files (lines start with % and # ignored).
            %                   Default is true.
            % Output : null.
            % Author : Eran Ofek (Dec 2023)

            arguments
                List
                Command                       = 'funpack';
                Args.RegExp logical        = false;
                Args.ListList logical      = true;
            end

            List = io.files.filelist(List, Args.RegExp, Args.ListList);

            Nlist = numel(List);

            % check if external code installed
            Res = system(Command);
            if Res~=255
                error('fpack not installed - in linux use: apt-get install libcfitsio-bin')
            end

            for Ilist=1:1:Nlist
                system(sprintf('%s %s',Command, List{Ilist}));
            end

        end

    end
    
    methods (Static)   % write_old
        % write_old (previously fitswrite_my)
        function [Flag,HeaderInfo]=write_old(Image,FileName,HeaderInfo,DataType,varargin)
            % Write a simple 2D FITS image. OBSOLETE: Use FITS.write instead.
            % Package: @FITS
            % Description: Write a simple 2D FITS image.
            %              OBSOLETE: Use FITS.write instead.
            % Input  : - A 2D matrix to write as FITS file.
            %          - String containing the image file name to write.
            %          - Cell array containing header information to write to image.
            %            The keywords: SIMPLE, BITPIX, NAXIS, NAXIS1, NAXIS2, EXTEND
            %            will be re-written to header by default.
            %            The keywords BSCALE and BZERO wil be written to header if
            %            not specified in the header information cell array.
            %            Alternatively this could be a character array (Nx80)
            %            containing the header (no changes will be applied).
            %            If not given, or if empty matrix (i.e., []) than write a
            %            minimal header.
            %          - DataType in which to write the image, supported options are:
            %            'int8',8            
            %            'int16',16
            %            'int32',32
            %            'int64',64
            %            'single','float32',-32    (default)
            %            'double','float64',-64            
            %          * Arbitrary number of pairs of input arguments: 
            %            ...,keyword,value,... - possible keys are:
            %            'IdentifyInt'  - attempt to identify integers in header
            %                             automaticaly and print them as integer.
            %                             {'y' | 'n'}, default is 'y'.
            %            'ResetDefKey'  - Reset default keys {'y' | 'n'},
            %                             default is 'y'.
            %                             Default keys are:
            %                             {'SIMPLE','BITPIX','NAXIS','NAXIS1','NAXIS2',
            %                              'EXTEND','BSCALE',BZERO'}.
            %            'OverWrite'    - Over write existing image {'y' | 'n'},
            %                             default is 'y'.
            % Output : - Flag indicating if image was written to disk (1) or not (0).
            %          - Actual header information written to file.
            % See also: fitswrite_my.m
            % Bugs   : Don't write standard FITS file
            % Tested : Matlab 7.10
            %     By : Eran O. Ofek                      June 2010
            %    URL : http://wise-obs.tau.ac.il/~eran/matlab.html
            % Examples: [Flag,HeaderInfo]=FITS.write_old(rand(2048,1024),'Example.fits');
            % Reliable: 2
            %--------------------------------------------------------------------------


            Def.HeaderInfo = [];
            Def.DataType   = -32;
            if (nargin==2)
               HeaderInfo = Def.HeaderInfo;
               DataType   = Def.DataType;
            elseif (nargin==3)
               DataType   = Def.DataType;
            end

            % set default for additional keywords:
            DefV.IdentifyInt = 'y';
            DefV.ResetDefKey = 'y';
            DefV.OverWrite   = 'y';

            InPar = InArg.populate_keyval(DefV,varargin,mfilename);


            Flag = 1;

            switch DataType
             case {'int8',8}
                DataType = 8;
             case {'int16',16}
                DataType = 16;
             case {'int32',32}
                DataType = 32;
             case {'int64',64}
                DataType = 64;
             case {'single','float32',-32}
                DataType = -32;
             case {'double','float64',-64}
                DataType = -64;
             otherwise
                error('Unknown DataType option');
            end



            if (ischar(HeaderInfo)==0)

            %--- Set the FITS "mandatory" keywords ---
            ImSize = size(Image);
            switch lower(InPar.ResetDefKey)
               case 'n'
                  % do nothing
               case 'y'
                  if (isempty(HeaderInfo))
                     % do nothing
                  else
                     % delete "default" keys:
                     HeaderInfo = FITS.cellhead_delkey(HeaderInfo,{'SIMPLE','BITPIX','NAXIS','NAXIS1','NAXIS2','EXTEND','BSCALE','BZERO'});
                  end
                  HeaderInfo = FITS.cellhead_addkey(HeaderInfo,...
                                                    0,'SIMPLE',true(1,1),'file does conform to FITS standard',...
                                                    0,'BITPIX',int32(DataType),'number of bits per data pixel',...
                                                    0,'NAXIS' ,int32(length(ImSize)),'number of data axes',...
                                                    0,'NAXIS1',int32(ImSize(2)),'length of data axis 1',...
                                                    0,'NAXIS2',int32(ImSize(1)),'length of data axis 2',...
                                                    0,'EXTEND',false(1,1),'FITS dataset may contain extensions',...
                                                    0,'BZERO' ,single(0),'offset data range to that of unsigned short',...
                                                    0,'BSCALE',single(1),'default scaling factor');
               otherwise
                  error('Unknown ResetDefKey option');
            end

            % check if last keyword is END - delete if needed.
            if (strcmp(HeaderInfo{end,1},'END'))
                HeaderInfo = HeaderInfo(1:end-1,:);
            end


            %--- Write creation date to header ---
            Time = celestial.time.get_atime([],0,0);
            HeaderInfo = FITS.cellhead_addkey(HeaderInfo,...
                                              'CRDATE',Time.ISO,'Creation date of FITS file',...
                                              'COMMENT','','File Created by MATLAB fitswrite.m written by Eran Ofek');

            %--- Convert default keywords to int32 ---
            KeysInt32 = {'BITPIX','NAXIS','NAXIS1','NAXIS2','BZERO','BSCALE'};
            Ni32     = length(KeysInt32);
            for Ii32=1:1:Ni32
               I = find(strcmp(HeaderInfo(:,1),KeysInt32{Ii32})==1);
               if (~isempty(I))
                  HeaderInfo{I,2} = int32(HeaderInfo{I,2});
               end
            end


            %--- Convert default keywords to logical ---
            KeysLogi = {'SIMPLE','EXTEND'};
            Nlogi     = length(KeysLogi);
            for Ilogi=1:1:Nlogi
               I = find(strcmp(HeaderInfo(:,1),KeysLogi{Ilogi})==1);
               if (~isempty(I))
                  if (~islogical(HeaderInfo{I,2}))
                     switch HeaderInfo{I,2}
                      case 'F'
                         HeaderInfo{I,2} = false(1,1); 
                      case 'T'
                         HeaderInfo{I,2} = true(1,1);
                      otherwise
                         error('Keyword type is not logical');
                     end
                  end
               end
            end


            % check if last keyword is END - if not add.
            if (~strcmp(HeaderInfo{end,1},'END'))
                HeaderInfo{end+1,1} = 'END';
                HeaderInfo{end,2} = '';
                HeaderInfo{end,3} = '';
            end

            %--- Prepare string of header information to write to header ---
            HeaderBlock = '';
            [Nline,Nr] = size(HeaderInfo);

            Counter = 0;
            for Iline=1:1:Nline
               if (~isempty(HeaderInfo{Iline,1}) && strcmpi(HeaderInfo{Iline,1},'END')==0)
                  %--- write keyword name ---
                  HeaderLine = sprintf('%-8s',upper(HeaderInfo{Iline,1}));
                  switch upper(HeaderInfo{Iline,1})
                   case {'COMMENT','HISTORY'}
                      % do not write "=" sign
                      HeaderLine = sprintf('%s',HeaderLine);
                   otherwise
                      % write "=" sign
                      HeaderLine = sprintf('%s= ',HeaderLine);
                  end
                  %--- write keyword value ---
                  switch upper(HeaderInfo{Iline,1})
                   case {'COMMENT','HISTORY'}
                      % do not write value
                   otherwise
                      if (ischar(HeaderInfo{Iline,2}))
                         HeaderLine = sprintf('%s''%s''',HeaderLine,HeaderInfo{Iline,2});
                         Nblanks    = 20-(length(HeaderInfo{Iline,2})+2);

                         if (Nblanks<0)
                            Nblanks = 0;
                         end
                         HeaderLine = sprintf('%s%s',HeaderLine,blanks(Nblanks));

                      elseif (islogical(HeaderInfo{Iline,2}))
                          switch HeaderInfo{Iline,2}
                          case 1
                         HeaderLine = sprintf('%s%20s',HeaderLine,'T');
                          case 0
                         HeaderLine = sprintf('%s%20s',HeaderLine,'F');                 
                          otherwise
                         error('DataType is not logical');
                         end
                      elseif (isinteger(HeaderInfo{Iline,2}))
                         HeaderLine = sprintf('%s%20i',HeaderLine,HeaderInfo{Iline,2});
                      elseif (isfloat(HeaderInfo{Iline,2}))

                         switch InPar.IdentifyInt
                          case 'y'
                             % attempt to identify integers automatically
                             if (fix(HeaderInfo{Iline,2})==HeaderInfo{Iline,2})
                                % integer identified - print as integer
                                HeaderLine = sprintf('%s%20i',HeaderLine,HeaderInfo{Iline,2});
                             else
                                % float or double
                                HeaderLine = sprintf('%s%20.8f',HeaderLine,HeaderInfo{Iline,2});
                             end
                          case 'n'
                               % float or double
                               HeaderLine = sprintf('%s%20.8f',HeaderLine,HeaderInfo{Iline,2});
                          otherwise
                                 error('Unknown IdentifyInt option');
                         end
                      else
                         error('unknown Format in header information');
                      end
                  end
                  %--- write comment to header ---
                  if (Nr>2)
                     switch upper(HeaderInfo{Iline,1})
                      case {'COMMENT','HISTORY'}
                         % do not write "/"
                         HeaderLine = sprintf('%s%-s',HeaderLine,HeaderInfo{Iline,3});
                      otherwise
                         if (isempty(HeaderInfo{Iline,3}))
                            % do nothing - do not add comment
                         else
                            HeaderLine = sprintf('%s /%-s',HeaderLine,HeaderInfo{Iline,3});
                         end
                      end
                   end
                   % cut line if longer than 80 characters or paded with spaces
                   if (length(HeaderLine)>80)
                      HeaderLine = HeaderLine(1:80);
                   end
                   HeaderLine = sprintf('%-80s',HeaderLine);

                   %%HeaderBlock = sprintf('%s%s',HeaderBlock,HeaderLine);
                   Counter = Counter + 1;
                   HeaderBlock(Counter,:) = sprintf('%s',HeaderLine);
                end   
            end
            %--- Add End keyword to end of header ---
            %%HeaderBlock = sprintf('%s%-80s',HeaderBlock,'END');
            Counter = Counter + 1;
            HeaderBlock(Counter,:) = sprintf('%-80s','END');

            else
              %--- HeaderInfo is already provided as char array ---
              % assume contains also the 'END' keyword
              HeaderBlock = HeaderInfo;
            end


            %--- pad by spaces up to the next 2880 character block ---
            PadBlock = sprintf('%s',blanks(2880 - mod(numel(HeaderBlock),2880)));


            %--- pad by spaces up to the next 2880 character block ---
            %HeaderBlock = sprintf('%s%s',HeaderBlock,blanks(2880 - 80 -
            %mod(length(HeaderBlock),2880)));
            %--- Add End keyword to end of header ---
            %HeaderBlock = sprintf('%s%-80s',HeaderBlock,'END');


            %--- Open file for writing in rigth byte order in all platforms.
            switch lower(InPar.OverWrite)
               case 'n'
                  if ~java.io.File(FileName).exists
                  %if (exist(FileName,'file')==0)
                     % File does not exist - continue
                  else
                     error('Output file already exist');
                  end
               otherwise
                  % do nothing
            end

            Fid = fopen(FileName,'w','b');  % machineformat: ieee-be
            if (Fid==-1)
               fprintf('Error while attempting to open FITS file for writing\n');
               Flag = 0;
            else   
               %--- Write header to file ---

               %fwrite(Fid,HeaderBlock,'char');

               for Ic=1:1:size(HeaderBlock,1)
                 fprintf(Fid,'%-80s',HeaderBlock(Ic,:));
               end
               fprintf(Fid,'%s',PadBlock);

               %--- Write image data ---
               switch DataType
                case {'int8',8}
                   fwrite(Fid,Image.','uchar');
                case {'int16',16}
                   fwrite(Fid,Image.','int16');
                case {'int32',32}
                   fwrite(Fid,Image.','int32');
                case {'int64',64}
                   fwrite(Fid,Image.','int64');
                case {'single','float32',-32}
                   fwrite(Fid,Image.','single');
                case {'double','float64',-64}
                   fwrite(Fid,Image.','double');
                otherwise
                   fclose(Fid);
                   error('Unknown DataType option');
               end
               fclose(Fid);
            end
        end % FITS.write_old

        
        function [NewCellHead]=cellhead_addkey(CellHead,varargin)
            %--------------------------------------------------------------------------
            % FITS.cellhead_addkey function                                class/@FITS
            % Description: A utility program to add new keywords, values and
            %              comments to a cell array containing A FITS header
            %              information.
            %              The FITS header cell array contains an arbitrary number
            %              of rows and 3 columns, where the columns are:
            %              {keyword_name, keyword_val, comment}.
            %              The comment column is optional.
            % Input  : - Cell array containing the FITS header information.
            %          * An header cell array to add at the end of the existing
            %            array (e.g., FITS.cellhead_addkey(CellHead,AddHead);).
            %            Alternatively, vecor of positions in the existing header
            %            array in which to add the new keys
            %            (e.g., FITS.cellhead_addkey(CellHead,NewPos,AddHead);).
            %            Alternatively, triplets of :...,key,value,comment,...
            %            to add at the end of the existing header
            %            (e.g., FITS.cellhead_addkey(CellHead,'NAXIS',1024,'number of axes');)
            %            Alternatively, quadraplets of :...,pos,key,value,comment,...
            %            to add at a given position in the existing header specified by
            %            the integer pos.
            %            (e.g., FITS.cellhead_addkey(CellHead,Pos,'NAXIS',1024,'number of axes');)
            % Output : - The new header cell array.
            % Tested : Matlab 7.10
            %     By : Eran O. Ofek                    Jun 2010
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Reliable: 2
            %--------------------------------------------------------------------------
            if (isempty(CellHead))
               CellHead = cell(0,3);
            end

            Narg = length(varargin);
            if (Narg==0)
               % Do nothing
               AddHead = [];
            elseif (Narg==1)
               AddHead = varargin{1};
               VecPos  = zeros(size(AddHead,1),1)+Inf;
            elseif (Narg==2)
               VecPos     = varargin{1};
               AddHead = varargin{2};
            else
               if (Narg./3==floor(Narg./3) && ischar(varargin{1})==1)
                  % assume triplets: ...,key,value,comment,...
                  Counter = 0;
                  AddHead = cell(Narg./3,3);
                  VecPos  = zeros(Narg./3,1) + Inf;
                  for Iarg=1:3:Narg
                     Counter = Counter + 1; 
                     [AddHead{Counter,1:3}] = deal(varargin{Iarg:1:Iarg+2});
                  end
               elseif (Narg./4==floor(Narg./4) && isnumeric(varargin{1})==1)
                  % assume quadraplets: ...,pos,key,value,comment,...
                  Counter = 0;
                  AddHead = cell(Narg./4,3);
                  VecPos  = zeros(Narg./4,1);
                  for Iarg=1:4:Narg
                     Counter = Counter + 1; 
                     VecPos(Counter) = varargin{Iarg};
                     [AddHead{Counter,1:3}] = deal(varargin{Iarg+1:1:Iarg+3});
                  end
               else
                  error('Unknown format of additional parameters');
               end
            end

            [~,Ncr] = size(CellHead);
            [~,Nar] = size(AddHead);

            if (Ncr==Nar)
               % do nothing
            else
               if (Ncr==2 && Nar==3)
                  CellHead{1,3} = [];
               elseif (Ncr==3 && Nar==2)
                  AddHead{1,3} = [];
               else
                  error('Illegal number of columns');
               end
            end

            % sort AddHead by VecPos
            NewCellHead = CellHead;

            [SortedVecPos,SortedInd] = sort(VecPos);
            SortedAddHead = AddHead(SortedInd,:);
            Nadd = length(SortedVecPos);
            for Iadd=1:1:Nadd
               NewCellHead = tools.array.insert_ind(NewCellHead,SortedVecPos(Iadd)+(Iadd-1),SortedAddHead(Iadd,:));
            end

            NewCellHead = FITS.cellhead_fix(NewCellHead);
        end
        
        
        function [NewCellHead]=cellhead_delkey(CellHead,Keys)
            %--------------------------------------------------------------------------
            % FITS.cellhead_delkey function                                class/@FITS
            % Description: A utility program to delete a keywords, values and
            %              comments from a cell array containing A FITS header
            %              information.
            %              The FITS header cell array contains an arbitrary number
            %              of rows and 3 columns, where the columns are:
            %              {keyword_name, keyword_val, comment}.
            %              The comment column is optional.
            % Input  : - Cell array containing the FITS header information.
            %          - A string containing a keyword name to delete,
            %            or a cell of strings containing keyword names to delete.
            % Output : - The new header cell array.
            % Tested : Matlab 7.10
            %     By : Eran O. Ofek                    Jun 2010
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Reliable: 2
            %--------------------------------------------------------------------------

            if (ischar(Keys))
               Keys = {Keys};
            elseif (iscell(Keys))
               % do nothing
            else
               error('Unknown Keys DataType');
            end

            NewCellHead = CellHead;
            Nkeys = length(Keys);
            for Ikeys=1:1:Nkeys
               [~,Lines]=FITS.cellhead_getkey(NewCellHead,Keys{Ikeys});
               NewCellHead = tools.array.delete_ind(NewCellHead,Lines);
               Nl = length(Lines);
               for I=1:1:Nl-1
                  [~,Lines]=FITS.cellhead_getkey(NewCellHead,Keys{Ikeys});
                  NewCellHead = tools.array.delete_ind(NewCellHead,Lines);
               end
            end
        end
        
        
        function NewHeader=cellhead_fix(Header)
            %--------------------------------------------------------------------------
            % FITS.cellhead_fix function                                   class/@FITS
            % Description: Given an Nx3 cell array of FITS header. Remove blank lines
            %              and make sure the END keyword is at the end of the header.
            % Input  : - An Nx3 cell array of FITS header.
            % Output : - A fixed header.
            % Tested : Matlab R2013a
            %     By : Eran O. Ofek                    Mar 2014
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: NewHeader=FITS.cellhead_fix(Header);
            % Reliable: 2
            %--------------------------------------------------------------------------

            FlagEmpty = strcmp(Header(:,1),'') & strcmpi(Header(:,2),'') & strcmpi(Header(:,3),'');
            NewHeader = Header(~FlagEmpty,:);

            % remove END
            FlagEnd   = strcmp(NewHeader(:,1),'END');
            NewHeader = NewHeader(~FlagEnd,:);

            % add END
            NewHeader = [NewHeader; {'END','',''}];
        end
        
        
        function [NewCellHead,Lines]=cellhead_getkey(CellHead,Keys,NotExist,Multiple)
            %--------------------------------------------------------------------------
            % FITS.cellhead_getkey function                                class/@FITS
            % Description: A utility program to get a specific keywords, values and
            %              comments from a cell array containing A FITS header
            %              information.
            %              The FITS header cell array contains an arbitrary number
            %              of rows and 3 columns, where the columns are:
            %              {keyword_name, keyword_val, comment}.
            %              The comment column is optional.
            % Input  : - Cell array containing the FITS header information.
            %          - A string containing a keyword to look in the header
            %            or a cell array of keywords (case insensitive).
            %          - A parameter to control the behaviour when a specific keyword
            %            is not found.
            %            'Ignore' - ignore missing parameters and in that case the
            %                       length of NewCellHead will be shorter than the
            %                       length of (CellHead). Default.
            %            'NaN'    - Replace missing keywords by NaNs.
            %                       In that case the Lines vector and the NewCellHead
            %                       will contain NaNs
            %          - A flag indicating what to do if there are multiple apperances
            %            of a specific keyword. Options are: {'all','first','last'}.
            %            Default is 'last'.
            % Output : - The header cell array with only the requested lines.
            %          - Indices of requested lines in header.
            % Tested : Matlab 7.10
            %     By : Eran O. Ofek                    Jun 2010
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: [NewCellHead,Lines]=FITS.cellhead_getkey(CellHead,Keys,NotExist);
            % Reliable: 2
            %--------------------------------------------------------------------------

            Def.NotExist = 'Ignore';
            Def.Multiple = 'last';
            if (nargin==2)
               NotExist = Def.NotExist;
               Multiple = Def.Multiple;
            elseif (nargin==3)
                Multiple = Def.Multiple;
            elseif (nargin==4)
               % do nothing
            else
               error('Illegal number of input arguments');
            end

            if (ischar(Keys))
               Keys = {Keys};
            end

            Nkeys = length(Keys);
            Lines = zeros(0,1);
            for Ikeys=1:1:Nkeys
                switch lower(Multiple)
                    case 'all'
                        Ifound = find(strcmpi(CellHead(:,1),Keys{Ikeys})==1);
                    otherwise
                        Ifound = find(strcmpi(CellHead(:,1),Keys{Ikeys})==1,1,Multiple);
                end


               Lines  = [Lines; Ifound];

               if (isempty(Ifound))
                  switch lower(NotExist)
                   case 'nan'
                      Lines = [Lines; NaN];
                   otherwise
                      % do nothing
                  end
               end
            end

            if (sum(isnan(Lines))>0)
               Inan = find(isnan(Lines));
               Lines(Inan) = 1;
               NewCellHead = CellHead(Lines,:);
               for In=1:1:length(Inan)
                  NewCellHead(Inan(In),:) = {NaN NaN NaN};
               end
            else
               NewCellHead = CellHead(Lines,:);
            end
        end
        
    end

    
    methods
        function Nhdu = numHDU(Obj,FileName)
            % return the number of HDUs in a FITS file
            % Input  : - A FITS object with the file name populated (or the
            %            file name can be provided in the 2nd argument)
            %          - Optional single file name (in this case the 1st
            %            arg must contain a single element).
            % Output : - An array of number of HDUs in each file.
            %            NaN if file name is empty.
            % Author : Eran Ofek
            % Example: Nhdu=numHDU(Obj,FileName)
            
            arguments
                Obj
                FileName    = [];
            end
            % if file name is given than Obj must contain single element
            if ~isempty(FileName)
                if numel(Obj)>1
                    error('if file name is given than Obj must contain single element');
                else
                    Obj.File = FileName;
                end
            else
                % do nothing
            end
            
            Nobj = numel(Obj);
            Nhdu = nan(size(Obj));
            for Iobj=1:1:Nobj
                if ~isempty(Obj(Iobj).File)
                    Nhdu(Iobj) = FITS.numHDU1(Obj(Iobj).File);
                end
            end

        end
        
        
        function [Obj,HeadCell,Nhdu] = readHeader(Obj,FileName,HDUnum)
            % Read an header into a three column cell array
            % Input  : - A FITS object.
            %          - An optional single FITS file name.
            %            If empty/not provided use FITS.File property.
            %          - HDU number. If empty/not provided then
            %            If fits file name (2nd arg) is given than default
            %            is 1, else take from FITS.HDU property.
            % Output : - A FITS object with the Header field populated.
            %          - A three column cell array with the entire header.
            %            If more than one file was read, than this is the
            %            last header read.
            %          - Number of HDUs in file. If more than one file was read, than this is
            %            corresponds to the last file read.
            % Example:
            % FITS.readHeader('PTF_201211224002_i_p_scie_t093619_u014676207_f02_p100037_c02.fits')
            
            arguments
                Obj
                FileName char         = [];
                HDUnum                = 1;
            end
            
            if ~isempty(FileName)
                if numel(Obj)>1
                    error('if file name is given than Obj must contain single element');
                else
                    Obj.File = FileName;
                    Obj.HDU  = HDUnum;
                end
            else
                % do nothing
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                if ~isempty(Obj(Iobj).File)
                    [HeadCell,Nhdu] = FITS.readHeader1(Obj(Iobj).File,HDUnum);
                    Obj(Iobj).Header = HeadCell;
                end
            end
        
        end
        
        
        function Obj = read(Obj,FileName,HDUnum,Args)
            % Read all FITS file to a FITS object
            % Input  : - A FITS object
            %          - An optional file name (to read a single file)
            %          - An optional HDU (to read a single file)
            %          * ...,key,val,...
            %            'ReadHead' - Read the header. Default is true.
            %            'CCDSEC' - [xmin xmax ymin ymax] to read.
            %                   If empty read all. Default is empty.
            % Output : - A FITS object with the Data andHeader fields
            %            populated.
            % Author : Eran Ofek (Mar 2021)
            % Example: Obj=read(Obj,1,'CCDSEC',[1 10 1 10])
            
            arguments
                Obj
                FileName                 = [];
                HDUnum                   = [];
                Args.ReadHead logical    = true;
                Args.CCDSEC double       = [];  % Inf for the entire image [Xmin xmax ymin ymax]
            end
            
            if ~isempty(FileName)
                if numel(Obj)>1
                    error('if file name is given than Obj must contain single element');
                else
                    Obj.File   = FileName;
                    Obj.HDU    = HDUnum;
                    Obj.CCDSEC = Args.CCDSEC;
                end
            else
                [Obj.CCDSEC] = deal(Args.CCDSEC);
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % read each FITS file
                if ~isempty(Obj(Iobj).File)
                    
                    if Args.ReadHead
                        [Obj(Iobj).Data, Obj(Iobj).Header] = FITS.read1(Obj(Iobj).File, Obj(Iobj).HDU, 'CCDSEC',Obj(Iobj).CCDSEC);
                    else
                        [Obj(Iobj).Data] = FITS.read1(Obj(Iobj).File, Obj(Iobj).HDU, 'CCDSEC',Obj(Iobj).CCDSEC);
                    end
                    
                    %Data = fitsread(Obj(Iobj).File,PixelRegion);
            
                end
            end
        end
        
        
        function Obj = readTable(Obj,FileName,HDUnum,Args)
            % Read binary or ascii multiple FITS table into a FITS object.
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
            %                         'astrocatalog' - AstroCatalog object.
            %                         'astrotable' - AstroTable object in which
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
            % Output : - A FITS object populated with tables and headers.
            % Tested : Matlab R2014a
            %     By : Eran O. Ofek                    Jan 2015
            %    URL : http://weizmann.ac.il/home/eofek/matlab/
            % Example: Obj.readTable('asu.fit');
            % Reliable: 2
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
                if numel(Obj) > 1
                    error('if file name is given than Obj must contain single element');
                else
                    Obj.File   = FileName;
                    Obj.HDU    = HDUnum;
                end            
            end
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                % read each FITS file
                if ~isempty(Obj(Iobj).File)
                    
                    % Prepare Args to be sent to readTable1() below
                    KeyVal = tools.struct.struct2keyval(Args);
                    
                    % Read pair of image and header
                    [Obj(Iobj).Data, Obj(Iobj).Header] = ...
                        FITS.readTable1(Obj(Iobj).File, 'HDUnum', Obj.HDU, KeyVal{:});
                end
            end
        end        

    end
    
    
    methods (Static) % tests
        Result = unitTest(Obj)
            % unitTest for the FITS class
    end
    
end
