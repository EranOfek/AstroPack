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
                List = io.files.filelist(FileName,'wild');
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
                Ihdu  =min(Ilist,Nhdu);
                Obj(Ilist).File = List{Ilist};
                Obj(Ilist).HDU  = ListHDU(Ihdu);
            end
            
        end
    end
    
    % list of needed functionality
    % write
    % writeTable
    
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
        
        function [HeadCell,Nhdu] = readHead1(FileName,HDUnum)
            % Read a single header from a FITS file (Static)
            % A static function of FITS class
            % Input  : - FITS file name
            %          - HDU number. Default is 1
            % Output : - A 3 column cell array of header entries
            %            [Key, Value, Comment]
            %          - Number of HDUs in FITS file
            % Author : Eran Ofek
            % Example: [HeadCell,Nhdu] = FITS.readHead1(FileName,HDUnum)
            
            arguments
                FileName char
                HDUnum               = 1;
            end
            
            
            KeyPos = 9;
            ComPos = 32;
            
            Fptr = matlab.io.fits.openFile(FileName);
            Nhdu = matlab.io.fits.getNumHDUs(Fptr);
            if (Nhdu>=HDUnum)
                Htype = matlab.io.fits.movAbsHDU(Fptr,HDUnum);

                Nkey = matlab.io.fits.getHdrSpace(Fptr);
                HeadCell = cell(Nkey,3);
                for Ikey = 1:1:Nkey
                   Card     = matlab.io.fits.readRecord(Fptr,Ikey);
                   LenCard = length(Card);
                   if (LenCard>=9)

                       if (strcmpi(Card(KeyPos),'='))
                           HeadCell{Ikey,1}  = tools.string.spacedel(Card(1:KeyPos-1));
                           % update comment position due to over flow
                           Islash = strfind(Card(ComPos:end),'/');
                           if (isempty(Islash))
                               UpdatedComPos = ComPos;
                           else
                               UpdatedComPos = ComPos + Islash(1)-1;
                           end
                           Value = Card(KeyPos+1:min(LenCard,UpdatedComPos-1));
                           PosAp = strfind(Value,'''');

                           if (isempty(PosAp))
                               if contains('TF',upper(strtrim(Value)))
                                   % a boolean
                                   Value=upper(strtrim(Value))=='T';
                               else
                                   % possible number
                                   Value = str2double(Value);
                               end
                           else
                               if (length(PosAp)>=2)
                                   % a string
                                   Value = strtrim(Value(PosAp(1)+1:PosAp(2)-1));
                               else
                                   Value = Card(PosAp(1)+10:end);
                               end
                           end

                           HeadCell{Ikey,2}  = Value; %Card(KeyPos+1:min(LenCard,ComPos-1));
                           if (LenCard>UpdatedComPos)
                               HeadCell{Ikey,3}  = Card(UpdatedComPos+1:end);    
                           else
                               HeadCell{Ikey,3}  = '';
                           end

                       end
                   end

                   % look for history and comment keywords
                   if numel(Card)>6
                       if (strcmpi(Card(1:7),'HISTORY'))
                           HeadCell{Ikey,1} = 'HISTORY';
                           HeadCell{Ikey,2} = Card(KeyPos:end);
                           HeadCell{Ikey,3} = '';
                       end
                       if (strcmpi(Card(1:7),'COMMENT'))
                           HeadCell{Ikey,1} = 'COMMENT';
                           HeadCell{Ikey,2} = Card(KeyPos:end);
                           HeadCell{Ikey,3} = '';
                       end
                   end
                end

            end
            matlab.io.fits.closeFile(Fptr);

            
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
                [HeadCell,Nhdu] = FITS.readHead1(FileName,HDUnum);
            end

            matlab.io.fits.closeFile(Fptr);

        end
        
        function [Cube]=read2cube(List,HDUnum,Args)
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
        
        function [Out,HeadCell,Col]=readTable1(TableName,Args)
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
            % Example: [Out,Head,Col]=FITS.read_table('asu.fit');
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
            
            Out = AstCat(Nfile,1);
            Col = tools.struct.struct_def({'Col','Cell','Units','TypeChar','Repeat','Scale','Zero','Nulval','Tdisp','Data'},Nfile,1);
                     
            Ifile = 1;
            % get header
            HeadCell    = FITS.readHead1(ListTableName{1},Args.HDUnum);

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
                [Col(Ifile).Cell{Icol},Col(Ifile).Units{Icol},Col(Ifile).TypeChar{Icol},...
                                       Col(Ifile).Repeat{Icol},Col(Ifile).Scale{Icol},Col(Ifile).Zero{Icol},...
                                       Col(Ifile).Nulval{Icol},Col(Ifile).Tdisp{Icol}]= Fun_getColParms(Fptr,Icol);


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
        
        function [KeysVal,KeysComment,Struct]=get_keys(Image,Keys,HDUnum,Str)
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

            import matlab.io.*
            Fptr = matlab.io.fits.openFile(Image);
            N = matlab.io.fits.getNumHDUs(Fptr);
            if (HDUnum>N)
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
                    Tmp = str2double(KeysVal{Ikey});
                    if (isnan(Tmp))
                        % do nothing - keep as a string
                    else
                        KeysVal{Ikey} = Tmp;
                    end
                end

            end
            matlab.io.fits.closeFile(Fptr);

            if (nargout>2)
               Struct = cell2struct(KeysVal,Keys,2);
            end
        end
        
        function [KeysVal,KeysComment,Struct,List]=mget_keys(Images,Keys,HDUnum,Str)
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

            [~,List] = io.files.filelist(ImageName,NaN);
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
    end
    
    methods
        function Nhdu=numHDU(Obj,FileName)
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
        
        function [Obj,HeadCell,Nhdu]=readHeader(Obj,FileName,HDUnum)
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
                    [HeadCell,Nhdu] = FITS.readHead1(Obj(Iobj).File,HDUnum);
                    Obj(Iobj).Header = HeadCell;
                end
            end
        
        end
        
        function Obj=read(Obj,FileName,HDUnum,Args)
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
            % Example: Obj=read(Obj)
            
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
        
        function Obj=readTable(Obj,FileName,HDUnum,Args)
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
                if numel(Obj)>1
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
                    KeyVal = tools.struct.struct2keyval(Args);
                    [Obj(Iobj).Data,Obj(Iobj).Header] = FITS.readTable1(Obj(Iobj).File,...
                                                                        'HDUnum',Obj.HDU,...
                                                                        KeyVal{:});
                end
            end
            
            
            
        end
        
    end
    
    methods (Static) % tests
        function unitTest(Obj)
            % unitTest for the FITS class
            F = FITS('*.fits');
            
            % test static functions
            Nh1=FITS.numHDU1(F(1).File)
            FITS.readHead1(F(1).File)
            [Im,H,Nh2]=FITS.read1(F(1).File);
            if Nh1~=Nh2
                error('unitTest for FITS class failed - number of HDus not consistent');
            end
            % read CCDSEC
            [Im,H,Nh]=FITS.read1(F(1).File,'CCDSEC',[1 10 1 10]);
            % read multiple images into a cube
            [Cube]=FITS.read2cube({F(1:2).File},1,'CCDSEC',[1 10 1 10])
            
            % BUG - FIX!!
            [Out, Head, Col] = FITS.readTable1('asu.fit');
            
            [KeysVal,KeysComment,Struct]=FITS.get_keys('WFPC2ASSNu5780205bx.fits',{'NAXIS1','NAXIS2'})
            
            [KeysVal,KeysComment,Struct,List]=FITS.mget_keys('*.fits',{'NAXIS1','NAXIS2'})
            
            % non static 
            F.numHDU  
            F.readHeader  % the headers are stored in F.Header
            F.read;
            F = FITS('*.fits');
            F(1:2).read([],[],'CCDSEC',[1 10 1 10])
            
            F = FITS('asu.fit',2);
            F.readTable
            
        end
    end
    
end
