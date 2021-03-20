classdef FITS1
    properties
        Data                                      = [];         % free format
        Header                                    = cell(0,3);  % free format
        File     {mustBeA(File,{'char','cell'})}  = '';
        HDU(1,1) uint8                            = 1;
        StoreData(1,1) logical                    = true;
    end
    
   
    methods % constructor
        function Obj=FITS1(FileName,ListHDU)
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
                % already a list of files
                List = FileName;
            elseif ischar(FileName)
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
    % get number of HDUs - ok
    % read headers - ok
    % read images
    % read tables
    
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
            if ~isempty(FileName) && numel(Obj)>1
                error('if file name is given than Obj must contain single element');
            else
                Obj.File = FileName;
            end
            
            Nobj = numel(Obj);
            Nhdu = nan(size(Obj));
            for Iobj=1:1:Nobj
                if ~isempty(Obj(Iobj).File)
                    Fptr = matlab.io.fits.openFile(Obj(Iobj).FileName);
                    Nhdu(Iobj) = matlab.io.fits.getNumHDUs(Fptr);
                    fits.closeFile(Fptr);
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
            % FITS.read_header('PTF_201211224002_i_p_scie_t093619_u014676207_f02_p100037_c02.fits')
            
            arguments
                Obj
                FileName char        
                HDUnum                = 1;           
            end
            
            % if file name is given than Obj must contain single element
            if ~isempty(FileName) && numel(Obj)>1
                error('if file name is given than Obj must contain single element');
            else
                Obj.File = FileName;
                Obj.HDU  = HDUnum;
            end
            
            KeyPos = 9;
            ComPos = 32;
            
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
            
                Fptr = matlab.io.fits.openFile(Obj(Iobj).File);
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
                               HeadCell{Ikey,1}  = Util.string.spacedel(Card(1:KeyPos-1));
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
                
                if Obj(Iobj).StoreData
                    Obj(Iobj).Header = HeadCell;
                end
            end
        
        end
        
        function [Obj,Data]=read(Obj,FileName,HDUnum)
            %
            
            arguments
                Obj
                FileName
                HDUnum
            end
            
            
        end
        
    end
    
    
end
