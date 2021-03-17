classdef FITS 
    properties
        Data                                      = [];
        Header                                    = cell(0,3);
        File     {mustBeA(File,{'char','cell'})}  = '';
        HDU(1,1) uint8                            = 1;
    end
    
   
    methods % constructor
        function Obj=FITS
            Obj.Data   = [];
            Obj.Header = cell(0,3);
            IObj.File  = '';
            Obj.HDU    = 1;
        end
    end
    
    methods
        function Nhdu=numHDU(Obj,FileName)
            % return the number of HDUs in a FITS file
           
            arguments
                Obj
                FileName    = [];
            end
            if isempty(FileName)
                FileName = Obj.File;
                if isempty(FileName)
                    error('FITS file name must be provided');
                end
            end
            
            Fptr = matlab.io.fits.openFile(FileName);
            Nhdu = matlab.io.fits.getNumHDUs(Fptr);
            fits.closeFile(Fptr);
            
        end
        
        function [HeadCell,Nhdu]=readHeader(Obj,FileName,HDUnum)
            % Read a single header into a three column cell array
            % Input  : - A FITS object.
            %          - A single FITS file name. If empty/not provided use
            %            FITS.File property.
            %          - HDU number. If empty/not provided use
            %            FITS.HDU property.
            % Output : - A three column cell array with the entire header.
            %          - Number of HDUs in file.
            % Example:
            % [HeadCell,Nhdu]=FITS.read_header('PTF_201211224002_i_p_scie_t093619_u014676207_f02_p100037_c02.fits')
            
            arguments
                Obj
                FileName              = Obj.File;
                HDUnum                = Obj.HDU;
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
        
        end
        
        
    end
    
    
end
