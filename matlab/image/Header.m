% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Header < Component
    % Properties
    properties (SetAccess = public)
        Data(:,3) cell            = cell(0,3);
        Key struct                = struct(); 
        
        File                      = '';
        HDU                       = ''; % HDU or dataset
        
%         filename        
%         configPath = "";
%         data
%         lines
%         userData
%         
%         inputImagePath
%         inputImageExt
    end
    

    methods
        % Constructor    
        function obj = Header()
        end
    end
    
    methods % read/write
        function Obj=read(Obj,Args)
            % read as a single/multiple headers from file/s into an Header object
            % Input  : - An Header object
            %          * ...,key,val,... or ,key=val,...
            %            'FileName' - File name or a cell array of file
            %                   names. Default is to use Header.File
            %                   property.
            %            'HDU' - HDU (scalae or vector) or dataset. Default
            %                   is to use Header.HDU.
            %            'Type' - File type. 'fits'|'hdf5'|['auto'].
            %                   'auto' will attempt automatic
            %                   identificatin.
            % Examples: H=Header;
            % H.read('FileName',{'File1.fits','File2.fits'});  % read two headers in default HDU.
            % H.read('FileName','File1.fits','HDU',[1 2]); % read 2 HDUs from a single file
            
            arguments
                Obj
                Args.FileName      = Obj.File;
                Args.HDU           = Obj.HDU;
                Args.Type char {mustBeMember(Args.Type,{'auto','fits','fit','FITS','FIT','fit.gz','fits.gz','hdf5','h5','hd5'})} = 'auto';
            end
            
            if ~iscell(Args.FileName)
                Args.FileName = {Args.FileName};
            end
            Nfile = numel(Args.FileName);
            Nhdu  = numel(Args.HDU);
            Nmax  = max(Nfile,Nhdu);
            
            switch Args.Type
                case 'auto'
                    FileParts = split(FileName,'.');
                    Args.Type = FileParts{end};
            end
            
            switch lower(Args.Type)
                case {'fits','fit','fit.gz','fits.gz'}
                    % read FITS file
                    FO = FITS;
                    for Imax=1:1:Nmax
                        Ih    = min(Nhdu,Imax);
                        Ifile = min(Nfile,Imax);
                        Obj(Ifile).Data = FO.readHeader(Args.FileName{Ifile},Args.HDU(Ih));
                        Obj(Ifile).File = Args.FileName{Ifile};
                        Obj(Ifile).HDU  = Args.HDU(Ih);
                    end
                case {'hdf5','h5','hd5'}
                    % read hdf5 file
                    
                otherwise
                    error('Unknown file Type option');
            end
            
            
            
        end
        
    end
    
    
    
    % Unit test
    methods(Static)
        function result = uTest()
            fprintf("Started\n");
            conf = Config("c:/temp/conf.txt")
            val = conf.getValue("key1");
            disp(val);           
            num = conf.getNum("key3");
            disp(num);
            result = true;
        end
    end    
        
end


