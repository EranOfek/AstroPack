% @Chen

% Configuration base Class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef AstroHeader %< Component
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
        function Obj = AstroHeader(FileNames,HDU,Args)
            % Construct AstroHeader object and populate it with headers
            % Input  : - Either a vector of the the size of the empty
            %            AstroHeader object (e.g., [2 2]).
            %            OR a file name with wild cards or regular
            %            expression from which multiple files will be
            %            searched.
            %            OR a cell array of strings array of file names.
            %            Default is 1.
            %          - The index of HDU from which to read the header.
            %            This can be a vector or a scalar.
            %            Default is 1.
            %          * ...,key,val,...
            %            'Method' - Files extraction method 'wild' |
            %                   'regexp'. See io.files.filelist.
            % Output : - An AstroHeader object with populated headers.
            % Author : Eran Ofek (Mar 20201)
            % Example: 
            
            arguments
                FileNames      = 1;   % name or array size
                HDU            = [];
                Args.Method    = 'wild';
            end
            
            if isnumeric(FileNames)
                % create an empty AstroHeader object
                List = cell(FileNames);
                
            elseif iscell(FileNames) || isstring(FileNames)
                % User provided a list of file names
                List = FileNames;
            else
                % read file names
                List = io.files.filelist(FileNames,Args.Method);
            end
            
                
            Nh = numel(List);
            for Ih=1:1:Nh
                Obj(Ih).File = List{Ih};
            end
            Obj = reshape(Obj,size(List));
            
            Nhdu = numel(HDU);
            % read files
            for Ih=1:1:Nh
                if ~isempty(Obj(Ih).File)
                    Ihdu = min(Ih,Nhdu);
                    Obj(Ih).Data = FITS.readHeader1(Obj(Ih).File,HDU(Ihdu));
                end
            end
               
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
                    error('Read Header from HDF5 file is not available yet');
                otherwise
                    error('Unknown file Type option');
            end
            
            
            
        end
        
    end
    
    methods  % functions for internal use
        %
        
        
    end
    
    methods 
        function [Val,Key,Comment]=keyVal(Obj,KeySynonym,Args)
            %
            
            arguments
                Obj
                KeySynonym char               = '';
                Args.CaseSens(1,1) logical    = true;
                Args.SearchType char {mustBeMember(Args.SearchType,{'strcmp','regexp'})} = 'strcmp';
                Args.Fill                                                       = NaN;
                Args.Val2Num(1,1) logical     = true;
                Args.UseDict(1,1) logical     = true;
            end
            
            if numel(Obj)>1
                error('Use mkeyVal for Header object with multiple entries or multiple keys');
            end
            
            % I need the dictionary in order to continue
            %[SC,FE,II] = imUtil.headerCell.getVal(Obj.Data,KeySynonym)
            
        end
        
        function mkeyVal(Obj,KeySynonym,Args)
            %
            
            arguments
                Obj
                KeySynonym  {mustBeA(KeySynonym,{'char','cell'})}  = '';
                Args.CaseSens(1,1) logical    = true;
                Args.NotExist char  {mustBeMember(Args.NotExist,{'NaN','fail'})} = 'NaN';
                Args.Val2Num(1,1) logical     = true;
                Args.UseDict(1,1) logical     = true;
                Args.OutType char {musBeMember(Args.OutType,{'cell','Header'})} = 'cell';
            end
            
        end
        
        function Result=deleteKey(Obj,KeySynonym,Args)
            %
           
            arguments
                Obj
                KeySynonym  {mustBeA(KeySynonym,{'char','cell'})}  = '';
                Args.CaseSens(1,1) logical    = true;
                Args.UseDict(1,1) logical     = true;
            end
            
        end
        
        function insertKey(Obj,KeyValComment,Args)
            %
            
            arguments
                Obj
                KeyValComment  {mustBeA(KeyValComment,{'char','cell'})}  = '';
                Args.Pos(1,1) double {mustBePositive(Args.Pos)}       = Inf;
            end
        
        end
        
        
        function replaceVal(Obj,Key,NewVal,Args)
            %
            arguments
                Obj
                Key    {mustBeA(Key,{'char','cell'})} = '';
                NewVal                                     = ''; 
                Args.NewComment                            = '';
                Args.NotExist char {mustBeMemeber(Args.NotExist,{'add','fail'})} = 'add'; 
            end
        end
        
        function search(Obj,Val,Args)
            %
            arguments
                Obj         
                Val                 
                Args.Column                  = 1;
                Args.CaseSens(1,1) logical   = true;
            end
        end
        
        
        function isKeyVal(Obj,Key,Val,Args)
            %
            arguments
                Obj
                Key    {mustBeA(Key,{'char','cell'})} = '';
                Val                                        = [];
                Args.LogicalOperator char {mustBeMemeber(Args.LogicalOperator,{'and','or'})} = 'and'; 
                Args.CaseSens(1,1) logical                      = true;
            end
        
        end
        
        % isKeyExist
        
        % julday
        
        % findGroups
        
        % getObsCoo
        
        % getCoo
        
    end
    
    
    % Unit test
    methods(Static)
        function Result = unitTest()
            %
            
            % construct an empty AstroHeader
            A=AstroHeader([2 2]);
            % read headers to AstroHeader, on construction
            A=AstroHeader('*.fits',1);
            
            
            
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


