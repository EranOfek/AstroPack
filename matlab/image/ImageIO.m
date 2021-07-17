
classdef ImageIO < Component
    % Read/write images, tables, headers from input file or stream
    properties
        File = '';
        HDU  = 1;
    end
    
    properties (Dependent)
        Dataset
    end
    
    properties
        Data    = [];
        Header  = cell(0,3);
        CCDSEC  = [];
    end
    
    properties % parameters
        FileType     = [];  % auto
    end
    
    properties (SetAccess = private)
        IsData(1,1) logical      = false;
        IsHeader(1,1) logical    = false;
        IsTable(1,1) logical     = false;
    end
    
    methods % constructor
        function Obj = ImageIO(FileNames, Args)
            % Construct an ImageIO object with optionaly reading data.
            % Input  : - Either:
            %            [] - will return a single element empty object.
            %            [N, M,...] - will return an empty objects
            %                   which size is [N, M,...].
            %            A table to put in the Data property.
            %            A cell array of matrices to put in the Data
            %                   property.
            %            FileName with or without wild cards or regexp, 
            %                   or a cell of file names to read.
            %         * ...,key,val',...
            %            'HDU' - HDU number of HDF5 dataset name.
            %                   Default is 1.
            %            'FileType' - [] will attempt to identify
            %                   automatically. Otherwise, 'fits' | 'hdf5'.
            %                   Default is [].
            %            'ReadHeader' - Default is true.
            %            'IsTable' - True if attempt to read table.
            %                   Default is false.
            %            'UseRegExp' - Use regexp (true) in
            %                   io.file.filelist. Default is false.
            %            'CCDSEC' - CCDSEC for image to read [Xmin Xmax
            %                   Ymin Ymax]. If empty, read the entire image.
            %                   Default is [].
            %            'readTableArgs' - A cell array of additional
            %                   arguments to pass to FITS.readTable1.
            %                   Default is {}.
            % Output : - An ImageIO object.
            % Example: I = ImageIO
            %          I = ImageIO([2, 2]);
            %          I = ImageIO('asu.fit','IsTable',true);
            %          I = ImageIO('WFPC2ASSNu5780205bx.fits','ReadHeader',0)
            %          I = ImageIO('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10])
            %          I = ImageIO({rand(2,2),rand(3,3)});
            %          I = ImageIO({'WFPC2ASSNu5780205bx.fits','WFPC2u5780205r_c0fx.fits'});
            %          I = ImageIO('*.fits')
            
            arguments
                FileNames                    = [];
                Args.HDU                     = 1;
                Args.FileType                = [];   % 'fits' | 'fits' | 'fits' | 'fitsbinary' | 'hdf5' | 'jpg' | 'tiff' | ...
                Args.ReadHeader(1,1) logical = true;
                Args.IsTable(1,1) logical    = false;
                Args.UseRegExp(1,1) logical  = false;
                Args.CCDSEC                  = [];
                Args.readTableArgs cell      = {};
            end
            
            
            if isempty(FileNames)
                % create an empty Object
                Obj.Data = [];
            else
                List = {};
                if isnumeric(FileNames)
                    % recursive call
                    Nobj = prod(FileNames);
                    for Iobj=1:1:Nobj
                        Obj(Iobj) = ImageIO([]);
                    end
                    Obj = reshape(Obj, FileNames);
                else
                    if istable(FileNames)
                        Obj.Data    = FileNames;
                        Obj.IsTable = true;
                    else
                        % traet file names
                        if iscellstr(FileNames)
                            List = FileNames;
                        else
                            if iscell(FileNames)
                                Nobj = numel(FileNames);
                                for Iobj=1:1:Nobj
                                    Obj(Iobj) = ImageIO([]);
                                    if Args.IsTable
                                        Obj(Iobj).Data = array2table(FileNames{Iobj});
                                    else
                                        Obj(Iobj).Data = FileNames{Iobj};
                                    end
                                end
                            else
                                List = io.files.filelist(FileNames, Args.UseRegExp);
                            end
                        end
                    end
                end 
                
                Nobj = numel(List);
                for Iobj=1:1:Nobj
                    Obj(Iobj) = ImageIO([]);
                    if Args.ReadHeader
                        [Obj(Iobj).Data, Obj(Iobj).Header] = ImageIO.read1(List{Iobj}, 'HDU',Args.HDU, 'FileType',Args.FileType, 'CCDSEC',Args.CCDSEC, 'IsTable',Args.IsTable, 'readTableArgs',Args.readTableArgs);
                    else
                        Obj(Iobj).Data = ImageIO.read1(List{Iobj}, 'HDU',Args.HDU, 'FileType',Args.FileType, 'CCDSEC',Args.CCDSEC, 'IsTable',Args.IsTable, 'readTableArgs',Args.readTableArgs);
                    end
                    Obj(Iobj).IsTable = Args.IsTable;
                    Obj(Iobj).CCDSEC  = Args.CCDSEC;
                end
                
            end
            
        end
        
    end
    
    
    methods % setters/getters
        
    end
    
    methods (Static)   % static functions read/write
        function [Data, Header] = read1(FileName, Args)
            % read a single image/table file with header in fits/hdf5/other format
            % Input  : - File name.
            %          * ...,key,val,...
            %            'ReadData' - Read data (image or table).
            %                   Default is true.
            %            'HDU' - HDU number of HDF5 dataset name.
            %                   Default is 1.
            %            'FileType' - [] will attempt to identify
            %                   automatically. Otherwise, 'fits' | 'hdf5'.
            %                   Default is [].
            %            'IsTable' - True if attempt to read table.
            %                   Default is false.
            %            'CCDSEC' - CCDSEC for image to read [Xmin Xmax
            %                   Ymin Ymax]. If empty, read the entire image.
            %                   Default is [].
            %            'readTableArgs' - A cell array of additional
            %                   arguments to pass to FITS.readTable1.
            %                   Default is {}.
            % Output : - Data. Either image matrix, or table of table data.
            %          - Header (3 columns cell array).
            % Author : Eran Ofek (Apr 2021)
            % Example: [D,H]=ImageIO.read1('asu.fit','IsTable',1);
            %          [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
            %          [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10]);
            %          [D]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
            %          [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','ReadData',false);
            
            arguments
                FileName                     = [];
                Args.ReadData(1,1) logical   = true;
                Args.HDU                     = 1;
                Args.FileType                = [];   % 'fits' | 'fitstable' | 'fitsascii' | 'fitsbinary' | 'hdf5' | 'jpg' | 'tiff' | ...
                Args.IsTable(1,1) logical    = false;
                Args.CCDSEC                  = [];
                Args.readTableArgs cell      = {};
            end
            
            Data = [];
            Header = cell(0,3);
            
            if isempty(Args.FileType)
                % attempt to figure out file type automatically
                [~, ~, Ext] = fileparts(FileName);
                Ext = strrep(Ext,'.',''); % remove dot from extension
                
                switch lower(Ext)
                    case {'fits','fit'}
                        Args.FileType = 'fits';
                    case {'hdf5','h5','hd5'}
                        Args.FileType = 'hdf5';
                    otherwise
                        Args.FileType = 'other';
                end
            end
            
            % read file
            switch Args.FileType
                case 'fits'
                    if nargout>1 && ~Args.ReadData
                        Header = FITS.readHeader1(FileName, Args.HDU);
                    else
                        if Args.IsTable
                            [Data, Header] = FITS.readTable1(FileName, Args.readTableArgs{:});
                        else
                            if Args.ReadData && nargout<2
                                [Data] = FITS.read1(FileName, Args.HDU, 'CCDSEC',Args.CCDSEC);
                            else
                                [Data, Header] = FITS.read1(FileName, Args.HDU, 'CCDSEC',Args.CCDSEC);
                            end
                        end
                    end
                    
                case 'hdf5'
                    error('hdf5 is not yet supported');
                    
                otherwise
                    if Args.IsTable
                        error('IsTable is truw while file type is not fits or hdf5');
                    else
                        if Args.ReadData
                            Data   = imread(FileName);
                        end
                    end
            end
                    
                
                
            
            
            
        end
        
        function FileName = write1(Data, FileName, Args)
            % Write a single data array into a file
            
            arguments
                Data                                   % array or Table
                FileName                      = [];
                Args.Dir                      = '';
                Args.Header                   = [];
                Args.HDU                      = 1;
                Args.FileType                 = 'fits';
                Args.DataType                 = '';  % only for images - force data type
                Args.IsTable(1,1) logical     = false;
                Args.ColNames                 = {};
                Args.ColUnits                 = {};
                Args.CCDSEC                   = [];      % only for 2D images
                
                Args.Append(1,1) logical      = false;
                Args.OverWrite(1,1) logical   = false;
                Args.WriteTime(1,1) logical   = false;
                
                Args.MatVersion               = 'v7.3';
                
            end
            
            % if FileName is empty - generate a tmp file
            if isempty(FileName)
                FileName = tempname;
            end
            
            DataType = [];
            if ~Args.IsTable
                if ~isempty(Args.DataType)
                    % force data type
                    Data = cast(Data, Args.DataType);
                end
                DataType = class(Data);
                
                if ~isempty(Args.CCDSEC)
                    if ndims(Data)==2
                        Data = Data(CCDSEC(3):CCDSEC(4), CCDSEC(1):CCDSEC(2));
                    else
                        error('CCDSEC is currently possible only for 2D images');
                    end
                end
            end
            
            % convert header to a 3-col cell array
            if iscell(Args.Header)
                Header = Args.Header;
            elseif isa(Args.Header,'AstroHeader')
                % get cell
                Header = Args.Header.Data;
            else
                error('Unsupported Header type');
            end
                
            PWD = pwd;
            if ~isempty(Args.Dir)
                cd(Args.Dir)
            end
            
            switch lower(Args.FileType)
                case 'fits'
                    if Args.IsTable
                        % write FITS binary table
                        error('Writing FITS binary tables is not supported yet');
                    else
                        % write FITS image
                        FITS.write(Data, FileName, 'Header', Header,...
                                                   'DataType',DataType,...
                                                   'Appned',Args.Appned ,...
                                                   'OverWrite',Args.OverWrite ,...
                                                   'WriteTime',Args.WriteTime);
                    end
                        
                case {'hdf5','h5z','h5','hd5'}
                    
                case 'matflat'
                    % save Data and Header variables to a mat file
                    if Args.Append
                        save(FileName,'Data','Header',Args.MatVersion,'-append');
                    else
                        save(FileName,'Data','Header',Args.MatVersion);
                    end
                case 'matstruct'
                    % save a struct with Data and Header fields to a mat
                    % file
                    St.Data   = Data;
                    St.Header = Header;
                    if Args.Append
                        save(FileName,'StData',Args.MatVersion,'-append');
                    else
                        save(FileName,'StData',Args.MatVersion);
                    end
                case 'jpg'
                    
                case 'tiff'
                    
                otherwise
                    error('Unknown FileType option');
            end
            
            cd(PWD);
            
            
        end
    end
    
    methods (Static)  % unitTest
        function Result = unitTest()
            % unitTest for ImageIO class
            
            % static class to read a single image/table/header
            [D,H]=ImageIO.read1('asu.fit','IsTable',1);
            [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
            [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10]);
            [D]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
            [D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','ReadData',false);
            
            % constroctor
            I = ImageIO;
            I = ImageIO([2, 2]);
            I = ImageIO('asu.fit','IsTable',true);
            I = ImageIO('WFPC2ASSNu5780205bx.fits','ReadHeader',0);
            I = ImageIO('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10]);
            I = ImageIO({rand(2,2),rand(3,3)});
            I = ImageIO({'WFPC2ASSNu5780205bx.fits','WFPC2u5780205r_c0fx.fits'});
            I = ImageIO('*.fits');
            
            Result = true;
            
        end
    end
        
    
end
