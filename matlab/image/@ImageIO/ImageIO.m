% ImageIO
%

% #functions (autogen)
% ImageIO - Construct an ImageIO object with optionaly reading data.
% read1 - read a single image/table file with header in fits/hdf5/other format
% write1 - Write a single data array into a file
% writeHDF5 - Save matrix data to HDF5 file with possible attributes
% #/functions (autogen)
%

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
                                List = io.files.filelist(FileNames, 'UseRegExp',Args.UseRegExp, 'AddPath',true);
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
            
            if isempty(Args.HDU)
                Args.HDU = 1;
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
                            %Data = table2array(Data);
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
            % Input  : - Image/table data.
            %            This can be a matrix, table, AstroImage,
            %            ImageComponent.
            %          - File name to save (with or without full path; see
            %            also Args.Dir).
            %            If empty, will generate a tmp file name.
            %            Default is empty.
            %          * ...,key,val,...
            %            'Dir' - Directory name in which to save the image.
            %                   If empty, use current dir.
            %                   Default is ''.
            %            'Header' - An header (3 col cell array).
            %                   or an AstroHeader object.
            %                   Default is {}.
            %            'HDU' - FITS HDU number, or HDF5 dataset name.
            %                   Default is 1.
            %            'FileType' - File type to save:
            %                   'fits'
            %                   {'hdf5','h5z','h5','hd5'}
            %                   'matflat' - save Data and Header in flat
            %                           mat file.
            %                   'matstruct' - save structure with data and
            %                           Header fields.
            %                   {'jpg','tif','tiff','gif','png','bmp','hdf','jp2','jpx','jpeg','pcx','pgm'}
            %                           Use imwrite.m
            %            'DataProp' - If input is AstroImage, or
            %                   ImageComponent, read the data from this property.
            %            'DataType' - Data Type of Data to save.
            %                   If empty, then use the original Data data
            %                   type. Default is empty.
            %            'IsTable' - A logical indicating if data is a
            %                   table to save. Default is false.
            %            'ColNames' - A cell array of table column names.
            %            'ColUnits' - A cell arrat of table units names.
            %            'CCDSEC' - CCDSEC to save. If empty, save full
            %                   image. Default is [].
            %            'WriteMethodImages' - write method, def. 'Full', if 'Simple', 'Mex', or 'ThreadedMex'
            %            uses io.fits.writeSimpleFITS, io.fits.writeMexFITS or io.fits.writeThreadMexFITS (fastest).
            %            'Append' - Append image as a multi extension to an
            %                      existing FITS file. Default is false.
            %            'OverWrite'- Overwrite an existing image. Default
            %                       is false.
            %            'WriteTime'- Add creation time to image header.
            %                       Default is false.
            %            'MatVersion' - MAT file version.
            %                   Default is '-v7.3'.
            % Output : - File name.
            % Author : Eran Ofek (Jul 2021)
            % Example: ImageIO.write1(rand(10,10),'tmp.fits')
            
            arguments
                Data                                   % array or Table
                FileName                      = [];
                Args.Dir char                 = '';
                Args.Header                   = {};
                Args.HDU                      = 1;
                Args.FileType char            = 'fits';   % 'mat'
                Args.DataProp char            = 'Image';  % Data prop in AstroImage
                Args.DataType                 = '';  % only for images - force data type
                Args.IsTable(1,1) logical     = false;
                Args.ColNames cell            = {};
                Args.ColUnits cell            = {};
                Args.CCDSEC                   = [];      % only for 2D images                
                Args.WriteMethodImages        = 'Full';  % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
                Args.Append(1,1) logical      = false;
                Args.OverWrite(1,1) logical   = false;
                Args.WriteTime(1,1) logical   = false;
                
                Args.MatVersion               = 'v7.3';
                
            end
            
            % convert data from other data types
            if isa(Data,'AstroImage')
                if isempty(Args.Header)
                    % user did not supply header - so read from AstroHeader
                    Args.Header = Data.Header;
                end
                Data = Data.Image;
            elseif isa(Data,'ImageComponent')
                Data = Data.Image;
            else
                % do nothing
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
                    % this will work only for a matrix Data
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
                        if strcmpi(Args.WriteMethodImages,'full') 
                            FITS.write(Data, FileName, 'Header', Header,...
                                                   'DataType',DataType,...
                                                   'Append',Args.Append,...
                                                   'OverWrite',Args.OverWrite,...
                                                   'WriteTime',Args.WriteTime);                            
                        else
                            FITS.writeSimpleFITS(Data, FileName, 'Header', Header,...
                                                   'DataType',DataType,...
                                                   'WriteMethodImages',Args.WriteMethodImages);
                        end
                    end
                        
                case {'hdf5','h5z','h5','hd5'}
                    if isnumeric(Args.HDU)
                        Args.HDU = sprintf('/%d',Args.HDU);
                    elseif ischar(Args.HDU)
                        if ~strcmp(Argd.HDU(1),'/')
                            Args.HDU = sprintf('/%s',Args.HDU);
                        end
                    else
                        error('Unknown HDU option');
                    end
                    h5create(FileName, Args.HDU, size(Data));
                    h5write(FileName, Args.HDU, Data);
                    % FFU
                    warning('Save Header in HDF5 option is not yet available');
                    
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
                case {'jpg','tif','tiff','gif','png','bmp','hdf','jp2','jpx','jpeg','pcx','pgm'}
                    imwrite(Data, FileName, Args.FileType)
                otherwise
                    error('Unknown FileType option');
            end
            
            cd(PWD);
            
            
        end
        
        function writeHDF5(Data, FileName, Args)
            % Save matrix data to HDF5 file with possible attributes
            % Input  : - data matrix to save.
            %          - File name.
            %          * ...,key,val,...
            %            'DataSet' - HDF5 dataset. Default is '/cat'.
            %            'Header' - A two column cell array with header
            %                   information. Default is {}.
            %            'ColNames' - This argument is ignored if Header is
            %                   provided. This is a cell vector of column
            %                   names.
            %            'ColUnits' - Like column name but for units.
            % Output : null
            % Author : Eran Ofek (Oct 2021)
            % Example: ImageIO.writeHDF5(rand(100,3),'ab.h5','ColNames',{'a','b','c'});
            
            arguments
                Data
                FileName
                Args.DataSet  = '/cat';
                Args.Header   = {};
                Args.ColNames = {};
                Args.ColUnits = {};
            end
           
            if isempty(Args.Header)
                if isempty(Args.ColNames)
                    Att = {};
                else
                    if isempty(Args.ColUnits)
                        Args.ColUnits = cell(numel(Args.ColNames), 1);
                    end
                    if numel(Args.ColUnits)~=numel(Args.ColNames)
                        error('ColUnits and ColNames must have the same number of entries');
                    end
                    Att = [Args.ColNames(:), Args.ColUnits(:)];
                end
            else
                Att = Args.Header(:,1:2);
            end
            HDF5.save(Data, FileName, Args.DataSet, Att);
            
        end
        
        function Files = write(ObjIn, ObjFN, DataProp, Args)
            % A multi file general-purpose file write
            %   Can be used to write the content of AstroImage or
            %   MatchedSources objects.
            % Input  : - An AstroImage or MatchedSources object.
            %          - A FileNames object. The number of file entries
            %            must be equal to the number of elements in the first
            %            input object.
            %          - Data property in AstroImage to save.
            %            Default is 'Image'.
            %          * ...,key,val,...
            %            'HDU' - FITS HDU, or 'hdf5 DataSet in which to
            %                   write the data. Relevant only for AstroImage
            %                   input.
            %            'WriteHeader' - Write header.
            %                   Relevant only for AstroImage input.
            %                   Default is true.
            %            'WriteMethodImages' - write method, can be 'Full', if 'Simple', 'Mex', or 'ThreadedMex'
            %            uses io.fits.writeSimpleFITS, io.fits.writeMexFITS or io.fits.writeThreadMexFITS (fastest).
            %            'DataType' - cast to data type. Default is [].
            %            'ImageFileType' - Default is 'fits'.
            %            'MatchedFileType' - Default is 'hdf5'.
            % Output : - A cell array of file namesfull path that were
            %            written.
            % Author : Eran Ofek (Dec 2022)
            
            arguments
                ObjIn
                ObjFN
                DataProp                   = 'Image';
                Args.HDU                   = 1;
                Args.WriteHeader logical   = true;                
                Args.DataType              = [];
                Args.WriteMethodImages     = 'Simple';  % can be 'Simple', 'Full', 'Mex', or 'ThreadedMex'
                Args.ImageFileType         = 'fits';
                Args.MatchedFileType       = 'hdf5';
            end
            
            
            
            ObjFN = ObjFN.copy;
            switch DataProp
                case 'Image'
                    Istable = false;
                    ObjFN.Product = 'Image';
                case 'Mask'
                    Istable = false;
                    ObjFN.Product = 'Mask';
                case 'Back'
                    Istable = false;
                    ObjFN.Product = 'Back';
                case 'Var'
                    Istable = false;
                    ObjFN.Product = 'Var';
                case 'Cat'
                    Istable = true;
                    ObjFN.Product = 'Cat';
                case 'PSF'
                    Istable = false;
                    ObjFN.Product = 'PSF';
                otherwise
                    error('Unknown DataProp option');
            end
            
            Files = ObjFN.genFull;
            Nf    = numel(Files);
            Nobj  = numel(ObjIn);
            if Nf~=Nobj
                error('Number of file names must be equal to the number of elements in object to save');
            end
            
            Header = [];
            switch class(ObjIn)
                case 'AstroImage'
                    %
                    for Iobj=1:1:Nobj
                        if Args.WriteHeader
                            Header = ObjIn(Iobj).HeaderData;
                        end
                        ImageIO.write1(ObjIn(Iobj).(DataProp), Files{Iobj},'HDU',Args.HDU,...
                                                                           'FileType',Args.ImageFileType,...
                                                                           'WriteMethodImages',Args.WriteMethodImages,...
                                                                           'IsTable',IsTable,...
                                                                           'DataType',Args.DataType,...
                                                                           'Header',Header);
                    end
                    
                case 'MatchedSources'
                    for Iobj=1:1:Nobj
                        ObjIn(Iobj).write1(Files{Iobj}, 'FileType',Args.MatchedFileType);
                    end
                    
                otherwise
                    error('ObjIn %s is not supported',class(ObjIn));
            end
            
            
        end
        
    end
    
    methods (Static)  % unitTest
        Result = unitTest()
            % unitTest for ImageIO class
    end
        
    
end
