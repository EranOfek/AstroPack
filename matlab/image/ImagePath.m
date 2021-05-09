% This class is a wrapper for imUtil.util.file.construct_filename

% ImagePath class - construct image file name and path
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: +imUtil
% Example : ImP = ImagePath; ImP.ProjName='ULTRASAT'
%           [FileName,Path]=construct_filename(ImP,'Level','proc')
% Reliable: 2
%--------------------------------------------------------------------------

classdef ImagePath < handle % & matlab.mixin.CustomDisplay
    properties
        FullName char       = '';
        Path char           = '';
        FileName char       = '';
        
    end
    properties
        ProjName char       = 'none';
        Date                = NaN;
        Filter char         = 'clear';
        FieldID             = '';
        FormatFieldID char  = '%06d';
        Type char           = 'sci';
        Level char          = 'raw';
        SubLevel char       = '';
        Product char        = 'im';
        Version             = 1;
        FormatVersion       = '%03d';
        FileType            = 'fits';
        TimeZone            = 2;
        RefVersion          = 1;
        FormatRefVersion    = '%03d';
        SubDir              = '';
        DataDir             = 'data';
        Base                = '/home/last';
        
        
    end
    properties (Hidden, SetAccess = public)
        UserData
    end
    properties (Hidden, SetAccess=private)
        ParUpdated logical    = false;  % beeing set to true when ProjName...Base parameters are changed
    end
    
    
    methods % Constructor
       
        function D=ImagePath
            % Base class constructor
            % Package: @Base           
            
        end
    end
    
    
    % setters and getters
    methods
        function Result = get.FullName(Obj)
            % getter for FullName
            
            if isempty(Obj.FullName) || Obj.ParUpdated
                construct_filename(Obj);
                Result = Obj.FullName;
            else
                Result = Obj.FullName;
            end
            Obj.ParUpdated = false;
        end
        
        
        function Result = get.Path(Obj)
            % getter for Path
            
            if isempty(Obj.Path) || Obj.ParUpdated
                construct_filename(Obj);
                Result = Obj.Path;
            else
                Result = Obj.Path;
            end
            Obj.ParUpdated = false;
        end
        
        
        function Result = get.FileName(Obj)
            % getter for FileName
            
            if isempty(Obj.FileName) || Obj.ParUpdated
                construct_filename(Obj);
                Result = Obj.FileName;
            else
                Result = Obj.FileName;
            end
            Obj.ParUpdated = false;
        end
       
        
        function set.ProjName(Obj,Val)
            % setter
            Obj.ProjName   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Date(Obj,Val)
            % setter
            Obj.Date   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Filter(Obj,Val)
            % setter
            Obj.Filter   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.FieldID(Obj,Val)
            % setter
            Obj.FieldID   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.FormatFieldID(Obj,Val)
            % setter
            Obj.FormatFieldID   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Type(Obj,Val)
            % setter
            Obj.Type   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Level(Obj,Val)
            % setter
            Obj.Level   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.SubLevel(Obj,Val)
            % setter
            Obj.SubLevel   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Product(Obj,Val)
            % setter
            Obj.Product   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Version(Obj,Val)
            % setter
            Obj.Version   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.FormatVersion(Obj,Val)
            % setter
            Obj.FormatVersion   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.FileType(Obj,Val)
            % setter
            Obj.FileType   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.TimeZone(Obj,Val)
            % setter
            Obj.TimeZone   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.RefVersion(Obj,Val)
            % setter
            Obj.RefVersion   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.FormatRefVersion(Obj,Val)
            % setter
            Obj.FormatRefVersion   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.SubDir(Obj,Val)
            % setter
            Obj.SubDir   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.DataDir(Obj,Val)
            % setter
            Obj.DataDir   = Val;
            Obj.ParUpdated = true;
        end
        
        
        function set.Base(Obj,Val)
            % setter
            Obj.Base   = Val;
            Obj.ParUpdated = true;
        end
    end
    
    
    methods
        function [FileName,Path] = construct_filename(Obj,varargin)
            % Construct image/catalog file name based on the LAST/ULTRASAT standard
            % Package: +imUtil/+util/+file
            % Description: Return data product file name and path according to the
            %              LAST/ULTRASAT standard.
            %              <ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>
            % Input  : * Pairs of ...,key,val,... Possible keywords include:
            %            'ProjName' - Default is 'LAST.0.1'.
            %            'Date' - If empty, then will use current computer time, and
            %                   will assume the time is in UTC.
            %                   If numeric then assume the time is in JD, if char then
            %                   assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF
            %                   format.
            %                   Default is [].
            %            'TimeZone' - Time zone [hours]. Default is 2.
            %            'Filter' - Default is 'clear'.
            %            'FieldID' - Default is ''.
            %            'FormatFieldID' - Formatting of FieldID if is given as number.
            %                   Default is '%06d'.
            %            'Type' - either bias, dark, domeflat, twflat, skyflat, fringe,
            %                   sci, wave.
            %                   Default is 'sci'.
            %            'Level' - either log, raw, proc, stack, coadd, ref.
            %                   Default is 'raw'.
            %            'SubLevel' - Options are:
            %                   n - normal
            %                   s - proper subtraction S
            %                   sp - proper subtraction S tag.
            %                   d - proper subtraction D
            %                   t - Translient
            %                   r - proper coaddition R
            %                   m - matched filter with unspecified filter
            %                   Default is ''.
            %            'Product' - either: im, back, var, imflag, exp, Nim, psf, cat, spec.
            %                   Default is 'im'.
            %            'Version' - Default is 1.
            %            'FormatVersion' - Formatting of Version if is given as number.
            %                   Default is '%03d'.
            %            'FileType' - Default is 'fits'.
            %            'RefVersion' - Reference image version. Default is 1.
            %            'FormatRefVersion' - Format for numeric reference version.
            %                   Default is '%03d'.
            %            'SubDir' - This is the area/location directory below the
            %                   coadd/ref directory. Default is ''.
            %            'DataDir' - Default is 'data'.
            %            'Base' - Default is '/home/last'.
            % Output : -File name.
            %          - Path string.
            % Example: FileName=imUtil.util.file.construct_filename
            %          [FileName,Path]=imUtil.util.file.construct_filename('FieldID',100)


            InPar = inputParser;
            addOptional(InPar,'ProjName',Obj.ProjName);
            addOptional(InPar,'Date',Obj.Date); % if empty use now | JD | full string
            addOptional(InPar,'Filter',Obj.Filter);
            addOptional(InPar,'FieldID',Obj.FieldID);
            addOptional(InPar,'FormatFieldID',Obj.FormatFieldID);
            addOptional(InPar,'Type',Obj.Type);
            addOptional(InPar,'Level',Obj.Level);
            addOptional(InPar,'SubLevel',Obj.SubLevel);
            addOptional(InPar,'Product',Obj.Product);
            addOptional(InPar,'Version',Obj.Version);
            addOptional(InPar,'FormatVersion',Obj.FormatVersion);
            addOptional(InPar,'FileType',Obj.FileType);
            addOptional(InPar,'TimeZone',Obj.TimeZone);
            addOptional(InPar,'RefVersion',Obj.RefVersion);
            addOptional(InPar,'FormatRefVersion',Obj.FormatRefVersion);
            addOptional(InPar,'SubDir',Obj.SubDir);
            addOptional(InPar,'DataDir',Obj.DataDir);
            addOptional(InPar,'Base',Obj.Base);
            parse(InPar,varargin{:});
            InPar = InPar.Results;

            % copy new inputs into object properties
            PropName = fieldnames(InPar);
            Nprop    = numel(PropName);
            for Iprop=1:1:Nprop
                Obj.(PropName{Iprop}) = InPar.(PropName{Iprop});
            end
            
            KeyVal = imUtil.util.struct2keyval(InPar);
            [FileName,Path] = imUtil.util.file.construct_filename(KeyVal{:});
            Obj.FileName    = FileName;
            Obj.FullName    = sprintf('%s%s%s',Path,filesep,FileName);
            Obj.Path        = Path;
            

        end

        
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');
    
            p = ImagePath;
            fn = p.FullName;
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
            Result = true;
        end
    end    
        
    
    
end