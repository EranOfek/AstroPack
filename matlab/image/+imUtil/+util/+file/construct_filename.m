function [FileName,Path] = construct_filename(varargin)
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
addOptional(InPar,'ProjName','LAST.0.1');
addOptional(InPar,'Date',[]); % if empty use now | JD | full string
addOptional(InPar,'Filter','clear');
addOptional(InPar,'FieldID','');
addOptional(InPar,'FormatFieldID','%06d');
addOptional(InPar,'Type','sci');
addOptional(InPar,'Level','raw');
addOptional(InPar,'SubLevel','');
addOptional(InPar,'Product','im');
addOptional(InPar,'Version',1);
addOptional(InPar,'FormatVersion','%03d');
addOptional(InPar,'FileType','fits');
addOptional(InPar,'TimeZone',2);
addOptional(InPar,'RefVersion',1);
addOptional(InPar,'FormatRefVersion','%03d');
addOptional(InPar,'SubDir','');
addOptional(InPar,'DataDir','data');
addOptional(InPar,'Base','/home/last');
parse(InPar,varargin{:});
InPar = InPar.Results;


%<ProjName>.<TelescopeID>_YYYYMMDD.HHMMSS.FFF_<filter>_<FieldID>_<type>_<level>.<sub level>_<Product>_<version>.<FileType>


if isempty(InPar.Date)
    InPar.Date = celestial.time.julday;
end

if isnumeric(InPar.Date)
    % assume JD
    InPar.Date = convert.time(InPar.Date,'JD','StrDate');
    InPar.Date = InPar.Date{1};
end

if isnumeric(InPar.FieldID)
    InPar.FieldID = sprintf(InPar.FormatFieldID,InPar.FieldID);
end



Path=imUtil.util.file.construct_path('Date',InPar.Date,...
                                     'TimeZone',InPar.TimeZone,...
                                     'Level',InPar.Level,...
                                     'Type',InPar.Type,...
                                     'RefVersion',InPar.RefVersion,...
                                     'FormatRefVersion',InPar.FormatRefVersion,...
                                     'SubDir',InPar.SubDir,...
                                     'DataDir',InPar.DataDir,...
                                     'Base',InPar.Base);
Path = sprintf('%s%s',Path,filesep);
% clean from multiple /
Path = regexprep(Path,sprintf('%s{2,5}',filesep),'/');

                                     

% verify Type
switch InPar.Type
    case {'bias','dark','flat','domeflat','twflat','skyflat','fringe','sci','wave'}
        % ok
    otherwise
        error('Unknown Type option');
end

% verify Level
switch InPar.Level
    case {'log','raw','proc','stack','ref','coadd'}
        % ok
    otherwise
        error('Unknown Level option');
end

if isempty(InPar.SubLevel)
    MergedLevel = InPar.Level;
else
    MergedLevel = sprintf('%s.%s',InPar.Level,InPar.SubLevel);    
end

% verify Product
switch InPar.Product
    case {'im','back','var','exp','nim','psf','cat','spec','pixflag','imflag'}
        % ok
    otherwise
        error('Unknown Product option');
end

if isnumeric(InPar.Version)
    InPar.Version = sprintf(InPar.FormatVersion,InPar.Version);
end

% remove - and : from date
if iscellstr(InPar.Date)
    InPar.Date = InPar.Date{1};
end
InPar.Date = strrep(InPar.Date,'-','');
InPar.Date = strrep(InPar.Date,'T','.');
InPar.Date = strrep(InPar.Date,':','');


FileName = sprintf('%s_%s_%s_%s_%s_%s_%s_%s.%s',InPar.ProjName,...
                           InPar.Date,...
                           InPar.Filter,...
                           InPar.FieldID,...
                           InPar.Type,...
                           MergedLevel,...
                           InPar.Product,...
                           InPar.Version,...
                           InPar.FileType);
                       
                       