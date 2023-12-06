function Path=construct_path(varargin)
% Construct image/catalog file path based on the LAST/ULTRASAT standard
% Package: +imUtil/+util/+file
% Description: Construct file path for ULTRASAT/LAST standard.
%              The file path is described in the LAST/ULTRASAT file naming
%              convension document.
%              Options are:
%              /data/YYYY/MM/DD/raw/ - contains all the science raw data
%              /data/YYYY/MM/DD/log/  - contains all the log files
%              /data/YYYY/MM/DD/proc/ - contains all the single processed images including: image, mask, back (if provided), var (if provided), PSF (if provided), and catalogs.
%              /data/YYYY/MM/DD/calib/ - contains all the processed calibration images/variance/masks/catalogs
%              /data/YYYY/MM/DD/stacked/ - contains all the processed coadd images (coaddition of images of the same field taken continuously only) - images/masks/catalogs/PSF/subtraction products 
%              /data/ref/version<#>/area/ - All sky reference/coadd image - images/masks/catalogs/PSF
%              /data/coadd/area/ - arbitrary coadded images (coadd images of arbitrary field over arbitrary time periods) 
% Input  : * Pairs of ...,key,val,... Possible keywords include:
%            'Date' - If empty, then will use current computer time, and
%                   will assume the time is in UTC.
%                   If numeric then assume the time is in JD, if char then
%                   assume the time is in the YYYY-MM-DDTHH:MM:SS.FFF
%                   format.
%                   Default is [].
%            'TimeZone' - Time zone [hours]. Default is 2.
%            'Level' - either log, raw, proc, stack, coadd, ref.
%                   Default is 'raw'.
%            'Type' - either bias, dark, domeflat, twflat, skyflat, fringe,
%                   sci, wave.
%                   Default is 'sci'.
%            'RefVersion' - Reference image version. Default is 1.
%            'FormatRefVersion' - Format for numeric reference version.
%                   Default is '%03d'.
%            'SubDir' - This is the area/location directory below the
%                   coadd/ref directory. Default is ''.
%            'DataDir' - Default is 'data'.
%            'Base' - Default is '/home/last'.
% Output : - Path string.
%      By : Eran Ofek                            Aug 2020
% Example: Path=imUtil.util.file.construct_path
%          Path=imUtil.util.file.construct_path('Level','ref','SubDir','x')
%          Path=imUtil.util.file.construct_path('Level','proc','Type','bias')



InPar = inputParser;
addOptional(InPar,'Date',[]); % if empty use now | JD | full string
addOptional(InPar,'TimeZone',2); % hours
addOptional(InPar,'Level','raw'); 
addOptional(InPar,'Type','sci');
addOptional(InPar,'RefVersion',1);
addOptional(InPar,'FormatRefVersion','%03d');
addOptional(InPar,'SubDir',[]);
addOptional(InPar,'DataDir','data'); % if empty use now | JD | full string
addOptional(InPar,'Base','/home/last'); % if empty use now | JD | full string
parse(InPar,varargin{:});
InPar = InPar.Results;



% convert date to JD
if isempty(InPar.Date)
    JD = celestial.time.julday;
else
    if ischar(InPar.Date) || iscellstr(InPar.Date)
        JD = convert.time(InPar.Date,'StrDate','JD');
    else
        % already in JD
        JD = InPar.Date;
    end    
end


switch InPar.Level
    case 'ref'
        if isnumeric(InPar.RefVersion)
            InPar.RefVersion = sprintf(InPar.FormatRefVersion,InPar.RefVersion);
        end

        DateDir = sprintf('%s%s%s%s%s',filesep,...
                                   InPar.Level,...
                                   filesep,...
                                   'version',...
                                   InPar.RefVersion,...
                                   filesep,...
                                   InPar.SubDir);
    case 'coadd'
        DateDir = sprintf('%s%s%s%s%s',filesep,...
                                   InPar.Level,...
                                   filesep,...
                                   InPar.SubDir);
    otherwise
        

        % check TimeZone!
        % convert to JD
        %JD = convert.time(InPar.Date,'StrDate','JD');
        [Y,M,D]=imUtil.util.file.date_directory(JD,InPar.TimeZone);
        
        %LocalJD = JD + InPar.TimeZone./24;
        %FloorLocalJD = floor(LocalJD);
        %FloorDate = celestial.time.jd2date(FloorLocalJD);
        
        
        
        YearDir = sprintf('%04d',Y);
        MDir   = sprintf('%02d',M);
        DDir   = sprintf('%02d',D);
        MDDir  = sprintf('%s%s%s',MDir,filesep,DDir);
        
        
        
        if strcmp(InPar.Level,'proc') && ~strcmp(InPar.Type,'sci')
            % calibration
            Level = 'calib';
        else
            Level = InPar.Level;
        end
        
        DateDir = sprintf('%s%s%s%s%s%s',filesep,...
                                YearDir,...
                                filesep,...
                                MDDir,...
                                filesep,...
                                Level);
end


Path = sprintf('%s%s%s',InPar.Base,...
                        filesep,...
                        InPar.DataDir,...
                        DateDir);
   
% clean path from multiple /

Path = regexprep(Path,sprintf('%s{2,5}',filesep),'/');
