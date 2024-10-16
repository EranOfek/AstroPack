function [Table,Str]=wget_ztf_phot(RA,Dec,Band,varargin)
% wget photometry and astrometry of a source/s from IRSA database.
% Package: VO.ZTF
% Description: wget photometry and astrometry of a source/s from IRSA
%              database.
% Input  : - Vector of J2000.0 R.A. [default in deg].
%          - Vector of J2000.0 Dec. [default in deg].
%          - Vector of Band names in a cell array or a vector of numbers
%            corresponding to band names.
%            'g' - 1; 'r' - 2; 'i' - 3
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'CooUnits' - {'deg'|'rad'}. Default is 'deg'.
%            'Radius'   - Search radius. Default is 5.
%            'RadiusUnits'- Radius units. Default is 'arcsec'.
%            'User'     - User name. See io.files.read_user_pass_file
%                         for options.
%            'Pass'     - Password.
%            More hidden parameters.
% Output : - A table containing the ZTF photometry.
%          - String containing the XML table.
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Sep 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Reference: https://irsa.ipac.caltech.edu/docs/program_interface/ztf_lightcurve_api.html
% Example: [Table,Str]=VO.ZTF.wget_ztf_phot(298.0025,29.87147,1);
% Reliable: 2
%--------------------------------------------------------------------------

BandDic={'g','r','i'};

DefV.CooUnits             = 'deg';
DefV.Radius               = 5;
DefV.RadiusUnits          = 'arcsec';
DefV.User                 = {'/home/eran/matlab/passwords/ztf_ipac_pass'}; 
DefV.Pass                 = [];
DefV.wgetProg             = 'wget';   % 'wget' | 'curl'
DefV.BaseURL              = 'https://irsa.ipac.caltech.edu';
DefV.AccountURL           = '/account/signon/login.do';
DefV.CookiesFile          = 'cookies.txt';
DefV.Wait                 = 1;  % seconds
DefV.email                = 'eran.ofek@weizmann.ac.il'; % note that in this service the e-mail is copuled to user/pass!
DefV.BaseURL              = 'https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?';
DefV.FilterDic            = {'zg','zr','zi'};
DefV.SaveFile             = '';
InPar = InArg.populate_keyval(DefV,varargin,mfilename);

RadiusDeg = convert.angular(InPar.RadiusUnits,'deg',InPar.Radius);

if ischar(RA)
    RA = celestial.coo.convertdms(RA,'gH','d');
end
if ischar(Dec)
    Dec = celestial.coo.convertdms(Dec,'gD','d');
end


RA    = convert.angular(InPar.CooUnits,'deg',RA);
Dec   = convert.angular(InPar.CooUnits,'deg',Dec);


if isnumeric(Band)
    Band = BandDic(Band);
elseif iscell(Band)
    Band = Band;
else
    error('Unknown Band option format');
end
Nband = numel(Band);


% get user/pass from passwords file
if (iscell(InPar.User))
    [InPar.User,InPar.Pass]=io.files.read_user_pass_file(InPar.User{1});
end

%wget --save-cookies=cookies.txt "https://irsa.ipac.caltech.edu/account/signon/login.do?josso_cmd=login&josso_username=eran.ofek@weizmann.ac.il&josso_password=XXX"
%wget --load-cookies=cookies.txt "https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?POS=CIRCLE+314.3880692+50.6085696+0.0028&BANDNAME=g&NOBS_MIN=3&TIME=58194.0+58735.0&BAD_CATFLAGS_MASK=32768&FORMAT=ipac_table" -O out.tbl


% set up the IRSA cookies
[Stat,Res]=VO.ZTF.irsa_set_cookies('CookiesFile',InPar.CookiesFile,...
                                   'BaseURL',InPar.BaseURL,...
                                   'AccountURL',InPar.AccountURL,...
                                   'User',InPar.User,...
                                   'Pass',InPar.Pass,...
                                   'wgetProg',InPar.wgetProg);




% https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?POS=CIRCLE%20298.0025%2029.87147%200.0014&BANDNAME=g
%[~,RR] = system('wget https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?POS=CIRCLE%20298.0025%2029.87147%200.0014&BANDNAME=g');
%[SS,RR] = system('wget https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?POS=CIRCLE 298.0025 29.87147 0.0014&BANDNAME=g');





Spacer = '%20';

N = numel(RA);
for I=1:1:N
    Ib = max(I,Nband);
    %&FORMAT=ipac_table',...
    URL = sprintf('%sPOS=CIRCLE %10.6f %10.6f %08.6f&BANDNAME=%s',... 
        InPar.BaseURL,RA(I),Dec(I),RadiusDeg,Band{Ib});
    URL = replace(URL,'   ',' ');
    URL = replace(URL,'  ',' ');

    %Options = weboptions('UserName',InPar.User,'Password',InPar.Pass);
    %webread(URL,Options);

    %system('wget --load-cookies=cookies.txt "https://irsa.ipac.caltech.edu/cgi-bin/ZTF/nph_light_curves?POS=CIRCLE+314.3880692+50.6085696+0.0028&BANDNAME=g" -O out.tbl');
    

    % OutFile = 'tmp.out';
    %CL = sprintf('wget --http-user=%s --http-passwd=%s -O %s "%s"',InPar.User,InPar.Pass,OutFile,URL);
    TmpFile = tempname;
    if isempty(InPar.User) || isempty(InPar.Pass)
        CL = sprintf('wget --no-check-certificate --auth-no-challenge "%s" -O %s',InPar.User,InPar.Pass,URL,TmpFile);
    else
        CL = sprintf('wget --no-check-certificate --load-cookies=%s "%s" -O %s',InPar.CookiesFile,URL,TmpFile);
    end
    %CL
    [Stat,Res] = system(CL);
    Str = io.files.file2str(TmpFile,'str');
    
    if ~isempty(InPar.SaveFile)
        movefile(TmpFile,InPar.SaveFile);
    end
        
    delete(TmpFile);
    
    %(URL)
    %Str=webread((URL));
    
    pause(InPar.Wait);
end

[Table] = VO.Util.read_votable(Str);

% populate filter code with filter number 
if (~isempty(Table))
    Nf = numel(InPar.FilterDic);
    FilterCode = nan(size(Table,1),1);
    for If=1:1:Nf
        FF = strcmp(Table.filtercode,InPar.FilterDic{If});
        FilterCode(FF) = If;
    end
    Table.filtercode = FilterCode;
end
