function Link=navigator_link(RA,Dec, DR)
% Given J2000 equatorial coordinates get link to SDSS navigator image.
% Package: VO.SDSS
% Description: Get link to SDSS navigator image
% Input  : - J2000.0 R.A. [rad, [H M S], sexagesimal string]
%          - J2000.0 Dec. [rad, [Sign D M S], sexagesimal string]
%          - DR. Default is 17.
% Output : - Cell array of links.
% Author : Eran Ofek (Oct 2018)
% Example: Link=VO.SDSS.navigator_link(pi,0.4)
% Reliable: 2

arguments
    RA
    Dec
    DR    = 17;
end

RAD = 180./pi;

%FindingChartURL   = 'http://cas.sdss.org/dr7/en/tools/chart/chart.asp';
%FindingChartURL = 'http://skyserver.sdss3.org/dr8/en/tools/chart/chart.asp';
%FindingChartNaviURL = 'http://cas.sdss.org/dr7/en/tools/chart/navi.asp';
%FindingChartNaviURL = 'http://skyserver.sdss3.org/dr9/en/tools/chart/navi.asp';
FindingChartNaviURL = sprintf('http://skyserver.sdss.org/dr%d/en/tools/chart/navi.aspx',DR);
%FindingChartImURL = 'http://casjobs.sdss.org/ImgCutoutDR7/getjpeg.aspx';
%FindingChartImURL = 'http://skyserver.sdss.org/dr12/en/tools/chart/image.aspx';
%Apos            = '''';

RA  = celestial.coo.convertdms(RA,'gH','r');
RA  = RA.*RAD;
Dec = celestial.coo.convertdms(Dec,'gD','R');
Dec = Dec.*RAD;


Nim = numel(RA);
Link = cell(1,Nim);
for Iim=1:1:Nim
   %--- Manual link with control ---
   Link{Iim} = sprintf('%s?ra=%f&dec=%f',FindingChartNaviURL,RA(Iim),Dec(Iim));
end
