function [Prop]=ztf_filename2prop(FileName,varargin)
% Extract information from ZTF file name
% Package: +VO/+ZTF
% Description: Extract information and image properties from ZTF file name.
%              Any per-epoch product parses - psfcat, sexcat, sciimg, mskimg -
%              since they differ only in the last token of the name.
% Input  : - ZTF File name, or a cell array of ZTF file name. A directory
%            part, and a .fz suffix, are ignored.
% Output : - A structure array with the following fields:
%            'FileType'
%            'Field'
%            'Filter'
%            'CCDID'
%            'QuadID'
%            'Year'
%            'Month'
%            'Day'
%            'Frac'
%            'JD'
%            'ImgTypeCode'  - 'o' (object), 'b'/'d'/'f' (calibration)
%            'FileFracDay'  - the 14-digit date+fraction token, as a char
%            'RCID'         - readout-channel id, 4*(CCDID-1)+QuadID-1 (0-63)
% License: GNU general public license version 3
%     By : Eran O. Ofek                    Oct 2019
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Prop]=VO.ZTF.ztf_filename2prop('ztf_20180815258044_000686_zr_c01_o_q1_psfcat.fits');
% Reliable: 2
%--------------------------------------------------------------------------


DefV.FilterList           = {'zg','zr','zi'};
InPar = InArg.populate_keyval(DefV,varargin,mfilename);


if ~iscell(FileName)
    FileName = {FileName};
end
Nfile = numel(FileName);


Prop = tools.struct.struct_def({'FileType','Field','Filter','FilterInd','CCDID','QuadID','ImgTypeCode','FileFracDay','Year','Month','Day','Frac','JD','RCID'},Nfile,1);
for Ifile=1:1:Nfile
    [~,Base,Ext] = fileparts(FileName{Ifile});
    Tmp = regexp([Base,Ext],'_','split');
    if numel(Tmp)<8
        error('Unsupported ztf file name: %s', FileName{Ifile});
    end
    % Any per-epoch product of the same exposure is named the same way and
    % differs only in the last token, so the type is read off rather than
    % switched on: psfcat, sexcat, sciimg, mskimg, ... all parse here. The
    % psfcat fields below are exactly what this function always returned.
    Prop(Ifile).FileType = lower(regexprep(Tmp{end},'\.fits(\.fz)?$',''));
    Prop(Ifile).Field    = str2double(Tmp{3});
    Prop(Ifile).Filter   = Tmp{4};
    Prop(Ifile).FilterInd= find(strcmp(Prop(Ifile).Filter ,InPar.FilterList));
    Prop(Ifile).CCDID    = str2double(Tmp{5}(2:3));
    if numel(Tmp{7})>1 && strcmpi(Tmp{7}(1),'q')
        Prop(Ifile).QuadID = str2double(Tmp{7}(2));
    else
        Prop(Ifile).QuadID = NaN;
    end
    Prop(Ifile).ImgTypeCode = Tmp{6};
    Prop(Ifile).FileFracDay = Tmp{2};
    Date    = datevec(Tmp{2}(1:8),'yyyymmdd');

    FracStr = Tmp{2}(9:end);
    Ndigit  = numel(FracStr);
    Frac    = str2double(FracStr)./(10.^Ndigit);
    Prop(Ifile).Year    = Date(1);
    Prop(Ifile).Month   = Date(2);
    Prop(Ifile).Day     = Date(3);
    Prop(Ifile).Frac    = Frac;
    Prop(Ifile).JD      = convert.date2jd([Prop(Ifile).Day, Prop(Ifile).Month, Prop(Ifile).Year, Frac]);
    % readout-channel id, the 0-63 index ZTF uses for a quadrant
    Prop(Ifile).RCID    = 4.*(Prop(Ifile).CCDID - 1) + Prop(Ifile).QuadID - 1;
end
            
            