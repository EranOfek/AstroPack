function FileName=filename_constructor(ProjName,StrDate,Field,Filter,Detector,SubImage,Type,Proc,Ext)
% 
% Package: @imUtil.util
% Input  : - Project name string.
%          - Str date, or numeric JD.
%          - Field number (up to 6 digits), or name
%          - Filter
%          - Detector
%          - SubImage number, 0 for full. up to 3 digits
%          - Image type: bias, dark, flat, domeflat, focus, science
%          - Processing stage: raw, proc, back, var, nim,  exp, mask, psf, cat
%          - Extension: fits, hdf5
% Output : - A cell array of file names.
% Example: FileName =
% imUtil.util.filename_constructor('LAST_1.1',2451545,1,'mono','qhy600',0,'science','raw','fits')

if isnumeric(StrDate)
    StrDate = convert.time(StrDate,'JD','StrDate');
    StrDate = StrDate{1};
end

if isnumeric(Field)
    Field = sprintf('%06d',Field);
end


% {bias |dark |flat|focus|science}
% <ProjName>_<date>_<field>_<filter>_<detector>_<subimage>_<type>_<proc>.Ext
% <type> - bias, dark, flat, domeflat, focus, science
% <proc> - processing stage: raw, proc, back, var, nim,  exp, mask, psf, cat
% <subimage> - 0 for full - number
% LAST1.1_yyyymmdd.ffffff_mono_qhy600_1_science_raw


FileName = sprintf('%s_%s_%s_%s_%s_%03d_%s_%s.%s',ProjName,StrDate,Field,Filter,Detector,SubImage,Type,Proc,Ext);