function Prop=filename2prop(FileName,ConvertField)
% Convert file name to properties
% Package: +imUtil.util.file
% Input  : - A file name or a cell array of file names
%          - Flag indicating if to convert Field ID to numeric.
%            Default is false.
% Output : - A structure array of properties constructed from the file
%            name.
% Example: Prop=imUtil.util.file.filename2prop('LAST_20200914.222852.890_clear_0.54_dark_proc.n_var_001.fits')

if nargin<2
    ConvertField = false;
end

if ~iscell(FileName)
    FileName = {FileName};
end

Nfile = numel(FileName);
for Ifile=1:1:Nfile
    Splitted = regexp(FileName{Ifile},'_','split');
    
    Prop(Ifile).ProjName = Splitted{1};
    Prop(Ifile).Date     = Splitted{2};
    DateVec              = datevec(Prop(Ifile).Date,'yyyymmdd.HHMMSS.FFF');
    Prop(Ifile).JD       = celestial.time.julday(DateVec([3 2 1 4 5 6]));
    Prop(Ifile).Filter   = Splitted{3};
    if ConvertField
        Prop(Ifile).Field    = str2double(Splitted{4});
    else
        Prop(Ifile).Field    = Splitted{4};
    end
    Prop(Ifile).Type     = Splitted{5};
    
    
    SplittedLevel = regexp(Splitted{6},'\.','split');
    switch numel(SplittedLevel)
        case 1
            Prop(Ifile).Level    = Splitted{6};
            Prop(Ifile).SubLevel = 'n';
        case 2
            Prop(Ifile).Level    = SplittedLevel{1};
            Prop(Ifile).SubLevel = SplittedLevel{2};
        otherwise
            error('Unknown Level format');
    end
    
    Prop(Ifile).Product    = Splitted{7};
    SplittedSuf = regexp(Splitted{8},'\.','split');
    Prop(Ifile).Version  = SplittedSuf{1};
    Prop(Ifile).FileType = SplittedSuf{2};
    
    
    
end
