function Report = compareMaskToSection(Input, Args)
    % Compare an image's mask bit against flag_ccdsec(size, SECTION, InOut).
    % Description: For a geometric mask bit that is supposed to be a pure
    %   function of a header CCDSEC-type section (the LAST 'Overlap' bit is
    %   exactly CCDSEC \ UNIQSEC), this predicts the bit's footprint from the
    %   header section via imUtil.ccdsec.flag_ccdsec(size, SECTION, InOut) and
    %   compares it, pixel for pixel, to the actual mask. It reports the
    %   over-set pixels (flagged but not predicted), under-set pixels
    %   (predicted but not flagged), and the per-edge inner-boundary overshoot
    %   (how far the actual band reaches past the section on each edge).
    %
    %   This is the direct check for the coadd 'Overlap' bug: the single-frame
    %   (sci_proc) masks match their UNIQSEC exactly, but the coadd (sci_coadd)
    %   'Overlap' band is smeared inward by the dither amplitude, so it no
    %   longer equals flag_ccdsec(size, UNIQSEC, false). A clean image returns
    %   Match=true and zero overshoot; a smeared coadd returns the +N px
    %   overshoot on the dither-ward edges.
    %
    % Input  : - Input, one of:
    %            * An AstroImage array with MaskData and HeaderData populated
    %              (HeaderData must carry the section keyword). For LAST load
    %              with AstroImage.readProducts(ImageFile,'ExtraOutProduct',"Mask").
    %            * A char/string directory path. Globs the FileType Image
    %              products (their header carries UNIQSEC), reads the paired
    %              _Mask_ file for the actual mask.
    %          * ...,key,val,...
    %            'BitName'     - Mask bit to check. Default 'Overlap'.
    %            'SectionKey'  - Header keyword giving the section fed to
    %                            flag_ccdsec. Default 'UNIQSEC'.
    %            'InOut'       - 3rd arg of flag_ccdsec: flag inside (true) or
    %                            outside (false) the section. The 'Overlap'
    %                            band is OUTSIDE UNIQSEC, so Default false.
    %            'BitDictName' - BitDictionary name. Default
    %                            'BitMask.Image.Default'.
    %            'FileType'    - (path mode) product token globbed as
    %                            '*<FileType>_Image_*.fits'. Default 'sci_coadd'.
    %            'Recursive'   - (path mode) walk sub-tree. Default true.
    %            'FieldId'     - (path mode) keep files with _<FieldId>_.
    %                            Default '' (no filter).
    %            'Filter'      - (path mode) keep files with _<Filter>_.
    %                            Default '' (no filter).
    %            'MaxImages'   - (path mode) cap. Default Inf.
    %            'Verbose'     - Per-image + summary printout. Default true.
    % Output : - Report struct with:
    %            .PerImage(k) - .Key, .CropID, .SizeYX [ny nx],
    %                .Section [xmin xmax ymin ymax], .NActual, .NPred,
    %                .NExtra (actual & ~pred = over-set), .NMissing
    %                (pred & ~actual), .FracExtra, .Overshoot [L R B T]
    %                (px the actual band reaches past the section inner edge;
    %                 NaN where that edge has no margin), .Match.
    %            .Aggregate - .NImages, .NMatch, .NMismatch, .TotalExtra,
    %                .TotalMissing, .MedianOvershoot [L R B T],
    %                .MaxOvershoot [L R B T].
    %            .BitName .BitIndex .SectionKey .InOut .Args
    % Author : D. Kovaleva (Aug 2026)
    % See also: imUtil.ccdsec.flag_ccdsec,
    %           pipeline.last.quality.masks.maskBitStatistics,
    %           pipeline.last.quality.masks.checkMaskConsistency.
    % Example:
    %   % One loaded coadd (AI = crops with Mask+header):
    %   R = pipeline.last.quality.masks.compareMaskToSection(AI);
    %   % A directory of coadds; find the smeared ones:
    %   R = pipeline.last.quality.masks.compareMaskToSection( ...
    %           '/archimedes/test1000/.../204158v1', 'FileType','sci_coadd');
    %   % Confirm single frames are clean:
    %   R = pipeline.last.quality.masks.compareMaskToSection(Dir,'FileType','sci_proc');
    arguments
        Input
        Args.BitName     (1,:) char = 'Overlap'
        Args.SectionKey  (1,:) char = 'UNIQSEC'
        Args.InOut             logical = false
        Args.BitDictName (1,:) char = 'BitMask.Image.Default'
        Args.FileType    (1,:) char = 'sci_coadd'
        Args.Recursive         logical = true
        Args.FieldId     (1,:) char = ''
        Args.Filter      (1,:) char = ''
        Args.MaxImages   (1,1) double = Inf
        Args.Verbose           logical = true
    end

    BD = BitDictionary(Args.BitDictName);
    [BitInd, ~] = BD.name2bit({Args.BitName});
    BitInd = double(BitInd(1));
    if isnan(BitInd)
        error('pipeline:last:quality:masks:compareMaskToSection:BadBit', ...
              'Bit "%s" not in dictionary %s', Args.BitName, Args.BitDictName);
    end
    BitVal = bitshift(uint32(1), uint32(BitInd));

    % --- Gather (mask, section, key, cropid) records ---------------------
    Rec = struct('Key',{},'CropID',{},'Mask',{},'Section',{});
    if isa(Input, 'AstroImage')
        for I = 1:numel(Input)
            [M, Sec] = i_fromAI(Input(I), Args.SectionKey);
            if isempty(M) || isempty(Sec); continue; end
            Rec(end+1) = struct('Key',sprintf('AI(%d)',I),'CropID',NaN,'Mask',M,'Section',Sec); %#ok<AGROW>
        end
    elseif ischar(Input) || isstring(Input)
        Files = i_discover(char(Input), Args.FileType, Args.Recursive, Args.FieldId, Args.Filter);
        if isfinite(Args.MaxImages) && Args.MaxImages < numel(Files)
            Files = Files(1:Args.MaxImages);
        end
        for I = 1:numel(Files)
            [M, Sec, Crop] = i_fromFile(Files{I}, Args.SectionKey);
            if isempty(M) || isempty(Sec); continue; end
            [~,fn,~] = fileparts(Files{I});
            Rec(end+1) = struct('Key',fn,'CropID',Crop,'Mask',M,'Section',Sec); %#ok<AGROW>
        end
    else
        error('pipeline:last:quality:masks:compareMaskToSection:BadInput', ...
              'Input must be an AstroImage array or a directory path.');
    end
    if isempty(Rec)
        error('pipeline:last:quality:masks:compareMaskToSection:NoData', ...
              'No usable (mask + %s) records found.', Args.SectionKey);
    end

    % --- Compare each ----------------------------------------------------
    N = numel(Rec);
    PI = repmat(i_piTemplate(), 1, N);
    for I = 1:N
        M   = uint32(Rec(I).Mask);
        Sec = Rec(I).Section;
        [ny, nx] = size(M);
        Actual = bitand(M, BitVal) ~= 0;
        Pred   = imUtil.ccdsec.flag_ccdsec([ny nx], Sec, Args.InOut);
        Extra   = Actual & ~Pred;      % flagged but not predicted (over-set)
        Missing = Pred & ~Actual;      % predicted but not flagged (under-set)
        PI(I).Key      = Rec(I).Key;
        PI(I).CropID   = Rec(I).CropID;
        PI(I).SizeYX   = [ny nx];
        PI(I).Section  = Sec;
        PI(I).NActual  = nnz(Actual);
        PI(I).NPred    = nnz(Pred);
        PI(I).NExtra   = nnz(Extra);
        PI(I).NMissing = nnz(Missing);
        PI(I).FracExtra = PI(I).NExtra / max(PI(I).NPred,1);
        PI(I).Overshoot = i_edgeOvershoot(Actual, Sec);
        PI(I).Match    = (PI(I).NExtra==0 && PI(I).NMissing==0);
        if Args.Verbose
            O = PI(I).Overshoot;
            fprintf('%-52s crop=%2d  extra=%6d missing=%6d  overshoot[L R B T]=[%s]  %s\n', ...
                i_short(Rec(I).Key), i_num(Rec(I).CropID), PI(I).NExtra, PI(I).NMissing, ...
                i_edgeStr(O), i_tf(PI(I).Match,'MATCH','MISMATCH'));
        end
    end

    % --- Aggregate -------------------------------------------------------
    OV = reshape([PI.Overshoot], 4, []).';   % N x 4
    Agg.NImages      = N;
    Agg.NMatch       = sum([PI.Match]);
    Agg.NMismatch    = N - Agg.NMatch;
    Agg.TotalExtra   = sum([PI.NExtra]);
    Agg.TotalMissing = sum([PI.NMissing]);
    Agg.MedianOvershoot = i_col(@(v)median(v,'omitnan'), OV);
    Agg.MaxOvershoot    = i_col(@(v)max(v,[],'omitnan'), OV);

    Report.PerImage   = PI;
    Report.Aggregate  = Agg;
    Report.BitName    = Args.BitName;
    Report.BitIndex   = BitInd;
    Report.SectionKey = Args.SectionKey;
    Report.InOut      = Args.InOut;
    Report.Args       = Args;

    if Args.Verbose
        fprintf(['compareMaskToSection [%s vs flag_ccdsec(%s)]: %d img, %d MATCH, %d MISMATCH\n' ...
                 '  total over-set px=%d  median overshoot[L R B T]=[%s]  max=[%s]\n'], ...
                 Args.BitName, Args.SectionKey, N, Agg.NMatch, Agg.NMismatch, ...
                 Agg.TotalExtra, i_edgeStr(Agg.MedianOvershoot), i_edgeStr(Agg.MaxOvershoot));
    end
end


% ==== helpers ===========================================================

function T = i_piTemplate()
    T = struct('Key','','CropID',NaN,'SizeYX',[NaN NaN],'Section',[NaN NaN NaN NaN], ...
               'NActual',0,'NPred',0,'NExtra',0,'NMissing',0,'FracExtra',NaN, ...
               'Overshoot',[NaN NaN NaN NaN],'Match',false);
end


function [M, Sec] = i_fromAI(AI, Key)
    M = []; Sec = [];
    try
        if ~isempty(AI.MaskData) && ~isempty(AI.MaskData.Image)
            M = AI.MaskData.Image;
        end
        v = AI.HeaderData.getVal(Key, 'ReadCCDSEC', true);
        if numel(v)>=4 && all(isfinite(v(1:4))); Sec = v(1:4); end
    catch
    end
end


function [M, Sec, Crop] = i_fromFile(ImageFile, Key)
    M = []; Sec = []; Crop = NaN;
    MaskFile = strrep(ImageFile, '_Image_', '_Mask_');
    try
        v = AstroHeader(ImageFile).getVal(Key, 'ReadCCDSEC', true);   % section lives in Image header
        if numel(v)>=4 && all(isfinite(v(1:4))); Sec = v(1:4); end
        if isfile(MaskFile); M = fitsread(MaskFile); end
        [~,fn,~] = fileparts(ImageFile);
        tok = regexp(fn, '_(\d+)_sci_', 'tokens', 'once');
        if ~isempty(tok); Crop = str2double(tok{1}); end
    catch
    end
end


function Ov = i_edgeOvershoot(Actual, Sec)
    % Overshoot [L R B T] = px the actual band reaches PAST the section inner
    % edge on each edge that has a margin; NaN where no margin. Middle-slice
    % measurement avoids the corners.
    [ny, nx] = size(Actual);
    xmin=Sec(1); xmax=Sec(2); ymin=Sec(3); ymax=Sec(4);
    Ov = [NaN NaN NaN NaN];
    Y0 = round((ymin+ymax)/2); X0 = round((xmin+xmax)/2);
    Y0 = min(max(Y0,1),ny); X0 = min(max(X0,1),nx);
    rowA = Actual(Y0,:); colA = Actual(:,X0).';
    if xmin>1                                   % L
        a = find(~rowA,1,'first')-1;            % last flagged col on the left
        if isempty(a); a = nx; end
        Ov(1) = a - (xmin-1);
    end
    if xmax<nx                                  % R
        b = find(~rowA,1,'last')+1;             % first flagged col on the right
        if b>nx+1; b = xmax+1; end
        Ov(2) = (xmax+1) - b;
    end
    if ymin>1                                   % B
        c = find(~colA,1,'first')-1;
        if isempty(c); c = ny; end
        Ov(3) = c - (ymin-1);
    end
    if ymax<ny                                  % T
        d = find(~colA,1,'last')+1;
        if d>ny+1; d = ymax+1; end
        Ov(4) = (ymax+1) - d;
    end
end


function Files = i_discover(BaseDir, FileType, Recursive, FieldId, Filter)
    Pattern = ['*' FileType '_Image_*.fits'];
    if Recursive; D = dir(fullfile(BaseDir,'**',Pattern)); else; D = dir(fullfile(BaseDir,Pattern)); end
    D = D(~[D.isdir]);
    Files = fullfile({D.folder},{D.name});
    Names = {D.name};
    if ~isempty(FieldId); k=contains(Names,['_' FieldId '_']); Files=Files(k); Names=Names(k); end
    if ~isempty(Filter);  k=contains(Names,['_' Filter '_']);  Files=Files(k); end
end


function v = i_col(fun, OV)
    if isempty(OV); v = [NaN NaN NaN NaN]; return; end
    v = arrayfun(@(j) fun(OV(:,j)), 1:4);
end

function s = i_edgeStr(v);  s = sprintf('%+g %+g %+g %+g', v(1),v(2),v(3),v(4)); end
function s = i_short(k);    if numel(k)>52; s=['...' k(end-48:end)]; else; s=k; end; end
function s = i_num(x);      if isnan(x); s=-1; else; s=x; end; end
function s = i_tf(c,a,b);   if c; s=a; else; s=b; end; end
