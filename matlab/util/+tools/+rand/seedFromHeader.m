function [Seed, Info] = seedFromHeader(Headers, Args)
    % Deterministic RNG seed derived from observational header keywords.
    %
    %   A stage that draws random numbers is not reproducible run to run
    %   unless its seed is a property of the data rather than of the run
    %   (issue #1319). This builds such a seed from the keywords that identify
    %   an observation, so reprocessing a visit, or reusing one reference,
    %   repeats the same draw.
    %
    %   Two stages working on the same image must not share a stream, or their
    %   draws move together. Pass a different 'Salt' per stage - the drawing
    %   function's own name is a good choice.
    %
    %   The seed is meant for a private stream, not for rng():
    %       Stream = RandStream('threefry', 'Seed',Seed);
    %       X      = randn(Stream, Ny, Nx);
    %   rng() would reseed the global generator and so change the draws of
    %   every caller up the stack.
    %
    % Input  : - An AstroHeader or AstroImage array, or anything with a getVal
    %            method or a HeaderData property. Every element is folded into
    %            the one seed, so pass [AD.New, AD.Ref] to tie the seed to the
    %            pair rather than to one side.
    %          * ...,key,val,...
    %            'Salt' - Char array naming the drawing stage, folded into the
    %                   hash so two stages on the same image get independent
    %                   streams. Default is ''.
    %            'KeyList' - Keywords to look for, in order. Missing ones come
    %                   back NaN from getVal and are dropped, so the list may
    %                   be longer than any one header. Note that CROPID is the
    %                   only default that separates crops of the same
    %                   exposure; without it every crop of a visit shares a
    %                   seed.
    %                   Default is {'MIDJD','JD','CROPID','FIELDID',
    %                   'MOUNTNUM','CAMNUM','COUNTER','EXPTIME'}.
    %            'FallbackVals' - Numeric vector, or a function handle
    %                   returning one, hashed instead when no keyword is found
    %                   at all. A handle is only evaluated in that case, so an
    %                   expensive fallback costs nothing on the normal path.
    %                   Give something that still separates the images.
    %                   Default is [].
    %            'Warn' - Warn when the fallback is taken. Default is true.
    % Output : - A uint32 seed for RandStream.
    %          - A struct with the keywords found, the values hashed, and
    %            whether the fallback was taken.
    % Author : Claude (Sep 2026)
    % Example: Seed   = tools.rand.seedFromHeader([AD.New, AD.Ref], 'Salt','replaceNaN:New');
    %          Stream = RandStream('threefry', 'Seed',Seed);

    arguments
        Headers
        Args.Salt                 = '';
        Args.KeyList              = {'MIDJD','JD','CROPID','FIELDID','MOUNTNUM','CAMNUM','COUNTER','EXPTIME'};
        Args.FallbackVals         = [];
        Args.Warn logical         = true;
    end

    KeyList = Args.KeyList;
    if ischar(KeyList) || isstring(KeyList)
        KeyList = cellstr(KeyList);
    end

    Nhead = numel(Headers);
    Nkey  = numel(KeyList);
    Vals  = nan(Nhead, Nkey);
    Found = false(1, Nkey);

    for Ihead=1:1:Nhead
        H = local_header(Headers(Ihead));
        if isempty(H)
            continue
        end
        for Ikey=1:1:Nkey
            try
                % A cell overrides the dictionary, i.e. an exact keyword search.
                Val = H.getVal(KeyList(Ikey));
            catch
                continue
            end
            if isnumeric(Val) && isscalar(Val) && isfinite(Val)
                Vals(Ihead, Ikey) = Val;
                Found(Ikey)       = true;
            end
        end
    end

    Info = struct('Keys',{KeyList(Found)}, 'Fallback',false, 'Vals',[]);

    Flat = Vals(:).';
    Flat = Flat(isfinite(Flat));

    if isempty(Flat)
        Info.Fallback = true;
        Fb = Args.FallbackVals;
        if isa(Fb, 'function_handle')
            Fb = Fb();
        end
        Flat = double(Fb(:)).';
        Flat = Flat(isfinite(Flat));
        if Args.Warn
            if isempty(Flat)
                warning('tools:rand:seedFromHeader:noKeysNoFallback', ...
                        ['No header keyword found and no usable fallback: the seed ' ...
                         'comes from the salt alone, so every image sharing it draws ' ...
                         'the same numbers.']);
            else
                warning('tools:rand:seedFromHeader:noKeys', ...
                        'No header keyword found; seeding on the fallback values instead.');
            end
        end
    end
    Info.Vals = Flat;

    %   FNV-1a over the formatted values and the salt.
    %
    %   Deliberately not tools.checksum.xxhash: that one is mex only, and a
    %   seed has to come out the same on a machine where the mex was never
    %   built. Not keyHash either - it is not documented as stable across
    %   MATLAB releases, which is the one property a reproducibility seed
    %   cannot do without. 12 significant digits keep neighbouring JDs apart.
    Str  = [sprintf('%.12g|', Flat), char(Args.Salt)];
    Hash = uint32(2166136261);
    for Ichar=1:1:numel(Str)
        Hash = bitxor(Hash, uint32(double(Str(Ichar))));
        Hash = uint32(mod(double(Hash).*16777619, 4294967296));
    end
    Seed = Hash;
end

function H = local_header(Obj)
    % The header of an AstroImage, or the object itself when it is a header.
    H = [];
    if ~(isobject(Obj) || isstruct(Obj))
        return
    end
    if isprop(Obj, 'HeaderData') && ~isempty(Obj.HeaderData)
        H = Obj.HeaderData;
    elseif ismethod(Obj, 'getVal')
        H = Obj;
    end
end
