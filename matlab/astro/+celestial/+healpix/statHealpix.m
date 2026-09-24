function [UniquePix,Result, PixRA, PixDec] = statHealpix(NSide, RA, Dec, Vals, Args)
    % Bin sky values onto HEALPix pixels and compute per-pixel statistics.
    %   Map (RA,Dec) samples to HEALPix pixels (via ang2pix). For each
    %   occupied pixel, apply one or more user-defined statistical
    %   functions to selected columns of Vals. Empty pixels are omitted
    %   (sparse output over UniquePix only).
    % Input  : - HEALPix NSide (power of 2).
    %          - RA / longitude of samples (same size as Dec).
    %          - Dec / latitude of samples.
    %          - Matrix of values, one row per (RA,Dec) sample.
    %            Columns are selected per function via 'FunsCol'.
    %          * ...,key,val,...
    %            'Units' - Units of input RA/Dec and of output PixRA/PixDec
    %                   (passed to ang2pix / pix2ang as CooUnits).
    %                   Default is 'deg'.
    %            'Type' - HEALPix numbering scheme: 'nested'|'ring'.
    %                   Default is 'nested'.
    %            'UniqueID' - If true, use unique HEALPix IDs
    %                   (see pix2uniqueId / uniqueId2pix) instead of plain
    %                   pixel indices. Default is false.
    %            --- functions ---
    %            'Funs' - Cell array of function handles applied to the
    %                   values within each pixel. Each Fun must return a
    %                   scalar. Default is {@median, @std}.
    %            'FunsCol' - Cell array parallel to 'Funs'. Element k is the
    %                   column index (or indices) of Vals passed to Funs{k}
    %                   as a single block: Vals(Flag, FunsCol{k}).
    %                   Default is {1, 1}.
    %            'FunsArgs' - Cell array parallel to 'Funs'. Element k is a
    %                   cell of extra arguments: Funs{k}(Vals(...), FunsArgs{k}{:}).
    %                   Default is {{1,'omitnan'}, {0,1,'omitnan'}} for
    %                   median(...,1,'omitnan') and std(...,0,1,'omitnan').
    %                   Use {{}} for a Fun with no extra arguments.
    % Output : - Column vector of unique HEALPix pixel indices that contain
    %            at least one sample (Type / UniqueID as requested).
    %          - Matrix of statistics [Npix x Nfun], where Result(i,j) is
    %            Funs{j} applied to the values in UniquePix(i).
    %          - RA / longitude of UniquePix pixel centers [Units].
    %          - Dec / latitude of UniquePix pixel centers [Units].
    % Author : Eran Ofek (2026 Jul)
    % Example: RA  = 360.*rand(1000,1);
    %          Dec = asind(2.*rand(1000,1)-1);
    %          V   = randn(1000,1);
    %          [Pix,S,PixRA,PixDec] = celestial.healpix.statHealpix(8, RA, Dec, V);
    %          % mean and count, with multi-column Vals:
    %          V2 = [V, abs(V)];
    %          [Pix,S] = celestial.healpix.statHealpix(8, RA, Dec, V2,...
    %                       'Funs',{@mean,@numel},...
    %                       'FunsCol',{1,1},...
    %                       'FunsArgs',{{1,'omitnan'},{}});

    arguments
        NSide
        RA
        Dec
        Vals
        Args.Units             = 'deg';
        Args.Type              = 'nested';
        Args.UniqueID          = false;
        Args.Funs              = {@median, @std};
        Args.FunsCol           = {1, 1};
        Args.FunsArgs          = {{1,'omitnan'}, {0,1,'omitnan'}};
    end

    Nfun = numel(Args.Funs);

    Pix = celestial.healpix.ang2pix(NSide, RA, Dec, 'Type',Args.Type, 'CooUnits',Args.Units, 'UniqueID',Args.UniqueID);

    UniquePix = unique(Pix);
    Nup       = numel(UniquePix);
    Result    = nan(Nup, Nfun);
    for Iup=1:1:Nup
        Flag = (Pix == UniquePix(Iup));
        
        for Ifun=1:1:Nfun
            FunArgs = Args.FunsArgs{Ifun};
            if isempty(FunArgs)
                Result(Iup, Ifun) = Args.Funs{Ifun}(Vals(Flag, Args.FunsCol{Ifun}));
            else
                Result(Iup, Ifun) = Args.Funs{Ifun}(Vals(Flag, Args.FunsCol{Ifun}), FunArgs{:});
            end
        end
    end

    [PixRA, PixDec] = celestial.healpix.pix2ang(NSide, UniquePix, 'CooUnits',Args.Units, 'Type',Args.Type, 'UniqueID',Args.UniqueID);

end
