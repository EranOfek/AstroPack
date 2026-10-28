function R = tlogg2picklesClass(Teff,logg)
    % spectral class and luminosity class to be used with AstroSpec.specStarsPickles
    % Description: The luminosity class is set by log(g):
    %                   V: logg >= 3.9, IV: 3.5-3.9, III: 1.5-3.5, II: 1.0-1.5, I: < 1.0,
    %              and the spectral class is that of the template of this luminosity class
    %              with the nearest Teff, among the solar-abundance templates present in the
    %              Pickles library. If the class has no template within 15% of Teff (e.g.
    %              there are no hot II templates), the nearest luminosity class that has
    %              one is used instead (R.Fallback = true).
    %              Template Teff: for V and IV the dwarf scale, for III, II and I the giant
    %              scale below (both interpolated over the spectral subtype for the types
    %              that are not listed), as used by the previous version of this function.
    % Input  : - Teff - eff. temp. in K (a scalar or a vector)
    %          - logg - log(g) (a scalar or a vector)
    % Output : - a structure array R(i,j) for Teff(i) and logg(j), with the fields:
    %            .class    - the spectral class, e.g. 'g2'
    %            .lumclass - the luminosity class, e.g. 'v'
    %            .Fallback - true if the template is taken from a neighbouring luminosity class
    % Author : A.M. Krassilchtchikov (Sep 2023, Oct 2026)
    % Example: R = astro.stars.tlogg2picklesClass(6e3,4.6);
    %          Spec = AstroSpec.specStarsPickles(R.class,R.lumclass);
    % References: https://www.pas.rochester.edu/~emamajek/EEM_dwarf_UBVIJHK_colors_Teff.txt
    %             https://sites.uni.edu/morgans/astro/course/Notes/section2/spectraltemps.html

    if any(~isfinite(Teff(:))) || any(~isfinite(logg(:)))
        error('tlogg2picklesClass: Teff and logg must be finite');
    end

    persistent Name Lum TeffT          % the template table, built once (the function is called per star)
    if isempty(Name)
        [Name, Lum, TeffT] = templateTable();
    end
    LumOrder  = {'v','iv','iii','ii','i'};
    LoggEdges = [3.9 3.5 1.5 1.0];        % the lower log(g) edges of V, IV, III, II
    MaxRelDiff = 0.15;

    for i = 1:numel(Teff)
        for j = 1:numel(logg)
            Ic = find(logg(j) >= LoggEdges, 1);    % 1 = V ... 4 = II
            if isempty(Ic)
                Ic = 5;                            % I
            end
            RelDiff = abs(TeffT ./ Teff(i) - 1);
            % the luminosity classes in order of distance from the star's own class
            [~, Order] = sort(abs((1:5) - Ic) + 0.1.*((1:5) > Ic));   % ties: the higher gravity first
            Best = [];
            for Ik = Order
                Ind = find(strcmp(Lum, LumOrder{Ik}));
                [Dmin, Imin] = min(RelDiff(Ind));
                if Dmin <= MaxRelDiff
                    Best = Ind(Imin);
                    break
                end
            end
            if isempty(Best)                       % no template within 15% at all: the nearest one
                [~, Best] = min(RelDiff);
            end
            R(i,j).class    = Name{Best};
            R(i,j).lumclass = Lum{Best};
            R(i,j).Fallback = ~strcmp(Lum{Best}, LumOrder{Ic});
        end
    end
end

function [Name, Lum, TeffT] = templateTable()
    % the solar-abundance Pickles templates (uk<class><lumclass>.mat) and their Teff [K]
    Avail.v   = {'o5','o9','b0','b1','b3','b57','b8','b9','a0','a2','a3','a5','a7','f0','f2','f5','f6', ...
                 'f8','g0','g2','g5','g8','k0','k2','k3','k4','k5','k7','m0','m1','m2','m2p5','m3','m4','m5','m6'};
    Avail.iv  = {'b2','b6','a0','a47','f02','f5','f8','g0','g2','g5','g8','k0','k1','k3'};
    Avail.iii = {'o8','b12','b3','b5','b9','a0','a3','a5','a7','f0','f2','f5','g0','g5','g8','k0','k1', ...
                 'k2','k3','k4','k5','m0','m1','m2','m3','m4','m5','m6','m7','m8','m9','m10'};
    Avail.ii  = {'b2','b5','f0','f2','g5','k01','k34','m3'};
    Avail.i   = {'b0','b1','b3','b5','b8','a0','a2','f0','f5','f8','g0','g2','g5','g8','k2','k3','k4','m2'};

    % Teff scales [K] by spectral type, as in the previous version of this function
    % (dwarfs: the EEM scale); the subtypes in between are interpolated
    DwarfType  = {'o5','o9','b0','b1','b3','b57','b8','b9','a0','a2','a3','a5','a7','f0','f2','f5','f6', ...
                  'f8','g0','g2','g5','g8','k0','k2','k3','k4','k5','k7','m0','m1','m2','m3','m4','m5','m6'};
    DwarfTeff  = [5.40e4 3.78e4 2.92e4 2.30e4 1.76e4 1.35e4 1.23e4 1.14e4 9.60e3 9.04e3 8.75e3 8.31e3 7.92e3 ...
                  7.35e3 7.05e3 6.70e3 6.55e3 6.30e3 6.05e3 5.80e3 5.66e3 5.44e3 5.24e3 4.96e3 4.80e3 4.60e3 ...
                  4.40e3 4.00e3 3.75e3 3.70e3 3.60e3 3.50e3 3.40e3 3.20e3 3.10e3];
    GiantType  = {'o8','b12','b3','b5','b9','a0','a3','a5','a7','f0','f2','f5','g0','g5','g8','k0','k1', ...
                  'k2','k3','k4','k5','m0','m1','m2','m3','m4','m5','m6','m7'};
    GiantTeff  = [3.00e4 1.40e4 1.28e4 1.15e4 9.70e3 9.40e3 8.80e3 8.30e3 8.00e3 7.50e3 7.20e3 6.80e3 5.80e3 ...
                  5.10e3 5.05e3 4.90e3 4.70e3 4.50e3 4.30e3 4.10e3 3.75e3 3.66e3 3.60e3 3.50e3 3.30e3 3.20e3 ...
                  3.10e3 2.95e3 2.60e3];

    Classes = fieldnames(Avail);
    Name = {}; Lum = {}; TeffT = [];
    for Ic = 1:numel(Classes)
        Types = Avail.(Classes{Ic});
        if any(strcmp(Classes{Ic}, {'v','iv'}))
            Scale = [cellfun(@typeCode, DwarfType); DwarfTeff];
        else
            Scale = [cellfun(@typeCode, GiantType); GiantTeff];
        end
        Code  = cellfun(@typeCode, Types);
        T     = interp1(Scale(1,:), Scale(2,:), min(max(Code, Scale(1,1)), Scale(1,end)), 'linear');
        Name  = [Name, Types];                                   %#ok<AGROW>
        Lum   = [Lum, repmat(Classes(Ic), 1, numel(Types))];     %#ok<AGROW>
        TeffT = [TeffT, T];                                      %#ok<AGROW>
    end
end

function Code = typeCode(Type)
    % a numerical spectral type: O0 = 0, B0 = 10, ... M0 = 60, plus the subtype; the
    % combined subtypes of the library are taken at their middle (b57 = B6, a47 = A5.5,
    % b12 = B1.5, k01 = K0.5, k34 = K3.5, f02 = F1), m2p5 = M2.5
    Letter = 'obafgkm';
    Code   = 10 .* (find(Letter == Type(1)) - 1);
    Sub    = strrep(Type(2:end), 'p', '.');
    if numel(Sub) == 2 && all(isstrprop(Sub, 'digit')) && ~strcmp(Sub, '10')
        Code = Code + (str2double(Sub(1)) + str2double(Sub(2))) ./ 2;
    else
        Code = Code + str2double(Sub);
    end
end
