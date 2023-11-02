function [Coef, F] = getClaret2020_LimbDarkeningWD(Teff,Logg, Args)
    % Get the Claret (2020) WD limb darkening coef.
    %   The data is stored in the cats.stars.limbDarkening.LimbDarkeningWD_4par
    %   file and this file should be installed.
    %   Use the Installer class to install the +ctas dir.
    %   To use this coef, use: astro.stars.limbDarkening
    % Input  : - Vector of Eff. Temperature [K].
    %          - Vector of logg (7-8)
    %          * ...,key,val,...
    %            'Filter' - Filter name for which to return the coef.
    %                   G Gb, Gr,   Tes, Ke,   UBVRI, ugrizy
    %                   Default is 'Gb'.
    %            'Model' - 'WD-DA-3D' | 'WD-DB-3D'
    %                   Default is 'WD-DA-3D'.
    %            'Type' - Limb darkening type. Default is '4par'.
    % Output : - A matrix in which the columns represents limb darkening
    %            corf, and lines for each requested T/logg.
    % Author : Eran Ofek (Oct 2023)
    % Example: [Coef, F] = astro.stars.getClaret2020_LimbDarkeningWD(10000,[7 8]);
    
    arguments
        Teff
        Logg
        Args.Filter   = 'Gb';   % G Gb, Gr,   Tes, Ke,   UBVRI, ugrizy
        Args.Model    = 'WD-DA-3D';
        Args.Type     = '4par';
    end
    
    switch Args.Type
        case '4par'
            LD = cats.stars.limbDarkening.LimbDarkeningWD_4par;
            ParList = {'a1','a2','a3','a4'};
        otherwise
            error('Type %s is not supported',Args.Type);
    end
    
    Flag = strcmp(LD.Catalog.Model, Args.Model) & strcmp(LD.Catalog.Filter, Args.Filter);
    
    Npar = numel(ParList);
    N    = max(numel(Teff), numel(Logg));
    Teff = Teff(:).*ones(N,1);
    Logg = Logg(:).*ones(N,1);
    
    Coef = zeros(N, Npar);
    for Ipar=1:1:Npar
        F(Ipar).F = scatteredInterpolant([LD.Catalog.Teff(Flag), double(LD.Catalog.logg(Flag))], LD.Catalog.(ParList{Ipar})(Flag));
    
        Coef(:,Ipar) = F(Ipar).F(Teff, Logg);
    end
end
