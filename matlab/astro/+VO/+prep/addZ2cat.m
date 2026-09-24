function [AllZ,Z] = addZ2cat(Cat, Args)
    % Add redshift to catalog
    % Input  : - Matrix containing catalog.
    %            This matrix containis RA and Dec in radians.
    %          * ...,key,val,... 
    %            'ColRA' - Index of RA column. Default is 1.    
    %            'ColDec' - Index of Dec column. Default is 2.
    %            'Radius' - Search radius [arcsec]. Default is 5.
    %            'ColRadius' - If radius is empty, then will take it from
    %                   this column index in the catalog.
    %                   Default is 4.
    %            'Radius2as' - If Radius is empty, then use this function
    %                   to convert the radius column in the catalog into arcsec.
    %                   Default is @(V) 3.*10.^V
    %            'CatZ'  - Which catsHTM catalogs to use, and the column
    %                   index of the redshift columns.
    %                   Default is 'NEDz', 'SpecSDSSDR17' and 'DESIdr1zpix'
    %
    % Output : - A amtrix of redshift from which catalog and for each
    %            source.
    %          - A vector of best redshift for each source.
    % Author : Eran Ofek (2026 May) 
    % Example: AllZ=VO.prep.addZ2cat(PGC.Cat);

    arguments
        Cat
        Args.ColRA             = 1;
        Args.ColDec            = 2;
        Args.Radius            = 5;
        Args.ColRadius         = 4
        Args.Radius2as         = @(V) 3.*10.^V;

        Args.CatZ              = [];
    end

    CatZ = Args.CatZ;

    if isempty(CatZ)
        I = 1;
        CatZ(I).Name = 'NEDz';
        CatZ(I).ColZ = 3;
        I = I + 1;
        CatZ(I).Name = 'SpecSDSSDR17';
        CatZ(I).ColZ = 9;
        I = I + 1;
        CatZ(I).Name = 'DESIdr1zpix';
        CatZ(I).ColZ = 8;
        
    end

    Nz = numel(CatZ);

    N = size(Cat,1);

    AllZ = nan(N,Nz);
    for I=1:1:N
        RA  = Cat(I,Args.ColRA);
        Dec = Cat(I,Args.ColDec);
        if isempty(Args.Radius)
            Radius = Args.Radius2as(Cat(I,Args.ColRadius));
        else
            Radius = Args.Radius;  % [arcsec]
        end

        for Iz=1:1:Nz
            [CatNED, ColNED] = catsHTM.cone_search(CatZ(Iz).Name, RA, Dec, Radius);
            if ~isempty(CatNED)                
                AllZ(I,Iz) = min(CatNED(:,CatZ(Iz).ColZ));
            end

        end
        
    end
    Z = tools.array.selectFirstNotNaN(AllZ(:,1),AllZ(:,2),AllZ(:,3));

end
