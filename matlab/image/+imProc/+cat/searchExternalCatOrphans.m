function Result=searchExternalCatOrphans(Obj, Args)
    % Search for orphans using MergedCat external catalog information
    %   This function is for the selection of sources that do not appear in
    %   external catalog (orphans). The program works with AstroCatalog
    %   objects, or AstroImage containing AstroCatalog object. The object
    %   must contains columns of MergedCatMask (see
    %   imProc.match.match_catsHTMmerged) and a FLAGS columns (propagated
    %   from the bit mask).
    %   It return a list of candidate orphans.

    % Example: Result = imProc.cat.searchExternalCatOrphans(Coadd(2));

    arguments
        Obj                                             % AstroCatalog | AstroImage
        Args.ColFLAGS              = 'FLAGS';
        Args.ColMergedCatMask      = 'MergedCatMask';
        Args.ColExtra              = {'RA','Dec','SN_1','SN_3','Nobs','PSF_CHI2DOF','MAG_PSF','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS','MAG_APER_3','MAGERR_APER_3','X','Y','X2','Y2','XY'};
        
    end

    Nobj = numel(Obj);

    Result = AstroCatalog;
    for Iobj=1:1:Nobj

        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        else
            % AstroCatalog | AstroTable
            Cat = Obj(Iobj);
        end

        ColFlags       = Cat.getCol(Args.ColFLAGS);
        ColMergedCat   = Cat.getCol(Args.ColMergedCatMask);
        ColExtra       = Cat.getCol(Args.ColExtra);
       
        % select sources with no external catalog match

        
        % select sources with no bad flags

        % save candidates to table:
        % [ColExtra, ColFLAGS, ColMergedMask, Iobj]

        

    end



end
