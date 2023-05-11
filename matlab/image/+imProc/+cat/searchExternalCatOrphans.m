function Result=searchExternalCatOrphans(Obj, Args)
    % Search for orphans using MergedCat external catalog information
    %   This function is for the selection of sources that do not appear in
    %   external catalog (orphans). The program works with AstroCatalog
    %   objects, or AstroImage containing AstroCatalog object. The object
    %   must contains columns of MergedCatMask (see
    %   imProc.match.match_catsHTMmerged) and a FLAGS columns (propagated
    %   from the bit mask).
    %   It return a list of candidate orphans.
    % Input  : - An AstroCatalog or AstroImage object.
    %            Will search for candidate transients in all elements of
    %            input object.
    %          * ...,key,val,...
    %            'ColFLAGS' - Column name in input object containing FLAGS
    %                   information. Default is 'FLAGS'.
    %            'ColMergedCatMask' - Column name in input object containing MergedCatMask
    %                   information. Default is 'MergedCatMask'.
    %            'ColExtra' - A cell array of column names to extract from
    %                   catalog and to store in the AstroCatalog of transient
    %                   candidates.
    %                   Default is {'RA','Dec','SN_1','SN_3','Nobs','PSF_CHI2DOF','MAG_PSF','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS','MAG_APER_3','MAGERR_APER_3','X','Y','X2','Y2','XY'}
    %            'MergedCatBitMask' - MergedCat bit mask name, or a
    %                   BitDictionary object containing the bit mask
    %                   dictionary. Default is 'BitMask.MergedCat.Default'.
    %            'RemoveCat' - A cell array of catalogs to remove.
    %                   A transient candidate should not appear in these
    %                   catalogs.
    %                   Default is {'GAIA_DRE3', 'PS1_DR1', 'DECaLS_DR4'}.
    %            'SrcBitMask' - A BitDictionary object or name from whicj
    %                   to extract the FLAGS dictionary.
    %                   If empty, read from AstroImage input.
    %                   Default is [].
    %            'RemoveFlags' - A cell array of FLAGS to remove.
    %                   A transient candidate should not have these 
    %                   FLAGS. Default is
    %                   {'Saturated','NaN','Negative','Spike','CR_DeltaHT','NearEdge','Overlap'}.
    %
    % Output : - An AstroCatalog object with all the transient candidates.
    % Author : Eran Ofek (May 2023)
    % Example: Result = imProc.cat.searchExternalCatOrphans(Coadd(2));

    arguments
        Obj                                             % AstroCatalog | AstroImage
        Args.ColFLAGS              = 'FLAGS';
        Args.ColMergedCatMask      = 'MergedCatMask';
        Args.ColExtra              = {'RA','Dec','SN_1','SN_3','Nobs','PSF_CHI2DOF','MAG_PSF','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS','MAG_APER_3','MAGERR_APER_3','X','Y','X2','Y2','XY'};
        
        Args.MergedCatBitMask      = 'BitMask.MergedCat.Default';
        Args.RemoveCat             = {'GAIA_DRE3', 'PS1_DR1', 'DECaLS_DR4'};
        
        Args.SrcBitMask            = [];
        Args.RemoveFlags           = {'Saturated','NaN','Negative','Spike','CR_DeltaHT','NearEdge','Overlap'};
        
    end
    
    Ncol = numel(Args.ColExtra) + 3;
    
    if isa(Obj, 'AstroImage')
        BD = Obj(1).MaskData.Dict;
    else
        if isempty(Args.SrcBitMask)
            error('SrcBitMask must be provided when input is not an AstroImage');
        else
            BD = Args.SrcBitMask;
        end
    end
    
    if ischar(Args.MergedCatBitMask)
        MergedCatBD = BitDictionary(Args.MergedCatBitMask);
    else
        MergedCatBD = Args.MergedCatBitMask;
    end

    Nobj = numel(Obj);

    Result   = AstroCatalog;
    CandCat  = zeros(0, Ncol);
    
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
        FlagInCat      = MergedCatBD.findBit(ColMergedCat, Args.RemoveCat, 'Method','any');
        % select sources with no bad flags
        FlagBad        = BD.findBit(ColFlags, Args.RemoveFlags, 'Method','any');
        

        FlagCand       = ~FlagInCat & ~FlagBad;
        Ncand          = sum(FlagCand);
        
        % save candidates to table:
        % [ColExtra, ColFLAGS, ColMergedMask, Iobj]
        CandCat = [CandCat; [ColExtra(FlagCand,:), ColFLAGS(FlagCand), ColMergedCat(FlagCand), Iobj+ones(Ncand,1)]];
        
    end

    Result = AstroCatalog;
    Result.Catalog  = CandCat;
    Result.ColNames = [Args.ColExtra, Args.ColFLAGS, Args.ColMergedCatMask, 'Iobj'];

end
