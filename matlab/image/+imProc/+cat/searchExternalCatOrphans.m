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
    %            'RemoveFlagsSoft' - A cella array of FLAGS to remove, if
    %                   they appear for a source with S/N<SoftSN.
    %                   Default is {'HighRN','DarkHighVal','Hole'}
    %            'SoftSN' - SoftSN is the S/N below to remove sources in
    %                   which on of the RemoveFlagsSoft bits are on.
    %                   Default is 8.
    %            'ColSN' - The column name for the S/N to use for the
    %                   SoftSN argument. Default is 'SN_3'.
    %
    % Output : - An AstroCatalog object with all the transient candidates.
    % Author : Eran Ofek (May 2023)
    % Example: Result = imProc.cat.searchExternalCatOrphans(Coadd(2));

    arguments
        Obj                                             % AstroCatalog | AstroImage
        Args.ColFLAGS              = 'FLAGS';
        Args.ColMergedCatMask      = 'MergedCatMask';
        Args.ColExtra              = {'RA','Dec','SN_1','SN_3','SN_5','Nobs','PSF_CHI2DOF','MAG_PSF','BACK_IM','VAR_IM','BACK_ANNULUS','STD_ANNULUS','MAG_APER_3','MAGERR_APER_3','X','Y','X2','Y2','XY'};
        
        Args.MergedCatBitMask      = 'BitMask.MergedCat.Default';
        Args.RemoveCat             = {'GAIA_DRE3', 'PS1_DR1', 'DECaLS_DR4', 'SDSS_DR10'};
        
        Args.SrcBitMask            = [];
        Args.RemoveFlags           = {'Saturated','NaN','Negative','Spike','CR_DeltaHT','NearEdge','Overlap'};

        Args.RemoveFlagsSoft       = {'HighRN','DarkHighVal','Hole'};
        Args.SoftSN                = 8;
        Args.ColSN                 = 'SN_3';

        Args.ColSN_delta           = {'SN_1','SN_3'};
        Args.SN_delta              = -1;

        Args.RemoveCooNaN logical  = true;
        
    end
    
    
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
    Ncol     = numel(Args.ColExtra) + 4;
    CandCat  = zeros(0, Ncol);
    
    for Iobj=1:1:Nobj

        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
            JD  = Obj(Iobj).julday;
        else
            % AstroCatalog | AstroTable
            Cat = Obj(Iobj);
            JD  = Cat.JD;
        end

        % verify Cat is not empty
        if Cat.sizeCatalog>0
            
            ColFlags       = Cat.getCol(Args.ColFLAGS);
            ColMergedCat   = Cat.getCol(Args.ColMergedCatMask);
            ColExtra       = Cat.getCol(Args.ColExtra);

            % select sources with no external catalog match
            FlagInCat      = MergedCatBD.findBit(ColMergedCat, Args.RemoveCat, 'Method','any');
            % select sources with no bad flags
            FlagBad        = BD.findBit(ColFlags, Args.RemoveFlags, 'Method','any');

            if Args.RemoveCooNaN
                XY = Cat.getXY;
                FlagBadCoo    = isnan(sum(XY,2));
            else
                FlagBadCoo    = false(size(FlagBad));
            end

            if isempty(Args.RemoveFlagsSoft)
                FlagBadSoft = false(size(FlagBad));
            else
                FlagBadSoft = BD.findBit(ColFlags, Args.RemoveFlagsSoft, 'Method','any');
                ColSNsoft   = Cat.getCol(Args.ColSN);
                FlagBadSoft = FlagBadSoft & (ColSNsoft < Args.SoftSN);
            end

            if isempty(Args.ColSN_delta)
                FlagSNdBad     = false(size(FlagBad));
            else
                SNd            = Cat.getCol(Args.ColSN_delta);
                SNd            = SNd(:,1) - SNd(:,2);
                FlagSNdBad     = SNd>Args.SN_delta;
            end

            FlagCand       = ~FlagInCat & ~FlagBad & ~FlagBadCoo & ~FlagBadSoft & ~FlagSNdBad;
            Icand          = find(FlagCand);
            Ncand          = sum(FlagCand);

            % save candidates to table:
            % [ColExtra, ColFLAGS, ColMergedMask, Iobj, Icand]
            if Ncand==0
                VecIobj = zeros(0,1);
            else
                VecIobj = repmat(Iobj,[Ncand, 1]);
            end
            CandCat = [CandCat; [ColExtra(Icand,:), ColFlags(Icand), ColMergedCat(Icand), VecIobj, Icand]];

            %BD.bitdec2name(ColFlags(Icand))
        end
    end

    Result = AstroCatalog;
    Result.Catalog  = CandCat;
    Result.ColNames = [Args.ColExtra, Args.ColFLAGS, Args.ColMergedCatMask, 'Iobj', 'Icand'];

    % Add JD:
    Result.JD = JD;

end
