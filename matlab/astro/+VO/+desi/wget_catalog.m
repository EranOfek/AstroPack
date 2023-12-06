function DESI_EDR=wget_catalog(Args)
    % wget DESI catalog of spectra
    % Input  : * ...,key,val,...
    %            'CatURL' - default is https://data.desi.lbl.gov/public/edr/spectro/redux/fuji/zcatalog/zall-pix-fuji.fits
    % Output : - AstroCatalog object containing the catalog, sorted by
    %            declination. RA,Dec in radians.
    % Author : Eran Ofek (Jun 2023)

    arguments
        Args.CatURL = 'https://data.desi.lbl.gov/public/edr/spectro/redux/fuji/zcatalog/zall-pix-fuji.fits';
    end
    RAD = 180./pi;

    %This is the link to the catalog of DESI EDR spectra: https://data.desi.lbl.gov/public/edr/spectro/redux/fuji/zcatalog/ and download zall-pix-fuji.fits. Here, you can get the targetid, ra, dec, and nominal classification.
    %Then, to see individual spectra, go to this link (and replace targetid at the end) https://www.legacysurvey.org/viewer/desi-spectrum/edr/targetid39628483684466824
    % Basic statistics:
    %2.8 million entries in .fits file
    %1.8 million objects with good quality

    www.pwget(Args.CatURL,'--no-check-certificate -U Mozilla');

    T=FITS.readTable1('zall-pix-fuji.fits','BreakRepCol',false,'OutClass',[]);

    %%

    DESI_EDR = AstroCatalog;
    DESI_EDR.Catalog = table(T.TARGET_RA, T.TARGET_DEC, T.TARGETID, T.PMRA, T.PMDEC, T.REF_EPOCH, T.Z, T.ZERR, T.CHI2, char(T.SPECTYPE), char(T.SUBTYPE),...
        T.EBV, T.FIBERFLUX_G, T.FIBERFLUX_R, ...
        T.GAIA_PHOT_G_MEAN_MAG, T.GAIA_PHOT_BP_MEAN_MAG, T.GAIA_PHOT_RP_MEAN_MAG, ...
        T.MASKBITS, T.SERSIC);
    
    DESI_EDR.Catalog.Properties.VariableNames = {'RA','Dec','TargetID','PMRA','PMDEC','EPOCH','Z','Zerr','Chi2','SpecType','SubType','Ebv','FiberFlux_g','FiberFlux_r','GAIA_G_MEAN_MAG','GAIA_BP_MEAN_MAG','GAIA_RP_MEAN_MAG',...
        'MaskBits', 'Sersic'};
    
    DESI_EDR.ColNames = DESI_EDR.Catalog.Properties.VariableNames;
    DESI_EDR.Catalog(:,1:2) = DESI_EDR.Catalog(:,1:2)./RAD;

end
