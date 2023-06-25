function wget_catalog(Args)
    %

    arguments
        Args.CatURL = 'https://data.desi.lbl.gov/public/edr/spectro/redux/fuji/zcatalog/zall-pix-fuji.fits';
    end


    www.pwget(Args.CatURL,'--no-check-certificate -U Mozilla');

    T=FITS.readTable1('zall-pix-fuji.fits','BreakRepCol',false);

    %%
    
    DESI_EDR=table(T.TARGET_RA, T.TARGET_DEC, T.PMRA, T.PMDEC, T.REF_EPOCH, T.Z, T.ZERR, T.CHI2, char(T.SPECTYPE), char(T.SUBTYPE),...
        T.EBV, T.FIBERFLUX_G, T.FIBERFLUX_R, ...
        T.GAIA_PHOT_G_MEAN_MAG, T.GAIA_PHOT_BP_MEAN_MAG, T.GAIA_PHOT_RP_MEAN_MAG, ...
        T.MASKBITS, T.SERSIC);
    
    DESI_EDR.Properties.VariableNames = {'RA','Dec','PMRA','PMDEC','EPOCH','Z','Zerr','Chi2','SpecType','SubType','Ebv','FiberFlux_g','FiberFlux_r','GAIA_G_MEAN_MAG','GAIA_BP_MEAN_MAG','GAIA_RP_MEAN_MAG',...
        'MaskBits', 'Sersic'};
       

end
