function Result=unitTest()
    % unitTest for AstroDiff


    % AstroDiff
    AD = AstroDiff;

    cd /marvin/LAST.01.05.01/2023/12/14/proc/000545v0
    AD.Ref = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.000535.997_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');
    cd /marvin/LAST.01.05.01/2023/12/14/proc/002834v0
    AD.New = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.002824.520_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');

    AD.astrometryRefine
    
    % Register the Ref image into the New image (New won't change)
    AD.register;


    % AstroZOGY
    AD = AstroZOGY;

    cd /marvin/LAST.01.05.01/2023/12/14/proc/000545v0
    AD.Ref = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.000535.997_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');
    cd /marvin/LAST.01.05.01/2023/12/14/proc/002834v0
    AD.New = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.002824.520_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');

    AD.astrometryRefine
    
    RR=AD.subAsFunFn

    % Register the Ref image into the New image (New won't change)
    AD.register;


    AD.subtractionD;
    
    AD.subtractionS;

    AD.subtractionS('NormMethod','none')

    AD.translient;
    
    AD.subtractionScorr;


    Result = true;

end
