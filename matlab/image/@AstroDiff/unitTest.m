function Result=unitTest()
    % unitTest for AstroDiff


    %
    AD = AstroDiff;

    %cd /raid/eran/projects/telescopes/LAST/Images_PipeTest/testPipe/LAST.01.02.01/2023/08/27/proc/014050v0/

    AD.Ref = AstroImage.readFileNamesObj('LAST.01.02.01_20230828.014050.716_clear_358+34_001_001_010_sci_proc_Image_1.fits');
    %AD.New = AstroImage.readFileNamesObj('LAST.01.02.01_20230828.014110.723_clear_358+34_002_001_010_sci_proc_Image_1.fits');
    AD.New = AstroImage.readFileNamesObj('LAST.01.02.01_20230828.014710.841_clear_358+34_020_001_010_sci_proc_Image_1.fits');

    cd /marvin/LAST.01.05.01/2023/12/14/proc/000545v0
    AD.Ref = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.000535.997_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');
    cd /marvin/LAST.01.05.01/2023/12/14/proc/002834v0
    AD.New = AstroImage.readFileNamesObj('LAST.01.05.01_20231215.002824.520_clear_TXS0506+056_000_001_010_sci_coadd_Image_1.fits');



    % Register the Ref image into the New image (New won't change)
    AD.register
    AD.subtractionD
    
    Result = true;

end
