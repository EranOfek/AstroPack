function Result = unitTest()
    % unitTest for CalibImages class

    
    cd /data/euler/archive/AstroPack/data/LAST/TestImages
    
    CI = CalibImages;
    CI.createBias('*_dark.fits');
    
    FlatImages = CI.debias('*_twflat.fits');
    FlatImages.setKeyVal('FILTER','clear');
    
    CI.createFlat(FlatImages(1:5));
    
    Image = AstroImage('LAST.2.1.2_20200821.015622.166_clear_0_science.fits');
    Image.setKeyVal('FILTER','clear');
    Image = CI.processImages(Image, 'SubtractOverscan',false);
    
    
    
    Result = true;
end
