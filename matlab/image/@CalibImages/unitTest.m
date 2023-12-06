function Result = unitTest()
    % unitTest for CalibImages class

    
    HaveLASTimages = false;
    if HaveLASTimages
        cd /data/euler/archive/AstroPack/data/TestImages/LAST

        tic;
        CI = CalibImages;
        CI.createBias('*_dark.fits');

        FlatImages = CI.debias('*_twflat.fits');
        FlatImages.setKeyVal('FILTER','clear');

        CI.createFlat(FlatImages(1:5));
        toc
        

        Image = AstroImage('LAST.2.1.2_20200821.015622.166_clear_0_science.fits');
        Image.setKeyVal('FILTER','clear');
        Image.setKeyVal('SATURVAL',55000);
        Image = CI.processImages(Image, 'SubtractOverscan',false, 'InterpolateOverSaturated',false);
        
        
    end
    
    
    
    Result = true;
end
