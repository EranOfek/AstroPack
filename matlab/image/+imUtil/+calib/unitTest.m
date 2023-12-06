function Answer=unitTest
    % unitTest for the imUtil.calib package
    
    
    % testing: imUtil.calib.calibDesignMatrix
    

    MagErr = 0.03;
    Nimage = 50;
    Nstar  = 300;
    Mag    = rand(Nstar,1).*10;
    ZP     = rand(Nimage,1).*2;

    InstMag = ZP + Mag.';
    InstMag = InstMag + MagErr.*randn(size(InstMag));

    H1=imUtil.calib.calibDesignMatrix(Nimage, Nstar,'Sparse',false);
    H=imUtil.calib.calibDesignMatrix(Nimage, Nstar,'Sparse',true);
    
    Par1 = H1\InstMag(:);
    Par = H\InstMag(:);
    
    ParZP = Par(1:Nimage);
    ParM  = Par(Nimage+1:end);
    std(ParZP - ZP)   % should be eq to MagErr/sqrt(Nimage)
    std(ParM  - Mag)  % should be eq to MagErr/sqrt(Nstar)

    Answer = true;
end