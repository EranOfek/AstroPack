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


    % testing: imUtil.calib.resid_vs_mag
    % fewer than two bins must not crash (issue #1365)
    for N=[0 1 2 30]
        for Span=[0 0.4 1.4]
            Mag   = 15 + linspace(0,Span,N).';
            Resid = 0.1 + 0.01.*randn(N,1);
            [Flag,Res] = imUtil.calib.resid_vs_mag(Mag, Resid);
            assert(isequal(size(Flag),[N 1]) && isequal(size(Res.InterpMeanResid),[N 1]), 'resid_vs_mag: wrong output size');
            if N>2
                assert(all(Res.InterpMeanResid==median(Resid)), 'resid_vs_mag: wrong single-bin mean');
            end
        end
    end
    % an outlier is flagged in the single-bin case
    Resid(5) = 10;
    Flag = imUtil.calib.resid_vs_mag(Mag, Resid);
    assert(~Flag(5) && sum(Flag)>=N-3, 'resid_vs_mag: single-bin outlier not flagged');
    % the binned case is unchanged
    Mag   = linspace(12,16,40).';
    Resid = abs(randn(40,1));
    [Flag,Res] = imUtil.calib.resid_vs_mag(Mag, Resid);
    assert(all(isfinite(Res.InterpMeanResid)) && any(Flag), 'resid_vs_mag: binned case failed');

    Answer = true;

end